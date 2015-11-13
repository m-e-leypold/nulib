(* *  Nu.System: A DSL for calling external programs                  -*- mode: tuareg -*-
 *    (C) 2015 M E Leypold                                                              *
 *                                                                                      *       
 * This program is free software: you can redistribute it and/or modify                 *
 * it under the terms of the GNU General Public License as published by                 *
 * the Free Software Foundation, either version 3 of the License, or                    *
 * (at your option) any later version.                                                  *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful,                      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of                       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                        *
 * GNU General Public License for more details.                                         *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License                    *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.                *
 *                                                                                      *
 *
 *)

(* * Types & Constructors                                                               *)

module Script : sig                                    (* basic construction of scripts *)

    (* A script is represented by an opaque type Script.t. Scripts can be create from strings with
       Script.verbatim. There the user has to ensure that the strings is properly quoted. This is useful for short shell
       commands that will be embeded in a program. In the general case Script.from_words will be the better choice:
       Every word will be properly quoted.

       Examples:

          Expression                        Resulting/equivalent command line
          ----------

          Script.verbatim "ls -l"          =>   ls -l
          Script.verbatim "ls -l *"        =>   ls -l *
          Script.verbatim "ls -l '*'"      =>   ls -l '*'
          Script.verbatim "ls -l  \"*"     =>   ls -l "*           # ! not a valid commandline ('"' is not closed)

          Script.from_words ["ls";"-l"]    =>   ls -l
          Script.from_words ["ls";"*"]     =>   ls '*'
          Script.from_words ["ls";"-l *"]  =>   ls '-l *'

       'script' (see below) has been defined to be an abbreviation for 'Script.from_words'.
     
     *)
    
    type t
	   
    val  from_words : ?chdir:bytes -> bytes list  -> t	       
    val  verbatim   : ?chdir:string -> bytes -> t

    (* Script.to_string allows to extract a string command line (as is later passed to system() by the run operators)
       from a script.  This is mostly there for debugging and tracing, definitely not for manipulating the string and
       later wrapping it into the Script.t again with Script.verbatim. Don't do that.
     *)
						  
    val  to_string  : t             -> bytes
					 
    (* Tokens can be appended to a script. A token is, basically, a function that modifies an existing script. Tokens
       can only be created with the functions given below, which take care of proper quoting. *)
					 
    type token = t -> t
  end

val  script : ?chdir:bytes -> bytes list  -> Script.t (* = Script.from_words*)
					       
(* * Running scripts                                                                    *)

module Run : sig

    (* Scripts can be run and their output obtained by applying run operators. A run operator takes a script and returns
       a value. If an error occurs a Run.Error exception will be raised.
     *)

    type 'a operator = Script.t -> 'a
    exception Error of string * string * Unix.process_status

    (* run the script, check the exit code. If it is not 0 and not in the ignore_exit list, raise a Run.Error *)
					   
    val and_check_exitcode : ?ignore_exit:int list -> unit -> unit operator
								   
    (* run the script, collect and return the standard output, check the exit code. If it is not 0 and not in the
       ignore_exit list, raise a Run.Error 
     *)
								   
    val and_get_stdout     : ?ignore_exit:int list -> unit -> string list operator

    (* run and return the exitcode. A Run.Error will only be raised if the process got terminated by a signal. You'll
       have to check the exitcode yourself, if one or more possible exit codes indicate an error. 
     *)
								     
    val and_get_exitcode   : int operator
				 
				 (* TODO: get stdout + exitcode *)
  end
						 
val run          : ?ignore_exit:int list -> unit -> unit Run.operator         (* short for: Run.and_check_exitcode *)
val get_stdout   : ?ignore_exit:int list -> unit -> string list Run.operator  (* short for: Run.and_get_stdout     *)
val get_exitcode : int Run.operator                                           (* short for: Run.and_get_exitcode   *)

		       
(* * Specifying Arguments                                                               *)
(* ** Positional Arguments                                                              *)
		       
val arg  : bytes      -> Script.token
val args : bytes list -> Script.token

(* ** Options                                                                           *)

val option  : bytes -> bytes option -> Script.token
val flag    : bytes -> bool -> Script.token					
val goption : bytes -> bytes option -> Script.token
val gflag   : bytes -> bool -> Script.token

(* ** Argument type annotations + conversion 						*)
				 
module Arg_types: sig

    (* Operators + vocabulary to provide types fopr option arguments 

       Examples:

         open Arg_types

         let _ = script ["foo";"bar"] 
                 |> goption "block-size" (500 >: int) 
                 |> run ()

         let geometry = new_type_spec (fun (x,y) -> (string_of_int x) ^ "x" ^ (string_of_int y)

         let _ = script ["xfoo";"-restore"]
                 |> option "geometry" ( (200,300) >: geometry )
     *)

    type 'a type_spec	    
    val  new_type_spec : ('a -> bytes) -> 'a type_spec        (* create new type_spec from conversion procedure *)
    val ( >: )         : 'a -> ('a type_spec) -> bytes option (* 'is_a' annotation operator *)

    val int   : int   type_spec
    val float : float type_spec 

    val optional : ('a type_spec) -> (('a option) type_spec)
  end

(* ** Fancy syntax 									*)
		    
module Fancy_syntax : sig

    (* fancy syntax for script construction that resembles shell script more closely. I'm actually not totally convinced
       of the merits of this sub module. So it might go some time in future.

       Example:

       script ["ls","-l"]
         |-- ("block-size",500 >: int)
         |-+ "/etc"
         |>  stdout to_file "/tmp/foo"
     *)
    
    val ( |--  ) : Script.t -> bytes * bytes option -> Script.t      (* ~ goption *)
    val ( |-?  ) : Script.t -> bytes * bool -> Script.t              (* ~ flag    *)
    val ( |--? ) : Script.t -> string * bool -> Script.t             (* ~ gflag   *)
    val ( |- )   : Script.t -> string * string option -> Script.t    (* ~ option  *)
							 
    val ( |-+ ) : Script.t -> string -> Script.t                     (* ~ arg  *)
    val ( |-@ ) : Script.t -> string list -> Script.t                (* ~ args *)

  end
					    
(* * Redirection             								*)

module Redirect : sig

    (* For redirection we provide some vocabulary that can be combined into a redirection operator. A redirection
       operator is a token that can be appended to a script.
     *)
    
    type operator = Script.token

    (* Examples:
         
         Expression                          Equivalent to       Meaning
         ----------                          -------------       -------

         script |> stderr to_null             ... 2>/dev/null    redirect stderr to /dev/null
         script |> stderr to_stdout           ... 2>&1           redirect stderr to stdout
         script |> output 3 (to_file "foo")   ... 3>"foo"        redirect file descriptor 3 to file "foo"

       Most output redirection expressions are composed of two parts: A specification what to redirect, which is a
       function that as a last argument takes a target specification. Those again are obtained by functions (e.g. 'to_fd
       5') or by some opaque value exported by Redirect.

       The specifications what to redirect are (output n), stderr and stdout.
     *)
		      
    type target
		    
    val output         : int    -> target -> operator
    val stderr         : target -> operator
    val stdout         : target -> operator

    (* The output redirection targets can be specified by the following functions or values.

       Notable is the specification 'closed_output' which means to close the filedescriptor instead of setting it to
       some specific target.
     *)
				     
    val to_fd          : int    -> target
    val to_file        : string -> target
    val append_to_file : string -> target					
				     
    val to_null        : target
    val to_tty         : target
			  
    val to_stdout      : target
    val to_stderr      : target
    val closed_output  : target
			   
    type source

    val input        : int -> source -> operator		    
		    
    val stdin        : source -> operator
				 
    val from_tty     : source
    val from_stdin   : source
    val from_null    : source
    val closed_input : source
			 
    val from_fd      : int    -> source
    val from_file    : string -> source

    (* The following rediretions are basically shortcuts *)
				   
    val closed        : int -> operator       (* close the decriptor for in and output *)
    val closed_stdin  : operator              (* close stdin  *)
    val closed_stdout : operator              (* close stdout *)
    val closed_stderr : operator              (* close stderr *)

    (* the following redirection operators swap two filedescriptor.  In case one would like to catch stdout instead of
       stderr, one would write

       script [...]
         |> swap_stdoe
         |> get_stdout

       There is a shortcut for that: get_stderr.

     *)
			  
    val swap_outputs  : int -> int -> operator              
    val swap_inputs   : int -> int -> operator
    val swap_stdoe    : operator			    (* swap stderr with stdout *)

  end


(* * # *)
(* 
 * Local Variables:
 * fill-column: 120
 * eval: (outline-minor-mode 't)
 * End:
 *)
