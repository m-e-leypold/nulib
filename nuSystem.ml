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

(* *  Imports                                                  		  		*)
    
open Unix

(* *  Types + Constructors                                                              *)
(* ** Internally a script is just a buffer,                                             *
 *    to which we append escaped strings (another design decision would have been 
      possible, but sadly has not manifested) 
 *)
       
type script = Buffer.t
let initial_buffer_size = ref 50   (* some time we want to be able to change that -- not yet. *)
let new_buffer ()       = Buffer.create (!initial_buffer_size)
					
(* ** Tools for escaping strings and extending scripts (internal)       		*)
	    
let raw_append_items buf l =
  List.iter (fun s -> Buffer.( add_char buf ' '; add_string buf s )) l		
let shell_escape s = "'" ^ (Str.global_replace (Str.regexp "[']") "'\"'\"'" s) ^ "'"
let append_items buf l = raw_append_items buf (List.map shell_escape l)
let append_item  buf x = append_items buf [x]
					      
let add_preamble buffer opt_dir =
  let open Buffer in
  add_string buffer " set -e; set -u; " ;
  match opt_dir with
  | None   -> ()
  | Some d -> let append = add_string buffer  in
	      List.iter append [" cd "; (shell_escape d); "; "]
			
(* ** Creating new scripts 								*)
		   
module Script      = struct
    
    type t = script
    let to_string script = Buffer.contents script
					   
    let from_words  ?(chdir) (s :bytes list ) = let b = Buffer.create 50 in add_preamble b chdir; append_items b s; b			   
    let verbatim ?(chdir) (s :bytes)	      = let b = new_buffer () in add_preamble b chdir; Buffer.add_string b s; b

    type token     = script -> script
  end
		       
let script     = Script.from_words (* alias for export *)
					  
(* *  Adding to scripts		                                                        *)
(* ** Extension operators (internal)                                                    *)

type extension_operator = Buffer.t -> Buffer.t
								  
(* We only allow to extend scripts by appending stuff: By using this ploy, we can render directly into the buffer
 * and do not need to store single items for later use. Extension operators will be used with the pipelining operator
 * (|>) like this:
 *
 *    script  ["ls";"-l"] 
 *      |> Redirect.stdout Stderr 
 *      |> Redirect.stderr  (ToFile "/dev/null") 
 *      |> run ()
 *
 * Extension operators can be easily created from "appender" functions to Buffer.t using fun extension_operator.
 *
 *)
					
let extension_operator (append_to: Buffer.t -> unit) = fun (script:script) ->     
  let open Buffer in
  add_char  script ' '; (* always insert a spacer *)
  append_to script;     (* append the required payload *)
  script                (* and return script for operator chaining *)
	
(* ** Redirections (sub module) 							*)
	
module Redirect = struct

    type operator = script -> script

    let redirection_operator
	  (part1: Buffer.t -> unit) (part2:string) =
      extension_operator
	(fun script -> (part1 script); Buffer.add_string script part2)	    
	
    type source = unit -> bytes
			    
    let dev_tty_in  = ref "/dev/tty"

    let redir_input n script = Buffer.add_string script (string_of_int n); Buffer.add_char script '<'

    let input n inspec = redirection_operator (redir_input n) (inspec ())

    let stdin        = input 0
    let from_stdin   = fun () -> "&1"
    let from_fd n    = fun () -> "&" ^ (string_of_int n)
    let from_file s  = fun () -> shell_escape s
    let from_tty     = fun () -> !dev_tty_in
    let from_null    = fun () -> "/dev/null"
    let closed_input = fun () -> "&-"
				       			       			  
    type target = unit -> bytes
				
    let tty  = ref "/dev/tty"
		   
    let redir_output n script = Buffer.add_string script (string_of_int n); Buffer.add_char script '>'
			      								    
    let output n outspec = redirection_operator (redir_output n) (outspec ())
						
    let stdout = output 1
    let stderr = output 2
			
    let to_tty           = fun () -> !tty (* might need to be different on other system, so far no way to change *)
    let to_null          = fun () -> "/dev/null"
    let to_stdout        = fun () -> "&1"
    let to_stderr        = fun () -> "&2"
    let to_fd n          = fun () -> "&" ^ (string_of_int n)
    let to_file s        = fun () -> shell_escape s
    let append_to_file s = fun () -> ">"^(shell_escape s)
    let closed_output    = fun () -> "&-"
				       
    let closed n script  = output n closed_output (input n closed_input script)
    let closed_stdin     = stdin  closed_input
    let closed_stdout    = stdout closed_output
    let closed_stderr    = stderr closed_output
				  
				  
    let swap_outputs n m = fun script -> output 9 (to_fd n) script |> output n (to_fd m) |> output m (to_fd 9)
    let swap_inputs n m  = fun script  -> input 9 (from_fd n) script |> input n (from_fd m) |> input m (from_fd 9)
    let swap_stdoe       = swap_outputs 1 2
  end

(* ** Arguments 									*)
		    
let args xs = extension_operator (fun buf -> append_items buf xs)
let arg  x  = args (x::[])

(* ** Options and Flags								        *)
		       
let option label = function
  | None    -> extension_operator (fun _ -> ())
  | Some s  -> extension_operator (fun buf -> append_items buf ["-"^label;s])
				  
let goption label = function
  | None    -> extension_operator (fun _ -> ())
  | Some s  -> extension_operator (fun buf -> append_item buf ("--"^label^"="^s))
				  
let flag label select =
  if select then (extension_operator (fun buf -> append_item buf ("-"^label)))
  else (extension_operator (fun _ -> ()))
	 
let gflag label select =
  if select then (extension_operator (fun buf -> append_item buf ("--"^label)))
  else (extension_operator (fun _ -> ()))

(* ** Argument Type annotations 							*)
			
module Arg_types = struct

    type 'a converter = 'a -> bytes
    type 'a type_spec=  'a -> bytes option
    let  new_type_spec (x : 'a converter) : 'a type_spec  = fun v -> Some (x v)
			  
    let (>:) v (f:'a type_spec) : bytes option = (f v) (* the is_a operator :-) *)
			  
    let int   : int   type_spec  = new_type_spec string_of_int
    let float : float type_spec  = new_type_spec string_of_float
		        
    (* lifting annotations into option types *)
    let optional (t : 'a type_spec) : ('a option) type_spec = function None -> None | Some x -> (x >: t)    
  end
		   		   

(* ** Fancy syntax for options (sub module) 						*)
		     
module Fancy_syntax = struct
    let (|--)  s (l,v) = goption l v s
    let (|--?) s (l,v) = gflag   l v s				
    let (|-)  s (l,v)  = option  l v s
    let (|-?) s (l,b)  = flag l b s
    let (|-+) s a      = arg  a s
    let (|-@) s aa     = args aa s			    

  end


	 
(* *  Running Scripts 									*)
(* ** Internal primitives for running scripts  and checking status 			*)
	 
exception Termination_error of process_status (* used internally *)
	 
let exitcode_from_status = function 
  | WSIGNALED _ | WSTOPPED _ as s -> raise (Termination_error s)
  | WEXITED   e -> e

let check_exitcode ~ignore_exit e  =
  if (e!=0) && not (List.exists (fun k -> e==k) ignore_exit)
  then (raise (Termination_error (WEXITED e)))
	 
let collect_output script : (process_status * string list) = 
  let ch_in = open_process_in (Script.to_string script) in
  let rec loop lines =                                              (* refactor to tail recursion || while *)
    try
      let line =  (input_line ch_in)  in
      loop (line::lines)
    with
      End_of_file -> lines
  in
  let output = List.rev (loop []) in
  ( (close_process_in ch_in), output )
       
(* ** Run (module) 									*)
    
module Run           = struct

    exception Error of string * string * process_status
								  
    (* here the first string is the commandline that has been run, the second an explanatory message and the
     * process_status resulted from the attempt to run the script.
     *
     * We (mostly) use the following function to generate the second string from the process status 
     *)
					   
    let string_from_status = function 
      | WSIGNALED s -> "got signal: " ^ (string_of_int s)
      | WSTOPPED  s -> "stopped by signal: " ^ (string_of_int s)
      | WEXITED   e -> "exit code: " ^ (string_of_int e)						     
					 					   
    type 'a operator = script -> 'a
				   
    let raise_error script status = raise (Error ((Script.to_string script), (string_from_status status), status))

    let run_operator f script =
      try f script with (Termination_error s) -> raise_error script s

    let get_exitcode =
      run_operator ( fun script -> exitcode_from_status (system (Script.to_string script)))
							     
    let run  ?(ignore_exit:int list=[]) () =
      run_operator ( fun script ->
		     (get_exitcode script) |> (check_exitcode ~ignore_exit) )

    let get_stdout ?(ignore_exit:int list=[]) ()  =
      run_operator ( fun script ->
		     let (s,lines) = collect_output script in
		     check_exitcode ~ignore_exit (exitcode_from_status s);
		     lines )
						    				
    let  and_check_exitcode = run
    let  and_get_stdout     = get_stdout
    let  and_get_exitcode   = get_exitcode
  end

let run          = Run.and_check_exitcode
let get_stdout   = Run.and_get_stdout
let get_exitcode = Run.and_get_exitcode
		     


(* * # *)
(* 
 * Local Variables:
 * fill-column: 120
 * eval: (outline-minor-mode 't)
 * End:
 *)
