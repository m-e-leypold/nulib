(* * Nu.CommandLineInterface: A DSL to define commandline parsers -*- mode: tuareg -*-
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
 *)


(* * Some basic types 								    *)

type 'p program            = 'p -> int    (* returns the exit status, usually 0 for OK and != 0 for errors      *)			  
type 'p transform          = 'p -> 'p     (* this is what most of the arguments compile to, at least internally *)
type 'p argv_parser        = bytes list -> 'p program   (* this is the "compiler" *)


(* * Flag and Option Specification 						    *)

type 'p optval_spec = {
    name    : bytes;
    aliases : bytes list; 
    apply   : bytes -> 'p -> 'p
  }

type 'p flag_spec = {
    name    : bytes;
    aliases : bytes list; 
    set     : 'p -> 'p
  }

type 'p option_spec = Option of 'p optval_spec | Flag of 'p flag_spec
let flag   name ?(aliases=[]) set   : 'p option_spec = Flag   { set   ; name; aliases }
let option name ?(aliases=[]) apply : 'p option_spec = Option { apply ; name; aliases }

exception Too_many_arguments   of bytes list * bytes
exception Not_enough_arguments of bytes list * bytes					       

let too_many_arguments args msg   = raise (Too_many_arguments (args, msg ))
let not_enough_arguments args msg = raise (Not_enough_arguments (args, msg ))
			 
let create_lookup_table (options:'p option_spec list) =
  let add_to_assoc_list assoc_list names item =
    List.fold_left (fun assoc_list name -> (name,item)::assoc_list) assoc_list names
  in
  let add_item (option_list,flag_list) item =
    match item with
    | Flag   { name; aliases; set}   -> ( option_list ,(add_to_assoc_list flag_list (name::aliases) set))
    | Option { name; aliases; apply} -> ((add_to_assoc_list option_list (name::aliases) apply), flag_list)
  in
  List.fold_left add_item ([],[]) options

let lookup assoc_list key =
  try
    Some (List.assoc key assoc_list)
  with
  | Not_found -> None

let lookup_excn assoc_list key = (List.assoc key assoc_list)

let is_option arg = (arg.[0] == '-')
let is_gnu_option arg   = ( (is_option arg) && (arg.[1] == '-'))
let non_gnu_label arg   = String.sub arg 1 ((String.length arg)-1)

exception InternalError
	    
let split_gnu_option arg =
  let len = String.length arg in
  let name_start = 2 in
  let name_len   =
    (try (String.index_from arg name_start '=') with Not_found -> len) - name_start
  in
  if (name_len = 0) then raise InternalError (* XXX actually option syntax error *)
  else
    let name = String.sub arg name_start name_len in
    let val_start = name_start + name_len + 1 in
    if (val_start>len) then (name, None)
    else (name, Some (String.sub arg val_start (len - val_start)))

let selftest_split_gnu_option () =
  assert( (split_gnu_option "--alpha")      = ("alpha",None) );
  assert( (split_gnu_option "--alpha=beta") = ("alpha",(Some "beta")));
  assert( (split_gnu_option "--alpha=")     = ("alpha",(Some "")))

exception Unknown_option of bytes list * bytes * bytes
						   
let unknown_option args name   = raise (Unknown_option (args, "unknown option/flag or wrong usage: "^name, name))
	    
let parse_options (option_list, flags_list) args =
  let
    process_gnu_option = function
    | [] -> assert false
    | (arg::rest as args) ->
       let name, maybe_value = split_gnu_option arg in
       try 
	 match maybe_value with	   
	 | Some value -> ((lookup_excn option_list name) value),rest
	 | None       -> (lookup_excn flags_list name),rest 
       with Not_found -> unknown_option args name	      
  and
    process_non_gnu_option = function	
    | [] -> assert false
    | (arg::rest as args) ->       
       let name = non_gnu_label arg in
       match lookup flags_list name with
       | Some transform -> (transform,rest)
       | None ->
	  match lookup option_list name with
	  | None -> unknown_option args name
	  | Some apply ->
	     match rest with
	     | [] -> (not_enough_arguments args ("option "^name^" needs an argument"))
	     | value::rest -> ((apply value), rest)				
				(* XXX somewhere above allow to aggregate single letter flags *)
				
  in  
  let rec loop transform args =
    let ok_done = (transform, args) in
    match args with
    | [ ]        -> ok_done 
    | arg::args' -> begin
	let len = (String.length arg) in
	if len < 2 then ok_done
	else	  
	  match arg.[0], arg.[1] with
	  | '-', '-' when len = 2 -> (transform, args')
	  | '-', c -> let process = (if (c=='-') then process_gnu_option else process_non_gnu_option )
		      in
		      let transform', args = process args in (loop (fun p -> (transform' (transform p))) args)
	  | _       -> ok_done
      end     
  in loop (fun p -> p) args
									
let combine_options (options:'p option_spec list) =  parse_options (create_lookup_table options)


(* * Collecting positional arguments 						    *)

type 'p posargs_collector = bytes list -> 'p transform
					     
let no_posargs = ( function
		   | [] -> (fun x -> x)
		   | _::_ as args -> too_many_arguments args "expected: no arguments" )		      

(* * Combining to command line interfaces 					    *)		   
(* ** Interfaces for a single command		                                    *)
		   
type commandline_interface = bytes list -> int

let single_command_interface (type params)
	    ~(defaults:params)
	    ?(options: params option_spec list = [])
	    ?posargs:(collect : params posargs_collector = no_posargs)
	    ~call:(proc : params program )
    : commandline_interface =

  let parse_options = (combine_options options)
  in
  
  fun (args:bytes list) ->

  (* XXX Parse errors and nice printing should be handled here or in a wrapper. I'm currently
     not too sure how this is done best *)
  
  let (apply_options, rest) =(parse_options) args in
  let params = (defaults |>  apply_options |>  (collect rest)) in
  (proc params)

    

(* ** Interfaces for multiple commands                                              *)

type 'p subcommand_spec = {
    name     : bytes;
    aliases  : bytes list;
    parse    : 'p argv_parser;
  }

let subcommand (type p) (type q)
	       (name:bytes)
	       ?(aliases  :bytes list =[] )
	       ~(defaults :p->q )	       
	       ?(options:  q option_spec list = [])
	       ?posargs:(collect : q posargs_collector = no_posargs)	       
	       (proc : q program )
    : p subcommand_spec	
  =
  
  let parse_options = (combine_options options)
  in
  
  { name  = name; aliases = aliases;
    
    parse = fun args ->
	    let (apply_options, rest) =(parse_options) args in
	    
	    fun params ->
	    let params = (defaults params |>  apply_options |>  (collect rest)) in
	    (proc params)
  }

exception Unknown_subcommand of bytes list * bytes * bytes
						   
let unknown_subcommand args name   = raise (Unknown_option (args, "unknown option/flag or wrong usage: "^name, name))

   					   
let combine_subcommands (type p) (specs: p subcommand_spec list) : p argv_parser = 

  let add_item  assoc_list spec =
    List.fold_left (fun assoc_list name -> (name,spec.parse)::assoc_list) assoc_list ((spec.name)::(spec.aliases))
  in 
  let subcommands_table = List.fold_left add_item [] specs
  in
  let get_subcommand name = List.assoc name subcommands_table
  in 
  begin function
    | []                  -> not_enough_arguments [] "expected subcommand here, found end of arguments"
    | (cmd::rest) as args ->
       match (lookup subcommands_table cmd) with
       | None       -> unknown_subcommand args cmd
       | Some parse -> (parse rest)
  end


let multiple_command_interface (type p)
			       ~(defaults: p)
			       ?(options: p option_spec list = [])
			       ~(commands: p subcommand_spec list)
    
    : commandline_interface

  = let parse_options  = (combine_options options)      in
    let get_subcommand = (combine_subcommands commands) in    
    fun (args:bytes list) ->  
    let (apply_options, rest) = (parse_options) args    in
    let subcommand = get_subcommand rest                in
    let params = (defaults |>  apply_options )          in
    (subcommand params)
    
    

    
(* * # 
 * Local Variables:
 * eval: (outline-minor-mode 1)
 * End:
 *) 
