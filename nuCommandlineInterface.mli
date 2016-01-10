(* * Nu.CommandLineInterface: A DSL to define commandline parsers     -*- mode: tuareg -*-
 *   (C) 2015 M E Leypold                                                               *
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

(* * Some general types                                                                 *)

type 'params program     = 'params -> int
type 'params transform   = 'params -> 'params
type 'params argv_parser = bytes list -> 'params program

(* * Flag and Option Specification 							*)

type 'params option_spec
val flag   : bytes (* name/label *) -> ?aliases:bytes list -> ('params -> 'params)           -> 'params option_spec
val option : bytes                  -> ?aliases:bytes list -> (bytes -> 'params -> 'params)  -> 'params option_spec

val selftest_split_gnu_option: unit -> unit 
													
(* ** Example:
 * 
 *    With program parameters  
 *           type params = { verbose : bool; outfile : bytes option }							      
 *    write 
 *           flag   "verbose" ~aliases:["v"]  (fun params -> { params with verbose = true })
 *    or 
 *           option "to-file" ~aliases:["-o"] (fun filename params -> { params with outfile = Some filename })
*
 *    where options need to be specified (see below for details)
 *   
 *)

(* * Collecting positional arguments 						        *)

type 'params posargs_collector = bytes list -> 'params transform
					     
val no_posargs : 'a posargs_collector

(* * Errors                                                                             *)

exception Too_many_arguments   of bytes list * bytes
exception Not_enough_arguments of bytes list * bytes					       
exception Unknown_option       of bytes list * bytes * bytes

val too_many_arguments   : bytes list -> bytes -> 'a   (* you can use these from your pos_args collector *)
val not_enough_arguments : bytes list -> bytes -> 'a
							 
(* * Interfaces for a single command		                                        *)
		    
type commandline_interface = bytes list -> int
					     
val single_command_interface :
  defaults:'params
  -> ?options:'params option_spec list 
  -> ?posargs:'params posargs_collector
  -> call:'params program
  -> commandline_interface

(* * Interfaces for multiple commands                                                   *)

type 'params subcommand_spec
			    
val subcommand :
  bytes                       (* the name *)
  -> ?aliases:bytes list
  -> defaults:('common_params -> 'params)
  -> ?options:'params option_spec list
  -> ?posargs:'params posargs_collector
  -> 'params program
  -> 'common_params subcommand_spec
			     
exception Unknown_subcommand of bytes list * bytes * bytes

val multiple_command_interface :
  defaults:'common_params
  -> ?options:'common_params option_spec list
  -> commands:'common_params subcommand_spec list
  -> commandline_interface
       
(* * # 
 * Local Variables:
 * eval: (outline-minor-mode 1)
 * End:
 *) 
