(* *  nuSystem-checks: Check Nu.System (DSL for calling external programs) -*- mode: tuareg -*-   
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

open Nu.System       
open Redirect
open Fancy_syntax
open Arg_types
       
open FEATURE(nuTesting)

let echo_args =
  "print_args(){
     while test \"$#\" -gt 0; do
       echo \"$1\"
       shift
     done
   }

   print_args "
  
(* ----------------------------------------------------------- *)

let TEST(construction) = begin (* Does it correctly create scripts? *)
    
    EXPECT( ["hello world"] = ( script ["echo"; "hello"; "world" ] |> get_stdout () ));
    EXPECT( ["hi guys"] = ( Script.verbatim "echo hi guys" |> get_stdout () ));
  end
    
    
let TEST(run_operators) = begin  (* check basic run operators *)

    (* Behaviour of run:
       - raise on exit code != 0 and not on the ignore list
       - raise on signal
     *)
   
    EXPECT_WONT_RAISE( Script.verbatim "exit 0" |> run () ); 
    EXPECT_WONT_RAISE( Script.verbatim "exit 1" |> run ~ignore_exit:[1] () );

    EXPECT( try let () = (Script.verbatim "exit 1" |> run ()) in false with Run.Error _ -> true );
    EXPECT( try let () = (Script.verbatim "exit 2" |> run ~ignore_exit:[1] ()) in false with Run.Error _ -> true );
    
    (* Behaviour of get_stdout:
       - return stdout
       - raise on exit code != 0 and not on the ignore list
       - raise on signal
     *)

    EXPECT( ["hi guys"] = ( Script.verbatim "echo hi guys" |> get_stdout () ));
    
    EXPECT( try ignore (Script.verbatim "exit 1"  |> get_stdout ()); true with Run.Error _ -> true );
    EXPECT( try ignore (Script.verbatim "kill $$" |> get_stdout ()); true with Run.Error _ -> true );

    
    EXPECT_WONT_RAISE( ignore (Script.verbatim "exit 0" |> get_stdout ()) );    
    EXPECT_WONT_RAISE( ignore (Script.verbatim "exit 1" |> get_stdout ~ignore_exit:[1] ()) );
    
    EXPECT( try let _ = (Script.verbatim "exit 2" |> get_stdout ~ignore_exit:[1] ()) in false with Run.Error _ -> true );
    
    (* Behaviour of get_excitcode:
         - return exit code, don't check
         - throw on signal
     *)
    
    EXPECT( (Script.verbatim "exit 1"  |> get_exitcode) = 1 );
    EXPECT( (Script.verbatim "exit 0"  |> get_exitcode) = 0 );
    
    EXPECT( try  let _ = (Script.verbatim "kill $$" |> get_exitcode) in false
	    with Run.Error _ -> true )

  end
     
let TEST(quoting) =
  begin
    EXPECT(   					(* check arg/args, proper quoting and get_stdout() *)	
	["hello";"foo";"bar";"ba'al";"*"]  
	= ( Script.verbatim echo_args |> arg  "hello" |> args ["foo";"bar";"ba'al";"*"]  |> get_stdout () )		     
      );
  end

let TEST(redirection) =
  begin
    EXPECT( ["hello"] = ( Script.verbatim "echo hello" |> get_stdout () ) );
    EXPECT( []        = ( Script.verbatim "echo 1>/dev/null hello" |> get_stdout () ) );
    EXPECT( ["hello"] = ( Script.verbatim "( echo 1>&2 hello )" |> swap_stdoe |> get_stdout () ) );
  end
    
    
