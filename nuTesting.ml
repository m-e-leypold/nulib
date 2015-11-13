(* *  Nu.Testing: Ad hoc testing framework                            -*- mode: tuareg -*-   
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

(* THIS IS WORK IN PROGRESS, ONLY. ONLY READ IF YOU CAN'T AVOID TO :-)                  *)

let log = stdout
open Printf
       
type origin = string * int
	    
(* expectation operators: check for condition true/false, unexpected exception, failed comparison/condition *)

type saved_exception = { exn : exn;  backtrace : Printexc.raw_backtrace; origin: origin option; source: bytes option ; }		   
let  save_exception ?origin ?source exn = { exn; backtrace = (Printexc.get_raw_backtrace ()) ; origin; source; }

type capture   = out_channel -> unit
type captures  = string * capture list
					    
type saved_condition = { msg : bytes; backtrace : Printexc.raw_backtrace; origin: origin option; source: bytes option ; captures: captures option }
let  save_condition ?origin ?source ?captures msg = { msg; backtrace = (Printexc.get_callstack 100) ; origin; source; captures }
			 
(* expectation operators evaluate/run a snippet of code. The result of
   that evaluation is one of the following:

   - true   => all expectations met
   - false  => condition failed

   - raises a Failed_condition (this is courtesy of the special condition operators).
   - raises an exception from a positive list (not a failure)
   - raises another exception

   It depends on the expectation operator which of those results are considered a failure (see below). If there
   is a failure, the expectation operator is expected to raise, to force a nonlocal exit:

   - Failed_condition (...)
   - Unexpected_exception (...)
   
   expect_wont_raise is an exception: it will pass the result value of
   the expression executed, the expression is not expected to have
   bool result type.

 *)

					   
exception Unexpected_exception of saved_exception
exception Failed_condition of saved_condition
exception Failed_predicate of captures option
	    
let expect_wont_raise ?origin ?(source:bytes option) (snippet: unit -> 'a) : 'a =
  try   snippet ()
  with
  (* | Failed_predicate (p,c) -> raise (Failed_condition ( save_condition ?origin ?source "failed check" )) *)
  | x -> raise ( Unexpected_exception (save_exception ?origin ?source x))
		   
let expect ?origin ?source f =
  try
    let result = f () in  if (not result) then raise (Failed_condition (save_condition ?origin ?source "condition evaluates to false"))
  with
  | Failed_condition _ as x -> raise_notrace x
  | Failed_predicate captures ->
     raise (Failed_condition ( save_condition ?origin ?source ?captures "failed condition" )) (* don't print message when source *)
  | x -> raise ( Unexpected_exception (save_exception ?origin ?source x))
		  
let expect_no_excn ?origin ?source f =
  try
    let _  = f () in () (* ignore the return value *)
  with
  | Failed_condition _ as x   -> raise_notrace x
  | Failed_predicate captures -> raise (Failed_condition ( save_condition ?origin ?source ?captures "failed condition" )) (* don't print message when source *)     
  | x                          -> raise ( Unexpected_exception (save_exception ?origin ?source x))

  | x -> raise ( Unexpected_exception (save_exception ?origin ?source x))
						       
let ( ***** ) f v = f v  (* a stronger application operator *)
		      		    	    
type test_procedure = unit -> bool
				
type test_result =
  |  Succeeded
  |  Failed_check      of saved_condition
  |  Raised_unexpected of saved_exception
  			    
(* TODO: should have: wild exception / Panic / error *)

type

  test = {
    label          : bytes;
    suite          : suite;
    test_procedure : test_procedure;
    mutable result : test_result option;
  }
	   	   
and
  suite = {
    name  : bytes;
    mutable tests : test list
  }

let init_test label suite test_procedure =  { label; suite; test_procedure; result = None }
let init_suite name = { name ; tests = [] }
let add_test suite label test_procedure =	     
  let t =  { label; suite; test_procedure; result = None } in
  let l = t::suite.tests in
  suite.tests <- l; t

let run_test t =
  (* TODO: improve this! *)
    t.result <-
      try 
	let _ = t.test_procedure () in    
	(Some Succeeded)
      with
      | Failed_condition cond   -> Some (Failed_check cond)
      | Unexpected_exception x  -> Some (Raised_unexpected x)
      | x                       -> Some (Raised_unexpected (save_exception x)) (* should save test origin here? *)

					
let run_suite s run_test =

  (* catch/try, print banners *)
  
  let rec loop tests =
    match tests with
    | []         -> ()
    | test::more -> run_test test; loop more
  in
  loop s.tests

let get_result_lists tests =
  let rec loop tests passed failed not_run =
    match tests with
    | []     -> (List.rev passed, List.rev failed, List.rev not_run)
    | hd::tl -> begin
	match hd.result with
	| None             -> (* count as not_run, but properly it's an error *)  loop tl passed failed (hd::not_run)
	| (Some Succeeded) -> loop tl (hd::passed) failed not_run 
	| Some _           -> (* should differentiate! *) loop tl passed (hd::failed) not_run 
      end		     
  in
  loop tests [] [] [] 

(* TODO: refactor report_* *)
       
let report_unexpected_exception (x:saved_exception) =
  
  fprintf log "%s\n" " *** UNEXPECTED EXCEPTION ***";
  
  ( match x.origin with 
    | None -> fprintf log "   exception: %s.\n  " (Printexc.to_string x.exn);
    | Some origin ->
       fprintf log "%s:%i: unexpected exception" (fst origin) (snd origin);
       fprintf log ": %s.\n" (Printexc.to_string x.exn) );
  
  ( match x.source with
    | None   -> ()
    | Some s -> fprintf log "  in: (%s)\n" s );
  
  fprintf log "\nBacktrace:\n%s\n" (Printexc.raw_backtrace_to_string x.backtrace)

let report_failed_check (x:saved_condition) =
  
  print_string " *** FAILED CHECK ***\n\n";

  ( match x.origin with 
    | None -> fprintf log "   failed check: %s.\n  " x.msg;
    | Some origin ->
       fprintf log "%s:%i: failed check" (fst origin) (snd origin);
       fprintf log ": %s.\n" x.msg );
  
  ( match x.source with
    | None   -> ()
    | Some s -> fprintf log "  condition: (%s)\n" s );

  ( match x.captures with
    | None   -> ()
    | Some (pattern,args) ->
       if (args == [] ) then ()
       else (
	 fprintf log "  failed: %s with\n" pattern;
	 let rec loop n l = match l with 
	   | []      -> ()
	   | c::rest -> ( fprintf log "  $%d = " n; c log; fprintf log "%s\n" ""; loop (n+1) rest )
	 in loop 1 args ));
       
  fprintf log "\nBacktrace:\n%s\n" (Printexc.raw_backtrace_to_string x.backtrace)
    
  (*  fprintf log "%s:%i: unexpected exception from: %s\n" (fst x.origin) (snd x.origin) (x.source) *)
	  
let run_test_wrapper t =
  print_string "=> running test ";
  print_string t.suite.name; print_string "."; print_string t.label;
  print_string " ... ";
  run_test t;
  match t.result with
  | None                       -> print_string " SKIPPED.\n"
  | Some Succeeded             -> print_string " OK.\n"
  | Some(Raised_unexpected  x) -> report_unexpected_exception x					  
  | Some(Failed_check c)       -> (report_failed_check c)
 	              		      
let begin_suite name : suite =
  print_string "\n--------------------------------\n" ;
  print_string "=[ Defining test suite: " ;
  print_string name;
  print_string " ...";
  print_newline ();
  init_suite name

let new_test suite label test_procedure =
  print_string "=> registering test '";
  print_string suite.name; print_string "."; print_string label;
  print_string "'.";
  print_newline ();
  add_test suite label test_procedure

let print_stat_line msg test =
  if List.length test >0 then begin
      print_string "=> ";
      print_string msg;
      print_string " ";
      print_int (List.length test);
      print_string " test(s):";
      let rec print_tests = function
	| []     -> ();
	| hd::tl -> print_string " "; print_string hd.label; print_tests tl
      in
      print_tests test;
      print_string ".\n";      
    end
	   
let finish_suite suite =
  suite.tests <- List.rev suite.tests;
  print_string "=] Done (defining suite '";
  print_string suite.name;
  print_string "').\n";
  print_string "=> Test suite ";
  print_string suite.name;
  print_string " ...\n";  
  run_suite suite run_test_wrapper;
  print_string "--------------------------------\n" ;
  print_string "== Results for suite '";
  print_string suite.name;
  print_string "':\n";
  let (passed, failed, not_run) = get_result_lists suite.tests
  in
  print_stat_line "Passed :" passed;
  print_stat_line "Failed :" failed;
  print_stat_line "Not Run:" not_run;
  print_string "\n";
  if ((List.length failed) > 0) then (exit 1) else ()
						    
(* TODO expect_raises, better reporting and modification *)
			 
       	      		    						       
module With = struct
    
    module List = struct
	type compared_t = List
	include List
	let contains it = List.exists (fun s -> (s=it))
      end
		    
    module Int = struct
	let trace_failure operator opd1 opd2 =
	  print_string "Failed condition: ";
	  print_int opd1;
	  print_string " "; print_string operator; print_string " ";
	  print_int opd2;
	  print_string ".";
	  print_newline ()
			
	let (==) x y =
	  if (not ((==) x y)) then
	    (trace_failure "==" x y; false)
	  else
	    true

	let ( > ) x y =
	  if (not (x > y)) then
	    (raise (Failed_predicate (Some ("$1 > $2",[(fun ch -> fprintf ch  "%d" x); (fun ch -> fprintf ch  "%d" y) ])))) (* more capturing *)
	  else
	    true

	      
      end		  		   
  end

		

  
