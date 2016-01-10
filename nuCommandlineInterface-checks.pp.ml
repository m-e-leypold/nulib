(* * nuCommandlineInterface checks.                                   -*- mode: tuareg -*-   
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

(* NOTE: The following tests a far from complete! *)

open FEATURE(nuTesting)
 
(* ----------------------------------------------------------- *)

	    
type params = { verbose : bool; outfile : bytes option }

open NuCommandlineInterface
    
let _ = [ flag   "verbose" ~aliases:["v"]  (fun params -> { params with verbose = true }); 
	  option "to-file" ~aliases:["-o"] (fun filename params -> { params with outfile = Some filename })
	]
	      
let TEST(selftest_split_gnu_option) = begin  (* => should be SELFTEST(...) *)
    EXPECT( try selftest_split_gnu_option (); true with | _ -> false )
  end	      


type params1 = {
    inputfiles  : bytes list;
    outfilename : bytes;
    verbosity   : bool;
    tag_as      : bytes;
  }

let defaults  = { outfilename = "-" ; inputfiles = []; verbosity = false; tag_as = "temp" }
let preset1   = { defaults with verbosity = true; outfilename="/tmp/foo"; tag_as = "foo"  }
let save_area = ref defaults
		 
let single_command =

  single_command_interface
	    
    ~defaults:  defaults
 
    ~options:   [
      flag    "verbose" ~aliases:["v"] (fun p   -> {p with verbosity     = true });
      option  "tag"     ~aliases:["t"] (fun s p -> {p with tag_as        = s    });
      option  "to-file" ~aliases:["o"] (fun s p -> {p with outfilename   = s    });

      (* the following are presets *)

      flag    "x"       (fun p -> preset1);
      
    ]

    ~posargs: ( fun posargs -> fun p -> { p with inputfiles = posargs } )
    ~call:    ( fun p -> save_area := p; 0 )

(* systematic tests begin here *)

let TEST(single_command_interface) = begin
    
    EXPECT( !save_area = defaults ); (* sanity *)
    
    ignore (single_command ["-v"]);
    EXPECT( !save_area = { defaults with verbosity = true });


    ignore (single_command ["-o"; "/tmp/foo"; "-v"; "-tag"; "foo" ]);
    EXPECT( !save_area = { defaults with verbosity = true; outfilename="/tmp/foo"; tag_as = "foo"  });

    (* different ways to supply options *)
    
    let reference = !save_area
    in
    
    ignore (single_command ["-o"; "/tmp/foo"; "-v"; "-t"; "foo" ]);	           (* the aliases work *)
    EXPECT( !save_area = reference );
    ignore (single_command ["-to-file"; "/tmp/foo"; "-verbose"; "-tag"; "foo" ]);  (* x11 style long options *)	
    EXPECT( !save_area = reference );
    ignore (single_command ["--to-file=/tmp/foo"; "--verbose"; "--tag=foo" ]);	   (* gnu style long options *)	
    EXPECT( !save_area = reference );

    (* order of application *)

    ignore (single_command ["-x"; "--verbose"; "--tag=foo" ]);	                   (* options are applied in order of occurrence *)
    EXPECT( !save_area = { preset1 with verbosity = true; tag_as = "foo"; } );     
    ignore (single_command ["-x"; "a"; "b"] );                                     (* posargs are applied after options *)
    EXPECT( !save_area = { preset1 with inputfiles = ["a";"b"] } );

    (* dash and minus *)

    ignore (single_command ["-x"; "-" ; "-a"; "b"] );                              (* '-' is a positional argument *)
    EXPECT( !save_area = { preset1 with inputfiles = ["-";"-a";"b"] } );
    
    ignore (single_command ["-x"; "--" ; "-a"; "b"] );                             (* dash terminates option processing *)
    EXPECT( !save_area = { preset1 with inputfiles = ["-a";"b"] } );               (* everything following is a positional argument *)

    (* error detection *)

    EXPECT(
	try ignore (single_command ["-z"] );  false                                (* unknown option raises *)
	with Unknown_option _ -> true
      );

    EXPECT(
	try ignore (single_command ["-o"] );  false                                (* missing argument raises *)
       	with Not_enough_arguments _        -> true
	   | _                             -> false
      );

    EXPECT(
	try ignore (single_command ["--to-file"; "blubb"] );  false                (* missing argument raises, gnu style *)
       	with Unknown_option _                              -> true                 (* this is parsed as a flag currently *)
	   | _                                             -> false
      );
    	  
    ignore (single_command []);                                                    (* just sanity again *)
    EXPECT( !save_area = defaults )	       
  end
					

type params_common = {
    verbose : bool;
    topdir  : bytes;
  }

let defaults = { verbose = false; topdir = "."; }
		 
type paramsA = {
    common : params_common;
    a      : int;
    pargs  : bytes list;
  }

let defaultsA p = { common = p; a = 0; pargs = [] }
		 
type paramsB = {
    verbose : bool;
    b       : float;
  }

let defaultsB (p:params_common) = { verbose = p.verbose; b = 0.0; }
		 
let last_proc = ref 0
let saved_A   = ref (defaultsA defaults)
let saved_B   = ref (defaultsB defaults)	      	       

let proc_a p = saved_A := p; last_proc := 1; 0
let proc_b p = saved_B := p; last_proc := 2; 0

let main =
  multiple_command_interface
    
    ~defaults:defaults
    
    ~options: [      
    ]
    
    ~commands:[
      
      subcommand "alpha"
		 ~aliases:["a"]
		 ~defaults:defaultsA
		 ~options:[
		   option "p" (fun s p -> { p with a = (int_of_string s) } )
		 ]
		 ~posargs:(fun args p -> { p with pargs = args } )
		 proc_a  ; 

      subcommand "beta"
		 ~aliases:["b"]
		 ~defaults:defaultsB
		 ~options:[
		   option "q" (fun s p -> { p with b = (float_of_string s) } )
		 ]
		 ~posargs:no_posargs
		 proc_b		 
    ]
				       
let TEST(multiple_command_interface) = begin

    ignore (main ["alpha"]);    (* is dispatch working? *)    
    EXPECT( !last_proc = 1 );
    ignore (main ["beta"]);
    EXPECT( !last_proc = 2 );

    ignore (main ["a"]);        (* do aliases work? *)    
    EXPECT( !last_proc = 1 );
    ignore (main ["b"]);
    EXPECT( !last_proc = 2 );

    ignore (main ["a";"-p";"50";"-p";"7";"x";"y"] );                (* are command specific options applied? *)
    EXPECT( !saved_A = { common = defaults; a = 7; pargs = ["x";"y"] } );

    ignore (main ["b";"-q";"7.5";"-q";"3.5"] );
    EXPECT( !saved_B = { verbose = false ; b = 3.5 } );
    
  end
