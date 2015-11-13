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

(* THIS IS WORK IN PROGRESS, ONLY. ONLY READ IF YOU CAN'T AVOID TO :-) *)

type test_procedure = unit -> bool
type test_result

type test = {
  label : bytes;
  suite : suite;
  test_procedure : test_procedure;
  mutable result : test_result option;
}
and suite = { name : bytes; mutable tests : test list; }
val init_test : bytes -> suite -> test_procedure -> test
val init_suite : bytes -> suite
val begin_suite : bytes -> suite
val new_test : suite -> bytes -> test_procedure -> test
val finish_suite : suite -> unit

type origin = string * int

val expect_wont_raise : ?origin:origin -> ?source:bytes -> (unit -> 'a) -> 'a
val expect_no_excn    : ?origin:origin -> ?source:bytes -> (unit -> 'c) -> unit
val expect            : ?origin:string * int -> ?source:string -> (unit -> bool) -> unit

val ( ***** ) : ('a -> 'b) -> 'a -> 'b  

module With :
  sig
    module List :
      sig
        val length : 'a list -> int
        val hd : 'a list -> 'a
        val tl : 'a list -> 'a list
        val nth : 'a list -> int -> 'a
        val rev : 'a list -> 'a list
        val append : 'a list -> 'a list -> 'a list
        val rev_append : 'a list -> 'a list -> 'a list
        val concat : 'a list list -> 'a list
        val flatten : 'a list list -> 'a list
        val iter : ('a -> unit) -> 'a list -> unit
        val iteri : (int -> 'a -> unit) -> 'a list -> unit
        val map : ('a -> 'b) -> 'a list -> 'b list
        val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
        val rev_map : ('a -> 'b) -> 'a list -> 'b list
        val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val fold_left2 :
          ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        val fold_right2 :
          ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        val for_all : ('a -> bool) -> 'a list -> bool
        val exists : ('a -> bool) -> 'a list -> bool
        val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val mem : 'a -> 'a list -> bool
        val memq : 'a -> 'a list -> bool
        val find : ('a -> bool) -> 'a list -> 'a
        val filter : ('a -> bool) -> 'a list -> 'a list
        val find_all : ('a -> bool) -> 'a list -> 'a list
        val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
        val assoc : 'a -> ('a * 'b) list -> 'b
        val assq : 'a -> ('a * 'b) list -> 'b
        val mem_assoc : 'a -> ('a * 'b) list -> bool
        val mem_assq : 'a -> ('a * 'b) list -> bool
        val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
        val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
        val split : ('a * 'b) list -> 'a list * 'b list
        val combine : 'a list -> 'b list -> ('a * 'b) list
        val sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
        val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
        val contains : 'a -> 'a list -> bool
      end
    module Int :
      sig
        val trace_failure : string -> int -> int -> unit
        val ( == ) : int -> int -> bool
	val ( > ) : int -> int -> bool
      end
  end
