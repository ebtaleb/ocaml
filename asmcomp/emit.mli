(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generation of assembly code *)

val fundecl : dwarf:Dwarf.t option -> Linearize.fundecl -> unit
val data: Cmm.data_item list -> unit
(* [begin_assembly] returns: start of code symbol, end of code symbol,
start of data symbol. *)
val begin_assembly: unit -> string * string * string
val end_assembly: Dwarf.t option -> unit

val emit_symbol : string -> string
val emit_label : Linearize.label -> string
val emit_expr : [`Byte | `Word |`Long | `Quad | `Sleb128 | `Uleb128] -> string -> unit

val emit_label_declaration : label_name:Linearize.label -> unit
val emit_section_declaration : section_name:string -> unit
val emit_switch_to_section : section_name:string -> unit
