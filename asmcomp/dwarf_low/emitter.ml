(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

type t = {
  emit_expr : [`Byte | `Word |`Long | `Quad | `Sleb128 | `Uleb128] -> string -> unit;
  emit_symbol : string -> string;
  emit_label : Linearize.label -> string;
  emit_label_declaration : label_name:Linearize.label -> unit;
  emit_section_declaration : section_name:string -> unit;
  emit_switch_to_section : section_name:string -> unit;
  target : [ `Other ];
  mutable strings : (string * Linearize.label) list;
}

let create ~emit_expr ~emit_symbol ~emit_label ~emit_label_declaration
           ~emit_section_declaration ~emit_switch_to_section ~target =
    { emit_expr; emit_symbol; emit_label; emit_label_declaration;
    emit_section_declaration; emit_switch_to_section; target;
    strings = [];
  }

let cache_string t s =
  try List.assoc s t.strings
  with Not_found -> begin
    let label = Linearize.new_label () in
    t.strings <- (s, label)::t.strings;
    label
  end

let emit_strings t =
  ListLabels.iter t.strings
    ~f:(fun (s, label_name) ->
          t.emit_label_declaration ~label_name;
          match t.target with
          | `Other ->
            X86_dsl.D.string_d(s))

let emit_symbol t = t.emit_symbol
let emit_label t = t.emit_label
let emit_label_declaration t = t.emit_label_declaration
let emit_expr t = t.emit_expr

let emit_section_declaration t ~section_name =
  t.emit_section_declaration
    ~section_name:(Section_names.name section_name);
  emit_label_declaration t
    ~label_name:(Section_names.starting_label section_name)

let emit_switch_to_section t ~section_name =
  t.emit_switch_to_section ~section_name:(Section_names.name section_name)

let emit_symbol_alias _t ~old_sym:_ ~new_sym:_ = ()

let emit_symbol_to_label_alias t ~old_label ~new_sym =
  let open X86_dsl.D in
  global (t.emit_symbol new_sym);
  str_setvar (t.emit_symbol new_sym, t.emit_label old_label);
  type_ (t.emit_symbol new_sym) "@function"

let target t = t.target