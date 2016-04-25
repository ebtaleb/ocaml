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

open Std_internal
module Proto_DIE = Proto_die

type t = {
  compilation_unit : Proto_DIE.t;
  mutable abbrev_table_and_dies
    : (Abbreviations_table.t * (Debugging_information_entry.t list)) option;
}

let create ~compilation_unit =
  { compilation_unit;
    abbrev_table_and_dies = None;
  }

(* For each pattern of attributes found in the tree of proto-DIEs (of which
   there should be few compared to the number of DIEs), assign an abbreviation
   code, generating an abbreviations table in the process.  At the same time,
   generate a list of DIEs in flattened format, ready for emission.  (These
   DIEs reference the particular patterns of attributes they use via the
   abbreviation codes.) *)
let assign_abbreviations t =
  if t.abbrev_table_and_dies <> None then begin
    failwith "proto-DIE to DIE conversion may only be performed once"
  end;
  let abbrev_table, dies =
    let next_abbreviation_code = ref 1 in
    Proto_DIE.depth_first_fold t.compilation_unit
      ~init:(Abbreviations_table.create (), [])
      ~f:(fun (abbrev_table, dies) action ->
        let abbrev_table, die =
          match action with
          | `End_of_siblings ->
            abbrev_table, Debugging_information_entry.create_null ()
          | `DIE (tag, has_children, attribute_values, label, name) ->
            (* Note that [Proto_DIE.create] sorted the attribute values,
               ensuring that a simple re-ordering does not cause a new
               abbreviation to be created. *)
            let attributes =
              List.map Attribute_value.attribute attribute_values
            in
            let abbrev_table, abbreviation_code =
              match
                Abbreviations_table.find abbrev_table ~tag ~has_children
                  ~attributes
              with
              | Some abbrev_code -> abbrev_table, abbrev_code
              | None -> 
                let abbreviation_code =
                  Abbreviation_code.of_int !next_abbreviation_code
                in
                incr next_abbreviation_code;
                let abbrev_table_entry =
                  Abbreviations_table_entry.create ~abbreviation_code ~tag
                    ~has_children ~attributes
                in
                Abbreviations_table.add abbrev_table abbrev_table_entry,
                  abbreviation_code
            in
            let die =
              Debugging_information_entry.create ~label ~abbreviation_code
                ~attribute_values ~name
            in
            abbrev_table, die
        in
        abbrev_table, die::dies)
  in
  t.abbrev_table_and_dies <- Some (abbrev_table, List.rev dies)

let dwarf_version = Version.four

let address_width_in_bytes_on_target = Value.as_byte Arch.size_addr

let debug_abbrev_offset t =
  let section = Section_names.debug_abbrev in
  Value.as_offset_from_label (Section_names.starting_label section) ~section

let size_without_first_word t ~dies =
  let (+) = Int64.add in
  let total_die_size =
    List.fold_left dies
      ~init:Int64.zero
      ~f:(fun size die -> size + Debugging_information_entry.size die)
  in
  Version.size dwarf_version
    + Value.size (debug_abbrev_offset t)
    + Value.size address_width_in_bytes_on_target
    + total_die_size

let size t =
  match t.abbrev_table_and_dies with
  | None ->
    failwith "must assign abbreviations before calculating debug info \
              section size"
  | Some (_abbrev_table, dies) ->
    let size_without_first_word = size_without_first_word t ~dies in
    let initial_length = Initial_length.create size_without_first_word in
    Int64.add (Initial_length.size initial_length) size_without_first_word

let emit t ~emitter =
  match t.abbrev_table_and_dies with
  | None ->
    failwith "must assign abbreviations before emitting debug info section"
  | Some (abbrev_table, dies) ->
    let size_without_first_word = size_without_first_word t ~dies in
    let initial_length = Initial_length.create size_without_first_word in
    Initial_length.emit initial_length ~emitter;
    Version.emit dwarf_version ~emitter;
    Value.emit (debug_abbrev_offset t) ~emitter;
    Value.emit address_width_in_bytes_on_target ~emitter;
    List.iter dies ~f:(Debugging_information_entry.emit ~emitter);
    abbrev_table