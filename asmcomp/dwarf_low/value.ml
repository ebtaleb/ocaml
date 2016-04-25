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

type t =
  | Byte of int
  | Two_byte_int of int
  | Four_byte_int of Int32.t
  | Eight_byte_int of Int64.t
  | Uleb128 of int
  | Leb128 of int
  (* [Absolute_offset] is 32 bits wide when emitting 32-bit DWARF format
     and 64 bits when emitting 64-bit DWARF.  (Thus potentially a
     different size from the target's address width.)  We check during
     emission that the value is not too large. *)
  | Absolute_offset of Int64.t
  | Offset_from_label of Linearize.label * Section_names.t
  | Offset_from_symbol of string * Section_names.t
  | Reference_from_label of Linearize.label
  (* CR-someday mshinwell: this will need adjusting for cross-compilation
     support *)
  (* Absolute or computed code addresses cannot be wider than the target's
     address width, whether or not we are emitting 32-bit or 64-bit DWARF
     format. *)
  | Code_address of Nativeint.t
  | Code_address_from_symbol of string
  | Code_address_from_label of Linearize.label
  | Code_address_from_label_diff of
      [ `Label of Linearize.label | `Symbol of string
      | `Symbol_plus_offset_in_bytes of string * int ]
    * [ `Label of Linearize.label | `Symbol of string ]
  (* CR mshinwell: remove the following once we probably address CR in
     location_list_entry.ml (to do with boundary conditions on PC ranges). *)
  | Code_address_from_label_diff_minus_8 of
      [ `Label of Linearize.label | `Symbol of string ]
    * string
  | String of string

exception Too_large_for_two_byte_int of int
exception Too_large_for_byte of int

let as_four_byte_int i = Four_byte_int i
let as_eight_byte_int i = Eight_byte_int i

let as_two_byte_int i =
  if not (i >= 0 && i <= 0xffff) then
    raise (Too_large_for_two_byte_int i);
  Two_byte_int i

let as_byte i =
  if not (i >= 0 && i <= 0xff) then
    raise (Too_large_for_byte i);
  Byte i

let as_uleb128 i =
  assert (i >= 0);
  Uleb128 i

let as_uleb128_64 i =
  (* CR mshinwell: see mli *)
  let i = Int64.to_int i in
  assert (i >= 0);
  Uleb128 i

let as_leb128 i =
  Leb128 i

let as_string s =
  String s

let as_absolute_offset o = Absolute_offset o
let as_offset_from_label l ~section = Offset_from_label (l, section)
let as_offset_from_symbol s ~section = Offset_from_symbol (s, section)
let as_reference_from_label l = Reference_from_label l

let as_code_address_from_symbol s =
  Code_address_from_symbol s

let as_code_address_from_label s =
  Code_address_from_label s

(* CR mshinwell: this mangling stuff is crap, and needs to be fixed *)
let as_code_address_from_label_diff s2 s1 =
  Code_address_from_label_diff (s2, s1)

let as_code_address_from_label_diff_minus_8 s2 s1 =
  Code_address_from_label_diff_minus_8 (s2, s1)

let as_code_address p =
  Code_address p

let size =
  (* DWARF-4 standard section 7.6. *)
  let rec uleb128_size i =
    assert (i >= 0);
    if i < 128 then 1
    else 1 + (uleb128_size (i lsr 7))
  in
  let rec leb128_size i =
    if i >= -64 && i < 64 then 1
    else 1 + (leb128_size (i asr 7))
  in
  fun t ->
    let size =
      match t with
      | Byte _ -> 1
      | Two_byte_int _ -> 2
      | Four_byte_int _ -> 4
      | Eight_byte_int _ -> 8
      | Uleb128 i -> uleb128_size i
      | Leb128 i -> leb128_size i
      | String _
        (* Strings are emitted as offsets into .debug_str.  The size of
           DW_FORM_strp depends on the DWARF format (DWARF-4 standard section
           7.4.3). *)
      | Absolute_offset _
      | Offset_from_label _
      | Offset_from_symbol _
      | Reference_from_label _ ->
        (* The size of offsets depends on the DWARF format being emitted, not
           on the target word size.  Ditto for "ref_addr" forms. *)
        begin match Dwarf_format.size () with
        | `Thirty_two -> 4
        | `Sixty_four -> 8
        end
      | Code_address _
      | Code_address_from_symbol _
      | Code_address_from_label _
      | Code_address_from_label_diff _
      | Code_address_from_label_diff_minus_8 _ -> Arch.size_addr
    in
    Int64.of_int size

let emit_directive_for_offset ~emitter =
  match Dwarf_format.size () with
  | `Thirty_two -> `Long
  | `Sixty_four -> `Quad

let emit_directive_for_nativeint ~emitter =
  match Arch.size_addr with
  | 4 -> `Long
  | 8 -> `Quad
  | _ -> failwith "DWARF emitter does not understand Arch.size_addr's value"

let emit_as_native_int datum ~emitter =
  let directive = emit_directive_for_nativeint ~emitter in
  match datum with
  | `Native_int n ->
    Emitter.emit_expr emitter directive (Printf.sprintf "%nd\n" n)
  | `Label label ->
    Emitter.emit_expr emitter directive (Emitter.emit_label emitter label)
  | `Symbol symbol ->
    Emitter.emit_expr emitter directive (Emitter.emit_symbol emitter symbol)

let rec emit t ~emitter =
  match t with
  | Eight_byte_int i ->
    Emitter.emit_expr emitter `Quad (sprintf "0x%Lx" i);

  | Four_byte_int i ->
    Emitter.emit_expr emitter `Long (sprintf "0x%lx" i);

  | Two_byte_int i ->
    Emitter.emit_expr emitter `Word (sprintf "0x%x" i)

  | Byte b ->
    Emitter.emit_expr emitter `Byte (sprintf "0x%x" b)

  | Uleb128 i ->
    Emitter.emit_expr emitter `Uleb128 (sprintf "0x%x" i)

  | Leb128 i ->
    Emitter.emit_expr emitter `Sleb128 (sprintf "0x%x" i)

  | Absolute_offset o ->
    (* CR mshinwell: share with initial_length.ml *)
    if Int64.compare o 0xfffffff0L >= 0 then begin
      failwith "Absolute offset is too large for 32-bit DWARF"
    end;

    let directive = emit_directive_for_offset ~emitter in
    Emitter.emit_expr emitter directive (sprintf "0x%Lx\n" o);

  | Offset_from_label (label, section) ->
    begin match Emitter.target emitter with
    | `Other ->
      let directive = emit_directive_for_offset ~emitter in
      Emitter.emit_expr emitter directive (Emitter.emit_label emitter label)
    end

  | Offset_from_symbol (symbol, section) ->
    begin match Emitter.target emitter with
    | `Other ->
      let directive = emit_directive_for_offset ~emitter in
      Emitter.emit_expr emitter directive (Emitter.emit_symbol emitter symbol)
    end

  | String s ->
    (* Strings are collected together into ".debug_str". *)
    let label = Emitter.cache_string emitter s in
    begin match Emitter.target emitter with
    | `Other ->
      emit (Offset_from_label (label, Section_names.debug_str)) ~emitter
    end

  | Reference_from_label label ->
    let dir = emit_directive_for_offset ~emitter in
    Emitter.emit_expr emitter dir (Emitter.emit_label emitter label)

  | Code_address p ->
    emit_as_native_int (`Native_int p) ~emitter

  | Code_address_from_symbol sym ->
    emit_as_native_int (`Symbol sym) ~emitter

  | Code_address_from_label label ->
    emit_as_native_int (`Label label) ~emitter

  | Code_address_from_label_diff (s2, s1) ->

    let directive = begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint ~emitter
    end in

    let ss2 = begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Symbol_plus_offset_in_bytes (s2, offset) ->
        Printf.sprintf "%s + %d" s2 offset
    | `Label s2 -> Emitter.emit_label emitter s2
    end in

    let min = begin match Emitter.target emitter with
    | `Other -> " - "
    end in

    let ss1 = begin match s1 with
    | `Symbol s1 -> Emitter.emit_symbol emitter s1
    | `Label s1 -> Emitter.emit_label emitter s1
    end in

    Emitter.emit_expr emitter directive (String.concat "" [ss2; min; ss1]);

  | Code_address_from_label_diff_minus_8 (s2, s1) ->

    let directive = begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint ~emitter
    end in

    let ss2 = begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Label s2 -> Emitter.emit_label emitter s2
    end in

    Emitter.emit_expr emitter directive (Printf.sprintf "%s - 1 - %s" ss2 (Emitter.emit_symbol emitter s1))

