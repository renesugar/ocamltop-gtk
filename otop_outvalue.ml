(*********************************************************************************)
(*                OCamltop-gtk                                                   *)
(*                                                                               *)
(*    Copyright (C) 2005-2012 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Define a class to display an Outcometree value. *)

open Outcometree

(** Whether the nodes in the tree are auto-expanded when created. *)
let auto_expand = ref true

(** A box to display Outcometree values. Use the [#coerce] method to pack it.*)
class out_value_box () =
  let wscroll = GBin.scrolled_window
      ~vpolicy: `AUTOMATIC
      ~hpolicy: `AUTOMATIC
      ()
  in
  let cols = new GTree.column_list in
  let col_text = cols#add Gobject.Data.string in
  let (col_value : Outcometree.out_value GTree.column) = cols#add Gobject.Data.caml in

  let store = GTree.tree_store cols in
  let view = GTree.view ~model: store ~packing: wscroll#add_with_viewport () in
  let col = GTree.view_column ()
      ~renderer:(GTree.cell_renderer_text [], ["text", col_text]) in
  let () = ignore (view#append_column col) in

  object (self)
    method coerce = wscroll#coerce

    method string_of_out_ident = function
        Oide_apply (i1, i2) ->
          (self#string_of_out_ident i1)^"("^
          (self#string_of_out_ident i2)^")"
      | Oide_ident s -> s
      | Oide_dot (i,s) ->
          (self#string_of_out_ident i)^"."^s

    method insert_sub parent ov =
      match ov with
      | Oval_array [] -> ()
      | Oval_array ovl -> List.iter (self#insert ~parent) ovl
      | Oval_char _ -> ()
      | Oval_constr (_, ovl) -> List.iter (self#insert ~parent) ovl
      | Oval_ellipsis -> ()
      | Oval_float _ -> ()
      | Oval_int _ -> ()
      |	Oval_nativeint _
      |	Oval_int64 _
      | Oval_int32 _ -> ()
      | Oval_list [] -> ()
      | Oval_list ovl -> List.iter (self#insert ~parent) ovl
      | Oval_printer _ -> ()
      | Oval_record oi_ov_l ->
          List.iter
            (fun (oi, ov) ->
              self#insert ~parent ~pre:((self#string_of_out_ident oi)^" = ") ov)
            oi_ov_l
      | Oval_string _ -> ()
      | Oval_stuff _ -> ()
      | Oval_tuple ovl -> List.iter (self#insert ~parent) ovl
      | Oval_variant (s, ov_opt) ->
          match ov_opt with
            None -> ()
          | Some ov -> self#insert ~parent ov

    method insert ?parent ?(pre="") ov =
      let (label, has_sub) =
        match ov with
        | Oval_array [] -> ("[| |]", false)
        | Oval_array ovl -> ("[| ... |]", true)
        | Oval_char c -> ("'"^(String.make 1 c)^"'", false)
        | Oval_constr (oi, ovl) ->
            (self#string_of_out_ident oi, ovl <> [])
        | Oval_ellipsis -> ("()", false)
        | Oval_float f -> (string_of_float f, false)
        | Oval_int n -> (string_of_int n, false)
        | Oval_nativeint n -> (Nativeint.to_string n, false)
        | Oval_int64 n -> (Int64.to_string n, false)
        | Oval_int32 n -> (Int32.to_string n, false)
        | Oval_list [] -> ("[ ]", false)
        | Oval_list ovl -> ("[ ... ]", true)
        | Oval_printer _ -> ("<printer>", false)
        | Oval_record oi_ov_l -> ("{ ... }", true)
        | Oval_string s -> ("\""^s^"\"", false)
        | Oval_stuff s -> (s, false)
        | Oval_tuple l ->
            let len = List.length l in
            let buf = Buffer.create (len * 4 + 1) in
            Buffer.add_string buf "(...";
            for i = 1 to len - 1 do
              Buffer.add_string buf ",..."
            done;
            Buffer.add_string buf ")";
            (Buffer.contents buf, true)
        | Oval_variant (s, ov_opt) -> ("`"^s, ov_opt <> None)
      in
      let row = store#append ?parent () in
      store#set row col_text label;
      store#set row col_value ov;

      if has_sub then
	(
	 let rr = store#get_row_reference (store#get_path row) in
	 self#collapse_row rr#iter;
	 if !auto_expand then view#expand_row rr#path
	)


    method clear =
      store#clear ()

    method update_data ov =
      self#clear ;
      self#insert  ov

    method expand_row row =
      let rr = store#get_row_reference (store#get_path row) in
      let v = store#get ~row: rr#iter ~column: col_value in
      self#insert_sub rr#iter v;
      ignore (store#remove (store#iter_children (Some rr#iter)))

    method collapse_row row =
      let rr = store#get_row_reference (store#get_path row) in
      (
       try
	 while true do
	   ignore(store#remove (store#iter_children (Some rr#iter)));
	 done
       with _ -> ()
      );
      let row = store#append ~parent: rr#iter () in
      store#set row col_text ""

    initializer
      ignore
        (view#connect#row_expanded
           (fun it _ -> self#expand_row it)
        );
      ignore
        (view#connect#row_collapsed
           (fun it _ -> self#collapse_row it)
        );
      ()
  end
