(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Graphical front-end to ocaml toplevel. *)

open Outcometree
open Parsetree

let _ = Toploop.set_paths ()
let _ = Toploop.initialize_toplevel_env()


(** {2 Options} *)

let home = try Sys.getenv "HOME" with Not_found -> "";;
let rc_file = Filename.concat home ".ocamltop-gtk" ;;

module C = Configwin

(** {2 Management of elements} *)

type element_kind =
    Class
  | Class_type
  | Module
  | Module_type
  | Value
  | Type
  | Exception

let string_of_element_kind = function
    Class -> "class"
  | Class_type -> "class type"
  | Module -> "module"
  | Module_type -> "module type"
  | Value -> "val"
  | Type -> "type"
  | Exception -> "exception"

type data = {
    mutable ele_name : string ;
    mutable ele_kind : element_kind ;
  }

class param () =
  let out_value_box = new Otop_outvalue.out_value_box () in
  object(self)
    inherit [data] Gtktop.param
    inherit Gtktop_installation.default_main_gui ()

    method about () = GToolbox.message_box Otop_messages.about
      Otop_messages.software_about
    method ini_file = rc_file

    method handle_pattern pos pat =
      match pat.ppat_desc with
        Ppat_any -> []
      |	Ppat_var s ->
          [ { ele_name = s ; ele_kind = Value }, pos ]
      |	Ppat_tuple l ->
          List.flatten (List.map (self#handle_pattern pos) l)
      |	_ -> []

    method handle_structure_item pos i =
      match i.pstr_desc with
        Pstr_type l ->
          List.map
            (fun (s,_) -> ({ ele_name = s; ele_kind = Type }, pos) )
            l
      |	Pstr_value (_, pat_exp_list) ->
          List.flatten (List.map (fun (p,_) -> self#handle_pattern pos p) pat_exp_list)
      |	Pstr_exception (s,_)
      |	Pstr_exn_rebind (s,_) ->
          [ { ele_name = s; ele_kind = Exception }, pos ]
      |	Pstr_class l ->
          List.map
            (fun pci -> { ele_name = pci.pci_name; ele_kind = Class}, pos)
            l
      |	Pstr_class_type l ->
          List.map
            (fun pci -> { ele_name = pci.pci_name; ele_kind = Class_type}, pos)
            l
      |	Pstr_module (s,_) ->
          [ { ele_name = s; ele_kind = Module}, pos ]
      |	Pstr_modtype (s,_) ->
          [ { ele_name = s; ele_kind = Module_type}, pos ]
      |	 _ ->
          []

    method handle_phrase pos p =
      match p with
      |	Ptop_dir _ -> []
      |	Ptop_def items -> List.map (self#handle_structure_item pos) items

    (** Get the characters of an error from a standard error message.
       Cut the first line (with the characters) and return also
       the rest of the message. *)
    method get_error_chars s =
      let characters = "Characters" in
      let lenc = String.length characters in
      let len = String.length s in
      try
        if len > lenc then
          if String.sub s 0 lenc = characters then
            (
             let pos = String.index_from s (lenc + 1) '-' in
             let pos2 = String.index_from s pos '\n' in
             let s1 = String.sub s (lenc + 1) (pos - lenc - 1) in
             let pos_start = int_of_string s1 in
             let s2 = String.sub s (pos + 1) (pos2 - pos - 2) in
             let pos_end = int_of_string s2 in
             let rest = String.sub s (pos2+1) (len - pos2 - 1) in
             (Some (pos_start, pos_end), rest)
            )
          else
            (None, s)
        else
          (None, s)
      with
        _ ->
          (None, s)

    (* Beware that this method does not catch exceptions.*)
    method execute_phrase com phrase =
      ignore(Toploop.execute_phrase true Format.str_formatter phrase);
      let output = Format.flush_str_formatter () in
      let items = List.flatten (self#handle_phrase 0 phrase) in
      (Glib.Convert.locale_to_utf8 output, items)

    method execute s =
      try
        let lexbuf = Lexing.from_string s in
        let phrase = !Toploop.parse_toplevel_phrase lexbuf in
        let (output, items) = self#execute_phrase s phrase in
        (Printf.sprintf "%s\n" output, items)
      with
        e ->
          begin
            (* a specific treatment is done when Location.input_name is ""
               but by default it is set to "_none_"; it may be fixed/changed in
               the future, so let's handle both cases *)
            try Errors.report_error Format.str_formatter e
            with Sys_error _ ->
                Location.input_name := "";
                Errors.report_error Format.str_formatter e
          end;
          let (pos_opt, mes) = self#get_error_chars (Format.flush_str_formatter ()) in
          (
           match pos_opt with
           | None ->
               raise (Gtktop.Error (mes, 0, 0))
           | Some (st,en) ->
               raise (Gtktop.Error (mes, st, en))
          )

    method compare d1 d2 =
      match Pervasives.compare d1.ele_name d2.ele_name with
        0 -> Pervasives.compare d1.ele_kind d2.ele_kind
      | n -> n

    method display_box = Some (out_value_box :> Gtktop.display_box)
    method display_elt =
      Some
        (fun e ->
           match e.ele_kind with
             Value ->
               (
                try
                  let com = e.ele_name^" ;;" in
                  let lexbuf = Lexing.from_string com in
                  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
                  ignore(Toploop.execute_phrase true Format.str_formatter phrase);
                  ignore (Format.flush_str_formatter ())
                with _ ->
                    ()
               )
           | _ -> ()
        )

    method window_title_prefix = Otop_messages.software

    method elts_columns =
        [ "kind", (fun e -> Glib.Convert.locale_to_utf8 (string_of_element_kind e.ele_kind));
          "Name", (fun e -> Glib.Convert.locale_to_utf8 e.ele_name) ;
        ]

    method sourceview_language = "text/x-ocaml"

    initializer
      let print_out_value (fmt:Format.formatter) ov =
        out_value_box#update_data ov;
        while Glib.Main.pending () do
          ignore (Glib.Main.iteration false)
        done
      in
      let old_printer = !Toploop.print_out_value in
      Toploop.print_out_value :=
        (fun fmt ov -> old_printer fmt ov ; print_out_value fmt ov) ;

  end

class gui param =
  object(self)
    inherit Gtktop.gui (param :> data Gtktop.param)
  end


let usage = "Usage: "^Sys.argv.(0)^" <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

let prepare ppf =
  Toploop.set_paths ();
  try List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects)
  with x ->
    try Errors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma"
  then preload_objects := name :: !preload_objects
  else exit
    (
     if prepare ppf then
       (
         let param = new param () in
        let gui = new gui param in
        gui#load_file name; GMain.Main.main (); 0)
     else
       2
    )

open Clflags

let main () =
    Arg.parse [
     "-I", Arg.String(fun dir ->
       let dir = Misc.expand_directory Config.standard_library dir in
       include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-labels", Arg.Clear classic, " Labels commute (default)";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-nolabels", Arg.Set classic, " Ignore labels and do not commute";
     "-nostdlib", Arg.Set no_std_include,
           " do not add default directory to the list of include directories";
     "-principal", Arg.Set principal, " Check principality of type inference";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    F/f enable/disable partially applied function\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Al\" (all warnings but labels enabled)";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Enable or disable fatal warnings according to <flags>\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)";
    ] file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  let param = new param () in
  let _gui = new gui param in
  GMain.Main.main ()
;;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main