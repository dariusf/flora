


type 'a ast = Node of 'a * string * 'a ast list

module Json = struct

  type t = Number | Object | Array

  let num n = Node (Number, string_of_int n, [])
  let array xs = Node (Array, "[]", xs)
  let obj kvs =
    Node (Object, "{}", kvs)

  let test = array [num 1; num 2]

end

(*
open LTerm_geom

let ( >>= ) = Lwt.( >>= )

(* helper functions *)
let make_key ?(ctrl = false) ?(meta = false) ?(shift = false) c =
  let code =
    match c with
    | `Char c -> LTerm_key.Char (CamomileLibrary.UChar.of_char c)
    | `Other key -> key in
  { LTerm_key.control = ctrl; meta; shift; code }

let frame widget =
  let frame = new LTerm_widget.frame in
  frame#set widget;
  frame

let main () =
  let waiter, wakener = Lwt.wait () in

  let ctrl_c = [make_key ~ctrl:true @@ `Char 'c'] in
  let tab = [make_key @@ `Other LTerm_key.Tab] in
  let quit = [LTerm_edit.Custom (Lwt.wakeup wakener)] in

  let vbox = new LTerm_widget.vbox in

  let top_editor = new LTerm_edit.edit () in
  let top_frame = frame top_editor in

  (* make bottom editor a fixed 10 rows in size *)
  let bottom_editor = new LTerm_edit.edit ~size:{ rows = 10; cols = 1 } () in
  (* changed my mind: make it 5 rows smaller *)
  bottom_editor#set_allocation
    { bottom_editor#allocation with row1 = bottom_editor#allocation.row1 - 5 };
  let bottom_frame = frame bottom_editor in

  vbox#add top_frame;
  (* in versions before PR#42 this would either crash or make the bottom editor unusable *)
  vbox#add ~expand:false bottom_frame;

  (* exit on C-c *)
  top_editor#bind ctrl_c quit;
  bottom_editor#bind ctrl_c quit;

  let send_key key =
    LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in

  (* switch editors on Tab *)
  top_editor#bind tab [send_key @@ `Other LTerm_key.Down];
  bottom_editor#bind tab [send_key @@ `Other LTerm_key.Up];

  let label = new LTerm_widget.label "Press Tab to switch between editors.\nPress C-c to exit." in
  vbox#add ~expand:false label;

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term ~save_state:false ~load_resources:false vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let main () =
  print_endline "lol";
  Lwt_main.run (main ())
*)

(*
 * focus.ml
 * ----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open Lwt
open LTerm_widget

let mode = try Sys.argv.(1) with _ -> "none"

let main () = 
  let waiter, wakener = wait () in

  let vbox = new vbox in

  let top = new button mode in

  let leftright = new hbox in
  let left = new button "left" in
  let right = new button "right" in
  let glue = new t "glue" in
  leftright#add ~expand:false left;
  leftright#add glue;
  leftright#add ~expand:false right;

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  vbox#add top;
  vbox#add ~expand:false leftright;
  vbox#add ~expand:false exit;

  (* we have a layout like
     [      top        ]
     [l][...........][r]
     [      exit       ]
     Focus will start in 'top'.  With no focus specifications when we press down
     focus will move to exit.  There's no way to get to the 'left'/'right' buttons.
     This is because lambda-term will search in a line down from the centre of top,
     through the 'glue' and hit exit.
     We can fix this two ways.  In the "set" mode when 'top' is focussed and down is 
     pressed we jump to 'left'.  In "glue" mode when we search down though the 'glue' 
     widget it points to the 'right' button and we jump there. 

     Finally, in "error" mode an exception is raised as focus is set to a widget with 
     can_focus=false.
  *)
  begin
    match mode with
    | "set" -> top#set_focus { top#focus with LTerm_geom.down = Some(left :> t) }
    | "glue" -> glue#set_focus { glue#focus with LTerm_geom.down = Some(right :> t) }
    | "error" -> top#set_focus { top#focus with LTerm_geom.left = Some(glue :> t) }
    | _ -> ()
  end;

  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let main () = Lwt_main.run (main ())


