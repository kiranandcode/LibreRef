(* * License *)
(*
    LibreRef is a free as in freedom digital referencing tool for artists.
    Copyright (C) <2021>  <Kiran Gopinathan>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

Also add information on how to contact you by electronic and paper mail.

  If your software can interact with users remotely through a computer
network, you should also make sure that it provides a way for users to
get its source.  For example, if your program is a web application, its
interface could display a "Source" link that leads users to an archive
of the code.  There are many ways you could offer source, and different
solutions will be better for different programs; see section 13 for the
specific requirements.

  You should also get your employer (if you work as a programmer) or school,
if any, to sign a "copyright disclaimer" for the program, if necessary.
For more information on this, and how to apply and follow the GNU AGPL, see
<https://www.gnu.org/licenses/>.
*)

(* * Definitions *)
(* ** Module Types *)
module type RUNTIME_CONTEXT = sig
  val w: GWindow.window
  val d: GMisc.drawing_area
end

module type LOGIC = sig
  val clear_scene: unit -> unit
  val is_scene_dirty: unit -> bool
  val current_scene_name: unit -> string option
  val get_title: unit -> string
  val add_files_to_scene: float * float -> string list -> string list
  val open_scene_from_file: string -> string list
  val save_scene_as: string -> string list
end  

module type DIALOG = sig
  val handle_quit_application : unit -> unit
  val show_right_click_menu : GdkEvent.Button.t -> unit
  val show_errors : string list -> unit
end

module type UI = sig
  val expose: Cairo.context -> bool
  val on_move: GdkEvent.Motion.t -> bool
  val on_button_release: GdkEvent.Button.t -> bool
  val on_button_press: GdkEvent.Button.t -> bool
  val on_scroll: GdkEvent.Scroll.t -> bool
end

(* ** Implementations *)
module Filter = struct

  let is_prefix s1 s2 =
    let l1 = String.length s1 and l2 = String.length s2 in
    l1 <= l2 && s1 = String.sub s2 0 l1

  let all_file_filter () =
    let f = GFile.filter ~name:"All" () in
    f#add_pattern "*" ;
    f

  let scene_file_filter () =
    let f = GFile.filter ~name:"Scene files" () in
    f#add_pattern "*.libreref" ;
    f

  let image_filter () =
    let f = GFile.filter ~name:"Images" () in
    f#add_custom [ `MIME_TYPE ]
      ~callback:(fun info ->
          let mime = List.assoc `MIME_TYPE info in
          is_prefix "image/" mime);
    f

end

module BuildDialogs (RuntimeCTX : RUNTIME_CONTEXT)  (Logic: LOGIC) = struct

  let queue_draw () =
    GtkBase.Widget.queue_draw (GtkBaseProps.Widget.cast RuntimeCTX.d#as_widget);
    RuntimeCTX.w#set_title (Logic.get_title ())

  let show_errors = function [] -> () | errors ->
    let message =
      "While processing the requested action, ran into the following errors:\n\t - " ^
      String.concat "\n\t - " errors in
    let dialog =
      GWindow.message_dialog
        ~message
        ~message_type:`ERROR
        ~buttons:GWindow.Buttons.ok
        ~parent:RuntimeCTX.w ~title:"Libre-Ref - Non-fatal Error"
        ~urgency_hint:true ~icon_name:"dialog-error" () in
    begin match dialog#run () with
      | `OK | `DELETE_EVENT -> ()
    end;
    dialog#destroy ()

  let ask_save_file action ~do_save ~then_ =
    let dialog =
      let buttons = GWindow.Buttons.yes_no in
      let message =
        Printf.sprintf 
          "There are unsaved changes in the current scene. Save current scene before %s?" action in
      let title = "Save modifications to file?" in
      GWindow.message_dialog
        ~buttons ~message ~title
        ~message_type:`QUESTION
        ~type_hint:`DIALOG () in
    let result = dialog#run () in
    dialog#destroy ();
    match result with
    | `DELETE_EVENT -> ()
    | `NO -> then_ ()
    | `YES -> do_save (); then_ ()

  let ask_for_image_file callback =
    let dialog = GWindow.file_chooser_dialog
        ~action:`OPEN
        ~title:"Open image file"
        ~parent:RuntimeCTX.w () in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_select_button_stock `OPEN `OPEN;
    dialog#set_select_multiple true;
    dialog#add_filter (Filter.image_filter ());
    dialog#add_filter (Filter.all_file_filter ());
    let filenames = match dialog#run () with
      | `OPEN -> dialog#get_filenames
      | `DELETE_EVENT | `CANCEL -> [] in
    dialog#destroy ();
    callback filenames


  let ask_for_libreref_filename title action button_stock ~then_:callback =
    let dialog = GWindow.file_chooser_dialog
        ~action ~title
        ~parent:RuntimeCTX.w () in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_select_button_stock (fst button_stock) (snd button_stock);
    dialog#set_select_multiple false;
    dialog#add_filter (Filter.scene_file_filter ());
    dialog#add_filter (Filter.all_file_filter ());
    let filename = match dialog#run () with
      | `OPEN -> dialog#filename
      | `SAVE -> dialog#filename
      | `DELETE_EVENT | `CANCEL -> None in
    dialog#destroy ();
    match filename with Some filename -> callback filename | None -> ()

  let ask_user_for_filename_to_save_file_as ~then_:save_file_as_name =
    ask_for_libreref_filename
      "Save scene to file" `SAVE (`SAVE_AS, `SAVE)
      ~then_:(save_file_as_name)

  let save_current_scene ~current_filename save_file_as =
    match current_filename () with
    | Some name -> save_file_as name
    | None ->
      ask_user_for_filename_to_save_file_as  ~then_:save_file_as

  let save_user_changes_before query ~any_changes ~do_save ~then_ =
    if any_changes ()
    then ask_save_file query ~do_save ~then_
    else then_ ()

  let handle_load_images ~then_ =
    ask_for_image_file then_

  let handle_load_scene ~any_changes ~do_save load_file_from =
    save_user_changes_before "opening a new scene"
      ~any_changes ~do_save
      ~then_:(fun () ->
          ask_for_libreref_filename
            "Open scene from file" `OPEN (`OPEN, `OPEN)
            ~then_:load_file_from
        )

  let handle_quit_application ~any_changes ~do_save =
    save_user_changes_before "quitting"
      ~any_changes ~do_save
      ~then_:GMain.quit

  let handle_new_scene ~any_changes ~do_save new_scene =
    save_user_changes_before "creating a new scene"
      ~any_changes ~do_save
      ~then_:new_scene

  let handle_save_scene () =
    save_current_scene
      ~current_filename:Logic.current_scene_name
      (fun filename ->
         let errors = Logic.save_scene_as filename in
         queue_draw ();
         show_errors errors)

  let handle_new_scene () =
    handle_new_scene
      ~any_changes:Logic.is_scene_dirty
      ~do_save:handle_save_scene
      (fun () -> Logic.clear_scene (); queue_draw ())

  let handle_save_as () =
    ask_user_for_filename_to_save_file_as
      ~then_:(fun filename ->
          let errors = Logic.save_scene_as filename in
          queue_draw ();
          show_errors errors)

  let handle_load_scene () =
    handle_load_scene
      ~any_changes:Logic.is_scene_dirty
      ~do_save:handle_save_scene
      (fun file ->
         let errors = Logic.open_scene_from_file file in
         queue_draw ();
         show_errors errors)

  let handle_load_images button () =
    let x,y = GdkEvent.Button.x button, GdkEvent.Button.y button in
    handle_load_images ~then_:(fun files ->
        let errors = Logic.add_files_to_scene (x,y) files in
        queue_draw ();
        show_errors errors)

  let handle_quit_application () =
    handle_quit_application
      ~any_changes:Logic.is_scene_dirty
      ~do_save:handle_save_scene

  let handle_settings () =
    let window = GWindow.window
        ~type_hint:`UTILITY
        ~decorated:true ~deletable:true
        ~kind:`TOPLEVEL
        ~border_width:10
        ~title:"LibreRef Settings"
        () in
    let notebook = GPack.notebook ~packing:window#add () in
    let button =
      GButton.button ~label:"Page 1" ~packing:(fun w -> ignore (notebook#append_page w)) () in
    ignore @@ button#connect#clicked ~callback:
      (fun () -> prerr_endline "Hello again - cool button 1 was pressed");

    let button = GButton.button ~label:"Page 2" 
        ~packing:(fun w -> ignore (notebook#append_page w))
        () in
    ignore @@ button#connect#clicked ~callback:
      (fun () -> prerr_endline "Hello again - cool button 2 was pressed");
    ignore @@ notebook#connect#switch_page 
      ~callback:(fun i -> prerr_endline ("Page switch to " ^ string_of_int i));
    ignore @@ button#connect#clicked ~callback:
      (fun () -> prerr_endline "Coucou");

    window#show ();
    ()

  let show_right_click_menu button =
    let menu = GMenu.menu () in
    let new_scene_w = GMenu.menu_item ~label:"New scene" () in
    menu#add new_scene_w;
    ignore @@ new_scene_w#connect#activate
      ~callback:handle_new_scene;

    let save_scene_w = GMenu.menu_item ~label:"Save scene" () in
    menu#add save_scene_w;
    ignore @@ save_scene_w#connect#activate ~callback:handle_save_scene;


    let save_scene_w = GMenu.menu_item ~label:"Save scene as" () in
    menu#add save_scene_w;
    ignore @@ save_scene_w#connect#activate
      ~callback:handle_save_as;

    let open_scene_w = GMenu.menu_item ~label:"Load scene" () in
    menu#add open_scene_w;
    ignore @@ open_scene_w#connect#activate
      ~callback:handle_load_scene;

    let load_image = GMenu.menu_item ~label:"Open image(s)" () in
    menu#add load_image;
    ignore @@ load_image#connect#activate
      ~callback:(handle_load_images button);

    let settings = GMenu.menu_item ~label:"Configure LibreRef" () in
    menu#add settings;
    ignore @@ settings#connect#activate
      ~callback:handle_settings;

    let quit_application = GMenu.menu_item ~label:"Quit LibreRef" () in
    menu#add quit_application;
    ignore @@ quit_application#connect#activate
      ~callback:handle_quit_application;


    let button = GdkEvent.Button.button button and time = GdkEvent.Button.time button in
    menu#popup ~button ~time


end


module Make (Logic: LOGIC) (BuildUI: functor (R: RUNTIME_CONTEXT) (D: DIALOG) -> UI) = struct 
  let main () =
    let _ = GMain.init () in
    let w = GWindow.window ~resizable:true ~title:"Libre-ref" ~width:1500 ~height:1500 () in
    let d = GMisc.drawing_area ~packing:w#add () in

    let module RuntimeCTX = struct
      let w = w
      let d = d
    end in
    let module Dialogs = BuildDialogs (RuntimeCTX) (Logic) in

    let module UI = BuildUI (RuntimeCTX) (Dialogs) in


    w#set_title "Libre ref";
    w#event#add [ `SCROLL ; `BUTTON1_MOTION; `BUTTON3_MOTION; `BUTTON_PRESS ; `BUTTON_RELEASE  ];


    ignore @@ d#misc#connect#draw ~callback:(UI.expose);

    ignore @@ w#event#connect#motion_notify ~callback:UI.on_move;
    ignore @@ w#event#connect#button_release ~callback:UI.on_button_release;
    ignore @@ w#event#connect#button_press ~callback:UI.on_button_press;
    ignore @@ w#event#connect#scroll ~callback:UI.on_scroll;
    ignore(w#connect#destroy ~callback:Dialogs.handle_quit_application);

    w#show();
    GMain.main()

end
