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
(* * Implementation *)
(* ** Resources *)
let scene = ref @@ Scene.init []

let f = Web.get_sync

module Logic : Gui.LOGIC = struct

  let clear_scene () = scene := Scene.init []
        
  let is_scene_dirty () = !scene.any_changes

  let add_raw_image_to_scene pos pixbuf =
    match (Scene.add_raw_image_at pos pixbuf !scene) with
    | Ok s' -> scene := s'; []
    | Error error -> [error]

  let add_files_to_scene pos images =
    let s', errors = (Scene.add_images_at pos images !scene) in
    scene := s';
    errors

  let open_scene_from_file file =
    match Scene.read_from_file file  with
    | Ok (s, errors) -> scene := s; errors
    | Error error -> [error]

  let current_scene_name () = !scene.filename

  let save_scene_as filename =
    let s, errors = Scene.write_to_file !scene filename in
    scene := s;
    errors

  let get_title () = Scene.generate_title !scene

end

(* ** Helpers *)
module BuildUI (RuntimeCtx: Gui.RUNTIME_CONTEXT) (Dialog: Gui.DIALOG) : Gui.UI = struct

  open RuntimeCtx

  let queue_draw () =
    GtkBase.Widget.queue_draw (GtkBaseProps.Widget.cast d#as_widget);
    w#set_title (Scene.generate_title !scene)

  let draw cr =
    Scene.draw !scene cr

  let expose cr =
    draw cr;
    true

  let on_move = fun m ->
    let x = GdkEvent.Motion.x m and y = GdkEvent.Motion.y m in
    scene := Scene.mouse_motion (x,y) !scene;
    queue_draw ();
    false 

  let on_button_release = fun _m ->
    scene := Scene.mouse_released !scene;
    queue_draw ();
    false 

  let on_button_press = fun m ->
    let x = GdkEvent.Button.x m and y = GdkEvent.Button.y m in
    let button = GdkEvent.Button.button m in
    begin match button with
      | 1 ->
        scene := Scene.mouse_pressed (x,y) !scene;
        queue_draw ()
      | 3 -> Dialog.show_right_click_menu m
      | _ -> ()
    end;
    false

  let on_scroll = fun m ->
    let x,y = GdkEvent.Scroll.x m, GdkEvent.Scroll.y m in
    begin match GdkEvent.Scroll.direction m with
      | `DOWN  ->
        scene := Scene.zoom_around ~by:(-.0.05) (x,y) !scene;
        queue_draw ()
      | `UP ->
        scene := Scene.zoom_around ~by:(0.05) (x,y) !scene;
        queue_draw ()
      |`SMOOTH |`LEFT |`RIGHT -> ()
    end;
    true


  let on_key_press (key: GdkEvent.Key.t) =
    let keyval = GdkEvent.Key.keyval key 
    and state = GdkEvent.Key.state key in
    begin match keyval, state with
      | keyval, [`CONTROL] when keyval = GdkKeysyms._v ->
        let pos = Scene.camera_focus !scene in
        Dialog.handle_paste_images_at pos ()
    | _ -> ()
    end;
    false

end

(* ** Main loop *)
module Gui = Gui.Make (Logic) (Config) (BuildUI)

let main config initial_scene () =
  Option.iter Config.set_config_path config;
  Config.load_config ();
  Option.iter (fun s -> print_endline @@ Printf.sprintf "got config %s" s) config;
  Option.iter (fun s -> print_endline @@ Printf.sprintf "got scene %s" s) initial_scene;
  Gui.main ?initial_scene ()

let () =
  let open Cmdliner in

  let main_command =
    let config_flag =
      let doc = "Path to the base directory used to store configuration \
                 for LibreRef. Configuration options are stored under \
                 VAL/libre-ref" in
      let env = Term.env_info ~doc:"Directory under which to store \
                                    configuration files." "XDG_CONFIG_HOME" in
      let config_info = Arg.info ~doc ~env [ "config"; "c"] in
      Arg.(value & opt (some string) None & config_info) in
    let initial_scene =
      let doc = "(Optional) LibreRef scene to open." in
      let initial_scene_info = Arg.info ~docv:"SCENE" ~doc [] in
      Arg.(value & pos 0 (some string) None & initial_scene_info) in
    Term.(const main $ config_flag $ initial_scene $ const ()) in
  let libreref_info =
    let envs = [] in
    let man : Manpage.block list = [
      `S Manpage.s_bugs;
      `P "Email bug reports to <kirang at comp.nus.edu.sg>."
    ] in
    Term.info
      ~doc:"A free as in freedom digital referencing tool for artists."
      ~man ~envs
      "libre-ref" in
  Term.exit @@ Term.eval @@ (main_command, libreref_info)

