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
let set_source_rgbi cr (r,g,b) =
  Cairo.set_source_rgb cr (r) (g) (b)

type state =
  | ButtonPressOnImage of Image.t * (float * float) * [`Base of Image.t | `Selected ] list
  | ButtonPress of (float * float) * Image.t list * bool
  | MovingActive of  (float * float) * Image.t list
  | ScalingActive of [`NW | `NE | `SE | `SW ] * (float * float) * Image.t list
  | Normal of Image.t list

let state_elts = function
  | Normal ls -> ls
  | MovingActive (_, ls) -> ls
  | ScalingActive (_, _, ls) -> ls
  | ButtonPress (_, ls, _) -> ls
  | ButtonPressOnImage (selected, _, ls) ->
    List.map (function `Base img -> img | `Selected -> selected) ls

type t = {
  state: state;
  active: Image.t option;
  camera: Camera.t;
  filename: string option;
  any_changes: bool;
}

let generate_title =
  let rec last = function | [] -> "" | h :: [] -> h | _ :: t -> last t in
  function
  | {filename= Some filename; any_changes; _ } ->
    Printf.sprintf "%s - LibreRef%s"
      (String.split_on_char '/' filename |> last)
      (if any_changes then " (*)" else "")
  | {filename= None; any_changes; _ } ->
    Printf.sprintf "Untitled Scene - LibreRef%s"
      (if any_changes then " (*)" else "")

let init images = {
  state=Normal images;
  active=None;
  camera=Camera.create ();
  filename=None;
  any_changes=false;
}

let elts scene =
  Option.to_list scene.active @ List.rev @@ state_elts scene.state


let from_serialized ?filename (scene: Serialized.Scene.t) =
  let (images,errors) =
    Error.acc_map Image.from_serialized scene.images in
  let camera = Camera.from_serialized scene.camera in
  {state=Normal images; active=None; camera; filename; any_changes=false}, errors

let to_serialized scene =
  Serialized.Scene.{
    images= elts scene |> List.map Image.to_serialized;
    camera = Camera.to_serialized scene.camera 
  }

let read_from_file filename =
  let open Error in
  let+ scene = Serialized.Helpers.read_from_file Serialized.Scene.encoding filename in
  Ok (from_serialized ~filename scene)

let write_to_file scene filename =
  let sscene = to_serialized scene in 
  match Serialized.Helpers.write_to_file Serialized.Scene.encoding filename sscene with
  | Error error -> scene, [error]
  | Ok () -> {scene with filename=Some filename; any_changes=false}, []

let find_split p ls =
  let rec loop aux = function
    | [] -> None
    | x :: xs when p x ->
      let xs = List.map (fun v -> `Base v) xs in
      Some (x, List.rev_append (`Selected :: aux) xs)
    | x :: xs -> loop (`Base x :: aux) xs in
  loop [] ls

let mouse_pressed p scene =
  let p_in_world = Camera.screen_to_world scene.camera (fst p) (snd p) in
  let selected_image = find_split (Image.contains p_in_world) (state_elts scene.state) in
  let state =
    match
      selected_image,
      Option.map (Image.contains p_in_world) scene.active,
      Option.bind scene.active (Image.on_corner p_in_world)
    with
    | _, _, Some corner ->
      let anchor = Option.get scene.active |> Image.anchor in
      ScalingActive (corner, anchor corner, state_elts scene.state)
    | _, Some true, _ ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      MovingActive (p, state_elts scene.state)
    | None, _, _ -> ButtonPress (p, state_elts scene.state, false)
    | Some (selected, rest), _, _ -> ButtonPressOnImage (selected, p, rest) in
  let scene = {scene with state} in
  scene

let mouse_motion p scene =
  let update_camera (o_x,o_y) (x,y) =
    let dx,dy = o_x -. x, o_y -. y in
    Camera.move_by dx dy scene.camera in
  let update_image (o_x,o_y) (x,y) =
    let dx,dy = o_x -. x, o_y -. y in
    Option.map (Image.move_by dx dy) scene.active in
  let update_image_scale corner anchor p =
    Option.map (Image.scale_using_corner ~anchor ~corner p) scene.active in
  let camera, active, state, any_changes = match scene.state with
    | ButtonPressOnImage (_, op, _) as v ->
      let elts = state_elts v in
      update_camera op p, scene.active, ButtonPress (p, elts, true), true
    | MovingActive (op, elts) ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      scene.camera, update_image op p, MovingActive (p, elts), true
    | ScalingActive (cnr, anchor, elts) ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      scene.camera, update_image_scale cnr anchor p, ScalingActive (cnr, anchor, elts), true
    | ButtonPress (op, ls, _) ->
      update_camera op p, scene.active, ButtonPress (p, ls, true), true
    | Normal elts -> scene.camera, scene.active, ButtonPress (p, elts, true), scene.any_changes in
  let scene = {scene with camera; state; active; any_changes} in
  scene

let mouse_released scene =
  let active, state = match scene.state with
    | ButtonPressOnImage (img, _, images) ->
      let elts = List.filter_map (function `Selected -> None | `Base v -> Some v) images @
                 Option.to_list scene.active in
      Some (img), Normal elts
    | MovingActive (_, elts) -> scene.active, Normal (elts)
    | ScalingActive (_, _, elts) -> scene.active, Normal (elts)
    | Normal _ as v -> scene.active, v 
    | (ButtonPress (_, _, any_motion) as v) ->
      match any_motion with
      | false -> None, Normal (state_elts v @ Option.to_list scene.active)
      | true -> scene.active, Normal (state_elts v) in
  let scene = {scene with state; active} in
  scene

let draw scene cr  =
  set_source_rgbi cr !Config.background_color;
  Cairo.paint cr;

  Cairo.set_matrix cr (Camera.to_view_matrix scene.camera);
  List.iter (Image.draw cr) (state_elts scene.state);
  Option.iter (Image.draw_selected cr) scene.active

let zoom_around ~by (x,y)  scene =
  let camera = Camera.zoom_around x y by scene.camera in
  {scene with camera}

let add_image_at pos filename scene =
  let open Error in
  let+ image = Image.load_from_file ~at:pos filename in
  let state, active =
    Normal (state_elts scene.state @ Option.to_list scene.active),
    Some image in
  Ok {scene with state; active; any_changes=true}

let add_images_at pos filenames scene =
  let _, images, errors = 
    Error.fold_left_flat_map (fun pos filename ->
        let open Error in
        let+ image = Image.load_from_file ~at:pos filename in
        Ok (Image.shift_point_left_of image pos, image)
      ) pos filenames in
  match images with
  | [] -> scene, errors
  | first :: rest ->
    let state, active =
      Normal (state_elts scene.state @ Option.to_list scene.active @ rest),
      Some first in
    {scene with state; active; any_changes=true}, errors


