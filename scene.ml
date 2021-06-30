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
c*)

let set_source_rgbi cr (r,g,b) =
  Cairo.set_source_rgb cr (r) (g) (b)

module Cache = struct

  type t = { surface: [`Surface ] Cairo.Pattern.t; x: float; y: float; }

  let init (images: Image.t list) : t option =
    let (let+) x f = Option.bind x f in
    if not @@  Config.get_cache_drawing ()
    then None
    else
      let+ (x,y,w,h) = Image.calculate_bounding_box images in
      let surface = Cairo.Image.(create RGB24 ~w:(Int.of_float w) ~h:(Int.of_float h)) in
      let () =
        let cr = Cairo.create surface in
        Cairo.Pattern.set_filter (Cairo.get_source cr) Cairo.Pattern.FAST;
        Cairo.translate cr (-.x) (-.y);
        set_source_rgbi cr !Config.background_color;
        Cairo.paint cr;
        List.iter (Image.draw cr) (images) in
      let surface =   Cairo.Pattern.create_for_surface surface in
      Cairo.Pattern.set_filter surface Cairo.Pattern.FAST;
      Some {surface; x; y}

  let draw (cache: t) cr : unit =
    Cairo.save cr;
    ignore @@ Cairo.translate cr (cache.x) (cache.y);
    Cairo.set_source cr cache.surface;  
    Cairo.paint cr;
    Cairo.restore cr

end


type state =
  | ButtonPress of (float * float)
  | MovingActive of  (float * float)
  | ScalingActive of [`NW | `NE | `SE | `SW ] * (float * float)
  | Normal


type t = {
  state: state;
  active: Image.t option;
  images: Image.t list;
  camera: Camera.t;
  filename: string option;
  any_changes: bool;
  mutable cache: Cache.t option;
}

let camera_focus t =
  Camera.get_center t.camera

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
  state=Normal;
  images;
  active=None;
  camera=Camera.create ();
  filename=None;
  any_changes=false;
  cache = Cache.init images
}

let elts scene =
  Option.to_list scene.active @ List.rev @@ scene.images


let from_serialized ?filename (scene: Serialized.Scene.t) =
  let (images,errors) =
    Error.acc_map Image.from_serialized scene.images in
  let camera = Camera.from_serialized scene.camera in
  {
    state=Normal; images; active=None; camera;
    filename; any_changes=false; cache = Cache.init images;
  }, errors

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
      Some (x, List.rev_append (aux) xs)
    | x :: xs -> loop (x :: aux) xs in
  loop [] ls

let mouse_select_pressed p scene =
  let p_in_world = Camera.screen_to_world scene.camera (fst p) (snd p) in
  let selected_image = find_split (Image.contains p_in_world) scene.images in
  let state, images, active, cache =
    match
      selected_image,
      Option.map (Image.contains p_in_world) scene.active,
      Option.bind scene.active (Image.on_corner p_in_world)
    with
    | _, _, Some corner ->
      let anchor = Option.get scene.active |> Image.anchor in
      ScalingActive (corner, anchor corner), scene.images, scene.active, scene.cache
    | _, Some true, _ ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      MovingActive (p), scene.images, scene.active, scene.cache
    | None, _, _ ->
      let images = scene.images @ Option.to_list scene.active in
      Normal, images, None, (if Option.is_some scene.active then None else scene.cache)
    | Some (selected, rest), _, _ ->
      let images = rest @ Option.to_list scene.active in
      Normal, images, Some selected, None in
  let scene = {scene with state; images; active; cache} in
  scene


let mouse_drag_pressed p scene =
  let state = ButtonPress (p) in
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
    | MovingActive (op) ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      scene.camera, update_image op p, MovingActive (p), true
    | ScalingActive (cnr, anchor) ->
      let p = Camera.screen_to_world scene.camera (fst p) (snd p) in
      scene.camera, update_image_scale cnr anchor p, ScalingActive (cnr, anchor), true
    | ButtonPress (op) ->
      update_camera op p, scene.active, ButtonPress (p), scene.any_changes
    | Normal -> scene.camera, scene.active, Normal, scene.any_changes in
  let scene = {scene with camera; state; active; any_changes} in
  scene

let mouse_released scene =
  let active, state, images, cache = match scene.state with
    | MovingActive (_) -> scene.active, Normal, scene.images, scene.cache
    | ScalingActive (_, _) -> scene.active, Normal, scene.images, scene.cache
    | Normal as v -> scene.active, v, scene.images, scene.cache
    | ButtonPress (_) ->
      scene.active, Normal, scene.images, scene.cache in
  let scene = {scene with state; images; active; cache} in
  scene

let draw scene cr  =
  set_source_rgbi cr !Config.background_color;
  Cairo.paint cr;
  Cairo.set_matrix cr (Camera.to_view_matrix scene.camera);

  if Config.get_cache_drawing () then begin match scene.cache with
    | None -> scene.cache <- Cache.init scene.images
    | _ -> ()
  end;

  begin match scene.cache with 
    | None  ->
      List.iter (Image.draw cr) (scene.images);
    | Some cache ->
      Cache.draw cache cr;
  end;

  Option.iter (Image.draw_selected cr) scene.active

let zoom_around ~by (x,y)  scene =
  let camera = Camera.zoom_around x y by scene.camera in
  {scene with camera}

let can_delete scene = Option.is_some scene.active

let delete_active_image scene =
  {scene with active=None; cache=None; any_changes=true}

let add_image_at (x,y) filename scene =
  let pos = Camera.screen_to_world scene.camera x y in
  let open Error in
  let+ image = Image.load_from_file ~at:pos filename in
  let state, images, active =
    Normal, (scene.images @ Option.to_list scene.active),
    Some image in
  Ok {scene with state; images; active; any_changes=true; cache=None}

let add_raw_image_at (x,y) pixbuf scene =
  let pos = Camera.screen_to_world scene.camera x y in
  let open Error in
  let+ image = Image.load_from_pixbuf ~at:pos pixbuf in
  let state, images, active =
    Normal, (scene.images @ Option.to_list scene.active),
    Some image in
  Ok {scene with state; images; active; any_changes=true; cache=None}

let add_images_at (x,y) filenames scene =
  let pos = Camera.screen_to_world scene.camera x y in
  let _, images, errors = 
    Error.fold_left_flat_map (fun pos -> function
        | `File filename ->
          let open Error in
          let+ image = Image.load_from_file ~at:pos filename in
          Ok (Image.shift_point_left_of image pos, image)
        | `Web url ->
          let open Error in
          let+ image = Image.load_from_url ~at:pos url in
          Ok (Image.shift_point_left_of image pos, image)
      ) pos filenames in
  match images with
  | [] -> scene, errors
  | first :: rest ->
    let state, images, active =
      Normal, (scene.images @ Option.to_list scene.active @ rest),
      Some first in
    {scene with state; images; active; any_changes=true; cache=None}, errors

