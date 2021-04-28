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
(* * Imports *)
module E = Data_encoding.Encoding
module G = Gui
(* * Definitions *)
(* ** Error handling *)
module Error = struct
  let (let+) x f = Result.bind x f

  let wrapping_exceptions f = try Ok (f ()) with e -> Error (Printexc.to_string e)

  let acc_map f l =
    let rec aux l_accu e_accu = function
      | [] -> (List.rev l_accu, List.rev e_accu)
      | x :: l ->
        match f x with
        | Ok (x) -> aux (x :: l_accu) e_accu l
        | Error e -> aux l_accu (e :: e_accu) l in
    aux [] [] l

  let fold_left_flat_map f accu l =
    let rec aux accu l_accu e_accu = function
      | [] -> (accu, List.rev l_accu, List.rev e_accu)
      | x :: l ->
        match f accu x with
        | Ok (accu,x) -> aux accu (x :: l_accu) e_accu l
        | Error e -> aux accu l_accu (e :: e_accu) l in
    aux accu [] [] l

end
(* ** Serialization *)
(* *** Helpers *)
module SerializationHelpers = struct

  let read_whole_file filename =
    let ch = open_in filename in
    try
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      s
    with e ->
      close_in ch;
      raise e

  let error_to_string pp error = 
    pp Format.str_formatter error;
    Format.flush_str_formatter ()

  let read_error_to_string = error_to_string Data_encoding.Binary.pp_read_error
  let write_error_to_string = error_to_string Data_encoding.Binary.pp_write_error

  let read_from_file encoding filename =
    let (let+) x f = Result.bind x f in
    let+ data = try Ok (read_whole_file filename) with e -> Error (Printexc.to_string e) in
    Data_encoding.Binary.read encoding data 0 (String.length data)
    |> Result.map snd
    |> Result.map_error read_error_to_string

  let write_bytes_to_file filename bytes =
    try
      let ch = open_out_bin filename in
      try
        output_bytes ch bytes;
        close_out ch;
        Ok ()
      with e -> close_out ch; raise e
    with e -> Error (Printexc.to_string e)

  let write_to_file encoding filename t =
    let module B = Data_encoding.Binary in
    let (let+) x f = Result.bind x f in
    let buf = Bytes.create (B.length encoding t) in
    let+ writer_state =
      Data_encoding.Binary.make_writer_state buf ~offset:0 ~allowed_bytes:(Bytes.length buf)
      |> Option.to_result ~none:"Could not build writer." in
    let+ () = Data_encoding.Binary.write encoding t writer_state
              |> Result.map ignore
              |> Result.map_error write_error_to_string in
    write_bytes_to_file filename buf

end
(* *** SerialisedImage *)
module SerializedImage = struct
  type binarray =
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  let binarray_of_array = Bigarray.Array2.of_array Bigarray.int32 Bigarray.c_layout
  let binarray_to_array array =
    let open Bigarray.Array2 in
    let rows = dim1 array in
    let cols = dim2 array in
    Array.init rows (fun row ->
        Array.init cols (fun col ->
            unsafe_get array row col
          )
      )
  let binarray_encoding =
    E.conv binarray_to_array binarray_of_array E.(array @@ array @@ int32)

  type data =
    | Linked of string
    | Embedded of {data: binarray; w: int; h: int; alpha:bool }

  let embedded_encoding =
    E.obj4
      (E.req ~description:"raw image data" "data" binarray_encoding)
      (E.req ~description:"width of embedded image" "width" E.int31)
      (E.req ~description:"height of embedded image" "height" E.int31)
      (E.req ~description:"does the embedded image have an alpha channel" "alpha" E.bool)

  let data_encoding =
    E.union
      [
        E.case ~title:"Linked Image" ~description:"A external filepath to an image file."
          (E.Tag 1) E.string (function Linked str -> Some str | _ -> None) (fun str -> Linked str);
        E.case ~title:"Embedded Image" ~description:"An embedded image."
          (E.Tag 2) embedded_encoding
          (function Embedded {data;w;h;alpha} -> Some (data,w,h,alpha) | _ -> None)
          (fun (data,w,h,alpha) -> Embedded {data;w;h;alpha});
      ]

  type t = { data: data; position: float * float; scale: float; }

  let encoding =
    E.conv
      (fun {data;position;scale} -> (data,position,scale))
      (fun (data,position,scale) -> {data;position;scale}) @@
    E.obj3
      (E.req ~description:"Image data" "data" data_encoding)
      (E.req ~description:"Position of image" "position" (E.tup2 E.float E.float))
      (E.req ~description:"Scale of image" "scale" E.float)
end

(* *** SerializedCamera *)
module SerializedCamera = struct

  type t = { c_x: float; c_y: float; zoom: float; }

  let encoding =
    E.conv
      (fun {c_x;c_y;zoom} -> (c_x,c_y,zoom))
      (fun (c_x,c_y,zoom) -> {c_x;c_y;zoom}) @@
    E.obj3
      (E.req ~description:"X coordinate of NW corner of camera" "c_x" E.float)
      (E.req ~description:"Y coordinate of NW corner of camera" "c_y" E.float)
      (E.req ~description:"Zoom level of camera" "zoom" E.float)

end
(* *** Serialized Scene *)
module SerializedScene = struct

  type t = {
    images: SerializedImage.t list;
    camera: SerializedCamera.t;
  }

  let encoding =
    E.conv
      (fun {images; camera} -> (images,camera))
      (fun (images,camera) -> {images; camera}) @@
    E.obj2
      (E.req ~description:"Images in scene" "images" (E.list SerializedImage.encoding))
      (E.req ~description:"Saved camera placement in scene" "camera" SerializedCamera.encoding)

end

(* ** Image *)
module Image = struct

  let selector_radius = 10.
  let selector_line_width = 5.
  let image_default_spacing = 50.0

  type t =
    | Image of {
        data: Cairo.Surface.t;
        position: (float * float);
        scale: float;
        width: float; height: float;
        file_ref: [`Embedded | `File of string];
      }

  let move_by dx dy = function
    | Image { data; position; scale; width; height; file_ref; } ->
      Image {
        data; position= (fst position -. dx, snd position -. dy);
        scale; width; height; file_ref;
      }      

  let anchor = function
    | Image {position=(px,py); scale; width; height; _} ->
      let w, h = width *. scale, height *. scale in
      function
        | `NW -> px +. w, py +. h
        | `NE -> px, py +. h
        | `SE -> px, py
        | `SW -> px +. w, py

  let scale_using_corner ?anchor ~corner (x,y) : t -> t =
    function
    | Image {position=(px,py); width; height; scale; data; file_ref } ->
      let ax,ay = match anchor with Some p -> p | None ->
        let w, h = width *. scale, height *. scale in
        let ax,ay = match corner with
          | `NW -> px +. w, py +. h
          | `NE -> px, py +. h
          | `SE -> px, py
          | `SW -> px +. w, py in
        ax, ay in
      let nw, nh = Float.abs (ax -. x), Float.abs (ay -. y) in
      let wr, hr = nw /. width, nh /. height in
      let new_scale = Float.max wr hr in
      let nw, nh = new_scale *. width, new_scale *. height in
      let position = match corner with
        | `NW -> ax -. nw, ay -. nh
        | `NE -> px, ay -. nh
        | `SE -> px, py
        | `SW -> ax -. nw, py 
      in
      Image {position; width; height; scale=new_scale; data; file_ref}


  let draw cr = function
    | Image {data; position=(x,y); scale; width=w; height=h; _} ->
      let x,y = x/.scale, y /. scale in
      Cairo.scale cr scale scale;
      Cairo.set_source_surface cr data ~x ~y;
      Cairo.rectangle cr x y ~w  ~h;
      Cairo.fill cr; 
      Cairo.scale cr (1./.scale) (1./.scale)      

  let draw_selected cr = function
    | Image {data; position=(x,y); scale; width=w; height=h; _} ->
      let x,y = x/.scale, y /. scale in
      Cairo.scale cr scale scale;
      Cairo.set_source_surface cr data ~x ~y;
      Cairo.rectangle cr x y ~w  ~h;
      Cairo.fill cr;
      Cairo.rectangle cr x y ~w  ~h;
      Cairo.set_source_rgb cr 0. 0.75 0.6;
      Cairo.set_line_width cr (selector_line_width/.scale);
      Cairo.stroke cr;
      let circle (x,y) =
        Cairo.arc cr ~r:(selector_radius /. scale)  ~a1:0.0 ~a2:(2. *. Float.pi) x y in
      circle (x,y);
      Cairo.Path.sub cr;
      circle (x +. w,y);
      Cairo.Path.sub cr;
      circle (x +. w,y +. h);
      Cairo.Path.sub cr;
      circle (x,y +. h);
      Cairo.fill cr;
      Cairo.scale cr (1./.scale) (1./.scale)

  let is_suffix ~suffix str =
    let sfl = String.length suffix in
    let l = String.length str in
    sfl <= l && String.equal suffix (String.sub str (l - sfl) sfl)

  let load_from_file ?at ?(scale=1.0) filename =
    let (let+) x f = Result.bind x f in
    let+ data =
      match () with
      | () when is_suffix ~suffix:".png" filename  ->
        Error.wrapping_exceptions (fun () -> Cairo.PNG.create filename)
      | () when is_suffix ~suffix:".ps" filename  ->
        Error.wrapping_exceptions (fun () -> Cairo.PS.create filename ~w:100.0 ~h:100.0)
      | () when is_suffix ~suffix:".pdf" filename  ->
        Error.wrapping_exceptions (fun () -> Cairo.PDF.create filename  ~w:100.0 ~h:100.0)
      | ()  ->
        begin match Stb_image.load filename with
          | Ok image ->
            Error.wrapping_exceptions (fun () ->
                let pad_image_data ?default from to_ data =
                  let open Bigarray in
                  let old_data_byte_size = Array1.dim data in
                  let old_array_size = old_data_byte_size / from in
                  let new_array_byte_size = old_array_size * to_ in
                  let new_data = Array1.create int8_unsigned c_layout new_array_byte_size in
                  let new_pos = ref 0 in
                  let old_pos = ref 0 in
                  for _ = 1 to old_array_size do
                    let fv = data.{!old_pos} in
                    for offset = 0 to from - 1 do
                      new_data.{!new_pos + offset} <- data.{!old_pos + from - offset - 1};
                    done;
                    for _ = 0 to from - 1 do
                      incr new_pos; incr old_pos
                    done;
                    begin match default with
                      | None ->
                        for _ = 1 to to_ - from do
                          new_data.{!new_pos} <- fv;
                          incr new_pos
                        done
                      | Some deflt ->
                        for _ = 1 to to_ - from do
                          new_data.{!new_pos} <- deflt;
                          incr new_pos
                        done
                    end;
                  done;
                  new_data in
                let w = Stb_image.width image and h = Stb_image.height image in
                let channels = Stb_image.channels image in
                let data = match channels with
                  | 4 -> Stb_image.data image
                  | n when n > 0 && n <= 3 ->
                    pad_image_data n 4 (Stb_image.data image)
                  | n  ->
                    invalid_arg (
                      Printf.sprintf "image %s has an unsupported number of channels %d" filename n
                    ) in
                let format = match channels with 4 -> Cairo.Image.ARGB32 | _ -> Cairo.Image.RGB24 in
                let img = Cairo.Image.create_for_data8 ~stride:(w * 4) data format ~w ~h in
                img
              )
          | Error `Msg error ->
            Error error
        end
        (* Error.wrapping_exceptions (fun () -> Cairo.PNG.create filename) *)
        (* | _ -> Error (Printf.sprintf "Unsupported image type %s" filename) *) in
    let position = match at with Some v -> v | None -> (0., 0.) in
    let width,height = Float.of_int @@ Cairo.Image.get_width data,
                       Float.of_int @@ Cairo.Image.get_height data in
    Ok (Image {data; position; scale; width; height; file_ref=`File filename})

  let load_from_data ?at ?(scale=1.0) data w h alpha =
    let data = Cairo.Image.create_for_data32 ~w ~h ~alpha data in
    let position = match at with Some v -> v | None -> (0., 0.) in
    let width,height = Float.of_int @@ Cairo.Image.get_width data,
                       Float.of_int @@ Cairo.Image.get_height data in
    Image {data; position; scale; width; height; file_ref=`Embedded}

  let from_serialized (serialised: SerializedImage.t) =
    match serialised.data with
    | SerializedImage.Linked file ->
      load_from_file ~at:serialised.position ~scale:serialised.scale file
    | SerializedImage.Embedded { data; w; h; alpha } -> 
      Error.wrapping_exceptions (fun () -> load_from_data data w h alpha)

  let to_serialized = function
    | Image {data; position; scale; file_ref=`Embedded; _} ->
      SerializedImage.{
        position; scale;
        data=Embedded {
            data=Cairo.Image.get_data32 data;
            w=Cairo.Image.get_width data;
            h=Cairo.Image.get_height data;
            alpha = match Cairo.Image.get_format data with
              | Cairo.Image.ARGB32 -> true
              | Cairo.Image.RGB24 -> false
              | Cairo.Image.A8 -> false
              | Cairo.Image.A1 -> false
          }
      }
    | Image { position; scale; file_ref=`File filename; _} ->
      SerializedImage.{
        position; scale;
        data=Linked filename
      }

  let contains (x,y) = function
    | Image {position=(px,py); width; height; scale; _ } ->
      let w,h = width *. scale, height *. scale in
      (px <= x && x <= px +. w) &&
      (py <= y && y <= py +. h)

  let on_corner (x,y) = function
    | Image {position=(px,py); width; height; scale; _ } ->
      let within_circle (px,py) = (px -. x) ** 2. +. (py -. y) ** 2. <= selector_radius ** 2. in
      let w,h = width *. scale, height *. scale in
      match () with
      | () when within_circle (px,py) -> Some `NW
      | () when within_circle (px +. w,py) -> Some `NE
      | () when within_circle (px +. w, py +. h) -> Some `SE
      | () when within_circle (px, py +. h) -> Some `SW
      | _ -> None

  let shift_point_left_of image pos =
    if contains pos image
    then
      match image with Image { position=(px,py); width; scale; _ } ->
        let w = width *. scale in
        (px +. w +. image_default_spacing, py)
    else pos
    
end

(* ** Camera *)
module Camera = struct

  type t = { c_x: float; c_y: float; zoom: float; }

  let create () = { c_x = 0.; c_y = 0.; zoom = 1. }

  let to_view_matrix {c_x; c_y; zoom} =
    let matrix = Cairo.Matrix.init_translate (-.c_x) (-.c_y) in
    Cairo.Matrix.scale matrix zoom zoom;
    matrix

  let move_by dx dy {c_x; c_y; zoom} : t = {c_x=c_x +. dx; c_y = c_y +. dy; zoom}

  let screen_to_world {c_x;c_y; zoom} x y =
    let x_c, y_c = (x  +. c_x) /. zoom,  (y +. c_y) /. zoom in
    x_c, y_c

  let zoom_around x y by {c_x; c_y; zoom} : t =
    let x_w, y_w = screen_to_world {c_x; c_y; zoom} x y in
    let new_zoom = zoom +. by in
    let c_x = c_x +. x_w *. by and c_y = c_y +. y_w *. by in
    {c_x; c_y; zoom=new_zoom}

  let from_serialized (camera: SerializedCamera.t) =
    {c_x = camera.c_x; c_y=camera.c_y; zoom=camera.zoom}

  let to_serialized (camera: t) : SerializedCamera.t =
    SerializedCamera.{c_x = camera.c_x; c_y=camera.c_y; zoom=camera.zoom}

end
(* ** Scene *)

module Scene = struct

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


  let from_serialized ?filename (scene: SerializedScene.t) =
    let (images,errors) =
      Error.acc_map Image.from_serialized scene.images in
    let camera = Camera.from_serialized scene.camera in
    {state=Normal images; active=None; camera; filename; any_changes=false}, errors

  let to_serialized scene =
    SerializedScene.{
      images= elts scene |> List.map Image.to_serialized;
      camera = Camera.to_serialized scene.camera 
    }

  let read_from_file filename =
    let open Error in
    let+ scene = SerializationHelpers.read_from_file SerializedScene.encoding filename in
    Ok (from_serialized ~filename scene)

  let write_to_file scene filename =
    let sscene = to_serialized scene in 
    match SerializationHelpers.write_to_file SerializedScene.encoding filename sscene with
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

  let draw scene cr =
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


end

(* * Implementation *)
(* ** Resources *)
let scene = ref @@ Scene.init []

module Logic : Gui.LOGIC = struct

  let clear_scene () = scene := Scene.init []
        
  let is_scene_dirty () = !scene.any_changes

  let add_files_to_scene pos images =
    scene := fst (Scene.add_images_at pos images !scene)

  let open_scene_from_file file =
    match Scene.read_from_file file  with
    | Ok (s, _) -> scene := s
    | Error _ -> ()

  let current_scene_name () = !scene.filename

  let save_scene_as filename = ignore @@ Scene.write_to_file !scene filename

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
    true 

  let on_button_release =
    fun _m ->
    scene := Scene.mouse_released !scene;
    queue_draw ();
    true 

  let on_button_press =
    fun m ->
    let x = GdkEvent.Button.x m and y = GdkEvent.Button.y m in
    let button = GdkEvent.Button.button m in
    begin match button with
      | 1 ->
        scene := Scene.mouse_pressed (x,y) !scene;
        queue_draw ()
      | 3 -> Dialog.show_right_click_menu m
      | _ -> ()
    end;
    true

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

end

(* ** Main loop *)

module Gui = Gui.Make (Logic) (BuildUI)

let () = Gui.main ()
