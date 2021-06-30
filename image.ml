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
(* * Helpers *)
let set_source_rgbi cr (r,g,b) =
  Cairo.set_source_rgb cr (r) (g) (b)

(* * Implementation *)
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
    set_source_rgbi cr !Config.outline_color;
    (* Cairo.set_source_rgb cr 0. 0.75 0.6; *)
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
    | ()  ->
      begin match Stb_image.load filename with
        | Ok image ->
          print_endline @@ Printf.sprintf "Got image (wxh=%dx%d,channels=%d,offset=%d,stride=%d(%d*%d))\n"  image.width image.height image.channels image.offset image.stride image.width (image.stride/image.width);
          Error.wrapping_exceptions (fun () ->
              Utils.stb_buffer_to_cairo_surface image
            )
        | Error `Msg error ->
          Error error
      end
      (* Error.wrapping_exceptions (fun () -> Cairo.PNG.create filename) *)
      (* | _ -> Error (Printf.sprintf "Unsupported image type %s" filename) *) in
  let position = match at with Some v -> v | None -> (0., 0.) in
  let width,height = Float.of_int @@ Cairo.Image.get_width data,
                     Float.of_int @@ Cairo.Image.get_height data in
  Ok (Image {data; position; scale; width; height; file_ref=(
      if !Config.embed_images then `Embedded else `File filename
    )})

let load_from_url ?at ?(scale=1.0) url =
  let (let+) x f = Result.bind x f in
  let+ raw_image = Web.get_sync url |> Result.map_error Piaf.Error.to_string in
  let+ image = Utils.string_to_stb_image raw_image |> Result.map_error (function `Msg v -> v) in
  let+ data = Error.wrapping_exceptions (fun () ->
              Utils.stb_buffer_to_cairo_surface image
            ) in
  let position = match at with Some v -> v | None -> (0., 0.) in
  let width,height = Float.of_int @@ Cairo.Image.get_width data,
                     Float.of_int @@ Cairo.Image.get_height data in
  Ok (Image {data; position; scale; width; height; file_ref=( `Embedded )})

let load_from_pixbuf ?at ?(scale=1.0) pixbuf =
  let (let+) x f = Result.bind x f in
  let+ data =
    match () with
    | ()  ->
      begin match   Utils.pixbuf_to_stb_image pixbuf with
        | Ok image ->
          print_endline @@ Printf.sprintf "Got image (wxh=%dx%d,channels=%d,offset=%d,stride=%d(%d*%d))\n"  image.width image.height image.channels image.offset image.stride image.width (image.stride/image.width);
          Error.wrapping_exceptions (fun () ->
              Utils.stb_buffer_to_cairo_surface image
            )
        | Error `Msg error ->
          Error error
      end
      (* Error.wrapping_exceptions (fun () -> Cairo.PNG.create filename) *)
      (* | _ -> Error (Printf.sprintf "Unsupported image type %s" filename) *) in
  let position = match at with Some v -> v | None -> (0., 0.) in
  let width,height = Float.of_int @@ Cairo.Image.get_width data,
                     Float.of_int @@ Cairo.Image.get_height data in
  Ok (Image {data; position; scale; width; height; file_ref=( `Embedded )})


let load_from_data ?at ?(scale=1.0) data w h alpha =
  let data = Cairo.Image.create_for_data32 ~w ~h ~alpha data in
  let position = match at with Some v -> v | None -> (0., 0.) in
  let width,height = Float.of_int @@ Cairo.Image.get_width data,
                     Float.of_int @@ Cairo.Image.get_height data in
  Image {data; position; scale; width; height; file_ref=`Embedded}

let from_serialized (serialised: Serialized.Image.t) =
  match serialised.data with
  | Serialized.Image.Linked file ->
    load_from_file ~at:serialised.position ~scale:serialised.scale file
  | Serialized.Image.Embedded { data; w; h; alpha } -> 
    Error.wrapping_exceptions (fun () ->
        load_from_data ~at:serialised.position ~scale:serialised.scale data w h alpha)

let to_serialized = function
  | Image {data; position; scale; file_ref=`Embedded; _} ->
    print_endline @@ Printf.sprintf "loading data";
    Serialized.Image.{
      position; scale;
      data=Embedded {
          data  = Cairo.Image.get_data32 data;
          w     = Cairo.Image.get_width data;
          h     = Cairo.Image.get_height data;
          alpha = match Cairo.Image.get_format data with
            | Cairo.Image.ARGB32 -> true
            | Cairo.Image.RGB24 -> false
            | Cairo.Image.A8 -> false
            | Cairo.Image.A1 -> false
        }
    }
  | Image { position; scale; file_ref=`File filename; _} ->
    Serialized.Image.{
      position;
      scale;
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
    

let calculate_bounding_box (ls: t list) : (float * float * float * float) option =
  let result' = 
    List.fold_left
      (fun (lx,ly,mx,my) (Image { position=(px,py); width; height; scale=_; _}) ->
         let pxm, pym = px +. width (* *. scale *), py +. height (* *. scale *) in
         let lx', mx' = Float.min lx px, Float.max mx pxm 
         and ly', my' = Float.min ly py, Float.max my pym in
         (lx',ly',mx',my')
      ) (0., 0., 0., 0.) ls in
  let result =
    let (lx,ly, mx, my) = result' in
    lx, ly, (mx -. lx), (my -. ly) in
  if result = (0., 0., 0., 0.) then None else Some result

  
