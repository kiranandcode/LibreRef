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

let zoom_around x y by ({c_x; c_y; zoom} as prev) : t =
  let x_w, y_w = screen_to_world {c_x; c_y; zoom} x y in
  let new_zoom = zoom +. by in
  let new_zoom = if new_zoom <= !Config.min_zoom
    then !Config.min_zoom
    else if new_zoom >= !Config.max_zoom
    then !Config.max_zoom
    else new_zoom in
  if new_zoom <> zoom then
    let c_x = c_x +. x_w *. by
    and c_y = c_y +. y_w *. by in
    {c_x; c_y; zoom=new_zoom}
  else prev

let from_serialized (camera: Serialized.Camera.t) =
  {c_x = camera.c_x; c_y=camera.c_y; zoom=camera.zoom}

let to_serialized (camera: t) : Serialized.Camera.t =
  Serialized.Camera.{c_x = camera.c_x; c_y=camera.c_y; zoom=camera.zoom}

