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
(* * Values *)
let background_color = ref (1.0, 1.0, 1.0)
let outline_color = ref (0., 0.75, 0.6)
let min_zoom = ref 0.05
let max_zoom = ref 5.00
let embed_images = ref true


(* * Interface *)
let convert_color_to_internal (r,g,b) =
  let (!) v = Float.of_int v /. 65535. in
  (!r,!g,!b)

let convert_internal_to_color (r,g,b) =
  let (!) v = Float.to_int (v *. 65535.) in
  (!r,!g,!b)

let get_outline_colour : unit -> int * int * int = fun () ->
  convert_internal_to_color !outline_color

let set_outline_colour : int * int * int -> unit = fun (v) ->
  outline_color := convert_color_to_internal v

let get_background_colour : unit -> int * int * int = fun _ ->
  convert_internal_to_color !background_color

let set_background_colour : int * int * int -> unit = fun v ->
  background_color := convert_color_to_internal v

let get_min_zoom : unit -> float = fun () -> !min_zoom

let set_min_zoom : float -> unit = fun vl -> min_zoom := vl

let get_max_zoom : unit -> float = fun () -> !max_zoom

let set_max_zoom : float -> unit = fun vl -> max_zoom := vl

let get_embed_images : unit -> bool = fun () -> !embed_images

let set_embed_images : bool -> unit = fun vl -> embed_images := vl
