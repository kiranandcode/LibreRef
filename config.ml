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
(* * Modules *)
module StringMap = Map.Make (String)
(* * Helpers *)
let read_whole_file filename =
  let ch = open_in filename in
  try
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  with e ->
    close_in ch;
    raise e

let write_to_file ~filename bytes =
  let ch = open_out filename in
  try
    output_string ch bytes;
    close_out ch
  with e -> close_out ch; raise e


let convert_color_to_internal (r,g,b) =
  let (!) v = Float.of_int v /. 65535. in
  (!r,!g,!b)

let convert_internal_to_color (r,g,b) =
  let (!) v = Float.to_int (v *. 65535.) in
  (!r,!g,!b)
(* * Values *)
let get_default_path () =
  let path = 
    match Bos.OS.Env.var "XDG_CONFIG_HOME" |>
          Option.to_result ~none:(`Msg "") |> (fun v -> Result.bind v Fpath.of_string)
    with
    | Error _ ->
      let home = Bos.OS.Env.var "HOME" |> Option.get in
      Fpath.(v home / ".config")
    | Ok path -> path in
  Fpath.(to_string @@ path / "libre-ref")

let enforce_config_dir_exists path =
  let path = Fpath.of_string path |> Result.get_ok in
  ignore (path |> Fpath.parent |> Bos.OS.Dir.create |> Result.get_ok)

let config_path = ref @@ (get_default_path ())

let background_color = ref (1.0, 1.0, 1.0)
let outline_color = ref (0., 0.75, 0.6)
let min_zoom = ref 0.05
let max_zoom = ref 5.00
let embed_images = ref true
let cache_drawing = ref true

let initialize_from_config_file file =
  let txt = read_whole_file file in
  let mapping = 
    String.split_on_char '\n' txt
    |> List.filter_map (fun line ->
        let line = String.trim line in
        if String.equal line ""
        then None
        else
          match
            String.split_on_char ':' line |> List.map String.trim
            |> List.filter (Fun.negate @@ String.equal "")
          with
          | [key; vl] -> Some (key,vl)
          | _ -> None
      ) |> List.fold_left (fun map (key,vl) -> StringMap.add key vl map) StringMap.empty in

  let read_color str =
    match
      String.split_on_char ',' str |> List.map String.trim
      |> List.filter_map int_of_string_opt
    with
    | [r;g;b] ->  Some (convert_color_to_internal (r,g,b))
    | _ -> None in
  let set_ref_from rf ~key ~from_ =
    match StringMap.find_opt key mapping |> (fun v -> Option.bind v from_) with
    | None -> ()
    | Some vl -> rf := vl in
  set_ref_from background_color ~key:"background" ~from_:read_color;
  set_ref_from outline_color ~key:"outline" ~from_:read_color;
  set_ref_from min_zoom ~key:"min-zoom" ~from_:float_of_string_opt;
  set_ref_from max_zoom ~key:"max-zoom" ~from_:float_of_string_opt;
  set_ref_from embed_images ~key:"embed" ~from_:bool_of_string_opt;
  set_ref_from embed_images ~key:"cache" ~from_:bool_of_string_opt

let save_to_config_file filename =
  let color_to_string v =
    let (r,g,b) = convert_internal_to_color v in
    Printf.sprintf "%d,%d,%d" r g b in
  let txt = 
    Printf.sprintf "background: %s\noutline: %s\nmin-zoom: %f\nmax-zoom: %f\nembed: %b\ncache: %b"
      (color_to_string !background_color)
      (color_to_string !outline_color)
      (!min_zoom) (!max_zoom) (!embed_images) (!cache_drawing) in
  write_to_file ~filename txt

(* * Interface *)
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

let get_cache_drawing : unit -> bool = fun () -> !cache_drawing

let set_cache_drawing : bool -> unit = fun vl -> cache_drawing := vl

let save_config : unit -> string list = fun () ->
  try
    enforce_config_dir_exists !config_path;
    save_to_config_file !config_path; []
  with e -> [Printexc.to_string e]

let load_config : unit -> unit =
  fun () -> enforce_config_dir_exists !config_path; initialize_from_config_file !config_path

let set_config_path : string -> unit =
  fun path -> config_path := path; enforce_config_dir_exists !config_path
