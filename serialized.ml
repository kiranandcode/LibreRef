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
(* *** Definitions *)
module E = Data_encoding.Encoding
(* *** Helpers *)
module Helpers = struct

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
module Image = struct
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
module Camera = struct

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
module Scene = struct

  type t = {
    images: Image.t list;
    camera: Camera.t;
  }

  let encoding =
    E.conv
      (fun {images; camera} -> (images,camera))
      (fun (images,camera) -> {images; camera}) @@
    E.obj2
      (E.req ~description:"Images in scene" "images" (E.list Image.encoding))
      (E.req ~description:"Saved camera placement in scene" "camera" Camera.encoding)

end

