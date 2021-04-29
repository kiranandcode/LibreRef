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

