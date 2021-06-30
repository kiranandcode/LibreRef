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
(* * Constants *)
(* Size of quadtrees at root *)
let max_node_elements = 10
let root_quadtree_size = 2048

(* * Definitions *)
type vec = float * float
type dim = float * float
type rect = vec * dim

type 'a quadtree =
  | Leaf of 'a list * vec * float
  | Node of {
      corner: vec * float;

      elts: 'a list;

      nw: 'a quadtree;           (*  +-------------+ *)
      ne: 'a quadtree;           (*  |  NW  |  NE  | *)
                                (*  +-------------+ *)
      se: 'a quadtree;           (*  |  SW  |  SE  | *)
      sw: 'a quadtree;           (*  +-------------+ *)
    }


(* * Implementation *)
(* ** Utils *)
let contains : rect -> rect -> bool =
  fun ((r1_x, r1_y), (r1_w, r1_h)) ((r2_x, r2_y), (r2_w, r2_h)) ->
  Float.((r2_x +. r2_w < r1_x +. r1_w) &&
  (r2_x > r1_x) &&
  (r2_y > r1_y) &&
  (r2_y +. r2_h < r1_y +. r1_h))

let contains_point : rect -> vec -> bool =
  fun ((r1_x, r1_y), (r1_w, r1_h)) (x,y) ->
  Float.((r1_x <= x) && (x <= r1_x +. r1_w) &&
         (r1_y <= y) && (y <= r1_y +. r1_h))

let intersects : rect -> rect -> bool =
  fun ((r1_x, r1_y), (r1_w, r1_h)) ((r2_x, r2_y), (r2_w, r2_h)) ->
  not Float.((r1_x +. r1_w < r2_x) ||
             (r2_x +. r2_w < r1_x) ||
             (r1_y +. r1_h < r2_y) ||
             (r2_y +. r2_h < r1_y))

(* ** Quadtree *)
let get_surf : 'a quadtree -> rect = function
  | Leaf (_, corner, sz)
  | Node { corner=(corner, sz); _} ->
    (fst corner, snd corner), (sz, sz)

let quadimate : rect -> rect * rect * rect * rect =
  fun ((x,y), (w,h)) ->
  ((x,y), (w/.2., h/.2.)),
  ((x +. w/.2.,y), (w/.2., h/.2.)),
  ((x,y +. h/.2.), (w/.2., h/.2.)),
  ((x +. w/.2.,y +. h/.2.), (w/.2., h/.2.))

let rec insert_point ~get_shape vl tree = match tree with
  | Leaf (ns, corner, sz) ->
    create_quadtree ~get_shape (corner, (sz,sz)) (vl :: ns)    
  | Node {corner; elts; nw;ne;se;sw} ->
    let shape = get_shape vl in
    if contains (get_surf nw) shape
    then Node {corner; elts; nw=insert_point ~get_shape vl nw;ne;se;sw}
    else if contains (get_surf ne) shape
    then Node {corner; elts; nw; ne=insert_point ~get_shape vl ne;se;sw}
    else if contains (get_surf se) shape
    then Node {corner; elts; nw; ne; se=insert_point ~get_shape vl se;sw}        
    else if contains (get_surf sw) shape
    then Node {corner; elts; nw; ne; se; sw=insert_point ~get_shape vl sw}
    else Node {corner; elts=vl::elts; nw;ne;se;sw}
and create_quadtree ~get_shape surf elts =
  if List.length elts > max_node_elements
  then begin
    let (nw,ne,se,sw) = quadimate surf in
    let (nwp, nep, sep, swp, qp) =
      List.fold_left (fun (nwp, nep, sep, swp, qp) vl ->
          let shape = get_shape vl in
          if contains nw shape
          then (vl :: nwp, nep, sep, swp, qp)
          else if contains ne shape
          then (nwp, vl :: nep, sep, swp, qp)
          else if contains se shape
          then (nwp, nep, vl :: sep, swp, qp)
          else if contains sw shape
          then (nwp, nep, sep, vl :: swp, qp)
          else (nwp, nep, sep, swp, vl :: qp)
        ) ([],[],[],[],[]) elts in
    let nw = create_quadtree ~get_shape nw nwp
    and ne = create_quadtree ~get_shape ne nep
    and se = create_quadtree ~get_shape se sep
    and sw = create_quadtree ~get_shape sw swp in
    let corner = fst surf and sz = fst @@ snd surf in
    Node {corner=(corner,sz); elts=qp; nw; ne; se; sw}
  end
  else Leaf (elts, fst surf, fst @@ snd surf)

let rec retrieve_at ~get_shape point tree =
  let lookup f elts = 
    let rec find acc = function
      | vl :: rest ->
        if contains_point (get_shape vl) point
        then f (List.rev_append acc rest, vl)
        else find (vl :: acc) rest
      | [] -> None in
    find [] elts in
  match tree with
  | Leaf (elts, corner, sz) ->
    lookup (fun (elts, vl) -> Some (Leaf (elts, corner, sz), vl)) elts
  | Node { corner; elts; nw; ne; se; sw } ->
    match lookup (fun (elts, vl) -> Some (Node { corner; elts; nw; ne; se; sw }, vl)) elts with
    | Some v -> Some v
    | None -> 
      let (let+) x f =  Option.bind x f in
      if contains_point (get_surf nw) point
      then
        let+ nw, vl = retrieve_at ~get_shape point nw in
        Some (Node {corner; elts; nw;ne;se;sw}, vl)
      else if contains_point (get_surf ne) point
      then
        let+ ne,vl = retrieve_at ~get_shape point ne in
        Some (Node {corner; elts; nw;ne;se;sw}, vl)
      else if contains_point (get_surf se) point
      then
        let+ se, vl = retrieve_at ~get_shape point se in
        Some (Node {corner; elts; nw;ne;se;sw}, vl)
      else if contains_point (get_surf sw) point
      then
        let+ sw, vl = retrieve_at ~get_shape point sw in
        Some (Node {corner; elts; nw;ne;se;sw}, vl)
      else None

