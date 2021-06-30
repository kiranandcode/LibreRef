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

(** RUNTIME_CONTEXT encodes the core GTK elements used to run the application   *)
module type RUNTIME_CONTEXT = sig

  (** Window of main application *)
  val w : GWindow.window

  (** Primary drawing area for application *)
  val d : GMisc.drawing_area

end

(** CONFIG captures the interface with configuration parameters of LibreRef *)
module type CONFIG = sig

  (** retrieve the color used to draw outlines *)
  val get_outline_colour: unit -> (int * int * int)

  (** set the color used to draw outlines *)
  val set_outline_colour:  (int * int * int) -> unit

  (** get the color used to draw the background *)
  val get_background_colour: unit -> (int * int * int)

  (** set the color used to draw the background *)
  val set_background_colour:  (int * int * int) -> unit

  (** get the minimum zoom factor *)
  val get_min_zoom: unit -> float

  (** set the minimum zoom factor *)
  val set_min_zoom: float -> unit

  (** get the maximum zoom factor *)
  val get_max_zoom: unit -> float

  (** set the maximum zoom factor *)
  val set_max_zoom: float -> unit

  (** get whether images are embedded by default *)
  val get_embed_images: unit -> bool

  (** set whether images are embedded by default *)
  val set_embed_images: bool -> unit

  (** get whether caching is used for performance *)
  val get_cache_drawing: unit -> bool

  (** set whether caching is used for performance *)
  val set_cache_drawing: bool -> unit

  (** Save the current config to file  *)
  val save_config: unit -> string list

end


(** LOGIC captures the interface between the UI and the Logic of the application *)
module type LOGIC = sig

  (** [clear_scene ()] resets the application's stored scene to an empty scene   *)
  val clear_scene : unit -> unit

  (** [delete_selected_image ()] deletes the currently selected image.  *)
  val delete_selected_image: unit -> unit

  (** [is_scene_dirty ()] returns true if the application's stored scene has had changes  *)
  val is_scene_dirty : unit -> bool

  (** [current_scene_name ()] returns the filename corresponding to
     the current file if it exists. *)
  val current_scene_name : unit -> string option


  (** [get_title ()] returns the title corresponding to the current application state *)
  val get_title: unit -> string

  (** [add_raw_image_to_scene (x,y) files] loads an image from raw
     data and inserts the image at position (x,y) and returns the list
     of errors encountered while loading the images.

      Note: (x,y) are in screen coordinates not world coordinates.  *)
  val add_raw_image_to_scene: float * float -> GdkPixbuf.pixbuf -> string list

  (** [add_files_to_scene (x,y) files] loads each file in files and
     inserts the image at position (x,y) and returns the list of
     errors encountered while loading the images.

      Note: (x,y) are in screen coordinates not world coordinates.  *)
  val add_files_to_scene: float * float -> [`File of string | `Web of string ] list -> string list

  (** [open_scene_from_file filename] updates the application's stored
     scene to the scene contained in the file at filename, returning
     the list of errors encountered while loading the scene. *)
  val open_scene_from_file : string -> string list

  (** [save_scene_as filename] saves the current stored scene to the
     file at filename, returning the list of errors encountered while
     loading the scene *)
  val save_scene_as : string -> string list

end

module type DIALOG = sig

  (** [handle_quit_application ()] quits the application. If there are
     unsaved changes to the current scene, then it asks the user
     whether these changes should be saved *)
  val handle_quit_application : unit -> unit

  (** [handle_paste_images_at (x,y) ()] pastes images at  *)
  val handle_paste_images_at : float * float -> unit -> unit

  (** [show_right_click_menu ~can_delete button] pops up a menu at the position of
     the cursor *)
  val show_right_click_menu : can_delete:bool -> GdkEvent.Button.t -> unit

  (** [show_errors errors] pops up a dialog box listing all errors *)
  val show_errors : string list -> unit

end

module type UI = sig
  (** Called by GTK to paint the drawing area. *)
  val expose : Cairo.context -> bool

  (** Handle motion events.  *)
  val on_move : GdkEvent.Motion.t -> bool

  (** Handle mouse button release events.  *)
  val on_button_release : GdkEvent.Button.t -> bool

  (** Handle mouse button press events.  *)
  val on_button_press : GdkEvent.Button.t -> bool

  (** Handle scroll events. *)
  val on_scroll : GdkEvent.Scroll.t -> bool

  (** Handle key press  *)
  val on_key_press: GdkEvent.Key.t -> bool
end

module Make : functor
  (Logic : LOGIC)
  (Config: CONFIG)
  (BuildUI : functor (R : RUNTIME_CONTEXT) (D : DIALOG) -> UI) -> sig

  (** Run the libreref GUI *)
  val main : ?initial_scene:string -> unit -> unit

end
