module Pixel : sig
  type t =
    {
        x : int;
        y : int;
        rgb : int * int * int;
    }

  type position = int * int

  (* TODO: create module probably *)
  type rgb_color = int * int * int

  val rgb : t -> int * int * int

  val create : position -> rgb_color -> t
end

type t

type dimensions = { w : int; h : int }

val pixel_at : t -> int -> int -> Pixel.t

val set_pixel_at : t -> int -> int -> Pixel.t -> unit

val copy : t -> t

val get_dimensions : t -> dimensions

val for_each_pixel : t -> (Pixel.t -> unit) -> unit

module IO : sig

(* TODO: maybe use a abstraction for file_path instead of plain string *)
  val get_image : string -> t option
  
  val set_image : t -> string -> unit
end
