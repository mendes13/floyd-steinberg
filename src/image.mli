module Color : sig
  type t

  val of_triple : int * int * int -> t

  val sum : t -> t -> t

  val sub : t -> t -> t

  val map : t -> f:(int -> int) -> t

  val compress : t -> factor:int -> t
end

module Pixel : sig
  type position = int * int

  type t

  val position : t -> position

  val color : t -> Color.t

  val create : position -> Color.t -> t
end

type t

type dimensions = { w : int; h : int }

val pixel_at : t -> Pixel.position -> Pixel.t

val set_pixel_at : t -> Pixel.position -> Pixel.t -> unit

val copy : t -> t

val get_dimensions : t -> dimensions

val for_each_pixel : t -> (Pixel.t -> unit) -> unit

module IO : sig
  val get_image : string -> t option
  
  val set_image : t -> string -> unit
end
