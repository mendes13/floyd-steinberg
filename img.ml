module Pixel = struct
  type t =
    {
        x : int;
        y : int;
        rgb : (int * int * int);
    }
end

module FilePath : sig
  type t

  val of_string : string -> t
end
=
struct
  type t = string

  let of_string s = s
end

module Image : sig
  type t

  type dimensions = { w : int; h : int }

  val pixel_at : t -> int -> int -> Pixel.t

  val set_pixel_at : t -> int -> int -> Pixel.t -> unit

  val copy : t -> t

  val get_dimensions : t -> dimensions

  (* TODO: this should be in an IO module *)
  (* TODO: maybe use a abstraction for file_path instead of plain string *)
  val get_image : string -> t option

  val set_image : t -> string -> unit

  val for_each_pixel : t -> (Pixel.t -> unit) -> unit
end
=
struct
  open Pixel

  type t = (int, Bimage.u8, [`Rgb]) Bimage.Image.t

  type dimensions = { w : int; h : int }

  let pixel_at image x y =
    let open Bimage in
    let r = Image.get image x y 0 in
    let g = Image.get image x y 1 in
    let b = Image.get image x y 2 in
    { x = x; y = y; rgb = (r, g, b) }

  let set_pixel_at image x y pixel =
    let open Bimage in
    let (r, g, b) = pixel.rgb in
    Image.set image x y 0 r;
    Image.set image x y 1 g;
    Image.set image x y 2 b;
    ()

  let copy image = Bimage.Image.copy image

  let get_dimensions image =
    let (w, h, _) = Bimage.Image.shape image in
    { w = w; h = h }

  let get_image file_path =
    let open Bimage in
    let open Bimage_unix in
    match Magick.read u8 rgb file_path with
    | Ok img -> Some img
    | Error _ -> None

  let set_image image file_path =
    Bimage_unix.Magick.write file_path image

  let for_each_pixel image fn =
    Bimage.Image.for_each (fun x y _ -> fn (pixel_at image x y)) image
end

module IO : sig
  val get_image : string -> Image.t option

  val set_image : Image.t -> string -> unit
end
=
struct
  (* TODO: workaround, check how to fix this *)
  let get_image = Image.get_image
  let set_image = Image.set_image
end
  
let map_triple (a, b, c) f = (f a, f b, f c)

let sum_triples (a, b, c) (x, y, z) = (a+x, b+y, c+z)

let find_closest_palette_color pixel =
  let open Pixel in
  let (r, g, b) = pixel.rgb in
  let f x =
    let x_int = Float.of_int x in
    (Float.round (x_int /. 255.0)) *. 255.0
    |> Int.of_float
  in
  { x = pixel.x; y = pixel.y; rgb = (f r, f g, f b) }

let calculate_error old_pixel new_pixel =
  let open Pixel in
  let (r, g, b) = old_pixel.rgb in
  let (nr, ng, nb) = new_pixel.rgb in
  (r-nr, g-ng, b-nb)

module Dithering : sig
  val dither : Image.t -> Pixel.t -> unit
end
=
struct
  let calculate_neighbor_correction pixel error ratio =
    let open Pixel in
    let error_float = map_triple error Float.of_int in
    let multiplied_by_ratio = map_triple error_float (Float.mul ratio) in
    let mult_int = map_triple multiplied_by_ratio Int.of_float in
    let result = sum_triples pixel.rgb (mult_int) in
  
    { x = pixel.x ; y = pixel.y ; rgb = result }

  let set_neighbor_correction image x y ratio error =

    let pixel = (Image.pixel_at image x y) in
    let new_pixel = calculate_neighbor_correction pixel error ratio in
    Image.set_pixel_at image x y new_pixel

  let dither_neighbors image pixel error =
    let open Pixel in

    (* TODO: refactor *)
    let dimensions = Image.get_dimensions image in
    if pixel.x == (dimensions.w-1) || pixel.y == (dimensions.h-1) || pixel.x == 0
        then ()
    else
        let x = pixel.x in
        let y = pixel.y in
        set_neighbor_correction image (x+1) (y  ) (7.0 /. 16.0) error;
        set_neighbor_correction image (x-1) (y+1) (3.0 /. 16.0) error;
        set_neighbor_correction image (x  ) (y+1) (5.0 /. 16.0) error;
        set_neighbor_correction image (x+1) (y+1) (1.0 /. 16.0) error

  let dither image pixel =
    let old_pixel = pixel in
    let new_pixel = find_closest_palette_color old_pixel in
    (* TODO move side effect away *)
    Image.set_pixel_at image pixel.x pixel.y new_pixel;
    let error = calculate_error old_pixel new_pixel in
    dither_neighbors image pixel error
end

let main =
  match IO.get_image "input/img.jpg" with
  | None -> failwith ("Error loading image")
  | Some img ->
     let dest = Image.copy img in
     let () =
       Image.for_each_pixel dest (fun pixel -> Dithering.dither dest pixel)
     in
     IO.set_image dest "output/floydsteinberg-4.jpg"

let () = main
