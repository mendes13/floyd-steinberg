module Color = struct
  type t = (int * int * int)

  let of_triple t = t

  let sum (a, b, c) (x, y, z) = (a+x, b+y, c+z)

  let sub (a, b, c) (x, y, z) = (a-x, b-y, c-z)

  let map (a, b, c) ~f = (f a, f b, f c)

  let compress color ~factor =
    let f x =
      let factor = Float.of_int factor in
      let x_int = Float.of_int x in
      (Float.round (factor *. x_int /. 255.0)) *. (255.0 /. factor)
      |> Int.of_float
    in
    map color ~f:f
end

module Pixel = struct
  type position = int * int

  type t =
    {
      pos : position;
      rgb : Color.t;
    }

  let position p = p.pos

  let color p = p.rgb

  let create (x, y)  color =
    {
      pos = x, y;
      rgb = color;
    }
end

type t = (int, Bimage.u8, [`Rgb]) Bimage.Image.t

type dimensions = { w : int; h : int }

let pixel_at image (x, y) =
  let module B = Bimage.Image in
  let r = B.get image x y 0 in
  let g = B.get image x y 1 in
  let b = B.get image x y 2 in
  Pixel.create (x, y) (r, g, b)

let set_pixel_at image (x, y) pixel =
  let module B = Bimage.Image in
  let (r, g, b) = Pixel.color pixel in
  B.set image x y 0 r;
  B.set image x y 1 g;
  B.set image x y 2 b

let copy image = Bimage.Image.copy image

let get_dimensions image =
  let (w, h, _) = Bimage.Image.shape image in
  { w = w; h = h }

let for_each_pixel image fn =
  let f = fun x y _ -> fn (pixel_at image (x, y)) in
  Bimage.Image.for_each f image

module IO = struct
  let get_image file_path =
    let open Bimage in
    let module Magick = Bimage_unix.Magick in
    match Magick.read u8 rgb file_path with
    | Ok img -> Some img
    | Error _ -> None
  
  let set_image image file_path =
    Bimage_unix.Magick.write file_path image
end
