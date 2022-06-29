module Pixel = struct
  type t =
    {
        x : int;
        y : int;
        rgb : (int * int * int);
    }

  type position = int * int

  type rgb_color = int * int * int

  let rgb p = p.rgb

  let create (x, y)  rgb =
    {
      x = x;
      y = y;
      rgb = rgb;
    }
end

type t = (int, Bimage.u8, [`Rgb]) Bimage.Image.t

type dimensions = { w : int; h : int }

let pixel_at image x y =
  let module B = Bimage.Image in
  let r = B.get image x y 0 in
  let g = B.get image x y 1 in
  let b = B.get image x y 2 in
  Pixel.create (x, y) (r, g, b)

let set_pixel_at image x y pixel =
  let module B = Bimage.Image in
  let (r, g, b) = Pixel.rgb pixel in
  B.set image x y 0 r;
  B.set image x y 1 g;
  B.set image x y 2 b

let copy image = Bimage.Image.copy image

let get_dimensions image =
  let (w, h, _) = Bimage.Image.shape image in
  { w = w; h = h }

let for_each_pixel image fn =
  Bimage.Image.for_each (fun x y _ -> fn (pixel_at image x y)) image

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
