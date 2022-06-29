let main =
  match Image.IO.get_image "input/img.jpg" with
  | None -> failwith ("Error loading image")
  | Some img ->
     let dest = Image.copy img in
     Image.for_each_pixel dest (fun p -> Dithering.dither dest p);
     Image.IO.set_image dest "output/output.jpg"
