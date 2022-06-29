let args () =
  match Array.to_list Sys.argv with
  | [_; input; output] -> input, output
  | _ ->
     print_endline "Usage: dune exec ./src/main.exe [input file] [output file]";
     exit 0

let main =
  let (input, output) = args () in
  match Image.IO.get_image input with
  | None -> failwith "Error loading image"
  | Some img ->
     let dest = Image.copy img in
     Image.for_each_pixel dest (fun p -> Dithering.dither dest p);
     Image.IO.set_image dest output
