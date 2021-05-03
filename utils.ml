let stb_buffer_to_bigarray image =
  let pad_image_data ?default from to_ data =
    let open Bigarray in
    let old_data_byte_size = Array1.dim data in
    let old_array_size = old_data_byte_size / from in
    let new_array_byte_size = old_array_size * to_ in
    let new_data = Array1.create int8_unsigned c_layout new_array_byte_size in
    let new_pos = ref 0 in
    let old_pos = ref 0 in
    for _ = 1 to old_array_size do
      let fv = data.{!old_pos} in
      for offset = 0 to from - 1 do
        new_data.{!new_pos + offset} <- data.{!old_pos + from - offset - 1};
      done;
      for _ = 0 to from - 1 do
        incr new_pos; incr old_pos
      done;
      begin match default with
        | None ->
          for _ = 1 to to_ - from do
            new_data.{!new_pos} <- fv;
            incr new_pos
          done
        | Some deflt ->
          for _ = 1 to to_ - from do
            new_data.{!new_pos} <- deflt;
            incr new_pos
          done
      end;
    done;
    new_data in
  let channels = Stb_image.channels image in
  let data = match channels with
    | 4 -> Stb_image.data image
    | n when n > 0 && n <= 3 ->
      pad_image_data n 4 (Stb_image.data image)
    | n  ->
      invalid_arg (Printf.sprintf "image has an unsupported number of channels %d" n) in
  data

let stb_buffer_to_cairo_surface image =
  let w = Stb_image.width image and h = Stb_image.height image in
  let channels = Stb_image.channels image in
  let data = stb_buffer_to_bigarray image in
  let format = match channels with 4 -> Cairo.Image.ARGB32 | _ -> Cairo.Image.RGB24 in
  let img = Cairo.Image.create_for_data8 ~stride:(w * 4) data format ~w ~h in
  img
