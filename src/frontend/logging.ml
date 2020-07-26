
open MyUtil


let show_path abspath =
  let pathstr = get_abs_path_string abspath in
  if OptionState.show_full_path () then pathstr else Filename.basename pathstr


let begin_to_typecheck_file abspath_in =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  type checking '" ^ (show_path abspath_in) ^ "' ...")
  end


let begin_to_preprocess_file abspath_in =
  if not (OptionState.message_format_json ()) then
    print_endline ("  preprocessing '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_eval_file abspath_in =
  if not (OptionState.message_format_json ()) then
    print_endline ("  evaluating '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_parse_file abspath_in =
  if not (OptionState.message_format_json ()) then
    print_endline ("  parsing '" ^ (show_path abspath_in) ^ "' ...")


let pass_type_check opt =
  if not (OptionState.message_format_json ()) then
    match opt with
    | None ->
        print_endline ("  type check passed.")

    | Some(str) ->
        print_endline ("  type check passed. (" ^ str ^ ")")


let ordinal i =
    let suffix =
      match i mod 10 with
      | 1 -> "st"
      | 2 -> "nd"
      | 3 -> "rd"
      | _ -> "th"
    in
    (string_of_int i) ^ suffix


let start_evaluation i =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    if i <= 1 then
      print_endline ("  evaluating texts ...")
    else
      print_endline ("  evaluating texts (" ^ (ordinal i) ^ " trial) ...")
  end


let end_evaluation () =
  if not (OptionState.message_format_json ()) then
    print_endline ("  evaluation done.")


let start_page_break () =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  breaking contents into pages ...")
  end


let needs_another_trial () =
  if not (OptionState.message_format_json ()) then
    print_endline ("  needs another trial for solving cross references...")


let achieve_count_max () =
  if not (OptionState.message_format_json ()) then
    print_endline ("  could not reach to fixpoint when resolving cross references.")


let achieve_fixpoint unresolved_crossrefs =
  if not (OptionState.message_format_json ()) then
    if unresolved_crossrefs = [] then
      print_endline ("  all cross references were solved.")
    else
      print_endline ("  some cross references were not solved: " ^ String.concat " " unresolved_crossrefs ^ ".")


let end_output file_name_out =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  output written on '" ^ (show_path file_name_out) ^ "'.")
  end


let no_output () =
  if not (OptionState.message_format_json ()) then begin
    print_endline " ---- ---- ---- ----";
    print_endline "  no output."
  end


let target_file file_name_out =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  target file: '" ^ (show_path file_name_out) ^ "'")
  end


let dump_file dump_file_exists dump_file =
  if not (OptionState.message_format_json ()) then
    if dump_file_exists then
      print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (already exists)")
    else
      print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (will be created)")


let begin_to_embed_fonts () =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  embedding fonts ...")
  end


let begin_to_write_page () =
  if not (OptionState.message_format_json ()) then begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  writing pages ...")
  end


let show_single_font abbrev relpath =
  if not (OptionState.message_format_json ()) then
    print_endline ("    * `" ^ abbrev ^ "`: '" ^ (get_lib_path_string relpath) ^ "'")


let show_collection_font abbrev relpath i =
  if not (OptionState.message_format_json ()) then
    print_endline ("    * `" ^ abbrev ^ "`: '" ^ (get_lib_path_string relpath) ^ "' [" ^ (string_of_int i) ^ "]")


let show_fonts_main font_hash =
  font_hash |> List.iter (fun (abbrev, data) ->
    match data with
    | FontAccess.Single(relpath)        -> show_single_font abbrev relpath
    | FontAccess.Collection(relpath, i) -> show_collection_font abbrev relpath i
  )


let show_fonts font_hash =
  if not (OptionState.message_format_json ()) then begin
    print_endline "  all the available fonts:";
    show_fonts_main font_hash
  end


let show_math_fonts font_hash =
  if not (OptionState.message_format_json ()) then begin
    print_endline "  all the available math fonts:";
    show_fonts_main font_hash
  end


let warn_deprecated msg =
  if not (OptionState.message_format_json ()) then
    print_endline ("  [Warning] " ^ msg)


let warn_cmyk_image file_name =
  if not (OptionState.message_format_json ()) then begin
    print_endline ("  [Warning] (" ^ (show_path file_name) ^ ") Jpeg images with CMYK color mode are not fully supported.");
    print_endline ("  Please convert the image to a jpeg image with YCbCr (RGB) color model.")
  end


let warn_noninjective_cmap uchpre uch gidorg =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] Multiple Unicode code points (U+%04X and U+%04X) are mapped to the same GID %d.\n" (Uchar.to_int uchpre) (Uchar.to_int uch) gidorg


let warn_noninjective_ligature gidorglig =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] GID %d is used as more than one kind of ligatures.\n" gidorglig


let warn_nonattachable_mark gomark gobase =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] The combining diacritical mark of GID %d cannot be attached to the base glyph of GID %d.\n" gomark gobase


let warn_no_glyph abbrev uch =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] No glyph is provided for U+%04X by font `%s`.\n" (Uchar.to_int uch) abbrev


let warn_no_math_glyph mfabbrev uch =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] No glyph is provided for U+%04X by math font `%s`.\n" (Uchar.to_int uch) mfabbrev


let warn_duplicate_font_hash abbrev relpath =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] more than one font is named `%s`; '%s' will be associated with the font name.\n" abbrev (get_lib_path_string relpath)


let warn_duplicate_math_font_hash mfabbrev relpath =
  if not (OptionState.message_format_json ()) then
    Format.printf "  [Warning] more than one font is named `%s`; '%s' will be associated with the font name.\n" mfabbrev (get_lib_path_string relpath)
