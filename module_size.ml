open Rresult.R.Infix
open Astring

let of_hex str =
  String.fold_left (fun acc -> function
      | '0'..'9' as c -> (acc lsl 4) + int_of_char c - 0x30
      | 'a'..'f' as c -> (acc lsl 4) + int_of_char c - 0x61 + 10
      | 'A'..'F' as c -> (acc lsl 4) + int_of_char c - 0x41 + 10
      | c -> Logs.err (fun m -> m "expected hex, found %c" c) ; acc)
    0 str

let pp_typ ppf = function
  | `Text -> Fmt.string ppf "text"
  | `Data -> Fmt.string ppf "data"

let parse_typ typ =
  if String.length typ = 1 then
    match String.get typ 0 with
    | 'T' -> Ok `Text
    | 'D' -> Ok `Data
    | _ -> Error (`Msg ("unknown type: " ^ typ))
  else
    Error (`Msg ("cannot parse type: " ^ typ))

let parse_mark mark =
  if String.equal mark "code_begin" then Ok `Begin
  else if String.equal mark "code_end" then Ok `End
  else if String.equal mark "data_begin" then Ok `Begin
  else if String.equal mark "data_end" then Ok `End
  else begin
    Logs.debug (fun m -> m "neither code_begin nor code_end %s" mark) ;
    Error (`Msg "no code")
  end

let classify line =
  match String.cuts ~sep:" " line with
  | [ address ; typ ; name ] ->
    parse_typ typ >>= fun typ ->
    let addr = of_hex address in
    if String.is_prefix ~affix:"caml" name then
      match String.cuts ~sep:"__" (String.with_range ~first:4 name) with
      | [ one ; mark ] -> parse_mark mark >>| fun tag -> (tag, one, typ, addr)
      | [ one ; two ; mark ] ->
        let name = one ^ "_" ^ two in
        parse_mark mark >>| fun tag -> (tag, name, typ, addr)
      | _ ->
        Logs.debug (fun m -> m "cannot parse %s (no or multiple __)" name) ;
        Error (`Msg "no __")
    else begin
      Logs.debug (fun m -> m "ignoring %s, no caml prefix" name) ;
      Error (`Msg "no caml prefix")
    end
  | _ ->
    Logs.warn (fun m -> m "not a triple %s" line) ;
    Error (`Msg "not a triple")

let jump _ filename =
  let cmd = Bos.Cmd.(v "nm" % "-n" % filename) in
  Bos.OS.Cmd.(run_out cmd |> out_lines) >>= fun (lines, _) ->
  let code, data, tmp =
    List.fold_left (fun ((code, data, stash) as acc) l ->
        match classify l with
        | Error _ -> acc
        | Ok (`Begin, modname, typ, address) ->
          let stash' = (modname, typ, address) :: stash in
          code, data, stash'
        | Ok (`End, modname, typ, address) ->
          let matches (n, t, _) = String.equal modname n && typ = t in
          begin match List.find_opt matches stash with
            | Some (_, _, start) ->
              let stash' = List.filter (fun k -> not (matches k)) stash in
              let update map =
                let size = address - start in
                String.Map.add modname size map
              in
              begin match typ with
                | `Text -> update code, data, stash'
                | `Data -> code, update data, stash'
              end
            | None ->
              Logs.warn (fun m -> m "(ignoring) end mark of %s %a before start"
                            modname pp_typ typ) ;
              acc
          end) (String.Map.empty, String.Map.empty, []) lines
  in
  assert (tmp = []) ;
  let stat = try (Unix.stat filename).Unix.st_size with _ -> 0 in
  let cmodules = String.Map.bindings code
  and dmodules = String.Map.bindings data
  in
  let ctotal = List.fold_left (+) 0 (List.map snd cmodules)
  and dtotal = List.fold_left (+) 0 (List.map snd dmodules)
  in
  Logs.app (fun m -> m "%s is %d bytes on disk@.discovered %d bytes OCaml: %d bytes code and %d bytes data"
               filename stat (ctotal + dtotal) ctotal dtotal) ;
  let modules =
    List.map (fun (k, v) -> (k, `Text, v)) cmodules @
    List.map (fun (k, v) -> (k, `Data, v)) dmodules
  in
  let sorted = List.sort (fun (_, _, v) (_, _, v') -> compare v' v) modules in
  List.iter (fun (k, t, v) ->
      Logs.app (fun m -> m "%10d bytes %a %s" v pp_typ t k))
    sorted ;
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let file =
  let doc = "file to analyse" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let cmd =
  Term.(term_result (const jump $ setup_log $ file)),
  Term.info "size" ~version:"%%VERSION_NUM%%"
    ~doc:"Split the size of a binary into OCaml modules"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
