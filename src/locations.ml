open Lexing

type 'a loc = {
  obj: 'a;
  loc: location;
}

and location = {
    loc_start: position; loc_end: position;
    loc_ghost: bool;
    loc_source: source;
    loc_string: string option;
  }
and source =
  | String of string
  | File of string * lexbuf
  | Toplevel of lexbuf
  | Stdin of lexbuf
  | Unknown

let with_loc loc obj = { loc with obj = obj }
let map_loc f loc = with_loc loc (f loc.obj)


let loc_source = ref Unknown
let set_source src = loc_source := src
let get_source () = !loc_source

let get_loc_string source loc_start loc_end =
  let left = loc_start.Lexing.pos_cnum in
  let right = loc_end.Lexing.pos_cnum in
  let len = max left right - min left right in
  (* there is a strange bug here : on non-toplevel inputs (stdin,
     files), location positions are wrong (left < right) for the
     end-of-phrase ';;', wich cause String.sub to fail.

     I have no idea where the bug come from, despite serious
     searching. This is a not-that-bad workaround. *)
    match source with
    | Unknown ->
        None
    | String str ->
        Some (String.sub str left len)
    | Toplevel lb | Stdin lb | File (_, lb) ->
        (* for some very long tokens (comments and strings) 
           the right location might exceed the buffer length.
           In that case, cut it to avoid a String.sub failure. *)
        let str = Bytes.to_string lb.Lexing.lex_buffer in
        Some
          (if left + len <= String.length str then
            String.sub str left len
           else if left < String.length str then
            String.sub str left (String.length str - left) ^ "..."
           else "...")

let loc_aux ?(source = !loc_source) ghost p1 p2 =
  {
   loc_start = p1;
   loc_end = p2;
   loc_ghost = ghost;
   loc_source = source;
   loc_string = get_loc_string source p1 p2;
  }

let loc_ = loc_aux false

let dummy_loc = {
  loc_start = dummy_pos;
  loc_end = dummy_pos;
  loc_ghost = true;
  loc_source = Unknown;
  loc_string = None;
}

let dummy_located a = { obj = a; loc = dummy_loc }

let string_pos k = {
  pos_fname = ""; 
  pos_lnum = 1;
  pos_bol = k;
  pos_cnum = k;
}

let loc_of_string str =  {
  loc_start = string_pos 0; loc_end = string_pos (String.length str);
  loc_ghost = false;
  loc_source = String str;
  loc_string = Some str;
}

let loc ?(source= !loc_source) ?(ghost=false) e p1 p2 =
  (* Why isn't ?source passed to loc_aux ? *)
  ignore source;
  { obj = e; loc = loc_aux ghost p1 p2}

let noloc e = 
  { obj = e ; loc = loc_aux true dummy_pos dummy_pos }

let curr_loc lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false;
  loc_source = !loc_source;
  loc_string = Some (Lexing.lexeme lexbuf);
};;

let merge_source sa sb = match sa, sb with
  | String a, String b when a = b -> sa
  | File (a, _), File (b, _) when a = b -> sa
  | Toplevel _, Toplevel _
  | Stdin _, Stdin _
    -> sa
  | String _, _
  | File _, _
  | Toplevel _, _
  | Stdin _, _
  | Unknown, _
    -> Unknown

let join l1 l2 =
  let source = merge_source l1.loc_source l2.loc_source in
  loc_aux ~source (l1.loc_ghost || l2.loc_ghost) l1.loc_start l2.loc_end

let string_of_loc loc =
  if loc.loc_ghost then "<dummy loc>"
  else
    match loc.loc_string with
    | Some str -> str
    | None -> "<dummy loc string>"

            
let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  ("File \"", "\", line ", ", characters ", "-", ":", "")
   
open Format

let string_loc loc =
  match loc.loc_source with
  | File (filename, _) ->
     let pos = loc.loc_start in
     let linenum = pos.pos_lnum in
     let linebeg = pos.pos_bol in
     let startchar = pos.pos_cnum -linebeg in
     let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
     sprintf "%s%s%s%i" msg_file filename msg_line linenum ^
     sprintf "%s%i" msg_chars startchar ^
     sprintf "%s%i%s@.%s" msg_to endchar msg_colon msg_head
  | _ ->
     sprintf "Characters %i-%i:@."
       loc.loc_start.pos_cnum
       loc.loc_end.pos_cnum

let print_loc ppf loc = fprintf ppf "%s" (string_loc loc)

                          

       
