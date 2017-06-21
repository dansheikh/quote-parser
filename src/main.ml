open Core
open Parser

let main () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in
  try
    match args with
    | None -> Parser.warn None
    | Some [] -> Parser.warn None
    | Some (path :: order) -> Parser.parse path order
    (* | Some args -> List.iter ~f:(printf "%s ") args *)
  with
  | Invalid_argument msg -> printf "%s\n" msg

let () = main ()
