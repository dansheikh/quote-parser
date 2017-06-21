open Core

module Parser = struct
  type bid = {quantity : int; price : float}
  type ask = {quantity : int; price : float}
  type quote = {packet_time : string; accept_time : string; bids : bid list; asks : ask list}

  let warn = fun args ->
    if args = None then raise (Invalid_argument "Too few arguments; require (file) path and optional order (-r).")
    else raise (Invalid_argument "Too many arguments; only require (file) path and optional order (-r).")

  let rec destruct file buf window = In_channel.input_byte file |> function
    | None -> []
    | Some num -> if num = 255 then
        let chars = List.map buf ~f:(fun num -> Char.of_int num |> function Some c -> c | None -> ' ') |> Array.of_list in
        (* Extract data from character array *)
        let item = {packet_time = Time.format (Time.now ()) ~zone:(Lazy.force Time.Zone.local) "%H:%M:%S";
                    accept_time = "TODO";
                    bids = [];
                    asks = []} in
        item :: destruct file [] []
      else
      if List.length buf = 0 then
        match (window @ (num :: [])) with
        | [66; 54; 48; 51; 52] -> destruct file (buf @ (num :: [])) window
        | catchall -> if List.length catchall = 5 then destruct file buf (List.tl catchall |> function Some values -> values | None -> [])
          else destruct file buf catchall
      else
        destruct file (buf @ (num :: [])) window

  let parse filename order = In_channel.with_file filename ~f:(fun file -> destruct file [] [])
end
