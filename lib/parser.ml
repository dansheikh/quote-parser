open Core

module Parser = struct
  type bid = {quantity : int; price : float}
  type ask = {quantity : int; price : float}
  type quote = {packet_time : string; accept_time : string; bids : bid list; asks : ask list}

  let warn = fun args ->
    if args = None then raise (Invalid_argument "Too few arguments; require (file) path and optional order (-r).")
    else raise (Invalid_argument "Too many arguments; only require (file) path and optional order (-r).")

  let combine_array col = Array.fold_right ~f:(fun x y -> (Char.escaped x) ^ y) col ~init:""

  let combine_list col = List.fold_right ~f:(fun x y -> (Char.escaped x) ^ y) col ~init:""

  let extract_quantity_price col = let qty = (Array.slice col 0 5) and let price = (Array.slice col 5 12) in
    {quantity = Int.of_string (combine_array qty); price = Float.of_string (combine_array price)}

  let rec extract_quotes col idx = if idx = 60 then [] else extract_quantity_price (Array.slice col (idx - 12) idx) :: extract_quotes (Array.slice col idx (Array.length col)) (idx + 12)

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
