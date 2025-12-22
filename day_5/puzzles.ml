let in_range i (a, b) = i >= a && i <= b

let in_range_list r i = 
    List.fold_right (fun k acc -> acc || in_range i k) r false

let parse f input = let detect_not_empty a = not @@ String.equal a String.empty in
    let ranges = List.take_while detect_not_empty input in
    let range_tuples = f @@ List.map (
        fun s -> 
            let (a :: b :: _) = String.split_on_char '-' s in
            (int_of_string a, int_of_string b)
    ) ranges in
    let ids = List.map int_of_string @@ List.tl @@ List.drop_while detect_not_empty input in
    (range_tuples, ids)

let solve1 input = 
    let (range_tuples, ids) = parse Fun.id input in
    let valid_ids = List.filter (in_range_list range_tuples) ids in
    List.length valid_ids

let solve2 input = 
    let (range_tuples, _) = parse (List.sort compare) input in
    let rec merge ranges = 
        match ranges with 
        ((a1, a2) :: (b1, b2) :: xs) -> 
            if (a2 + 1 >= b1) then merge ((a1, if a2 >= b2 then a2 else b2) :: xs)
            else (a1, a2) :: merge ((b1, b2) :: xs)
        | c -> c
    in
    List.fold_left (fun acc (a, b) -> acc + b - a + 1) 0 (merge range_tuples)

let () = 
    In_channel.with_open_text "day_5/input.txt" (
        fun in_channel ->
            let input = In_channel.input_lines in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
    