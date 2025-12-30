open String
open List

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let rec only_uneven_lines l = 
    match l with
      [] -> []
    | [_] -> []
    | a :: b :: xs -> b :: only_uneven_lines xs

let parse input = 
    let start_index = index_from (hd input) 0 'S' in
    let splitter_lines = only_uneven_lines (tl input) in
    let splitter_positions = map (
        fun line -> 
            fst @@ String.fold_right (
                fun c (list, i) ->
                    if c = '^' then (i :: list, i + 1)
                    else (list, i + 1)
            ) line ([], 0)
    ) splitter_lines in
    (start_index, splitter_positions)

let solve1 input = 
    let (start_index, splitter_positions) = parse input in
    let start_set = IntSet.singleton start_index in
    fst @@ fold_left (
        fun (n, set) line -> 
            fold_left (
                fun (n, new_set) pos ->
                    if IntSet.mem pos set then
                        (n + 1, IntSet.remove pos new_set |> IntSet.add (pos + 1) |> IntSet.add (pos - 1))
                    else (n, new_set)
            ) (n, set) line
    ) (0, start_set) splitter_positions

let solve2 input = 
    let (start_index, splitter_positions) = parse input in
    let start_map = IntMap.singleton start_index 1 in
    let end_map = fold_left (
        fun m line -> 
            fold_left (
                fun new_m pos ->
                    match IntMap.find_opt pos new_m with
                    Some i -> 
                        let left_integration = 
                            match IntMap.find_opt (pos - 1) new_m with
                            Some v -> IntMap.add (pos - 1) (v + i) new_m
                            | None -> IntMap.add (pos - 1) i new_m
                        in let right_integration = match IntMap.find_opt (pos + 1) left_integration with
                            Some v -> IntMap.add (pos + 1) (v + i) left_integration
                            | None -> IntMap.add (pos + 1) i left_integration
                        in IntMap.remove pos right_integration
                    | None -> new_m
            ) m line
    ) start_map splitter_positions in
    IntMap.fold (fun _ i acc -> i + acc) end_map 0 

let () = 
    In_channel.with_open_text "day_7/input.txt" (
        fun in_channel ->
            let input = In_channel.input_lines in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
