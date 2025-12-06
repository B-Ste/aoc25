let find_max s = 
    let (max_val, max_index, _) = String.fold_left (
        fun (max_val, max_index, curr_index) c ->
            if max_val < c then (c, curr_index, curr_index + 1)
            else (max_val, max_index, curr_index + 1)
    ) ('0', 0, 0) s in (max_val, max_index)

let solve_single n input = 
    let (first_max_val, first_max_index) = find_max @@ String.sub input 0 @@ String.length input - n + 1 in
    let (max_val_list, _, _) = List.fold_left (fun (max_val_list, max_index, rem_n) _ ->
            let relevant_string = String.sub input (max_index + 1) (String.length input - max_index - rem_n) in
            let (max_val, new_max_index) = find_max relevant_string in
            ((int_of_char max_val - 48)::max_val_list, max_index + new_max_index + 1, rem_n - 1)
        ) ([int_of_char first_max_val - 48], first_max_index, n - 1) @@ List.init (n - 1) (fun _ -> 0) in
    List.fold_right (fun x y -> y * 10 + x) max_val_list 0

let solve n input_list = 
    List.fold_left (+) 0 @@ List.map (solve_single n) input_list

let solve1 = solve 2

let solve2 = solve 12

let () = 
    In_channel.with_open_text "day_3/input.txt" (
        fun in_channel ->
            let input = In_channel.input_lines in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
    