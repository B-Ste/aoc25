open List

let rec transpose t =
    match t with
    [] :: _ -> []
    | c -> (map hd c) :: transpose (map tl c)

let char_list_of_string s = init (String.length s) (String.get s)

let int_of_char_list c = fold_left (fun acc t -> acc * 10 + t) 0 
    @@ map (fun t -> t - 48) @@ map int_of_char (filter (fun t -> t != ' ') c)

let remove_empty_strings = filter (fun t -> not @@ String.equal String.empty t)

let parse1 input = 
    let split_input = map (String.split_on_char ' ') input in
    let split_input_filtered = map (filter (fun s -> not @@ String.equal String.empty s)) split_input in
    let problem_lists = map rev @@ transpose split_input_filtered in
    map (fun t -> (String.get (hd t) 0, map int_of_string (tl t))) problem_lists

let parse2 input = 
    let mod_input = map (fun s -> s ^ " ") input in
    let op_string = nth mod_input 4 in
    let op_sep_plus = remove_empty_strings (String.split_on_char '+' op_string) in
    let op_sep = remove_empty_strings (concat_map (String.split_on_char '*') op_sep_plus) in
    let lengths = map String.length op_sep in
    let input_dis = map (
        fun s -> 
            let s_l = char_list_of_string s in
            fst @@ fold_left (
                fun (list, remaining) current ->
                    (take (current + 1) remaining :: list, drop (current + 1) remaining)
            ) ([], s_l) lengths
    ) mod_input in 
    let problems = transpose input_dis in
    map (
        fun p ->
            let numbers = take 4 p in
            let numbers_t = transpose numbers in
            let numbers_int = map int_of_char_list numbers_t in
            let numbers_cut = take (length numbers_int - 1) numbers_int in
            let op = nth (nth p 4) 0 in
            (op, numbers_cut)
    ) problems

let solve parser input = 
    let problems = parser input in
    let solutions = map (
        fun (op, l) ->
            match op with
            '+' -> fold_left (+) 0 l
            | '*' -> fold_left Int.mul 1 l
            | _ -> raise (Failure "unknown operator")
    ) problems in
    fold_left (+) 0 solutions

let solve1 = solve parse1

let solve2 = solve parse2

let () = 
    In_channel.with_open_text "day_6/input.txt" (
        fun in_channel ->
            let input = In_channel.input_lines in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
