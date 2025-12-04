let is_valid1 x = 
    let s_x = string_of_int x in
    let l_x = String.length s_x in
    if l_x mod 2 != 0 then true 
    else 
        let s_r = String.sub s_x 0 (l_x/2) in
        let s_l = String.sub s_x (l_x/2) (l_x/2) in
        (if String.equal s_r s_l then false else true)

let is_valid2 x = 
    let s_x = string_of_int x in
    let rec of_seq s t = 
        if String.length t = 0 then true
        else
            (if String.starts_with s t then
                of_seq s @@ String.sub t (String.length s) (String.length t - String.length s)
            else false)
    in
        not @@ List.fold_left (||) false @@ 
        List.map (fun i -> of_seq (String.sub s_x 0 i) s_x) @@ 
        List.init (String.length s_x / 2) (fun i -> i + 1)

let solve_range valid_fun (a, b) = 
    let rec solve_range_internal a b = 
        if a > b then []
        else 
            (if not (valid_fun a) then
                a::solve_range_internal (a + 1) b
            else 
                solve_range_internal (a + 1) b)
    in solve_range_internal a b

let solve valid_fun input_string = 
    let ranges = List.map (
            fun s ->
                let (a::b::_) = String.split_on_char '-' s in
                (int_of_string a, int_of_string b)
        ) @@ String.split_on_char ',' input_string in
    List.fold_left (+) 0 @@ List.flatten @@ List.map (solve_range valid_fun) ranges

let solve1 = solve is_valid1

let solve2 = solve is_valid2

let () = 
    In_channel.with_open_text "day_2/input.txt" (
        fun in_channel ->
            let input = In_channel.input_all in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
