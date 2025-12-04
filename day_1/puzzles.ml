let modulo x y = 
    let r = x mod y in
    if r < 0 then r + y
    else r

let solve f input_list = snd @@ List.fold_left (
    fun (pos, res) input ->
        let num = int_of_string @@ String.sub input 1 @@ ((String.length input) - 1) in
        let new_pos = 
            if String.starts_with "R" input then 
                pos + num
            else 
                pos - num
        in f pos new_pos res
    ) (50, 0) input_list

let solve1 =
    solve (
        fun pos new_pos res ->
            let mnew_pos = modulo new_pos 100 in
            (mnew_pos, res + if mnew_pos = 0 then 1 else 0)
    )

let solve2 = 
    solve (
        fun pos new_pos res -> 
            (modulo new_pos 100, res + if (new_pos < 0 && pos != 0) || new_pos = 0 then (abs new_pos / 100) + 1 else abs new_pos / 100)
    ) 

let () = 
    In_channel.with_open_text "day_1/input.txt" (
        fun input_channel -> 
            let input = In_channel.input_lines input_channel in
            print_string "First puzzle: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Second puzzle: ";
            print_int @@ solve2 input;
            print_newline ()
    )
