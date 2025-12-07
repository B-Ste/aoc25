module IntTuples = struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
        match Stdlib.compare x0 x1 with
             0 -> Stdlib.compare y0 y1
            | c -> c
end

module IntTupleSet = Set.Make(IntTuples)

let parse_input input_list = snd @@ List.fold_left (
        fun (y, set) input_line ->
            let (_, new_set) = String.fold_left (
                fun (x, set) c ->
                    match c with
                    '@' -> (x + 1, IntTupleSet.add (x, y) set)
                    | _ -> (x + 1, set)
            ) (0, set) input_line
            in (y + 1, new_set)
    ) (0, IntTupleSet.empty) input_list

let neighbors x y x_limit y_limit = 
    List.filter (fun (x, y) -> x >= 0 && x < x_limit && y >= 0 && y < y_limit) 
        ((x - 1, y - 1) :: (x, y - 1) :: (x + 1, y - 1) :: (x - 1, y) :: 
        (x + 1, y) :: (x - 1, y + 1) :: (x, y + 1) :: (x + 1, y + 1) :: [])

let accessible_rolls roll_set x_limit y_limit = IntTupleSet.filter (
        fun (x, y) ->
            let next = neighbors x y x_limit y_limit in
            let n = List.fold_left (+) 0 @@ 
                List.map (fun i -> try let _ = IntTupleSet.find i roll_set in 1 with Not_found -> 0) next in
            n < 4
    ) roll_set

let solve1 input_list = 
    let roll_set = parse_input input_list in 
    let y_limit = List.length input_list in
    let x_limit = String.length (List.hd input_list) in
    IntTupleSet.cardinal @@ accessible_rolls roll_set x_limit y_limit

let solve2 input_list = 
    let roll_set = parse_input input_list in 
    let y_limit = List.length input_list in
    let x_limit = String.length (List.hd input_list) in
    let rec solve2_loop acc set = 
        let accs_rolls = accessible_rolls set x_limit y_limit in
        match IntTupleSet.cardinal accs_rolls with
          0 -> acc
        | c -> solve2_loop (acc + c) (IntTupleSet.diff set accs_rolls)
    in solve2_loop 0 roll_set

let () = 
    In_channel.with_open_text "day_4/input.txt" (
        fun in_channel ->
            let input = In_channel.input_lines in_channel in
            print_string "Puzzle 1: ";
            print_int @@ solve1 input;
            print_newline ();
            print_string "Puzzle 2: ";
            print_int @@ solve2 input;
            print_newline ()
    )
    