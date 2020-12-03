open Stdio
open Base

let rgx = Re2.create_exn "(\d+)-(\d+) (\w): (\w+)"

type input = {
  low: int;
  high: int;
  password: string;
  token: string;
}

let parseLine l = let matches = (Re2.find_submatches_exn rgx l) in {
    low = (Array.get matches 1) |> Option.value_exn |> Int.of_string;
    high = (Array.get matches 2) |> Option.value_exn |> Int.of_string;
    token    = (Array.get matches 3) |> Option.value_exn;
    password = (Array.get matches 4) |> Option.value_exn;
}

let findMatchCount t = let rgx = Re2.create_exn t.token in match (Re2.find_all rgx t.password) with 
  | Ok(v) -> List.length v
  | Error(_) -> 0

let isValid t = let c = findMatchCount t in if (c >= t.low) && (c <= t.high) then 1 else 0

let readLines : int = In_channel.fold_lines Stdio.stdin ~init:0 ~f:(fun x str -> x+ (parseLine str |> isValid)) 
let () = readLines  |> Int.to_string|> Stdio.print_string 
