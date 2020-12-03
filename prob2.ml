open Stdio
open Base

let rgx = Re2.create_exn "(\d+)-(\d+) (\w): (\w+)"

type input = { low : int; high : int; password : string; token : string }

let parseLine l =
  let matches = Re2.find_submatches_exn rgx l in
  {
    low = matches.(1) |> Option.value_exn |> Int.of_string;
    high = matches.(2) |> Option.value_exn |> Int.of_string;
    token = matches.(3) |> Option.value_exn;
    password = matches.(4) |> Option.value_exn;
  }

let findMatchCount t =
  let rgx = Re2.create_exn t.token in
  match Re2.find_all rgx t.password with Ok v -> List.length v | Error _ -> 0

let getStr h index = try h.[index-1] with Invalid_argument _ -> '@'

let secondPart t =
  let f = getStr t.password t.low in
  let s = getStr t.password t.high in
  let c = Char.of_string t.token in
  if
    (phys_equal f c && phys_equal s c)
    || ((not (phys_equal f c)) && not (phys_equal s c))
  then 0
  else 1

let isValid t =
  let c = findMatchCount t in
  if c >= t.low && c <= t.high then 1 else 0

let readLines : int =
  In_channel.fold_lines Stdio.stdin ~init:0 ~f:(fun x str ->
      x + (parseLine str |> secondPart))

let () = readLines |> Int.to_string |> Stdio.print_string
