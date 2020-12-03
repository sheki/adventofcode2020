let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

type res = Found of int | Continue of int list

let findPair target lst =
  match lst with
  | [] -> None
  | _ :: ss -> (
      try
        Some
          (List.find (fun n -> List.exists (fun a -> a == target - n) ss) lst)
      with Not_found -> None )

let find x lst =
  if List.exists (fun a -> a == 2020 - x) lst then Found ((2020 - x) * x)
  else Continue (x :: lst)

let find3 x lst =
  match findPair (2020 - x) lst with
  | Some y -> Found (y * x * (2020 - x - y))
  | None -> Continue (x :: lst)

let rec prob1gold acc =
  match maybe_read_line () with
  | None -> ()
  | Some line -> (
      match find3 (int_of_string line) acc with
      | Found x -> print_endline (string_of_int x)
      | Continue x -> prob1gold x )

let rec readline acc =
  match maybe_read_line () with
  | None -> ()
  | Some line -> (
      match find (int_of_string line) acc with
      | Found x -> print_endline (string_of_int x)
      | Continue x -> readline x )

let () = prob1gold []
