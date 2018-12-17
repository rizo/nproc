
open Printf
open Lwt.Infix

let ncpu = 8

let pool, terminate =
  Nproc.create ncpu


let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)


let main =
  let input = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let results = List.map
    (fun n ->
       Nproc.submit pool ~f:factorial n >>= function
       | Some res ->
         Lwt.return (printf "factorial %d = %d\n" n res)
       | None ->
         Lwt.return (printf "could not compute factorial %d\n" n))
    input in
  Lwt.join results

let () = Lwt_main.run main



