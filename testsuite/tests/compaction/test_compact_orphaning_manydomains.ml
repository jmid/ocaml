(* TEST
*)

let num_domains = 8
let list_size = 40

type cmd =
  | Compact
  | PreAllocList of unit list
  | RevList

let gen_cmd_list () =
 [ PreAllocList (List.init list_size (fun _ -> ()));
   RevList;
   Compact;
   RevList;
   Compact ]

let run c sut = match c with
  | Compact        -> Gc.compact ()
  | PreAllocList l -> sut := l
  | RevList        -> sut := List.rev !sut

let stress_prop_par cmds =
  let sut = ref [] in
  let barrier = Atomic.make num_domains in
  let main () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    List.map (fun c -> Domain.cpu_relax(); run c sut) cmds
  in
  let a = Array.init num_domains (fun _ -> Domain.spawn main) in
  let _ = Array.map Domain.join a in
  sut := [];
  Gc.major ()

let _ =
  for i=1 to 20 do
    let cmds = gen_cmd_list () in
    stress_prop_par cmds;
  done
