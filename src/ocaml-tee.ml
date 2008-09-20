
let () = 
  let env = 
    ref []
  in
  let metadata = 
    ref []
  in
  let timeout =
    ref None
  in
  let grace_time =
    ref 1.0
  in
  let cmdline =
    ref []
  in
  let output =
    ref None
  in
  let () =
    Arg.parse
      (Arg.align
         [
           "--full-env",
           Arg.Unit 
             (fun () -> 
                env := List.rev_append (Array.to_list (Unix.environment ())) !env
             ),
           " Add current environment to process environment";

           "--env",
           Arg.String (fun vl -> env := vl :: !env),
           "var=val Add a variable to process environment";

           "--metadata",
           Arg.String (fun md -> metadata := md :: !metadata),
           "str Add metadata to output";

           "--timeout",
           Arg.Float (fun tm -> timeout := Some tm),
           "seconds Time before considering running process stalled";

           "--grace-time",
           Arg.Set_float grace_time,
           "seconds Time to wait end of process";


           "--",
           Arg.Rest (fun arg -> cmdline := arg :: !cmdline),
           " Command line process to run";
         ]
      )

      (fun fn -> output := Some fn)

      "\
      ocaml-tee v0.0.1\n\
      Copyright © 2008, Sylvain Le Gall\n\
      License LGPL v2.1 or later, with OCaml linking exception\n\
      \n\
      A program to redirect of stdout/stderr to a file. \n\
      \n\
      ocaml-tee [options] file -- program [args]
      \n\
      Options:\n"
  in
  let f = 
    Tee.run ~env:(Array.of_list (List.rev !env)) ~metadata:(List.rev !metadata) ~grace_time:!grace_time 
  in
  let cmdline =
    List.rev !cmdline
  in
    match !timeout, !output with 
      | Some tm, Some fn ->
          f ~timeout:tm ~file:fn cmdline
      | Some tm, None ->
          f ~timeout:tm cmdline
      | None, Some fn ->
          f ~file:fn cmdline
      | None, None ->
          f cmdline
;;
