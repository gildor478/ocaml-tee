
(** Redirection of a process output to a file
    @author Sylvain Le Gall
  *)

open Genlex;;

(** 

  This module allow to redirect standard and error output of a process to a
  file. Contrary to standard POSIX "tee" command, the output is redirected in a
  specific format, allowing to still match standard and error output. 

  Use [Tee.run ~file:"test.txt" ["ls"; "-alh"]] to redirect output of "ls -alh" to
  "test.txt". You can read it back using [Tee.from_channel (open_in "test.txt")].

  *)

(** Origin of an output line
  *)
type origin = 
  | Stdout
  | Stderr
;; 

(** Time in seconds
  *)
type seconds = float
;;

(** Possible events 
  *)
type atom =
    
    Command of string       (* Command line to launch process *)
  | Env of string           (* Environment variable *)
  | ExitCode of int         (* Exit code of the process *)
  | Timeout of float        (* Process has not finished at the timeout 
                               defined 
                             *)
  | Warning of string       (* Warning message *)
  | Metadata of string      (* User defined metadata *)
  | Line of origin * string (* Output line of the process *)
;;

(** Full event, with time at which it has happened
  *)
type event =
    seconds * atom
;;

(** Start a process and redirect output
  *)
let run ?(env=Unix.environment ()) ?(metadata=[]) ?timeout ?(grace_time=1.0) ?file cmdline =
  (* Find the correct channel to use and the corresponding way to close it at end *)
  let chn, close_chn = 
    match file with 
      | Some fn ->
          let chn = 
            open_out_gen 
              [Open_wronly; Open_creat; Open_append; Open_text] 
              0o666 
              fn
          in
            chn, 
            (fun () -> close_out chn)
      | None ->
          stdout,
          ignore
  in

  (* Extract and process command line provided *)
  let args =
    Array.of_list cmdline
  in

  let prog =
    match cmdline with 
      | prog :: _ ->
          prog
      | [] ->
          failwith "Empty command line for process to launch"
  in

  (* Time related function/variable *)
  let time = 
    Unix.gettimeofday
  in
  let start_time = 
    time ()
  in

  (* Write events to channel *)
  let output (time, atom) =
    Printf.fprintf chn "%f " time;
    (
      match atom with
        | Command str ->
            Printf.fprintf chn "COMMAND %S\n" str
        | Env str ->
            Printf.fprintf chn "ENV %S\n" str
        | ExitCode i ->
            Printf.fprintf chn "EXIT_CODE %d\n" i
        | Timeout f ->
            Printf.fprintf chn "TIMEOUT %f\n" f
        | Warning s ->
            Printf.fprintf chn "WARNING %S\n" s
        | Metadata str ->
            Printf.fprintf chn "METADATA %S\n" str
        | Line(Stdout, str) ->
            Printf.fprintf chn "STDOUT %S\n" str;
            print_endline str;
            flush stdout
        | Line(Stderr, str) ->
            Printf.fprintf chn "STDERR %S\n" str;
            prerr_endline str;
            flush stderr

    )
  in

  (* Write header *)
  let () = 
    output (start_time, Command (String.concat " " (List.map Filename.quote (Array.to_list args))));
    Array.iter (fun var -> output (start_time, Env var)) env;
    List.iter  (fun vl ->  output (start_time, Metadata vl)) metadata;
  in

  (* Create channel from process *)
  let (child_stdout_descr, proc_stdout_descr) =
    Unix.pipe ()
  in
  let (child_stderr_descr, proc_stderr_descr) = 
    Unix.pipe ()
  in

  (* Launch process *)
  let proc_pid =
    Unix.create_process_env
      prog
      args
      env
      Unix.stdin
      proc_stdout_descr
      proc_stderr_descr
  in

  (* Close useless descriptor *)
  let () = 
    Unix.close proc_stdout_descr;
    Unix.close proc_stderr_descr
  in

  (* Input buffer *)
  let strlen = 
    64
  in
  let buff_stdout = 
    Buffer.create strlen
  in 
  let buff_stderr =
    Buffer.create strlen
  in
  
  (* Flush input *)
  let flush_all_input () = 
    let flush_input orig child_descr buff = 
      let not_ended =
        try
          let buff_str = 
            String.make strlen ' '
          in
          let buff_ready () = 
            Unix.select [child_descr] [] [] 0.0 <> ([], [], [])
          in
          let buff_read () = 
            let byte_read = 
              Unix.read child_descr buff_str 0 strlen
            in
              if byte_read > 0 then
                Buffer.add_substring buff buff_str 0 byte_read
              else 
                raise End_of_file
          in
            while buff_ready () do
              buff_read ()
            done;
            true
        with End_of_file ->
          false
      in
      (** Print every line and return the remaining data *)
      let rec extract_line beg_line i str = 
        let line () = 
          String.sub str beg_line (min (i - beg_line) (String.length str))
        in
          if i < (String.length str) then
            (
              match str.[i] with 
                | '\n' ->
                    (
                      output (time (), Line (orig, line ()));
                      extract_line (i + 1) (i + 1) str
                    )
                | _ ->
                    (
                      extract_line beg_line (i + 1) str
                    )
            )
          else
            (
              line ()
            )
      in
      let remaining_line =
        extract_line 0 0 (Buffer.contents buff)
      in
        Buffer.clear buff;
        Buffer.add_string buff remaining_line;
        not_ended
    in
      flush_input Stdout child_stdout_descr buff_stdout &&
      flush_input Stderr child_stderr_descr buff_stderr
  in

  (* Empty input buffer by flushing remaining data in it, even if
   * it is not complete line of text 
   *)
  let close_all_input () = 
    let close_input orig buff =
      if (Buffer.length buff) > 0 then
        output (time (), Line (orig, Buffer.contents buff));
      Buffer.clear buff
    in
      close_input Stdout buff_stdout;
      close_input Stderr buff_stderr;
  in


  (* Loop for reading data from process *)
  let time_remaining =
    ref 0.0
  in
  let update_time_remaining () = 
    match timeout with 
      | Some f ->
          time_remaining := f -. (time () -. start_time)
      | None ->
          time_remaining := -.1.0
  in
  let () = 
    try 
      update_time_remaining ();
      while flush_all_input () && 
            (!time_remaining > 0.0 || 
             timeout = None) do
        let (_, _, _) =
          Unix.select
            [child_stdout_descr; child_stderr_descr]
            []
            []
            !time_remaining
        in 
          update_time_remaining ()
      done;
      (
        match timeout with 
          | Some f when  ((time ()) -. start_time) > f ->
              output (time (), Timeout ((time ()) -. start_time))
          | _ ->
              ()
      )
    with End_of_file ->
      ()
  in

  (* Ensure that process has stopped *)
  let exit_status = 
    let warning str = 
      output (time (), Warning str)
    in
    let ignore_bool: bool -> unit = 
      ignore
    in
    let check_exit () =
      match Unix.waitpid [Unix.WNOHANG] proc_pid with 
        | 0, _ ->
            None
        | _, status ->
            Some status
    in
    let wait_grace_time () = 
      let start_grace_time =
        time ()
      in
      let exit = 
        ref (check_exit ())
      in
        while !exit = None && 
              (time () -. start_grace_time) < grace_time do
          ignore(Unix.select [] [] [] (grace_time /. 10.0));
          ignore_bool(flush_all_input ());
          exit := check_exit ()
        done;
        ignore_bool(flush_all_input ());
        !exit
    in
      match wait_grace_time () with 
        | Some st ->
            st
        | None ->
            (
              (* Something goes wrong, enter recovery process *)
              warning "Process has not yet ended, killing it with SIGTERM!";
              Unix.kill proc_pid Sys.sigterm;
              match wait_grace_time () with 
                | Some st ->
                    st
                | None ->
                    (
                      warning "Process has still not ended, killing it with SIGKILL!";
                      Unix.kill proc_pid Sys.sigkill;
                      match wait_grace_time () with 
                        | Some st ->
                            st
                        | None ->
                            (
                              warning "Process has not ended, giving up!";
                              close_all_input ();
                              flush chn;
                              close_chn ();
                              failwith (Printf.sprintf "Cannot stop process %d" proc_pid);
                            )
                    )
            )
  in

  (* Close remaining file descriptor *)
  let () = 
    Unix.close child_stdout_descr;
    Unix.close child_stderr_descr
  in
    (* Write last message and close channel *)
    match exit_status with
      | Unix.WEXITED i | Unix.WSIGNALED i | Unix.WSTOPPED i ->
          (
            close_all_input ();
            output (time (), ExitCode i);
            flush chn;
            close_chn ()
          )
;;

(** Read tee data from a stream. This allow to read back a file which has been
    written with a [run] call.
  *)
let from_stream =
  let lexer = 
    make_lexer 
      [
        "COMMAND"; 
        "ENV"; 
        "EXIT_CODE"; 
        "TIMEOUT"; 
        "WARNING"; 
        "METADATA"; 
        "STDOUT"; 
        "STDERR";
      ]
  in
  let rec parse_event =
    parser
      | [<'Float tm; atom = parse_atom >] -> tm, atom
  and parse_atom = 
    parser
      | [<'Kwd "COMMAND"; 'String s >]  -> Command s
      | [<'Kwd "ENV"; 'String s >]      -> Env s
      | [<'Kwd "EXIT_CODE"; 'Int i >]   -> ExitCode i
      | [<'Kwd "TIMEOUT"; 'Float f >]   -> Timeout f
      | [<'Kwd "WARNING"; 'String s >]  -> Warning s
      | [<'Kwd "METADATA"; 'String s >] -> Metadata s
      | [<'Kwd "STDOUT"; 'String s >]   -> Line(Stdout, s)
      | [<'Kwd "STDERR"; 'String s >]   -> Line(Stderr, s)
  in
    fun st -> 
      Stream.from 
        (fun _ ->
           try
             Some (parse_event (lexer st))
           with Stream.Failure ->
             None
        )
;;

(** Read tee data from a channel.
  *)
let from_channel chn =
  from_stream (Stream.of_channel chn)
;;
