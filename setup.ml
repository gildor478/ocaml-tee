(* AUTOBUILD_START *)
(* DO NOT EDIT (digest: cefc6688d6750364f387b95bc592fba0) *)
module BaseUtils =
struct
# 1 "src/base/BaseUtils.ml"
  
  (** Remove trailing whitespace *)
  let strip_whitespace str =
    let strlen = 
      String.length str
    in
    let is_whitespace =
      function 
        | ' ' | '\t' | '\r' | '\n' -> true
        | _ -> false
    in 
    let skip_beg =
      let idx =
        ref 0
      in
        while !idx < strlen && is_whitespace str.[!idx] do 
          incr idx 
        done;
        !idx
    in
    let skip_end =
      let idx = 
        ref ((String.length str) - 1)
      in
        while !idx >= 0 && is_whitespace str.[!idx] do
          decr idx
        done;
        !idx
    in
      if skip_beg <= skip_end then
        String.sub str skip_beg (skip_end - skip_beg + 1)
      else
        ""
  ;;
  
  (** Split a string, separator not included
    *)
  let split sep str =
    let str_len =
      String.length str
    in
    let rec split_aux acc pos =
      if pos < str_len then
        (
          let pos_sep = 
            try
              String.index_from str pos sep
            with Not_found ->
              str_len
          in
          let part = 
            String.sub str pos (pos_sep - pos) 
          in
          let acc = 
            part :: acc
          in
            if pos_sep >= str_len then
              (
                (* Nothing more in the string *)
                List.rev acc
              )
            else if pos_sep = (str_len - 1) then
              (
                (* String end with a separator *)
                List.rev ("" :: acc)
              )
            else
              (
                split_aux acc (pos_sep + 1)
              )
        )
      else
        (
          List.rev acc
        )
    in
      split_aux [] 0
  ;;
end;;

module BaseMessage =
struct
# 1 "src/base/BaseMessage.ml"
  
  (** Message to user
      @author Sylvain Le Gall
    *)
  
  let verbose =
    ref true
  ;;
  
  (** Print a warning message 
    *)
  let warning str =
    if !verbose then
      prerr_endline str
  ;;
  
  (** Print an error message and exit.
    *)
  let error str =
    if !verbose then 
      prerr_endline str;
    exit 1
  ;;
  
  (** Print information message.
    *)
  let info str = 
    if !verbose then
      Printf.printf "%s\n%!" str
  ;;
  
  (** Print begin of line when checking for a feature.
    *)
  let checking str =
    if !verbose then
      Printf.printf "checking for %s... %!" str
  ;;
  
  (** Print end of line when checking for a feature.
    *)
  let result str =
    if !verbose then
      Printf.printf "%s\n%!" str
  ;;
  
  (** Print result and return it.
    *)
  let result_wrap str =
    result str;
    str
  ;;
  
end;;

module BaseExec =
struct
# 1 "src/base/BaseExec.ml"
  
  (** Running commands 
      @author Sylvain Le Gall
    *)
  
  (** Run a command 
    *)
  let run cmd args =
    let cmdline =
      String.concat " " (cmd :: args)
    in
      BaseMessage.info 
        (Printf.sprintf "Running command '%s'" cmdline);
      match Sys.command cmdline with 
        | 0 ->
            ()
        | i ->
            failwith 
              (Printf.sprintf 
                 "Command '%s' terminated with error code %d"
                 cmdline i)
  ;;
  
  (** Run a command and returns its output
    *)
  let run_read_output cmd args =
    let fn = 
      Filename.temp_file "ocaml-autobuild" ".txt"
    in
    let () = 
      try
        run cmd (args @ [">"; fn])
      with e ->
        Sys.remove fn;
        raise e
    in
    let chn =
      open_in fn
    in
    let routput =
      ref []
    in
      (
        try
          while true do 
            routput := (input_line chn) :: !routput
          done
        with End_of_file ->
          ()
      );
      close_in chn;
      Sys.remove fn;
      List.rev !routput
  ;;
  
  (** Run a command and returns only first line 
    *)
  let run_read_one_line cmd args = 
    match run_read_output cmd args with 
      | [fst] -> 
          fst
      | lst -> 
          failwith 
            (Printf.sprintf
               "Command return unexpected output %S"
               (String.concat "\n" lst))
  ;;
  
end;;

module BaseFileUtil =
struct
# 1 "src/base/BaseFileUtil.ml"
  
  (** {1 File operation (install, which...)
    *)
  
  (** Find a file among all provided alternatives
    *)
  let find_file paths exts = 
  
    (* Cardinal product of two list *)
    let ( * ) lst1 lst2 = 
      List.flatten 
        (List.map 
           (fun a -> 
              List.map 
                (fun b -> a,b) 
                lst2) 
           lst1)
    in
  
    let rec combined_paths lst = 
      match lst with
        | p1 :: p2 :: tl ->
            let acc = 
              (List.map 
                 (fun (a,b) -> Filename.concat a b) 
                 (p1 * p2))
            in
              combined_paths (acc :: tl)
        | [e] ->
            e
        | [] ->
            []
    in
  
    let alternatives =
      List.map 
        (fun (p,e) -> 
           if String.length e > 0 && e.[0] <> '.' then
             p ^ "." ^ e
           else
             p ^ e) 
        ((combined_paths paths) * exts)
    in
      try 
        List.find Sys.file_exists alternatives
      with Not_found ->
        failwith 
          (Printf.sprintf 
             "Cannot find any of the files: %s"
             (String.concat ", " 
                (List.map 
                   (Printf.sprintf "%S")
                   alternatives)))
  ;;
  
  (** Find real filename of an executable
    *)
  let which prg =
    let path_sep =
      match Sys.os_type with 
        | "Win32" ->
            ';'
        | _ ->
            ':'
    in
    let path_lst =
      BaseUtils.split 
        path_sep 
        (Sys.getenv "PATH")
    in
    let exec_ext = 
      match Sys.os_type with 
        | "Win32" ->
            "" 
            :: 
            (BaseUtils.split 
               path_sep 
               (Sys.getenv "PATHEXT"))
        | _ ->
            [""]
    in
      find_file [path_lst; [prg]] exec_ext;  
  ;;
  
  let cp src tgt = 
    match Sys.os_type with 
      | "Win32" ->
          BaseExec.run "copy" [src; tgt]
      | _ ->
          BaseExec.run "cp" [src; tgt]
  ;;
end;;

module BaseEnvironment =
struct
# 1 "src/base/BaseEnvironment.ml"
  
  (** Environment for configure variable
      @author Sylvain Le Gall
    *)
  
  module Msg    = BaseMessage;;
  module MapVar = Map.Make(String);;
  module SetVar = Set.Make(String);;
  
  (** Variable type
    *)
  type definition =
      {
        default: env -> string * env;
        hidden:  bool;
      }
  
  (** Environment type
    *)
  and env = 
      {
        defined:      definition MapVar.t;
        value:        string MapVar.t;
        print_hidden: bool;
      }
  ;;
  
  (** Add a variable to environment. [hide] allow to store
    * a variable that will be hidden to user (not printed).
    *)
  let var_define ?(hide=false) name dflt env =
    if not (MapVar.mem name env.defined) then
      {
        env with 
            defined = MapVar.add 
                        name 
                        {default = dflt; hidden = hide} 
                        env.defined;
      }
    else
      failwith 
        (Printf.sprintf
           "Variable %S is already defined"
           name)
  ;;
  
  (** Set value of a variable 
    *)
  let var_set name value env =
    if MapVar.mem name env.defined then
      {
        env with
            value = MapVar.add name value env.value;     
      }
    else
      failwith 
        (Printf.sprintf 
           "Cannot set variable %S because it is not defined" 
           name)
  ;;
  
  (** Retrieve the value of a variable
    *)
  let var_get ?(mandatory=false) name env =
    try 
      MapVar.find name env.value, env
    with Not_found ->
      (
        try
          let def = 
            MapVar.find name env.defined
          in
          let dflt, env = 
            def.default env
          in
            if not def.hidden then 
              (
                Msg.checking name;
                Msg.result dflt
              );
            dflt, var_set name dflt env
        with Not_found ->
          (
            if mandatory then
              failwith
                (Printf.sprintf 
                   "Variable %S not defined"
                   name)
            else
              raise Not_found
          )
      )
  ;;
  
  (** Try to get a variable value from environment and if it fails
    * compute it and store it in environment.
    *)
  let var_cache ?(hide) name dflt env =
    try 
      var_get name env
    with Not_found ->
      var_get name (var_define ?hide name dflt env)
  ;;
  
  (** Get all variable
    *)
  let var_all ?(include_hidden=false) ~include_unset env =
    (* Extract variable that have been set and return
     * a map containing defined value that have not 
     * been set yet.
     *)
    let varset_lst, vardflt_mp =
      MapVar.fold
        (fun name value (varset_lst, vardflt_mp) ->
           (* We look if each var is hidden or not *)
           try
             let def =
               MapVar.find name vardflt_mp
             in
             let vardflt_mp =
               (* We won't need this var in the future *)
               MapVar.remove name vardflt_mp
             in
               if include_hidden || (not def.hidden) then
                 name :: varset_lst, vardflt_mp
               else
                 varset_lst, vardflt_mp
           with Not_found ->
             (* No definition we display it *)
             name :: varset_lst, vardflt_mp)
        env.value
        ([], env.defined)
    in
  
      if include_unset then
        (
          (* Combine set var with defaulted var *)
          MapVar.fold
            (fun name def lst ->
               if include_hidden || (not def.hidden) then
                 name :: lst
               else
                 lst)
            vardflt_mp
            varset_lst
        )
      else
        (
          varset_lst
        )
  ;;
  
  (** Expand variable that can be found in string. Variable follow definition of
    * variable for {!Buffer.add_substitute}.
    *)
  let rec var_expand renv str =
    let all_vars () =
      String.concat ", " (var_all ~include_unset:true !renv)
    in
    let buff =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute 
        buff
        (fun var -> 
           try 
             let value, env = 
               var_get var !renv
             in
               renv := env;
               var_expand renv value
           with Not_found ->
             failwith 
               (Printf.sprintf 
                  "No variable %s defined when trying to expand %S \
                   (available: %s)"
                  str var (all_vars ()))) 
        str;
      Buffer.contents buff
  ;;
  
  (** Retrieve a variable value, without taking into account
      environment modification (even warning about it). Variable
      must be defined.
    *)
  let get env nm = 
    let vl, env' =
      var_get ~mandatory:true nm env
    in
      if env <> env' then
        Msg.warning 
          (Printf.sprintf 
             "Evaluation of variable '%s' lead to unexpected \
              environment modification"
             nm);
      vl
  ;;
  
  (** Environment file 
    *)
  let filename =
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      "setup.data"
  ;;
  
  (** Save environment on disk.
    *)
  let dump env = 
    let chn =
      open_out_bin filename
    in
    let _env: env =
      List.fold_left 
        (fun env nm -> 
           let vl, env = 
             var_get nm env 
           in
             Printf.fprintf chn "%s = %S\n" nm vl;
             env)
        env
        (var_all ~include_hidden:true ~include_unset:false env)
    in
      close_out chn
  ;;
  
  (** Initialize environment.
    *)
  let load ?(allow_empty=false) () = 
    let default =
        {
          defined      = MapVar.empty;
          value        = MapVar.empty;
          print_hidden = false;
        }
    in
      if Sys.file_exists filename then
        (
          let chn =
            open_in_bin filename
          in
          let st =
            Stream.of_channel chn
          in
          let line =
            ref 1
          in
          let st_line = 
            Stream.from
              (fun _ ->
                 try
                   match Stream.next st with 
                     | '\n' -> incr line; Some '\n'
                     | c -> Some c
                 with Stream.Failure -> None)
          in
          let lexer = 
            Genlex.make_lexer ["="] st_line
          in
          let rec read_file mp =
            match Stream.npeek 3 lexer with 
              | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String vl] ->
                  Stream.junk lexer; 
                  Stream.junk lexer; 
                  Stream.junk lexer;
                  read_file (MapVar.add nm vl mp)
              | [] ->
                  mp
              | _ ->
                  failwith 
                    (Printf.sprintf 
                       "Malformed data file '%s' line %d"
                       filename !line)
          in
          let mp =
            read_file default.value
          in
            close_in chn;
            {default with value = mp}
        )
      else if allow_empty then
        (
          default
        )
      else
        (
          failwith 
            (Printf.sprintf 
               "Unable to load environment file '%s', maybe run '%s -configure'"
               filename
               Sys.argv.(0))
        )
  ;;
  
  (** Display environment to user.
    *)
  let print env =
    let printable_vars, env =
      List.fold_left
        (fun (acc, env) nm ->
           let vl, env =
             var_get nm env
           in
             ((nm, vl) ::  acc), env)
        ([], env)
        (var_all ~include_hidden:env.print_hidden ~include_unset:false env)
    in
    let max_length = 
      List.fold_left
        (fun mx (name, _) -> max (String.length name) mx)
        0
        printable_vars
    in
    let dot_pad str =
      String.make ((max_length - (String.length str)) + 3) '.'
    in
  
    print_newline ();
    print_endline "Configuration: ";
    print_newline ();
    List.iter 
      (fun (name,value) -> 
         Printf.printf "%s: %s %s\n" name (dot_pad name) value)
      printable_vars;
    print_newline ();
  ;;
  
  (** Equality between two environment 
    *)
  let equal env1 env2 =
    (MapVar.equal (=) env1.value env2.value)
  ;;
  
  (** Default command line arguments 
    *)
  let args renv =
    [
      "--override",
       Arg.Tuple
         (
           let rvr = ref ""
           in
           let rvl = ref ""
           in
             [
               Arg.Set_string rvr;
               Arg.Set_string rvl;
               Arg.Unit (fun () -> renv := var_set !rvr !rvl !renv)
             ]
         ),
      "var+val  Override any configuration variable";
  
      "--print-hidden",
      Arg.Unit (fun () -> renv := {!renv with print_hidden = true}),
      " Print even non-printable variable (debug)";
    ]
  ;;
  
end;;

module BaseExpr =
struct
# 1 "src/base/BaseExpr.ml"
  
  (** Conditional expression like in OASIS.
      @author Sylvain Le Gall
    *)
  
  open BaseEnvironment;;
  
  type t =
    | Bool of bool
    | Not of t
    | And of t * t
    | Or of t * t
    | Flag of string
    | Test of string * string
  ;;
  
  type 'a choices = (t * 'a) list
  ;;
  
  (** Evaluate expression *)
  let rec eval env =
    function
      | Bool b ->
          b, env
      | Not e -> 
          let v, env =
            eval env e 
          in
            not v, env
      | And (e1, e2) ->
          let (v1, env) as res =
            eval env e1
          in
            if v1 then
              eval env e2
            else
              res
      | Or (e1, e2) -> 
          let (v1, env) as res =
            eval env e1
          in
            if not v1 then
              eval env e2
            else
              res
      | Flag nm ->
          let v, env =
            var_get nm env
          in
          let renv =
            ref env
          in
          let v =
            var_expand renv v
          in
          let env = 
            !renv
          in
            assert(v = "true" || v = "false");
            (v = "true"), env
      | Test (nm, vl) ->
          let v, env =
            var_get nm env
          in
          let renv =
            ref env
          in
          let v =
            var_expand renv v
          in
          let env = 
            !renv
          in
            (v = vl), env
  ;;
  
  let choose lst env =
    let res, env =
      List.fold_left
        (fun (res, env) (cond, vl) ->
           let res_cond, env =
             eval env cond
           in
             (if res_cond then Some vl else res), env)
        (None, env)
        lst
    in
      match res with 
        | Some vl ->
            vl, env
        | None ->
            failwith "No result for a choice list"
  ;;
  
end;;

module BaseArgExt =
struct
# 1 "src/base/BaseArgExt.ml"
  
  (** Handle command line argument
      @author Sylvain Le Gall
    *)
  
  open BaseEnvironment;;
  
  let tr_arg str =
    let buff =
      Buffer.create (String.length str)
    in
      String.iter 
        (function 
           | '_' | ' ' | '\n' | '\r' | '\t' -> Buffer.add_char buff '-'
           | c -> Buffer.add_char buff c
        )
        str;
      Buffer.contents buff
  ;;
  
  let enable name hlp default_choices renv =
    let arg_name =
      tr_arg name
    in
    let env =
      !renv
    in
    let default, env =
      BaseExpr.choose default_choices env
    in
    let default_str =
      if default then "true" else "false"
    in
    let env =
      var_set
        name
        default_str
        (var_define name (fun env -> default_str, env) env)
    in
      renv := env;
      [
        "--enable-"^arg_name,
        Arg.Unit (fun () -> renv := var_set name "true" !renv),
        " Enable "^hlp^(if default then " [default]" else "");
  
        "--disable-"^arg_name,
        Arg.Unit (fun () -> renv := var_set name "false" !renv),
        " Disable "^hlp^(if not default then " [default]" else "");
      ]
  ;;
   
  let wth name hlp default renv =
    let dflt env = 
      let renv' =
        ref env
      in
      let v =
        var_expand renv' default
      in
        v, !renv'
    in
      renv := var_set
                name
                default
                (var_define name dflt !renv); 
      [
        "--with-"^(tr_arg name),
        Arg.String (fun str -> renv := var_set name str !renv),
        hlp^" ["^default^"]"
      ]
  ;;
  
  let parse argv args env =
      (* Simulate command line for Arg *)
      let current =
        ref 0
      in
  
      let renv = 
        ref env
      in
      let args =
        List.flatten
          (List.map 
             (fun fargs -> fargs renv)
             args)
      in
        (
          try
            Arg.parse_argv
              ~current:current
              (Array.concat [[|"none"|]; argv])
              (Arg.align args)
              (fun str -> 
                 failwith 
                   ("Don't know what to do with arguments: '"^str^"'"))
              "configure options:"
          with Arg.Help txt | Arg.Bad txt ->
            BaseMessage.error txt
        );
        !renv
  ;;
  
  let default =
    (* Standard paths *)
    let lst =
      [
        "prefix",
        "install architecture-independent files dir",
        (match Sys.os_type with
           | "Win32" ->
               "%PROGRAMFILES%\\$pkg_name"
           | _ ->
               "/usr/local"
        );
        
        "eprefix",
        "Install architecture-dependent files in dir",
        "$prefix";
  
        "bindir",
        "User executables",
        Filename.concat "$eprefix" "bin";
  
        "sbindir",
        "System admin executables",
        Filename.concat "$eprefix" "sbin";
  
        "libexecdir",
        "Program executables",
        Filename.concat "$eprefix" "libexec";
  
        "sysconfdir",
        "Read-only single-machine data",
        Filename.concat "$prefix" "etc";
  
        "sharedstatedir",
        "Modifiable architecture-independent data",
        Filename.concat "$prefix" "com";
  
        "localstatedir",
        "Modifiable single-machine data",
        Filename.concat "$prefix" "var";
  
        "libdir",
        "Object code libraries",
        Filename.concat "$eprefix" "lib";
  
        "datarootdir",
        "Read-only arch.-independent data root",
        Filename.concat "$prefix" "share";
  
        "datadir",
        "Read-only architecture-independent data",
        "$datarootdir";
  
        "infodir",
        "Info documentation",
        Filename.concat "$datarootdir" "info";
  
        "localedir",
        "Locale-dependent data",
        Filename.concat "$datarootdir" "locale";
  
        "mandir",
        "Man documentation",
        Filename.concat "$datarootdir" "man";
  
        "docdir",
        "Documentation root",
        Filename.concat (Filename.concat "$datarootdir" "doc") "$pkg_name";
  
        "htmldir",
        "HTML documentation",
        "$docdir";
  
        "dvidir",
        "DVI documentation",
        "$docdir";
  
        "pdfdir",
        "PDF documentation",
        "$docdir";
  
        "psdir",
        "PS documentation",
        "$docdir";
      ]
    in
      fun renv ->
        List.fold_left
          (fun acc (name, hlp, dflt) ->
             renv := var_define name (fun env -> dflt, env) !renv;
             (
               "--"^name,
               Arg.String (fun str -> renv := var_set name str !renv),
               "dir "^hlp^" ["^dflt^"]"
             ) :: acc
          )
          (BaseEnvironment.args renv)
          lst
  ;;
end;;

module BaseVersion =
struct
# 1 "src/base/BaseVersion.ml"
  
  (** Version comparisons
      @author Sylvain Le Gall
    *)
  
  type t = string
  ;;
  
  type comparator = 
    | VGreater of t
    | VEqual of t
    | VLesser of t
    | VOr of  comparator * comparator
    | VAnd of comparator * comparator
  ;;
  
  (** Compare versions
    *)
  let version_compare v1 v2 =
    let is_digit =
      function
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
        | _ -> false
    in
  
    let buff =
      Buffer.create (String.length v1)
    in
  
    let rec extract_filter test (v, start, len) = 
      if start < len && test v.[start] then
        (
          Buffer.add_char buff v.[start];
          extract_filter test (v, start + 1, len)
        )
      else
        (
          let res =
            Buffer.contents buff
          in
            Buffer.clear buff;
            res, (v, start, len)
        )
    in
    let extract_int vpos =
      let str, vpos =
        extract_filter is_digit vpos
      in
        int_of_string str, vpos
    in
    let extract_non_int vpos =
      extract_filter 
        (fun c -> not (is_digit c)) 
        vpos
    in
    let rec compare_aux ((v1,start1,len1) as vpos1) ((v2,start2,len2) as vpos2) = 
      if start1 < len1 && start2 < len2 then
        (
          if is_digit v1.[start1] && is_digit v2.[start2] then
            (
              let i1, vpos1 =
                extract_int vpos1
              in
              let i2, vpos2 =
                extract_int vpos2
              in
                match i1 - i2 with
                  | 0 -> compare_aux vpos1 vpos2
                  | n -> n
            )
          else
            (
              let str1, vpos1 =
                extract_non_int vpos1
              in
              let str2, vpos2 =
                extract_non_int vpos2
              in
                match String.compare str1 str2 with
                  | 0 -> compare_aux vpos1 vpos2
                  | n -> n
            )
        )
      else 
        (
          len1 - len2 
        )
    in
      compare_aux 
        (v1, 0, (String.length v1))
        (v2, 0, (String.length v2))
  ;;
  
  (** Apply version comparator expression
    *)
  let rec comparator_apply v op =
    match op with
      | VGreater cversion ->
          (version_compare v cversion) > 0
      | VLesser cversion ->
          (version_compare v cversion) < 0
      | VEqual cversion ->
          (version_compare v cversion) = 0
      | VOr (op1, op2) ->
          (comparator_apply v op1) || (comparator_apply v op2)
      | VAnd (op1, op2) ->
          (comparator_apply v op1) && (comparator_apply v op2)
  ;;
  
end;;

module BaseCheck =
struct
# 1 "src/base/BaseCheck.ml"
  
  (** {1 Checking for particular features} 
    *)
  
  module Env = BaseEnvironment;;
  module Ver = BaseVersion;;
  module Msg = BaseMessage;;
  
  (** Look for a program among a list of alternative program
    * the first found is returned. 
    *)
  let prog_best prg prg_lst =
    Env.var_cache prg
      (fun env ->
         let alternate = 
           List.fold_left 
             (fun res e ->
                match res with 
                  | Some _ -> res
                  | None ->
                      try
                        Some (BaseFileUtil.which e)
                      with Not_found ->
                        None)
             None
             prg_lst
         in
           match alternate with
             | Some prg -> prg, env
             | None -> raise Not_found
      )
  ;;
  
  (** Check the presence of a particular program.
    *)
  let prog prg =
    prog_best prg [prg]
  ;;
  
  (** Check the presenc of a program or its native version
    *)
  let prog_opt prg = 
    prog_best prg [prg^".opt"; prg]
  ;;
  
  (** Check version, following Sys.ocaml_version convention
    *)
  let version feature var_prefix (str_cmp, cmp, var_cmp) fversion = 
    (* Really compare version provided *)
    let var = 
      var_prefix^"_version_"^var_cmp
    in
      Env.var_cache ~hide:true var
        (fun env ->
           let () = 
             Msg.checking (feature^" version "^str_cmp);
           in
           let version, env =
             match fversion env with 
               | "[Distributed with OCaml]", env ->
                   (* TODO: this is not sure ! *)
                   Sys.ocaml_version, env
               | res ->
                   res
           in
             if Ver.comparator_apply version cmp then
               version, env
             else
               raise Not_found
        )
  ;;
  
  (** Check for findlib package
    *)
  let package ?version_comparator pkg =
    let findlib_dir pkg = 
      let dir = 
        BaseExec.run_read_one_line
          "ocamlfind"
          ["query"; "-format"; "%d"; pkg]
      in
        if Sys.is_directory dir then
          dir
        else
          failwith
            (Printf.sprintf
               "When looking for findlib package %s, \
                directory %s return doesn't exist"
               pkg dir)
    in
    let findlib_version pkg =
      BaseExec.run_read_one_line 
        "ocamlfind"
        ["query"; "-format"; "%v"; pkg]
    in
    Env.var_cache ("pkg_"^pkg)
      (fun env ->
         let default_msg =
           "findlib package "^pkg
         in
         let () = 
           Msg.checking default_msg
         in
         let dir =
           findlib_dir pkg
         in
           match version_comparator with 
             | Some ver_cmp ->
                 (
                   let _, env = 
                     version 
                       default_msg 
                       ("pkg_"^pkg)
                       ver_cmp
                       (fun env -> 
                          findlib_version pkg, env)
                       env
                   in
                     dir, env
                 )
             | None -> 
                 dir, env
      )
  ;;
  
  (** Run checks *)
  let run checks env =
    List.fold_left
      (fun env chk -> snd (chk env))
      env
      checks
  ;;
end;;

module BaseOCamlcConfig =
struct
# 1 "src/base/BaseOCamlcConfig.ml"
  
  (** Read output of command ocamlc -config and transform it
    * into enviornment variable
    *)
  
  open BaseEnvironment;;
  
  module SMap = Map.Make(String);;
  
  let config =
    [
      "os_type";
      "system";
      "architecture";
      "ccomp_type";
      "ocaml_version";
    ]
  ;;
  
  let ocamlc_config_map ocamlc =
    (* Map name to value for ocamlc -config output 
       (name ^": "^value) 
     *)
    let rec split_field mp lst = 
      match lst with 
        | line :: tl ->
            let mp =
              try
                let pos_semicolon =
                  String.index line ':'
                in
                  if pos_semicolon > 1 then            
                    (
                      let name =
                        String.sub line 0 pos_semicolon 
                      in
                      let linelen =
                        String.length line
                      in
                      let value =
                        if linelen > pos_semicolon + 2 then
                          String.sub line (pos_semicolon + 2) (linelen - pos_semicolon - 2)
                        else
                          ""
                      in
                        SMap.add name value mp
                    )
                  else
                    (
                      mp
                    )
              with Not_found ->
                (
                  mp
                )
            in
              split_field mp tl
        | [] ->
            mp
    in
  
      var_cache
        "ocamlc_config_map"
        ~hide:true
        (fun env ->
           let ocamlc, env =
             ocamlc env
           in
           let map =
             split_field 
               SMap.empty
               (BaseExec.run_read_output 
                  ocamlc
                  ["-config"])
           in
           (Marshal.to_string map []), env)
  ;;
  
  let var_cache ocamlc nm =
    (* Extract data from ocamlc -config *)
    let avlbl_config_get env = 
      let map_marshal, env =
        ocamlc_config_map ocamlc env
      in
        (Marshal.from_string map_marshal 0), env
    in
    let nm_config =
      match nm with 
        | "ocaml_version" -> "version"
        | _ -> nm
    in
      var_cache
        nm 
        (fun env -> 
           try
             let map, env =
               avlbl_config_get env
             in
             let value = 
               SMap.find nm_config map
             in
               value, env
           with Not_found ->
             failwith
               (Printf.sprintf 
                  "Cannot find field '%s' in '%s -config' output"
                  nm
                  (fst (ocamlc env))))
  ;;
  
end;;

module BaseStandardVar =
struct
# 1 "src/base/BaseStandardVar.ml"
  
  (** Most standard variables for OCaml 
      @author Sylvain Le Gall
    *)
  
  open BaseCheck;;
  
  let ocamlc   = prog_opt "ocamlc";;
  let ocamlopt = prog_opt "ocamlopt";;
  
  let (ocaml_version,
       standard_library_default,
       standard_library,
       standard_runtime,
       ccomp_type,
       bytecomp_c_compiler,
       bytecomp_c_linker,
       bytecomp_c_libraries,
       native_c_compiler,
       native_c_linker,
       native_c_libraries,
       native_partial_linker,
       ranlib,
       cc_profile,
       architecture,
       model,
       system,
       ext_obj,
       ext_asm,
       ext_lib,
       ext_dll,
       os_type,
       default_executable_name,
       systhread_supported) =
    let c = 
      BaseOCamlcConfig.var_cache ocamlc
    in
      c "version",
      c "standard_library_default",
      c "standard_library",
      c "standard_runtime",
      c "ccomp_type",
      c "bytecomp_c_compiler",
      c "bytecomp_c_linker",
      c "bytecomp_c_libraries",
      c "native_c_compiler",
      c "native_c_linker",
      c "native_c_libraries",
      c "native_partial_linker",
      c "ranlib",
      c "cc_profile",
      c "architecture",
      c "model",
      c "system",
      c "ext_obj",
      c "ext_asm",
      c "ext_lib",
      c "ext_dll",
      c "os_type",
      c "default_executable_name",
      c "systhread_supported"
  ;;
  
  (** Check what is the best target for platform (opt/byte)
    *)
  let ocamlbest =
    Env.var_cache "ocamlbest"
      (fun env ->
         try
           "native", snd (ocamlopt env)
         with Not_found ->
           "byte", snd (ocamlc env))
  ;;
  
  (** Compute the default suffix for program (target OS dependent)
    *)
  let suffix_program =
    Env.var_cache "suffix_program"
      (fun env ->
         let os_type, env =
           os_type env
         in
           (match os_type with 
              | "Win32" -> ".exe" 
              | _ -> ""
           ),
           env
      )
  ;;
  
  (** Check against a minimal version.
    *)
  let ocaml_version_constraint version_cmp env = 
    version 
      "ocaml version constraint" 
      "ocaml" 
      version_cmp 
      ocaml_version
  ;;
end;;

module BaseFileAB =
struct
# 1 "src/base/BaseFileAB.ml"
  
  (** File .ab which content will be replaced by environment variable
     
      This is the same kind of file as .in file for autoconf, except we
      use the variable definition of ${!Buffer.add_substitute}. This is 
      the default file to be generated by configure step (even for 
      autoconf, except that it produce a master file before).
  
      @author Sylvain Le Gall
    *)
  
  open BaseEnvironment;;
  
  let to_filename fn =
    if not (Filename.check_suffix fn ".ab") then
      BaseMessage.warning 
        (Printf.sprintf 
           "File '%s' doesn't have '.ab' extension"
           fn);
    Filename.chop_extension fn
  ;;
  
  (** Replace variable in file %.ab to generate %
    *)
  let replace fn_lst env =
    let renv =
      ref env
    in
    let buff =
      Buffer.create 13
    in
      List.iter
        (fun fn ->
           let chn_in =
             open_in fn
           in
           let chn_out =
             open_out (to_filename fn)
           in
             (
               try
                 while true do
                  Buffer.add_string buff (var_expand renv (input_line chn_in));
                  Buffer.add_char buff '\n'
                 done
               with End_of_file ->
                 ()
             );
             Buffer.output_buffer chn_out buff;
             Buffer.clear buff;
             close_in chn_in;
             close_out chn_out)
        fn_lst;
      !renv
  ;;
end;;

module BaseSetup =
struct
# 1 "src/base/BaseSetup.ml"
  
  (** Entry point for ocaml-autobuild
      @author Sylvain Le Gall
    *)
  
  module Env = BaseEnvironment
  ;;
  
  type action_fun = Env.env -> string array -> unit;;
  
  type t =
      {
        configure:       action_fun;
        build:           action_fun;
        doc:             action_fun;
        test:            action_fun;
        install:         action_fun;
        clean:           unit -> unit;
        distclean:       unit -> unit;
        files_generated: string list;
      }
  ;;
  
  let distclean t =
    (* Call clean *)
    t.clean ();
    (* Remove generated file *)
    List.iter
      (fun fn ->
         if Sys.file_exists fn then
           (BaseMessage.info 
              (Printf.sprintf "Remove '%s'" fn);
            Sys.remove fn))
      (Env.filename :: t.files_generated);
    t.distclean ()
  ;;
  
  let setup t = 
    try
      let act =
        ref (fun () -> 
               failwith
                 (Printf.sprintf
                    "No action defined, run '%s %s -help'"
                    Sys.executable_name
                    Sys.argv.(0)))
  
      in
      let args =
        ref []
      in
      let arg_rest ?(configure=false) a =
        Arg.Tuple
          [
            Arg.Rest (fun str -> args := str :: !args);
            Arg.Unit 
              (fun () ->
                 (* Build initial environment *)
                 let env_org =
                   Env.load ~allow_empty:configure ()
                 in
                   act  := (fun () -> a env_org (Array.of_list (List.rev !args)));
                   args := []); 
          ]
      in
      let arg_clean a =
        Arg.Unit (fun () -> act := a);
      in
        Arg.parse 
          [
            "-configure",
            arg_rest ~configure:true t.configure,
            "[options*] Configure build process.";
  
            "-build",
            arg_rest t.build,
            "[options*] Run build process.";
  
            "-doc",
            arg_rest t.doc,
            "[options*] Build documentation.";
  
            "-test",
            arg_rest t.test,
            "[options*] Build and run tests.";
  
            "-install",
            arg_rest t.install,
            "[options*] Install library, data, executable and documentation.";
  
            "-clean",
            arg_clean t.clean,
            "[options*] Clean build environment.";
  
            "-distclean",
            arg_clean (fun () -> distclean t),
            "[options*] Clean build and configure environment.";
          ]
          (fun str -> failwith ("Don't know what to do with "^str))
          "Setup and run build process current package\n";
  
          !act ()
    with e ->
      BaseMessage.error (Printexc.to_string e);
  ;;
end;;


# 1622 "setup.ml"
module InternalConfigure =
struct
# 1 "src/internal/InternalConfigure.ml"
  
  (** Configure using ocaml-autobuild internal scheme
      @author Sylvain Le Gall
    *)
  
  module Msg = BaseMessage;;
  module Env = BaseEnvironment;;
  
  (** Build environment using provided series of check to be done
    * and then output corresponding file.
    *)
  let configure pkg_name pkg_version args checks ab_files env_org argv =
  
    let env = 
      List.fold_left
        (fun env (nm, vl) -> Env.var_define nm (fun env -> vl, env) env)
        env_org
        [
          "pkg_name", pkg_name;
          "pkg_version", pkg_version;
        ]
    in
    (* Parse command line *)
    let env =
      BaseArgExt.parse argv (BaseArgExt.default :: args) env
    in
  
    (* Do some check *)
    let env =
      BaseCheck.run checks env
    in
  
    (* Replace data in file *)
    let env =
      BaseFileAB.replace
        ab_files
        env
    in
  
      if not (Env.equal env_org env) then
        (
          Env.dump env;
          Env.print env
        )
  ;;
end;;

module InternalInstall =
struct
# 1 "src/internal/InternalInstall.ml"
  
  (** Install using ocaml-autobuild internal scheme
      @author Sylvain Le Gall
    *)
  
  type library =
      {
        lib_name:    string;
        lib_install: bool BaseExpr.choices;
        lib_modules: string list;
        lib_path:    string;
        lib_extra:   string list;
      }
  ;;
  
  type executable =
      {
        exec_name:    string;
        exec_install: bool BaseExpr.choices;
        exec_path:    string;
      }
  ;;
  
  let exec_hook =
    ref (fun env exec -> exec)
  ;;
  
  let lib_hook =
    ref (fun env lib -> lib)
  ;;
  
  let install libs execs env argv =
    
    let srcdir = 
      (* TODO: don't hardcode this *)
      "."
    in
  
    let builddir =
      (* TODO: don't hardcode this *)
      Filename.concat srcdir "_build"
    in
  
    let exec_ext =
      (* TODO: don't hardcode this *)
      ""
    in
  
    let bindir =
      (* TODO: don't hardcode this *)
      "/usr/bin"
    in
  
    let rootdirs =
      [srcdir; builddir]
    in
  
    let ( * ) lst1 lst2 = 
      List.flatten 
        (List.map 
           (fun a -> 
              List.map 
                (fun b -> a,b) 
                lst2) 
           lst1)
    in
  
    let make_filename =
      function
        | [] -> "" 
        | hd :: tl  -> List.fold_left Filename.concat hd tl
    in
  
    let make_module nm = 
      [String.capitalize nm; String.uncapitalize nm]
    in
  
    let find_file f lst = 
      let alternatives =
        List.map (fun e -> make_filename (f e)) lst
      in
        try 
          List.find Sys.file_exists alternatives
        with Not_found ->
          failwith 
            (Printf.sprintf 
               "Cannot find any of the files: %s"
               (String.concat ", " 
                  (List.map 
                     (Printf.sprintf "%S")
                     alternatives)))
    in
  
    let find_build_file dir fn =
      find_file
        (fun rootdir -> [rootdir; dir; fn])
        rootdirs
    in
  
    let install_lib env lib = 
      let lib =
        !lib_hook env lib
      in
      let (install, _) =
        BaseExpr.choose lib.lib_install env
      in
        if install then
          (
            let find_build_file =
              find_build_file lib.lib_path
            in
  
            let module_to_cmi modul =
              find_file 
                 (fun (rootdir, fn) -> [rootdir; lib.lib_path; (fn^".cmi")])
                 (rootdirs * (make_module modul))
            in
  
            let module_to_header modul =
              assert(modul <> "");
              find_file 
                 (fun ((rootdir, fn), ext) -> [rootdir; lib.lib_path; fn^ext])
                 (rootdirs * (make_module modul) * [".mli"; ".ml"])
            in
              
            let files =
              List.flatten
                (
                  [
                    find_build_file "META";
                    find_build_file (lib.lib_name^".cma");
                  ]
                  :: 
                  (
                    try 
                      [
                        find_build_file (lib.lib_name^".cmxa");
                        find_build_file (lib.lib_name^".a");
                      ]
                    with Not_found ->
                      []
                  )
                  ::
                  lib.lib_extra
                  ::
                  (
                    List.rev_map
                      (fun modul -> [module_to_cmi modul; module_to_header modul])
                      lib.lib_modules
                  )
                )
            in
              BaseExec.run "ocamlfind" ("install" :: lib.lib_name :: files)
          )
    in
  
    let install_exec env exec =
      let exec =
        !exec_hook env exec
      in
      let (install, _) = 
        BaseExpr.choose exec.exec_install env
      in
        if install then
          (
            let exec_file =
              find_file
                (fun ((rootdir, name), ext) -> [rootdir; name^ext])
                (rootdirs * [exec.exec_name] * [".native"; ".byte"; ""; exec_ext])
            in
            let tgt_file =
              Filename.concat 
                bindir
                exec.exec_name
            in
              BaseMessage.info 
                (Printf.sprintf 
                   "Copying file %s to %s"
                   exec_file
                   tgt_file);
              BaseFileUtil.cp exec_file tgt_file
          )
    in
  
      List.iter (install_lib env) libs;
      List.iter (install_exec env) execs
  ;;
  
end;;


# 1867 "setup.ml"
module OCamlbuildBuild =
struct
# 1 "src/ocamlbuild/OCamlbuildBuild.ml"
  
  (** Runtime support for autobuild/OCamlbuild
      @author Sylvain Le Gall
    *)
  
  module Env = BaseEnvironment 
  ;;
  
  let cond_targets_hook =
    ref (fun lst -> lst)
  ;;
  
  let build cond_targets env argv =
    let rtargets, env =
      List.fold_left
        (fun (acc, env) (choices, tgt) ->
           let choice, env =
             BaseExpr.choose choices env
           in
             (if choice then tgt :: acc else acc), env)
        ([], env)
        (!cond_targets_hook cond_targets)
    in
    let eget =
      Env.get env
    in
    let ocamlbuild = 
      eget "ocamlbuild"
    in
    let args = 
      List.rev_append rtargets (Array.to_list argv)
    in
    let args =
      if (eget "os_type") = "Win32" then
        [
          "-classic-display"; 
          "-no-log"; 
          "-install-lib-dir"; 
          (Filename.concat (eget "ocaml_stdlib") "ocamlbuild")
        ] @ args
      else
        args
    in
    let args =
      if (eget "ocamlbest") = "byte" || (eget "os_type") = "Win32" then
        "-byte-plugin" :: args
      else
        args
    in
      BaseExec.run ocamlbuild args
  ;;
  
  let clean () = 
    BaseExec.run "ocamlbuild" ["-clean"]
  ;;
  
end;;


# 1930 "setup.ml"
let setup () =
  BaseSetup.setup
    {
      BaseSetup.install =
        (InternalInstall.install
           [
             {
               InternalInstall.lib_name = ("tee");
               InternalInstall.lib_install =
                 ([(BaseExpr.Bool true, true); (BaseExpr.Bool true, true)]);
               InternalInstall.lib_modules = (["Tee"]);
               InternalInstall.lib_path = ("src");
               InternalInstall.lib_extra = ([]);
               }
           ]
           [
             {
               InternalInstall.exec_name = ("OCamlTee");
               InternalInstall.exec_install =
                 ([(BaseExpr.Bool true, true); (BaseExpr.Bool true, true)]);
               InternalInstall.exec_path = ("src");
               }
           ]);
      BaseSetup.test = (fun _ _ -> failwith "No implementation for test");
      BaseSetup.doc = (fun _ _ -> failwith "No implementation for doc");
      BaseSetup.build =
        (OCamlbuildBuild.build
           [
             ([(BaseExpr.Bool true, true)], "src/tee.cma");
             ([
                (BaseExpr.Bool true, true);
                (BaseExpr.Test ("ocamlbest", "byte"), false)
              ],
               "src/tee.cmxa");
             ([(BaseExpr.Bool true, true)], "src/OCamlTee.byte")
           ]);
      BaseSetup.configure =
        (InternalConfigure.configure
           "ocaml-tee"
           "0.0.1"
           []
           [
             BaseCheck.package "unix";
             BaseCheck.prog "ocamlbuild";
             BaseStandardVar.ocamlbest;
             BaseStandardVar.standard_library;
             BaseStandardVar.os_type
           ]
           []);
      BaseSetup.clean = (fun () -> OCamlbuildBuild.clean ());
      BaseSetup.distclean = (fun () -> ());
      BaseSetup.files_generated = ([]);
      }
  ;;
(* AUTOBUILD_STOP *)
setup ();;
