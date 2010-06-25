
(* AUTOBUILD_START *)
(* DO NOT EDIT (digest: 44a49b654e563b1e700a2ef6b0956411) *)
module BaseEnvRO =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseEnvRO.ml"
  
  (** Read-only environment
      @author Sylvain Le Gall
    *)
  
  (** Variable type
    *)
  type var_t = string
  
  (** Value type
    *)
  type val_t = 
      {
        order: int;
        value: string;
      }
  
  (** Read-only environment type
    *)
  type env_t = (var_t, val_t) Hashtbl.t
  
  (** Get all variable
    *)
  let var_all env =
    List.rev_map
      snd
      (List.sort
         (fun (i1, _) (i2, _) -> i2 - i1)
         (Hashtbl.fold 
            (fun nm vl acc -> (vl.order, nm) :: acc)
            env 
            []))
  ;;
  
  (** Expand variable that can be found in string. Variable follow definition of
    * variable for {!Buffer.add_substitute}.
    *)
  let rec var_expand env str =
    let buff =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute 
        buff
        (fun var -> 
           try 
             var_get ~handle_not_found:false var env
           with Not_found ->
             failwith 
               (Printf.sprintf 
                  "No variable %s defined when trying to expand %S \
                   (available: %s)"
                  var 
                  str 
                  (String.concat ", " (var_all env))))
        str;
      Buffer.contents buff
  
  (** Get variable 
    *)
  and var_get ?(handle_not_found=true) name env =
    let vl = 
      try 
        (Hashtbl.find env name).value
      with Not_found when handle_not_found ->
        failwith 
          (Printf.sprintf 
             "No variable %s defined (available: %s)"
             name
             (String.concat ", " (var_all env)))
    in
      var_expand env vl
  ;;
  
  (** Environment default file 
    *)
  let default_filename =
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      "setup.data"
  ;;
  
  (** Initialize environment.
    *)
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    let env =
      Hashtbl.create 13
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
          let rec read_file order =
            match Stream.npeek 3 lexer with 
              | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                  Stream.junk lexer; 
                  Stream.junk lexer; 
                  Stream.junk lexer;
                  Hashtbl.add env nm {order = order; value = value};
                  read_file (order + 1)
              | [] ->
                  ()
              | _ ->
                  failwith 
                    (Printf.sprintf 
                       "Malformed data file '%s' line %d"
                       filename !line)
          in
            read_file 0;
            close_in chn;
            env
        )
      else if allow_empty then
        (
          env
        )
      else
        (
          failwith 
            (Printf.sprintf 
               "Unable to load environment, the file '%s' doesn't exist."
               filename)
        )
  ;;
end;;


# 151 "myocamlbuild.ml"
module OCamlbuildFindlib =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/ocamlbuild/OCamlbuildFindlib.ml"
  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin
  
  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read
  
  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings
  
  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x
  
  let split_nl s = split s '\n'
  
  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s
  
  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")
  
  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]
  
  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]
  
  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());
  
          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());
  
          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])
  
      | _ -> 
          ()
  
end;;

module OCamlbuildBase =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/ocamlbuild/OCamlbuildBase.ml"
  
  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)
  
  open Ocamlbuild_plugin
  
  type dir = string
  type name = string
  
  type t =
      {
        lib_ocaml: (name * dir list) list;
      }
  ;;
  
  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 
  ;;
  
  let dispatch t = 
    function
      | Before_options ->
          let env = 
            BaseEnvRO.load ~filename:(Pathname.basename BaseEnvRO.default_filename) ()
          in
            Options.ext_obj := BaseEnvRO.var_get "ext_obj" env;
            Options.ext_lib := BaseEnvRO.var_get "ext_lib" env;
            Options.ext_dll := BaseEnvRO.var_get "ext_dll" env;
  
      | After_rules -> 
          (* Declare OCaml libraries *)
          List.iter 
            (function
               | lib, [] ->
                   ocaml_lib lib;
               | lib, dir :: tl ->
                   ocaml_lib ~dir:dir lib;
                   List.iter 
                     (fun dir -> 
                        flag 
                          ["ocaml"; "use_"^lib; "compile"] 
                          (S[A"-I"; P dir]))
                     tl)
            t.lib_ocaml
  
      | _ -> 
          ()
  ;;
  
  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        OCamlbuildFindlib.dispatch;
      ]
  ;;
end;;


# 326 "myocamlbuild.ml"
let package_default = {OCamlbuildBase.lib_ocaml = ([("src/tee", ["src"])]); };;

let dispatch_default = OCamlbuildBase.dispatch_default package_default;;

(* AUTOBUILD_STOP *)

Ocamlbuild_plugin.dispatch dispatch_default;;
