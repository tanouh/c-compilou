(*
Fichier principal du compilateur d'expression
(inspire de JC Fillietre
*)

open Format
open Lexing
open Errors

(* Option de compilation, pour s'arreter a l'issue du parser *)
let parse_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""
let set_file f s = f := s

(* Les options du compilateur que l'on affiche en donnant l'argument --help *)
let options =
  [
    ( "-parse-only",
      Arg.Set parse_only,
      "  Pour ne faire uniquement que la phase d'analyse syntaxique" );
    ( "-o",
      Arg.String (set_file ofile),
      "<file>  Pour indiquer le mom du fichier de sortie" );
  ]

let usage = "usage: calc.exe [option] file.c"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c - 1) c

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On verifie que le nom du fichier source a bien ete indique *)
  if !ifile = "" then (
    eprintf "Aucun fichier a compiler\n@?";
    exit 1);

  (* Ce fichier doit avoir l'extension .c *)
  if not (Filename.check_suffix !ifile ".c") then (
    eprintf "Le fichier d'entree doit avoir l'extension .c\n@?";
    Arg.usage options usage;
    exit 1);

  (* Par defaut, le fichier cible a le meme nom que le fichier source,
     seule l'extension change *)
  if !ofile = "" then ofile := Filename.chop_suffix !ifile ".c" ^ ".s";

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Creation d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est detectee.
       La fonction Lexer.token est utilisee par Parser.prog pour obtenir
       le prochain token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;

    (* On s'arrete ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;

    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine
       resultant de cette transformation doit etre ecrit dans le fichier
       cible ofile. *)
    Compilec.compile_program p !ofile
  with
  | Lexer.Lexing_error c ->
      (* Erreur lexicale. On recupere sa position absolue et
         on la convertit en numero de ligne *)
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Error in lexing: %s@." c;
      exit 1
  | Parser.Error ->
      (* Erreur syntaxique. On recupere sa position absolue et on la
         convertit en numero de ligne *)
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Error in parsing@.";
      exit 1
  | Error (msg, pos) ->
      (* Erreur d'utilisation de variable pendant la compilation *)
      eprintf "%s:%d: %s\n" !ifile pos.pos_lnum msg;
      exit 1
