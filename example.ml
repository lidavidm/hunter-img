(* Examples *)

(* First, we define our basic grammar: *)


#use "hunter.ml";;

let basic : grammar = {
    start_symbols = ["c"];
    lexicon =
      [
        ("alice",     [lie "d"],                            Constant "alice");
        ("alice.NOM", [lie "d"; lie "k"],                   Constant "alice");
        ("bob",       [lie "d"],                            Constant "bob");
        ("bob.NOM",   [lie "d"; lie "k"],                   Constant "bob");
        ("some",      [lic "n"; lie "d"; lie "q"],          Quantifier Exists);
        ("some.NOM",  [lic "n"; lie "d"; lie "k"; lie "q"], Quantifier Exists);
        ("every",     [lic "n"; lie "d"; lie "q"],          Quantifier Forall);
        ("every.NOM", [lic "n"; lie "d"; lie "k"; lie "q"], Quantifier Forall);
        ("boy",       [lie "n"],                            Constant "boy");
        ("girl",      [lie "n"],                            Constant "girl");
        ("fast",      [lic "n"; lie "n"],                   Constant "fast");
        ("blonde",    [lic "n"; lie "n"],                   Constant "blonde");
        ("chase",     [lic "d"; lic "d"; lie "v"],          Constant "chase");
        ("run",       [lic "d"; lie "v"],                   Constant "run");
        ("-s",        [lic "v"; lic "k"; lie "t"],          Constant "present");
        ("quickly",   [adj "v"],                            Constant "quick");
        ("he.1.NOM",  [lie "d"; lie "k"],                   Variable 1);
        ("ε",         [lic "t"; lie "c"],                   Constant "");
        ("ε.Q",       [lic "t"; lic "q"; lie "c"],          Constant "");
      ];
  };;



(* Next, we define our model: *)


let model = {
    entities    = ["alice"; "bob"; "carol"];
    events      = ["chasing"; "running"; "alice chasing carol"; "carol chasing bob"];
    assignments = [(1, "bob")];
    predicates  = [
        "present", ["chasing"; "running"; "alice chasing carol"; "carol chasing bob"];
        "chase", ["chasing"; "alice chasing carol"; "carol chasing bob"];
        "quick", ["chasing"; "alice chasing carol"];
        "run", ["running"];
        "girl", ["carol"];
        "fast", ["carol"];
      ];
    predicates2 = [
        "int", ["chasing", "bob"; "alice chasing carol", "carol"; "carol chasing bob", "bob"];
        "ext", [
            "chasing", "alice";
            "running", "alice";
            "alice chasing carol", "alice";
            "carol chasing bob", "carol"
          ]
      ];
  };;



(* Finally, we run our test sentences: *)


let show_formula formula =
  print_endline @@ string_of_formula formula;
  if eval model formula then
    (print_endline "True"; true)
  else
    (print_endline "False"; false);;

let show_example expected input =
  match parse basic input with
  | Some derivation ->
     let (_, expr, _) = derivation in
     let formula = get_expr_meaning expr in
     print_endline input;
     format_derivation (Format.std_formatter) derivation;
     let result = show_formula formula in
     (if result = expected then
       print_endline "PASS"
      else
        (print_endline "FAIL"; failwith "Test failed"));
     print_newline ()
  | None -> failwith "Could not parse";;


show_example true  "ε alice.NOM chase -s bob";;
show_example true  "ε alice.NOM chase -s bob quickly";;
show_example true  "ε alice.NOM run -s";;

show_example false "ε alice.NOM run -s quickly";;
show_example false "ε bob.NOM chase -s alice";;
show_example false "ε he.1.NOM chase -s alice";;

show_example true  "ε.Q alice.NOM chase -s some girl";;
show_example true  "ε.Q alice.NOM chase -s some fast girl";;
show_example true  "ε.Q alice.NOM chase -s some fast girl quickly";;
show_example true  "ε.Q alice.NOM chase -s every girl";;
show_example true  "ε.Q some.NOM girl chase -s bob";;
show_example true  "ε.Q every.NOM girl chase -s bob";;

show_example false "ε.Q bob.NOM chase -s some girl";;
show_example false "ε.Q bob.NOM chase -s every girl";;
show_example false "ε.Q some.NOM girl chase -s alice";;
show_example false "ε.Q every.NOM girl chase -s alice";;
