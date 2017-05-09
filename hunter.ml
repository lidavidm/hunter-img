(* Logical Forms *)

(* We represent logical forms using a combination of Conjunctivist and Neo-Davidsonian semantics. The former means that instead of having the full first-order logic, we instead have only conjunctions of monadic (1-argument) predicates. This limits our expressiveness, but as we'll see, we don't necessarily need so much power. Neo-Davidsonian semantics seeks to make events first-class entities, allowing them to be modified like any other entity. This makes the meaning of adverbial adjuncts clearer. Here, we integrate the two by adding an existential closure operator ~Closure~ (\(\left<\phi\right>\)) to our semantics, which asserts the existence of some event which is modified by the given formula. Also, to simplify implementation of quantifiers, we will make them first-class as well. (In Hunter's formalism, quantifiers are simply another type of monadic predicate with special interpretation. Since they already need special handling, we simply include them as a separate type of logical formula.) *)


type quantifier =
  | Exists
  | Forall

type formula =                      (* ℱ *)
  | Constant   of string            (* c *)
  | Variable   of int               (* x₁, x₂, etc *)
  | Int        of formula           (* int(φ) *)
  | Inti       of formula * int     (* int₁(φ), int₂(φ), etc. *)
  | Ext        of formula           (* ext(φ) *)
  | Conj       of formula * formula (* φ & ψ *)
  | Closure    of formula           (* <φ> *)
  | Quantifier of quantifier        (* Not formally part of Hunter's formalism *)

let rec string_of_formula (f : formula) =
  match f with
  | Constant x        -> x
  | Variable i        -> Format.sprintf "x_%d" i
  | Int f             -> Format.sprintf "int(%s)" @@ string_of_formula f
  | Inti (f, i)       -> Format.sprintf "int_%d(%s)" i @@ string_of_formula f
  | Ext f             -> Format.sprintf "ext(%s)" @@ string_of_formula f
  | Conj (f1, f2)     -> Format.sprintf "%s & %s" (string_of_formula f1) (string_of_formula f2)
  | Closure f         -> Format.sprintf "<%s>" @@ string_of_formula f
  | Quantifier Exists -> "some"
  | Quantifier Forall -> "every"
and toplevel_print_formula ppf (f : formula) =
  Format.fprintf ppf "%s" @@ string_of_formula f



(* We'll need this helper for later—it checks whether (at the top level) a formula is a quantificational one. Implicitly, the structure here means that a quantificational formula must include a quantifier all the way to the right in its tree structure. We will take advantage of this to determine its scope. *)


let rec is_quantifier = function
  | Quantifier _ -> true
  | Conj (x, _)  -> is_quantifier x
  | _            -> false



(* Logical formulas have no inherent truth value, only a value within some model of the world. Models are a combination of: *)

(* - Events which occur, *)
(* - entities in the world, *)
(* - assignments of pronouns to entities, *)
(* - and lists of entities satisfying predicates. *)

(* Note that we have both one-place and two-place predicates. While the logical forms themselves solely involve monadic predicates, we will need dyadic predicates when evaluating the truth value, since the relations \(\mathsf{Internal}\) and \(\mathsf{External}\) are dyadic, and part of the semantics, even if they do not appear directly in the logical form. Put another way, we use monadic predicates to implicitly associate entities with events through theta roles, but our model needs to make this explicit. *)


type model = {
    events:      string list;
    entities:    string list;
    assignments: (int * string) list;
    predicates:  (string * string list) list;
    predicates2: (string * (string * string) list) list;
  }



(* We can ask whether a formula is true or false under some model, or whether a particular entity satisfies it. This is mostly for our convenience, as the user will use a wrapper hiding these details. *)

type value =
  | Entity of string
  | True
  | False



(* Evaluation (This function essentially corresponds to \(\mathsf{Val}\) in Hunter's semantics.) *)

let rec has_value (value : value) (model : model) : formula -> bool = function
    (* TODO: make this compositional so "some girl" still works *)
  | Conj (Conj (Quantifier q, f1), f2) -> quantifier q value model f1 f2
  | Constant c                         -> lookup_constant value model c
  | Conj (f1, f2)                      -> has_value value model f1 && has_value value model f2
  | Closure f                          -> closure value model f
  | Int f                              -> predicate "int" value model f
  | Inti (f, i)                        -> failwith "has_value: unimplemented Inti"
  | Ext f                              -> predicate "ext" value model f
  | Variable i                         -> (Entity (List.assoc i model.assignments)) = value
  | Quantifier _                       -> false  (* Quantifiers themselves have no value *)
and quantifier quant value model predicate formula =
  let qfunc = match quant with
    | Exists -> List.exists
    | Forall -> List.for_all in
  let with_assignment model entity index =
    { model with assignments = (index, entity)::model.assignments } in
  match value, predicate with
  | True, Inti (f, i) ->
     let satisfiers = List.filter (fun ent -> has_value (Entity ent) model f) model.entities in
     qfunc (fun ent -> has_value True (with_assignment model ent i) formula) satisfiers
  | False, _ -> not (quantifier quant True model predicate formula)
  | Entity _, _ -> failwith "Individual entities cannot be quantified"
  | _ -> failwith "Quantifiers must be used with indexed theta role assigners"
and lookup_constant value model constant = match value with
  | True | False -> false
  | Entity e ->
     let in_predicates = try
         let list = List.assoc constant model.predicates in
         List.exists (fun x -> x = e) list
       with
       | Not_found -> false in
     let in_entities = List.exists (fun x -> x = e) model.entities && e = constant in
     in_predicates || in_entities
and closure value model formula = match value with
  | True     -> List.exists (fun e -> closure (Entity e) model formula) model.events
  | False    -> not (closure True model formula)
  | Entity _ -> has_value value model formula
and predicate pred value model formula = match value with
  | True       -> List.exists (fun e -> predicate pred (Entity e) model formula) model.events
  | False      -> not (predicate pred True model formula)
  | Entity evt -> List.exists (fun ent -> lookup_predicate2 model pred evt ent && has_value (Entity ent) model formula) model.entities
and lookup_predicate2 model predicate (event : string) (entity : string) =
  let internals = List.assoc predicate model.predicates2 in
  List.exists (fun x -> x = (event, entity)) internals
and eval model formula : bool = has_value True model formula

(* Tokenization *)

(* We are parsing English, so tokenization is simple; we simply split on whitespace. *)


type token = string

let tokenize : string -> token list = String.split_on_char ' '

(* Features *)

(* Hunter's conception of MG only includes movement features, along with an additional feature used solely for adjuncts. This special =*f= feature is not checked, but is selected by =-f= features. *)


type feature =
  | Licensor of string
  | Licensee of string
  | Adjunct  of string

(* Helper functions for quickly creating features. *)
let lic f = Licensor f
let lie f = Licensee f
let adj f = Adjunct  f

let string_of_feature (feat : feature) =
  match feat with
  | Licensor c -> Format.sprintf "+%s" c
  | Licensee c -> Format.sprintf "-%s" c
  | Adjunct  c -> Format.sprintf "*%s" c

let toplevel_print_feature ppf (feat : feature) =
  Format.fprintf ppf "%s" @@ string_of_feature feat

(* Lexicon *)

(* Our lexicon simply consists of words with features and a logical form. *)


type lexical_item = token * feature list * formula
type lexicon = lexical_item list

(* Grammar *)

(* A grammar is simply a list of lexical entries and a list of start symbols, used to determine when we are done parsing. *)


type grammar = {
    start_symbols: string list;
    lexicon: lexicon;
  }

let lookup_lexicon (grammar : grammar) (token : token) : lexical_item list =
  List.filter (fun (tok, _, _) -> tok = token) grammar.lexicon

(* Parsing *)

(* Parsing for Hunter's MG differs greatly from other formulations. Because we have only movement features, anything may combine with any other term in any position. Thus, while our parser has the structure of a standard chart parser like CKY, it does not function as efficiently. *)

(* Expressions consist of a phonological form, a kind, a list of features, a list of arguments, a list of children, and a logical form. The kind determines whether the expression was inserted from the lexicon or formed by combining other expressions. Arguments and children are used for deriving logical forms, as will be seen. Instead of having merge and move operations, this MG formalism uses merge and insert operations, described below, which necessitate the argument and children lists. *)


type kind =
  | Lexical
  | Derived

type argument = string * string * formula

type expression = Expr of string * kind * feature list * argument list * expression list * formula

let rec string_of_argument (token, category, meaning) =
  begin
    Format.sprintf
      "%s %s = %s"
      token category (string_of_formula meaning)
  end
and toplevel_print_argument ppf arg =
  begin
    Format.fprintf ppf "%s" (string_of_argument arg)
  end

let rec string_of_expr (Expr (token, kind, features, args, children, meaning)) =
  begin
    Format.sprintf
      "<%s%s%s = %s%s%s>"
      token
      (match kind with
       | Lexical -> "::"
       | Derived -> ":")
      (String.concat " " @@ List.map string_of_feature features)
      (string_of_formula meaning)
      (if args = [] then "" else
         ", " ^ (String.concat ", " @@ List.map string_of_argument args))
      (if children = [] then ", {}" else
         ", {" ^ (String.concat ", " @@ List.map string_of_expr children) ^ "}")
  end
and toplevel_print_expr ppf expr =
  begin
    Format.fprintf ppf "%s" (string_of_expr expr)
  end

let expr_of_lexical (lex : lexical_item) : expression =
  let (token, features, formula) = lex in
  Expr (token, Lexical, features, [], [], formula)

let get_expr_token   (Expr (token, _, _, _, _, _)   : expression) : string = token
let get_expr_meaning (Expr (_, _, _, _, _, meaning) : expression) : formula = meaning

let gensym =
  let counter = ref 0 in
  fun () ->
  let result = !counter in
  counter := result + 1;
  result

module OrderedInt : (Set.OrderedType with type t=int) =
struct
  type t = int
  let compare = compare
end
module RetiredSet = Set.Make(OrderedInt)

type derivation =
  LexicalEntry |
  DerivedEntry of string * chart_entry list * RetiredSet.t
 and chart_entry = int * expression * derivation

let format_derivation ppf chart_entry =
  let gensym =
    let counter = ref 0 in
    (fun () ->
      let v = !counter in
      counter := v + 1;
      "e" ^ (string_of_int v)
    )
  in
  let rec traverse fl fd (_, expr, derivation) =
    match derivation with
    | LexicalEntry ->
       let label = gensym () in
       fl label expr;
       label
    | DerivedEntry (op, children, _) ->
       let labels = List.map (traverse fl fd) children in
       let label = gensym () in
       fd label expr op labels;
       label
  in
  ignore @@ traverse
              (fun label expr ->
                Format.fprintf ppf "%s = " label;
                toplevel_print_expr ppf expr;
                Format.pp_print_newline ppf ())
              (fun label expr op children ->
                Format.fprintf ppf "%s = %s(%s) = " label op (String.concat ", " children);
                toplevel_print_expr ppf expr;
                Format.pp_print_newline ppf ())
    chart_entry

let chart_of_lexical (lex : lexical_item) : chart_entry =
  (gensym (), expr_of_lexical lex, LexicalEntry)

module OrderedExpression : (Set.OrderedType with type t=chart_entry) =
struct
  type t = chart_entry
  let compare = compare
end

module ExpressionSet = Set.Make(OrderedExpression)

type chart = ExpressionSet.t

let map_chart (f : chart_entry -> 'a) (chart : chart) : 'a list =
  List.rev_map f @@ ExpressionSet.elements chart

let insert_chart (entry : chart_entry) (chart : chart) : chart =
  ExpressionSet.add entry chart

let toplevel_print_exprset ppf (s : ExpressionSet.t) =
  Format.fprintf ppf "[";
  ExpressionSet.iter (fun (_, e, deriv) -> toplevel_print_expr ppf e; Format.fprintf ppf "; ") s;
  Format.fprintf ppf "]"

let toplevel_print_chart ppf (chart : chart) =
  Format.fprintf ppf "{%s}" @@
    String.concat ", " @@ List.map (fun (_, x, _) -> string_of_expr x) @@ ExpressionSet.elements chart;;

#install_printer toplevel_print_argument;;
#install_printer toplevel_print_expr;;
#install_printer toplevel_print_exprset;;
#install_printer toplevel_print_chart;;

type agenda = chart_entry list

type parse_state = grammar * int list * agenda * chart

let initial_state (grammar : grammar) (toks : token list) : parse_state =
  let empties = List.rev_map chart_of_lexical @@ lookup_lexicon grammar "" in
  let lexical = List.concat @@ List.mapi (fun idx tok ->
      let lexical_items = lookup_lexicon grammar tok in
      let exprs = List.map chart_of_lexical lexical_items in
      exprs
    ) toks in
  let chart = ExpressionSet.of_list (List.rev_append lexical empties) in
  (grammar, map_chart (fun (i, _, _) -> i) chart, map_chart (fun x -> x) chart, chart)

(** These operations are from [Kobele06; pg. 118] and [Fowlie]. *)

let genint =
  let counter = ref 0 in
  fun formula () ->
  counter := (!counter) - 1;
  Inti (formula, !counter)

let is_licensee_of cat : expression -> bool = function
  | Expr (_, _, (Licensee cat')::[], _, _, _) when cat = cat' -> true
  | _ -> false

let is_nonfinal_licensee_of cat : expression -> bool = function
  | Expr (_, _, (Licensee cat')::_, _, _, _) when cat = cat' -> true
  | _ -> false

let map_option (f : 'a -> 'b) (a : 'a option) : 'b option =
  match a with
  | Some x -> Some (f x)
  | None -> None

let bind_option (f : 'a -> 'b option) (a : 'a option) : 'b option =
  match a with
  | Some x -> f x
  | None -> None

let check expr1 expr2 =
  match expr1, expr2 with
  | (Expr (range1, kind1, ((Licensor f)::alpha as feats1), args1, children1, meaning1),
     Expr (range2, kind2, ((Licensee f')::beta as feats2), args2, children2, meaning2))
  | (Expr (range1, kind1, ((Licensee f)::alpha as feats1), args1, children1, meaning1),
     Expr (range2, kind2, ((Adjunct f')::beta as feats2), args2, children2, meaning2))
       when f = f' ->
     let child = Expr (range2, kind2, feats2, args2, children2, meaning2) in
     Some (Expr (range1, kind1, feats1, args1, child::children1, meaning1))
  | _ -> None

let insert_adjunct (expr1 : expression) (expr2 : expression) : expression option =
  match expr1, expr2 with
  | (Expr (_, _, (Licensee f)::_, _, _, _),
     Expr (_, _, (Adjunct f')::_, _, _, _))
       when f = f' ->
     check expr1 expr2
  | (Expr (_, _, (Adjunct f')::_, _, _, _),
     Expr (_, _, (Licensee f)::_, _, _, _))
       when f = f' ->
     check expr2 expr1
  | _ -> None

let insert (expr1 : expression) (expr2 : expression) : expression option =
  match expr1, expr2 with
  | (Expr (_, _, (Licensor f)::_, _, _, _),
     Expr (_, _, (Licensee f')::_, _, _, _))
       when f = f' ->
     check expr1 expr2
  | (Expr (_, _, (Licensee f')::_, _, _, _),
     Expr (_, _, (Licensor f)::_, _, _, _))
       when f = f' ->
     check expr2 expr1
  | _ -> None

let spellout (expr : expression) : expression option =
  let is_adjunct expr = match expr with
    | Expr (_, _, (Adjunct _)::_, _, _, _) -> true
    | _                                    -> false
  in
  let rec meaning_of_adjuncts base = function
    | []    -> base
    | x::xs -> Conj (get_expr_meaning x, meaning_of_adjuncts base xs)
  in
  let phonological_of_adjuncts adjuncts = match adjuncts with
    | [] -> ""
    | _  -> " " ^ (String.concat " " @@ List.map get_expr_token adjuncts)
  in
  match expr with
  | Expr (token, kind, ((Licensee "c")::_ as features), [(ext, _, ext_arg)], children, meaning) ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = ext ^ adjunct_spellout in
     let base_meaning          = ext_arg in
     let meaning'              = meaning_of_adjuncts base_meaning adjuncts in
     Some (Expr (phonological, kind, features, [], children', meaning'))
  | Expr (token, kind, ((Licensee "c")::_ as features), [(ext, _, ext_arg); (int, _, int_arg)], children, meaning) ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = int ^ adjunct_spellout in
     let base_meaning          = Conj (ext_arg, int_arg) in
     let meaning'              = meaning_of_adjuncts base_meaning adjuncts in
     Some (Expr (phonological, kind, features, [], children', meaning'))
  | Expr (token, kind, ((Licensee "v")::_ as features), [(ext, _, ext_arg)], children, meaning) ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = (String.concat " " [ext; token]) ^ adjunct_spellout in
     let base_meaning          = Conj (meaning, Ext ext_arg) in
     let meaning'              = meaning_of_adjuncts base_meaning adjuncts in
     Some (Expr (phonological, kind, features, [], children', meaning'))
  | Expr (token, kind, ((Licensee _)::_ as features), [(ext, _, ext_arg)], children, meaning)
       when is_quantifier meaning ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = (String.concat " " [token; ext]) ^ adjunct_spellout in
     let base_meaning          = Conj (meaning, genint ext_arg ()) in
     let meaning'              = meaning_of_adjuncts base_meaning adjuncts in
     Some (Expr (phonological, kind, features, [], children', meaning'))
  | Expr (token, kind, ((Licensee _)::_ as features), [(ext, _, ext_arg)], children, meaning) ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = (String.concat " " [token; ext]) ^ adjunct_spellout in
     let base_meaning          = Conj (meaning, ext_arg) in
     let meaning'              = meaning_of_adjuncts base_meaning adjuncts in
     Some (Expr (phonological, kind, features, [], children', meaning'))
    (* Order of arguments below is backwards compared to Hunter (2011) p. 74 *)
  | Expr (token, kind, features, [(ext, _, ext_arg); (int, _, int_arg)], children, meaning) ->
     let (adjuncts, children') = List.partition is_adjunct children in
     let adjunct_spellout      = phonological_of_adjuncts adjuncts in
     let phonological          = (String.concat " " [ext; token; int]) ^ adjunct_spellout in
     let meaning' = match features with
       | (Licensee "v")::_ ->
          let base_meaning = Conj (meaning, Conj (Int int_arg, Ext ext_arg)) in
          meaning_of_adjuncts base_meaning adjuncts
       | _ -> Closure (Conj (meaning, int_arg)) in
     Some (Expr (phonological, kind, features, [], children', meaning'))
  | _ -> None

let rec list_remove predicate = function
  | [] -> ([], [])
  | x::xs when predicate x ->
     let (result, remainder) = list_remove predicate xs in
     (x::result, remainder)
  | x::xs ->
     let (result, remainder) = list_remove predicate xs in
     (result, x::remainder)

let merge_comp (expr : expression) : expression option =
  match expr with
  | Expr (token1, Lexical, (Licensor f)::alpha, args, children, meaning) ->
     let (licensees, children') = list_remove (is_licensee_of f) children in
     begin
       match licensees with
       | [Expr (token2, _, _, _, children2, meaning2)] ->
          Some (Expr (token1, Derived, alpha, (token2, f, meaning2)::args, children' @ children2, meaning))
       | _ -> None
     end
  | _ -> None

let merge_spec (expr : expression) : expression option =
  match expr with
  | Expr (token1, Derived, (Licensor f)::alpha, args, children, meaning) ->
     let (licensees, children') = list_remove (is_licensee_of f) children in
     begin
       match licensees with
       | [Expr (token2, _, _, _, children2, meaning2)] ->
          Some (Expr (token1, Derived, alpha, ("_", f, meaning2)::args, children' @ children2, meaning))
       | _ -> None
     end
  | _ -> None

let merge_nonfinal (expr : expression) : expression option =
  let rec modify predicate func = function
    | [] -> []
    | x::xs when predicate x -> (func x)::xs
    | x::xs -> x::modify predicate func xs
  in
  match expr with
  | Expr (token1, _, (Licensor f)::alpha, args, children, meaning) ->
     let (licensees, _) = list_remove (is_nonfinal_licensee_of f) children in
     begin
       match licensees with
       | [Expr (token2, _, _, _, _, meaning2)] ->
          let check_feature = function
            | Expr(r, k, (Licensee f')::gamma, arg, ch, mn) when f = f' ->
               Expr(r, k, gamma, arg, ch, mn)
            | _ -> failwith "Could not check feature" in
          let children' = modify (is_nonfinal_licensee_of f) check_feature children in
          let inserted_meaning =
            if is_quantifier meaning2 then
              match meaning2 with
              | Conj (_, Inti (_, i)) -> Variable i
              | _                     -> failwith "Could not insert variable in place of quantifier"
            else
              meaning2 in
          Some (Expr (token1, Derived, alpha, (token2, f, inserted_meaning)::args, children', meaning))
       | _ -> None
     end
  | _ -> None

let retired_of_derivation = function
  | LexicalEntry                 -> RetiredSet.empty
  | DerivedEntry (_, _, retired) -> retired

let merge_retired d1 d2 = RetiredSet.union (retired_of_derivation d1) (retired_of_derivation d2)
let disjoint_retired d1 d2 =
  RetiredSet.is_empty @@ RetiredSet.inter (retired_of_derivation d1) (retired_of_derivation d2)

let id_of_expression (id, _, _) = id

let lift_chart (name : string) (f : expression -> expression option) : (chart_entry -> chart_entry option) =
  fun ((id1, expr1, deriv1) as entry1) ->
  let retired = retired_of_derivation deriv1 in
  bind_option (fun e ->
      if RetiredSet.mem id1 retired then
        None
      else
        Some (gensym (), e, DerivedEntry (name, [entry1], RetiredSet.add id1 retired))
    ) (f expr1)

let lift2_chart (name : string) (f : expression -> expression -> expression option) : (chart_entry -> chart_entry -> chart_entry option) =
  fun ((id1, expr1, deriv1) as entry1) ((id2, expr2, deriv2) as entry2) ->
  let retired = merge_retired deriv1 deriv2 in
  let disjoint = disjoint_retired deriv1 deriv2 in
  bind_option (fun e ->
      if RetiredSet.mem id1 retired || RetiredSet.mem id2 retired || not disjoint then
        None
      else
        let retired' = RetiredSet.add id1 @@ RetiredSet.add id2 retired in
        Some (gensym (), e, DerivedEntry (name, [entry1; entry2], retired'))) (f expr1 expr2)

let derive_one grammar (entry : chart_entry) chart : chart_entry list =
  let rec keep_somes = function
    | [] -> []
    | None::rest -> keep_somes rest
    | (Some x)::rest -> x::keep_somes rest in
  let merge1derivations = [(lift_chart "merge_comp" merge_comp) entry] in
  let merge2derivations = [(lift_chart "merge_spec" merge_spec) entry] in
  let merge3derivations = [(lift_chart "merge_nonfinal" merge_nonfinal) entry] in
  let insert_derivations = map_chart ((lift2_chart "insert" insert) entry) chart in
  let adjunct_derivations = map_chart ((lift2_chart "insert" insert_adjunct) entry) chart in
  let spellout_derivations = [(lift_chart "spellout" spellout) entry] in
  keep_somes @@ List.concat [
                    adjunct_derivations;
                    spellout_derivations;
                    merge1derivations;
                    merge2derivations;
                    merge3derivations;
                    insert_derivations;
                  ]

let derive grammar expr chart : chart_entry option =
  let derivations = derive_one grammar expr chart in
  match derivations with
  | [] -> None
  | x::_ -> Some x

let derive_step ((grammar, lexical_ids, agenda, chart) as state) : parse_state =
  match agenda with
  | [] -> state
  | expr::agenda' ->
     begin
       let rec make_derivation (item, agenda, chart) =
         match item with
         | None -> (None, agenda, chart)
         | Some item ->
            let derived = derive grammar item chart in
            begin
              match derived with
              | None -> (None, agenda, chart)
              | Some derived ->
                 (* format_derivation Format.std_formatter derived; *)
                 (* read_line (); *)
                 make_derivation (
                     Some derived,
                     derived::agenda,
                     insert_chart derived chart
                   )
            end
       in
       let (_, agenda'', chart') = make_derivation (Some expr, agenda', chart) in
       (grammar, lexical_ids, agenda'', chart')
     end

let is_terminal grammar lexical_ids (_, expr, deriv) = match expr with
  | Expr(_, _, [Licensee f], _, _, _) ->
     let retired = retired_of_derivation deriv in
     (List.exists (fun x -> x = f) grammar.start_symbols) &&
       (List.for_all (fun id -> RetiredSet.mem id retired) lexical_ids)
  | _                                 -> false

let rec derive_all ((grammar, lexical_ids, agenda, chart) as state) : parse_state =
  match agenda, ExpressionSet.is_empty @@ ExpressionSet.filter (is_terminal grammar lexical_ids) chart with
  | [], _ | _, false -> state
  | _ -> derive_all (derive_step state)

let parse grammar input : chart_entry option =
  let tokens           = tokenize input in
  let state            = initial_state grammar tokens in
  let (_, lexical_ids, _, chart) = derive_all state in
  try
    let terminals = ExpressionSet.elements @@ ExpressionSet.filter (is_terminal grammar lexical_ids) chart in
    let result = List.hd terminals in
    let spelled = (lift_chart "spellout" spellout) result in
    match spelled with
    | Some x -> Some x
    | None -> Some result
  with
  | Failure _ -> None

let recognize grammar input : bool =
  match parse grammar input with
  | Some _ -> true
  | None   -> false

(** {2 Citations}

[Kobele06]:

[Fowlie]: http://meaghanfowlie.com/documents/fowlie_mit_slides.pdf

 *)
