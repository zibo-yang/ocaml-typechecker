(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-37-39-60"]
open Util
open Syntax
open Type
open Error

(** 1. Environments *)


(** We represent the environment using maps.  (For short programs, we could
   use lists, but large programs with modules, may have quite large
   contexts.)  We separate the name spaces of expression variables [evar]
   from type variables, hence using a separate map.

   For type variables, we actually need two maps: [svar] maps source type
   variables (which may be shadowed) to internal type variables while [cvar]
   maps internal type variables (whih are never shadowed) to their kind or
   definition.

   You may need to add another map in type [env] for Task 4.  *)
                    
type env = {
    evar : ctyp Senv.t;
    svar : cvar Senv.t;
    cvar : kind Tenv.t;
    counter : (int ref) Senv.t;
}

let empty_env = {
    evar = Senv.empty;
    svar = Senv.empty;
    cvar = Tenv.empty;
    counter = Senv.empty;
}

(** Functions to query the environment accordingly. Same semantics as maps,
   except for the order of arguments. All `find_` functions raise the
   exception [Not_found] when their argument is not in the environment. *)
let find_cvar env a =  Tenv.find a env.cvar
let find_evar env x = Senv.find x env.evar
let find_svar env s = Senv.find s env.svar
let find_counter env s = Senv.find s env.counter 

(** Functions to modify the environment accordingly. Same semantics as maps,
   except for the order of arguments. *)
let add_evar env x t = { env with evar = Senv.add x t env.evar }
let add_cvar env a k = { env with cvar = Tenv.add a k env.cvar }

(** [add_svar] must also deal with shallow bindings *)
let add_svar env s a = { env with svar = Senv.add s a env.svar }
let add_counter env s scounter = {env with counter = Senv.add s scounter env.counter}

let shadow_or_not env cvar= 
  print_endline "************************";
  let shadow_name cvar = let suffix = (if cvar.id = 0
                                      then ""
                                      else Stdlib.string_of_int cvar.id) in
                         let k = String.cat cvar.name suffix in
                         Printf.printf "shadow %s: %s\n%!" cvar.name k; k in
  let cvar_list = List.filter_map (fun pair -> Some (snd pair)) (Senv.bindings env.svar) in
  let rec is_name_shadow cvar1 cvar2_list = (match cvar2_list with
                                        | [] -> false
                                        | cvar2::list2 -> (if (shadow_name cvar1 = shadow_name cvar2) && (cvar1.name <> cvar2.name)
                                                           then (Printf.printf "shadow inf %s: %s\n%!" cvar.name (shadow_name cvar);
                                                                true)
                                                           else is_name_shadow cvar1 list2)) in
  is_name_shadow cvar cvar_list

(*cvar_ne w is new cvar function based on env.counter and cvar function *)
let rec cvar_new env svar = try 
                              print_endline "********we enter cvar_new";
                              let svar_counter = find_counter env svar in
                              let next r = incr r; !r in
                              (while (let temp_cvar = { name = svar; id = next svar_counter; def = None } in
                                      shadow_or_not env temp_cvar) do
                                  Printf.printf "bsdfbdfbsdfgsdfgsdfgdfgsdfsvar_counter: %d\n%!" !svar_counter ;
                                  
                              done);
                              let temp_cvar = { name = svar; id = !svar_counter; def = None } in
                              env, temp_cvar
                            with 
                              Not_found -> let scounter = ref (-1) in
                                          let new_env = add_counter env svar scounter in
                                          cvar_new new_env svar

let rec cvar_def_new env svar ctype = try 
                                    let svar_counter = find_counter env svar in
                                    let next r = incr r; !r in
                                    (* env, { name = svar; id = next svar_counter; def = Some (make_def ctype) } *)
                                    (while (let temp_cvar = { name = svar; id = next svar_counter; def = Some (make_def ctype) } in
                                            shadow_or_not env temp_cvar) do
                                        Printf.printf "bsdfbdfbsdfgsdfgsdfgdfgsdfsvar_counter: %d\n%!" !svar_counter ;
                                        
                                    done);
                                    let temp_cvar = { name = svar; id = !svar_counter; def = Some (make_def ctype) } in
                                    env, temp_cvar
                                  with 
                                    Not_found -> let scounter = ref (-1) in
                                                let new_env = add_counter env svar scounter in
                                                cvar_def_new new_env svar ctype
(** [fresh_id_for env a] returns the smallest possible id for variable name
   [a] given the already allocated variables in [env]. Depending on the
   implementation, it may need to store information in env, hence it returns
   a possibly modified version of [env] *)

(** Assuming source type variables never end with an integer, a simple correct 
    implementation of [fresh_id_for] *)
let fresh_id_for_T1 env _a =
  env, fresh_id() 

let fresh_id_for =
     fresh_id_for_T1

(** [get_svar env a] is a wrapper around [find_evar env a] that turns a
   [Not_found] exception into a non-localized [Unbound] typing-error
   exception.  These will have to be localized by their calling context,
   using [within_loc]. *)
let get_svar env a =
  try find_svar env a
  with Not_found -> error_unloc (Unbound (Typ (None, a)))
                  
(** May raise non-localized [Unbound] error *)
let get_evar exp env x =
  try find_evar env x
  with Not_found -> error exp (Unbound (Exp x))

(** 2. Type minimization *)

(** Type checking must perform computation on types when checking for
   convertibilty.  This requires appropriate renamaing to avaid capture,
   hence generating many fresh variables, which may then disappear during
   reduction.  The resulting type may thefore use large integer suffixes,
   which are unpleasant for the user to read.

   For this reason, we minimize variable names after typechecking each
   declaration, and before printing error messages.  We take advantage of
   this to allow chose possibly large intergers when renaming local
   variables during computation on types, given that this will be minimized
   after computation.

   (We could use this to further optimize maps, using just some additional
   [uid] integer field for identificatiion instead of the pair [name] nad [id].)
*)


(** [minimize_typ env t] returns a renaming of [t] that minimizes the
   variables suffixes but still avoids shallowing internally and with
   respect to env [env] *)
let rec minimize_typ env t = t
 (* match t with
  | Tvar prim_t -> 
    let find_typ env1 prim1_t = 
      let same_typ prim1_t (pair : string * ctyp)= 
        match snd pair with
        |Tvar prim2_t -> if (prim2_t.name = prim1_t.name) then true else false
        |_ -> false
      in
      List.filter (same_typ prim1_t) (Senv.bindings env1.evar)
    in
    let typ_list = List.filter_map (fun pair -> Some (snd pair)) (find_typ env prim_t)
    in
    let type_suffix_list = List.filter_map (fun ctyp1 -> match ctyp1 with Tvar cvar1 -> Some cvar1.id|_ -> None) typ_list
    in
    let sorted_suffix_list = List.sort Int.compare type_suffix_list
    in 
    let rec index_of list item =
      match list with
      |[] -> raise Not_found
      |hd::tl ->  if hd = item then 0 else 1 + index_of tl item
    in
    let order_of = index_of sorted_suffix_list prim_t.id
    in
    let prim_t_ref = ref prim_t 
    in
    prim_t_ref := {prim_t with id = order_of};
    t
  |Tapp (t1, t2) -> Tapp (minimize_typ env t1, minimize_typ env t2)
  |Tprod t_list -> Tprod (List.filter_map (fun t2 -> Some (minimize_typ env t2)) t_list)
  |Trcd lab_t_list -> 
    let pair_filter pair = Some (fst pair, minimize_typ env (snd pair))
    in
    Trcd (List.filter_map pair_filter lab_t_list)
  |Tarr (t1, t2) -> Tarr (minimize_typ env t1, minimize_typ env t2)
  |Tbind (binder1, a1, kind1, t1) -> Tbind (binder1, a1, kind1, minimize_typ env t1)
 (* fix me *)*) 

(** [do_minimize] tells whether types should be minimized. It defaults to
   [true] and may be changed with the `--rawtypes` command line option, *)
let do_minimize = spec_true "--rawtypes"  "Do not minimize types"

let minimize_typ env t =
  if !do_minimize then minimize_typ (env, Tenv.empty) t else t

(** [type_typ env t] typechecks source type [t] returning its kind [k] and
   an internal representation of [t].  This may non-localized (Unbound and
   Kinding) typing error exceptions. *)
let rec type_typ env (t : styp) : kind * ctyp =
  match t with
   Tprim c -> 
    print_endline "type_typ tprim";
     Ktyp, Tprim c
  |Tvar t1 -> 
    print_endline "type_typ tvar";
    let ct1 = get_svar env t1 in
     Ktyp, Tvar ct1
  |Tapp (t1, t2) ->
    let kind1, internal_type1 = type_typ env t1 in
    let kind2, internal_type2 = type_typ env t2 in
    (match kind1 with 
       Ktyp -> failwith "Type Application Error"
      |Karr (kind11, kind12) -> 
        print_endline "type_typ tapp";
        if kind11 = kind2 
          then Ktyp, Tapp (internal_type1, internal_type2)
          else failwith "Type Application Error")
  
  |Trcd lab_t_list -> 
  let snd_type pair = 
      Some (fst pair, snd (type_typ env (snd pair)))
    in
    print_endline "type_typ trcd";
    Ktyp, Trcd (List.filter_map snd_type lab_t_list)
  |Tprod t_list ->
    print_endline "type_typ tprod";
    Ktyp, Tprod (List.filter_map (fun stype -> Some (snd (type_typ env stype))) t_list)
  |Tarr (t1, t2) -> 
    let kind1, internal_type1 = type_typ env t1 
    in
    let kind2, internal_type2 = type_typ env t2 
    in
    print_endline "type_typ tarr";
    Ktyp, Tarr (internal_type1, internal_type2)
  |Tbind (binder, a, kind, t1) -> 
    print_endline "type_typ tbind";
    let new_env_pre, ca = cvar_new env a in
    let new_env = add_svar new_env_pre a ca in
    let kind1, internal_type1 = type_typ new_env t1 in
    Ktyp, Tbind (binder, ca, kind, internal_type1)




(* Checking that local variables do not escape.  Typechecking of
   existential types requires that locally abstract variables do not escape
   from their scope.  This amounts to verifying that the returned type is
   well-formed.  It suffices to check that all variables are bound in the
   given environment.

   One must be careful of non linear redexes and delayed definitions.
   Therefore, we must not only check for well-formedness, but return an
   equivalent type that is well-formed.  This should just perform the
   necessary unfoldings and reductions.  *)
(* exception Escape of cvar *)
let rec new_subst_typ ctype1 ctype2 (ctype3: ctyp) = 
  match ctype1 with
  | Tvar tvar1 -> (match ctype2 with
                  | Tvar tvar2 -> (if tvar1 = tvar2 
                                    then ctype3 
                                    else Tvar tvar1)
                  | _ -> failwith "new_subst_type error: now we cannot substitute any complex structure")
  | Tapp (tapp11, tapp12) -> Tapp (new_subst_typ tapp11 ctype2 ctype3, new_subst_typ tapp12 ctype2 ctype3)
  | Tarr (tarr11, tarr12) -> Tarr (new_subst_typ tarr11 ctype2 ctype3, new_subst_typ tarr11 ctype2 ctype3)
  | Tbind (binder1, a1, kind1, ctype1) -> Tbind (binder1, a1, kind1, new_subst_typ ctype1 ctype2 ctype3)
  | Tprod ctype1_list -> Tprod (List.filter_map (fun x -> Some (new_subst_typ x ctype2 ctype3)) ctype1_list)
  | Trcd lab_list -> let map pair = Some (fst pair, (new_subst_typ (snd pair) ctype2 ctype3)) in
                     Trcd (List.filter_map map lab_list)
  | _ -> ctype1

let rec wf_ctyp env t : ctyp =
  match t with
  | Tvar a ->  t
     (* fix me
     if true then t else raise (Escape a) *)
  | Tapp (tapp1, tapp2) -> (match tapp1 with
                            |Tbind (binder1, a1, kind1, tapp1) ->
                              if binder1 = Tall
                                then let new_tapp2 = wf_ctyp env tapp2 in
                                new_subst_typ tapp1 (Tvar a1) new_tapp2
                                else failwith "binder is not Tall"
                            | Tarr (tarr1, tarr2) -> let new_tarr1 = wf_ctyp env tarr1 in
                                                     let new_tapp2 = wf_ctyp env tapp2 in
                                                     if new_tarr1 = new_tapp2
                                                      then wf_ctyp env tarr2
                                                      else failwith "input type is wrong"
                            | _ -> print_endline "cannot apply for the tapp1";t)
  | _ -> print_endline "no need to update";t


let (!@) = Locations.with_loc  
let (!@-) = Locations.dummy_located

let rec type_exp env exp : ctyp =
  match exp.obj with 
  | Evar x -> 
    print_endline "type_exp Evar";
    get_evar exp env x
  | Eprim (Int _) -> 
    print_endline "type_exp Eprim int";
    Tprim Tint
  | Eprim (Bool _) -> 
    print_endline "type_exp Eprim bool";
    Tprim Tbool
  | Eprim (String _) -> 
    print_endline "type_exp Eprim string";
    Tprim Tstring
  | Efun (binding_list, exp_new) -> 
    print_endline "type_exp Efun";
    (match binding_list with
    | [] -> print_endline "we come to []";
            type_exp env exp_new
    | hd::tl -> (match hd with
                | Typ (t, k) -> let new_binding_list = Efun (tl, exp_new) in
                                let new_env_pre2, ct = cvar_new env t in
                                let new_env_pre = add_svar new_env_pre2 t ct in
                                let new_env = add_cvar new_env_pre ct Ktyp in
                                let new_binding_list_exp = {obj = new_binding_list; loc = exp.loc} in
                                (* problem may exist for loc *)
                                Tbind (Tall, ct, Ktyp, type_exp new_env new_binding_list_exp)
                | Exp pat ->
                  (match pat.obj with
                   Pprim pat -> 
                    (match pat with
                    | Int _ -> let new_binding_list = Efun (tl, exp) in
                               let new_binding_list_exp = {obj = new_binding_list; loc = exp.loc} in
                               Tarr (Tprim Tint, type_exp env new_binding_list_exp)
                    | Bool pat -> let new_binding_list = Efun (tl, exp) in
                                  let new_binding_list_exp = {obj = new_binding_list; loc = exp.loc} in
                                  Tarr (Tprim Tbool, type_exp env new_binding_list_exp)
                    | String pat -> let new_binding_list = Efun (tl, exp) in
                                    let new_binding_list_exp = {obj = new_binding_list; loc = exp.loc} in
                                    Tarr (Tprim Tstring, type_exp env new_binding_list_exp))
                  |Pvar epat -> failwith "type_exp error: input without type"
                  |Ptyp (epat, stype_loc) ->
                    (match epat.obj with
                     Pvar epat -> (match (try let kind, ctype = type_typ env stype_loc.obj in
                                  Some (wf_ctyp env ctype) with Not_found -> None) with
                                  (*not_found not right *)
                                   Some wf_ctype -> let new_env = add_evar env epat wf_ctype in
                                             let new_binding_list = Efun (tl, exp_new) in
                                             let new_binding_list_exp = {obj = new_binding_list; loc = exp.loc} in
                                             Tarr (wf_ctype, type_exp new_env new_binding_list_exp)
                                  |None -> failwith "type_exp error: type not well_formed")
                    |_ -> failwith "type_exp error: function input not right")
                  |_ -> failwith "type_exp error: function input not right")))
  | Eappl (exp1, exp2_list) -> 
    print_endline "type_exp Eapp";
    let type1 = type_exp env exp1 in
    (match exp2_list with
    | [] -> type1
    | exp2::exp2_list -> 
      (match exp2 with
      | Typ type2 ->
       (match type1 with
       | Tbind (binder1, a1, kind1, ctype1) -> 
        let typ_app = Tapp (Tbind (binder1, a1, kind1, ctype1), snd (type_typ env type2.obj)) in
        wf_ctyp env typ_app
       | _ -> failwith "type_exp error1: Eapp not right type")
      | Exp exp2 -> 
        (match type1 with
        | Tarr (t1, t2) -> if t1 = type_exp env exp2 then t2 else failwith "type_exp error: eapp input not right"
        | _ -> failwith "type_exp error2: Eapp not right type")))
  | Elet (bool1, f, exp1, exp2) -> print_endline "type_exp Elet";
                              if bool1 = false then
                              (let ctype1 = type_exp env exp1 in
                              (match f.obj with
                              | Pvar f -> let new_env = add_evar env f ctype1 in type_exp new_env exp2
                              | Ptyp (f1, ctype_f) -> 
                                if ctype1 = snd (type_typ env ctype_f.obj) 
                                then (match f1.obj with 
                                     |Pvar f1 -> let new_env = add_evar env f1 ctype1 in type_exp new_env exp2
                                     |_ -> failwith "type_exp error: Elet")
                                else failwith "type_exp error: Elet"
                              | _ -> failwith "type_exp error: Elet"))
                              else failwith "type_exp error: Elet recursive"
  | Eprod exp_list -> print_endline "type_exp Eprod";
                      let lab_map exp1 = Some (type_exp env exp1) in
                      Tprod (List.filter_map lab_map exp_list)
  | Ercd exp_list -> print_endline "type_exp Ercd";
                     let lab_map pair = Some (fst pair, type_exp env (snd pair)) in
                     Trcd (List.filter_map lab_map exp_list)
  | Elab (exp, lab) -> print_endline "type_exp Elab";
                      (match exp.obj with
                       |Ercd exp_list -> (match exp_list with
                                          | [] -> failwith "type_exp error1: Elab"
                                          | hd::tl -> if lab = fst hd then type_exp env (snd hd) else failwith "type_exp error2: Elab")
                       |Evar exp1 -> let ctype1 = get_evar exp env exp1 in
                                     let rec get_lab_type ctype_list lab1 = 
                                     (match ctype_list with
                                     |[] -> None
                                     |hd :: tl -> if lab1 = fst hd 
                                                  then Some (snd hd) 
                                                  else get_lab_type tl lab1) 
                                      in 
                                     (match ctype1 with
                                     |Trcd lab_t_list -> (match get_lab_type lab_t_list lab with 
                                                         |Some ctype -> ctype 
                                                         |None -> failwith "type_exp error3: Elab: no such label")
                                     |_ -> failwith "type_exp error4: Elab")
                       |_ -> failwith "type_exp error5: Elab")
  | Eproj (exp, int) -> print_endline "type_exp Eproj";
                       (match exp.obj with
                       |Eprod exp_list -> (try 
                                            let new_exp = List.nth exp_list int in 
                                            type_exp env new_exp 
                                          with Failure _ -> failwith "type_exp error: Eproj")
                       |_ -> failwith "type_exp error: Eproj")
  | Eannot (exp, styploc) -> print_endline "type_exp Eannot";
                             let kind, ctype = type_typ env styploc.obj in
                             ctype
                             (* let new_type = type_exp env exp in
                             let kind, ctype = type_typ env styploc.obj in
                             if new_type = ctype then new_type else failwith "type_exp error: Eannot" *)
  | Epack (styploc1, exp, styploc2) -> print_endline "type_exp Epack";failwith "type_exp error: Epack"
  | Eopen (svar, cvar, exp1, exp2) -> print_endline "type_exp Eopen";failwith "type_exp error: Eopen"

 (* | _ -> failwith "not implemented"*)

let norm_when_eager =
  spec_true "--loose"  "Do not force toplevel normaliization in eager mode"

(*new function here *)
let is_cvar_equal cvar1 cvar2 =
  match (cvar1.def, cvar2.def) with
  |(Some (def1), Some (def2)) -> (match (def1.typ, def2.typ) with
                                 | (Tvar tvar1, Tvar tvar2) -> 
                                 (tvar1 = cvar2)||(tvar2 = cvar1)||(cvar1 = cvar2)||(tvar1 = tvar2)
                                 | _ -> false)
  | _ -> false


(*new function here *)
let rec pure_ctype ctype = 
  match ctype with
  | Tvar tvar -> (match tvar.def with
                  |Some {typ = new_ctype; scope = _} -> pure_ctype new_ctype
                  |None -> Tvar tvar)
  | Tprim tprim -> Tprim tprim
  | Tapp (tapp1, tapp2) -> Tapp (pure_ctype tapp1, pure_ctype tapp2)
  | Tarr (tarr1, tarr2) -> Tapp (pure_ctype tarr1, pure_ctype tarr2)
  | _ -> print_endline "we choose to ignore other complex structure to derive pure ctype";
         ctype


(*new function here *)
let rec is_ctype_equal ctype1 ctype2 =
  print_endline "start to compare two ctype";
  match (ctype1, ctype2) with
  | (Tvar cvar1, Tvar cvar2) -> print_endline "start to compare two tvar";is_cvar_equal cvar1 cvar2
  | (Tprim prim1, Tprim prim2) -> print_endline "start to compare two tprim";prim1 = prim2
  | (Tarr (arr11, arr12), Tarr (arr21, arr22)) -> print_endline "start to compare two tarr";(is_ctype_equal arr11 arr21)&&(is_ctype_equal arr12 arr22)
  | (Tvar cvar1, Tprim prim2) -> print_endline "start to compare tvar and tprim";
                                 (pure_ctype (Tvar cvar1)) = Tprim prim2
                                 (* (match cvar1.def with
                                 |Some ({typ = Tvar tvar; scope = _}) -> print_endline "inside prim1";
                                  (match tvar.def with
                                  |Some ({typ = Tprim prim1; scope = _}) -> prim1 = prim2
                                  |_ -> failwith "int def is wrong")
                                 |_ -> print_endline "empty";false) *)
  | (Trcd lab_list1, Trcd lab_list2) -> (match (lab_list1, lab_list2) with
                                        | ([], []) -> true
                                        | (hd1::tl1, hd2::tl2) -> (try 
                                                                    let new_lab_list2 = List.remove_assoc (fst hd1) lab_list2 in
                                                                    is_ctype_equal (Trcd tl1) (Trcd new_lab_list2)
                                                                  with Failure(e) -> false)
                                        | _ -> false)
  | _ -> print_endline "you might wanna identify two complex structure, now we count the result as wrong";
         failwith "is_ctype_equal error"



let type_decl env d =
  match d.obj with
  | Dtyp (svar1, k_or_styp) ->
    print_endline "type_decl Dtyp";
    (match k_or_styp with
    | Typ kind -> print_endline "type A: kind ";
                  let new_env_pre, cvar1 = cvar_new env svar1 in
                  let new_env_pre2 = add_svar new_env_pre svar1 cvar1 in
                  let new_env = add_cvar new_env_pre2 cvar1 kind in
                  Printf.printf "log %s: %d\n%!" cvar1.name cvar1.id;
                  new_env, Gtyp (cvar1, Typ kind)
    | Exp styp -> print_endline "type A = B";
                  let kind1, ctype1 = type_typ env styp.obj in
                  let new_env_pre, cvar1 = cvar_def_new env svar1 ctype1 in
                  let new_env = add_svar new_env_pre svar1 cvar1 in
                  Printf.printf "log %s: %d\n%!" cvar1.name cvar1.id;
                  new_env, Gtyp (cvar1, Exp (kind1, ctype1)))
  | Dlet (bool1, pat1, exp1) ->
    print_endline "type_decl Dlet";      
    if bool1 = false(*where bool might be problem *)
      then (match pat1.obj with
            | Pvar pat1 -> 
            print_endline "vavads"; let ctype1 = type_exp env exp1 in
                          let new_env = add_evar env pat1 ctype1 in
                          new_env, Glet (pat1, ctype1)
            | Ptyp (pat1, styploc1) -> let ctype2 = type_exp env exp1 in
                                      let kind1, ctype1 = type_typ env styploc1.obj in
                                      if is_ctype_equal ctype1 ctype2
                                        then (
                                              match pat1.obj with 
                                              |Pvar pat1 -> let new_env = add_evar env pat1 ctype1 in 
                                                            new_env, Glet (pat1, ctype1)
                                              |_ -> failwith "type_decl error: Dlet1")
                                        else failwith "type_decl error: Dlet2"
            | _ -> failwith "type_decl error: Dlet3")
      else (match pat1.obj with
            | Pvar pat1 -> let ctype1 = type_exp env exp1 in
                          let new_env = add_evar env pat1 ctype1 in
                          new_env, Glet (pat1, ctype1)
            | Ptyp (pat1, styploc1) -> let kind1, ctype1 = type_typ env styploc1.obj in
                                       (match pat1.obj with
                                       | Pvar pat1 -> let new_env = add_evar env pat1 ctype1 in
                                                      new_env, Glet (pat1, ctype1)
                                                      (*problem about exp *)
                                       |_ ->  failwith "type_decl error: Dlet4")    
            | _ -> failwith "type_decl error: Dlet5")
  | Dopen (svar1, evar1, exp1) -> 
    print_endline "type_decl Dlet";
    failwith "type_decl error: Dopen"

  
let type_program env p : env * typed_decl list =
  List.fold_left_map type_decl env p


(** Initial environment *)

let unit, int, bool, string, bot =
  let unit = Tprod [] in
  let int = Tprim Tint in
  let bool = Tprim Tbool in
  let string = Tprim Tstring in
  let bot = let a = svar "#" in Tbind (Tall, a, Ktyp, Tvar a) in
  unit, int, bool, string, bot
  
let primitive_types =     
  [ "unit", unit;
    "bool", bool;
    "int", int;
    "string", string;
    "bot", bot;
  ]

let initial_env, initial_program =
  let magic = evar "magic" in
  let p : program = 
    let pair (s, t) : decl = !@- (Dtyp (svar s, Exp !@- t)) 
    in
    List.map pair primitive_types @
    [ !@- (Dlet (true, !@- (Ptyp (!@- (Pvar magic), !@- bot)),
                 !@- (Evar magic))) ]
  in
  type_program empty_env p 


