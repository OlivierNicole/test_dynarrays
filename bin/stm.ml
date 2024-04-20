open QCheck
open STM

module type Elem = sig
  type t
  val arb : t QCheck.arbitrary
  val pp : Format.formatter -> t -> unit
  val ty_show : t STM.ty_show
  val equal : t -> t -> bool
  val init_state : t list list
  val mapping_fun : t -> t
  val mapping_fun_with_index : int -> t -> t
  val folding_fun : t -> t -> t
  val pred : t -> bool
  val filter_mapping_fun : t -> t option
end

module Dynarray_spec (Elem : Elem) = struct
  type elem = Elem.t

  (* We are plucking from a pool of Dynarrays. New arrays can be added to the
     pool, sometimes arrays can be removed. *)
  type sut = elem Dynarray.t list ref

  let init_sut () =
    ref (List.map Dynarray.of_list Elem.init_state)

  let cleanup _ = ()

  let add_array arr sut =
    sut := arr :: !sut

  type idx = I of int [@@unboxed]

  type _ cmd =
    | Create : unit cmd
    | Make : int * elem -> unit cmd
    | Get : idx * int -> (elem, exn) result cmd
    | Set : idx * int * elem -> (unit, exn) result cmd
    | Length : idx -> int cmd
    | Is_empty : idx -> bool cmd
    | Get_last : idx -> (elem, exn) result cmd
    | Find_last : idx -> elem option cmd
    | Copy : idx -> unit cmd
    | Add_last : idx * elem -> unit cmd
    | Append_array : idx * elem array -> unit cmd
    | Append_list : idx * elem list -> unit cmd
    | Append : idx * idx -> (unit, exn) result cmd
    | Append_seq : idx * elem array -> unit cmd
    | Append_iter : idx * elem array -> unit cmd
    | Pop_last_opt : idx -> elem option cmd
    | Remove_last : idx -> unit cmd
    | Truncate : idx * int -> unit cmd
    | Clear : idx -> unit cmd
    | Iter : idx -> unit cmd (* Allocate a short-lived cell for each element *)
    | Iteri : idx -> unit cmd (* Allocate a short-lived cell for each element *)
    | Map : idx -> unit cmd (* Negate all elements *)
    | Mapi : idx -> unit cmd (* Add indices and elements *)
    | Fold_left : elem * idx -> elem cmd (* Sum over elements *)
    | Fold_right : idx * elem -> elem cmd (* Sum over elements *)
    | Exists : idx -> bool cmd (* Predicate: (=) 0. *)
    | For_all : idx -> bool cmd (* Predicate: (=) 0. *)
    | Filter : idx -> unit cmd (* Predicate: (=) 0. *)
    | Filter_map : idx -> unit cmd (* f: fun x -> if x < 0 then Some (-.x) else None *)
    | Of_array : elem array -> unit cmd
    | To_array : idx -> elem array cmd
    | Of_list : elem list -> unit cmd
    | To_list : idx -> elem list cmd
    | Of_seq : elem array -> unit cmd
    | To_seq : idx -> elem list cmd
        (* The produced sequence is turned into a list immediately, see [run]. *)
    | To_seq_reentrant : idx -> elem list cmd
    | To_seq_rev : idx -> elem list cmd
    | To_seq_rev_reentrant : idx -> elem list cmd
    | Capacity : idx -> int cmd
    | Ensure_capacity : idx * int -> unit cmd
    | Ensure_extra_capacity : idx * int -> unit cmd
    | Fit_capacity : idx -> unit cmd
    | Set_capacity : idx * int -> unit cmd
    | Reset : idx -> unit cmd

  let show_cmd : type r. r cmd -> string =
    let open Format in
    function
    | Create -> "create"
    | Make (l, x) -> asprintf "make (%d, %a)" l Elem.pp x
    | Get (I arr_idx, elem_idx) -> sprintf "get (a%d, %d)" arr_idx elem_idx
    | Set (I arr_idx, elem_idx, x) ->
        asprintf "set (a%d, %d, %a)" arr_idx elem_idx Elem.pp x
    | Is_empty (I arr_idx) -> sprintf "is_empty a%d" arr_idx
    | Length (I arr_idx) -> sprintf "length a%d" arr_idx
    | Get_last (I arr_idx) -> sprintf "get_last a%d" arr_idx
    | Find_last (I idx) -> sprintf "find_last a%d" idx
    | Copy (I idx) -> sprintf "copy a%d" idx
    | Add_last (I idx, x) -> asprintf "add_last (a%d, %a)" idx Elem.pp x
    | Append_array (I idx, arr) ->
        asprintf
          "append_array (a%d, @[<hov 2>[| %a |]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Append_list (I idx, l) ->
        asprintf
          "append_list (a%d, @[<hov 2>[ %a ]@])"
          idx
          (pp_print_list ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          l
    | Append (I arr_i1, I arr_i2) -> sprintf "append (a%d, a%d)" arr_i1 arr_i2
    | Append_seq (I idx, arr) ->
        asprintf
          "append_seq (a%d, @[<hov 2>[ %a ]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Append_iter (I idx, arr) ->
        asprintf
          "append_iter (a%d, @[<hov 2>[| %a |]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Pop_last_opt (I idx) ->
        sprintf "pop_last_opt a%d" idx
    | Remove_last (I arr_idx) -> sprintf "remove_last a%d" arr_idx
    | Truncate (I arr_idx, len) -> sprintf "truncate (a%d, %d)" arr_idx len
    | Clear (I arr_i) -> sprintf "clear a%d" arr_i
    | Iter (I i) -> sprintf "iter a%d" i
    | Iteri (I i) -> sprintf "iteri a%d" i
    | Map (I i) -> sprintf "map a%d" i
    | Mapi (I i) -> sprintf "mapi a%d" i
    | Fold_left (init, I i) -> asprintf "fold_left (%a, a%d)" Elem.pp init i
    | Fold_right (I i, init) -> asprintf "fold_right (a%d, %a)" i Elem.pp init
    | Exists (I i) -> sprintf "exists a%d" i
    | For_all (I i) -> sprintf "for_all a%d" i
    | Filter (I i) -> sprintf "filter a%d" i
    | Filter_map (I i) -> sprintf "filter_map a%d" i
    | Of_array arr ->
        asprintf
          "of_array @[<hov 2>[| %a |]@]"
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | To_array (I i) -> sprintf "to_array a%d" i
    | Of_list l ->
        asprintf
          "of_list @[<hov 2>[ %a ]@]"
          (pp_print_list ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          l
    | To_list (I i) -> sprintf "to_list a%d" i
    | Of_seq arr ->
        asprintf
          "of_seq @[<hov 2>[| %a |]@]"
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | To_seq (I i) -> sprintf "to_seq a%d" i
    | To_seq_reentrant (I i) -> sprintf "to_seq_reentrant a%d" i
    | To_seq_rev (I i) -> sprintf "to_seq_rev a%d" i
    | To_seq_rev_reentrant (I i) -> sprintf "to_seq_rev_reentrant a%d" i
    | Capacity (I i) -> sprintf "capacity a%d" i
    | Ensure_capacity (I arr_idx, n) -> sprintf "ensure_capacity (a%d, %d)" arr_idx n
    | Ensure_extra_capacity (I arr_idx, n) ->
        sprintf "ensure_extra_capacity (a%d, %d)" arr_idx n
    | Fit_capacity (I arr_idx) -> sprintf "fit_capacity a%d" arr_idx
    | Set_capacity (I arr_idx, n) -> sprintf "set_capacity (a%d, %d)" arr_idx n
    | Reset (I arr_idx) -> sprintf "reset a%d" arr_idx

  type state = elem list list

  (* Existential pack type to make heterogeneous lists *)
  type packed_cmd = Pack_cmd : 'r cmd -> packed_cmd

  let show_packed_cmd (Pack_cmd c) =
    show_cmd c

  let arb_cmd state : packed_cmd QCheck.arbitrary =
    let open Gen in
    let mid_int = Gen.int_bound 11_000 in
    let arr_idx state = (fun i -> I i) <$> (int_bound (List.length state - 1)) in
    let elem = Elem.arb.gen in
    QCheck.make ~print:show_packed_cmd
      (frequency
        [ 5, return (Pack_cmd Create);
          5, (fun l x -> Pack_cmd (Make (l, x))) <$> mid_int <*> elem;
          ( 100
          , (fun arr_idx elem_idx -> Pack_cmd (Get (arr_idx, elem_idx)))
            <$> arr_idx state
            <*> small_nat );
          ( 100
          , (fun arr_idx elem_idx x -> Pack_cmd (Set (arr_idx, elem_idx, x)))
            <$> arr_idx state
            <*> small_nat
            <*> elem );
          100, (fun i -> Pack_cmd (Is_empty i)) <$> arr_idx state;
          100, (fun i -> Pack_cmd (Length i)) <$> arr_idx state;
          100, (fun i -> Pack_cmd (Get_last i)) <$> arr_idx state;
          100, (fun i -> Pack_cmd (Find_last i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Copy i)) <$> arr_idx state;
          100, (fun arr_i x -> Pack_cmd (Add_last (arr_i, x))) <$> arr_idx state <*> elem;
          33, (fun arr_i arr -> Pack_cmd (Append_array (arr_i, arr)))
               <$> arr_idx state
               <*> array elem;
          33, (fun arr_i l -> Pack_cmd (Append_list (arr_i, l)))
               <$> arr_idx state
               <*> list elem;
          33, (fun arr_i1 arr_i2 -> Pack_cmd (Append (arr_i1, arr_i2)))
               <$> arr_idx state
               <*> arr_idx state;
          33, (fun arr_i arr -> Pack_cmd (Append_seq (arr_i, arr)))
               <$> arr_idx state
               <*> array elem;
          33, (fun arr_i arr -> Pack_cmd (Append_iter (arr_i, arr))) <$> arr_idx state <*> array elem;
          100, (fun arr_i -> Pack_cmd (Pop_last_opt arr_i)) <$> arr_idx state;
          100, (fun arr_i -> Pack_cmd (Remove_last arr_i)) <$> arr_idx state;
          100, (fun arr_i len -> Pack_cmd (Truncate (arr_i, len)))
                <$> arr_idx state
                <*> nat;
          100, (fun arr_i -> Pack_cmd (Clear arr_i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Iter i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Iteri i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Map i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Mapi i)) <$> arr_idx state;
          5, (fun init i -> Pack_cmd (Fold_left (init, i))) <$> elem <*> arr_idx state;
          5, (fun i init -> Pack_cmd (Fold_right (i, init))) <$> arr_idx state <*> elem;
          50, (fun i -> Pack_cmd (Exists i)) <$> arr_idx state;
          50, (fun i -> Pack_cmd (For_all i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Filter i)) <$> arr_idx state;
          5, (fun i -> Pack_cmd (Filter_map i)) <$> arr_idx state;
          5, (fun arr -> Pack_cmd (Of_array arr)) <$> array elem;
          10, (fun i -> Pack_cmd (To_array i)) <$> arr_idx state;
          5, (fun l -> Pack_cmd (Of_list l)) <$> list elem;
          10, (fun i -> Pack_cmd (To_list i)) <$> arr_idx state;
          5, (fun arr -> Pack_cmd (Of_seq arr)) <$> array elem;
          50, (fun i -> Pack_cmd (To_seq i)) <$> arr_idx state;
          50, (fun i -> Pack_cmd (To_seq_reentrant i)) <$> arr_idx state;
          50, (fun i -> Pack_cmd (To_seq_rev i)) <$> arr_idx state;
          50, (fun i -> Pack_cmd (To_seq_rev_reentrant i)) <$> arr_idx state;
          100, (fun i -> Pack_cmd (Capacity i)) <$> arr_idx state;
          100, (fun i cap -> Pack_cmd (Ensure_capacity (i, cap)))
                <$> arr_idx state
                <*> nat;
          100, (fun i extra_cap -> Pack_cmd (Ensure_extra_capacity (i, extra_cap)))
                <$> arr_idx state
                <*> small_nat;
          100, (fun i -> Pack_cmd (Fit_capacity i)) <$> arr_idx state;
          100, (fun arr_i cap -> Pack_cmd (Set_capacity (arr_i, cap)))
                <$> arr_idx state
                <*> nat;
          33, (fun arr_i -> Pack_cmd (Reset arr_i)) <$> arr_idx state;
        ])

  let run : type r. r cmd -> sut -> r ty_show * r =
    fun cmd sut ->
    let elem = Elem.ty_show in
    let nth sut (I idx) = List.nth !sut idx in
    match cmd with
    | Create -> (unit, add_array (Dynarray.create ()) sut)
    | Make (l, x) -> (unit, add_array (Dynarray.make l x) sut)
    | Get (arr_i, elem_i) ->
        ( result elem exn
        , try Ok (Dynarray.get (nth sut arr_i) elem_i)
          with e -> Error e )
    | Set (arr_i, elem_i, x) ->
        ( result unit exn
        , try Ok (Dynarray.set (nth sut arr_i) elem_i x)
          with e -> Error e )
    | Length arr_i ->
        (int, Dynarray.length (nth sut arr_i))
    | Is_empty arr_i ->
        (bool, Dynarray.is_empty (nth sut arr_i))
    | Get_last arr_i ->
        ( result elem exn
        , try Ok (Dynarray.get_last (nth sut arr_i))
          with e -> Error e )
    | Find_last arr_i ->
        (option elem , Dynarray.find_last (nth sut arr_i))
    | Copy arr_i ->
        (unit, add_array (Dynarray.copy (nth sut arr_i)) sut)
    | Add_last (arr_i, x) ->
        (unit, Dynarray.add_last (nth sut arr_i) x)
    | Append_array (arr_i, arr) ->
        (unit, Dynarray.append_array (nth sut arr_i) arr)
    | Append_list (arr_i, l) ->
        (unit, Dynarray.append_list (nth sut arr_i) l)
    | Append (arr_i1, arr_i2) ->
        ( result unit exn
        , try Ok (Dynarray.append (nth sut arr_i1) (nth sut arr_i2))
          with Invalid_argument _ as e -> Error e)
    | Append_seq (arr_i, arr) ->
        (unit, Dynarray.append_seq (nth sut arr_i) (Array.to_seq arr))
    | Append_iter (arr_i, arr) ->
        ( unit
        , Dynarray.append_iter
            (nth sut arr_i)
            Array.iter
            arr )
    | Pop_last_opt arr_i -> (option elem , Dynarray.pop_last_opt (nth sut arr_i))
    | Remove_last arr_i -> (unit, Dynarray.remove_last (nth sut arr_i))
    | Truncate (arr_i, len) -> (unit, Dynarray.truncate (nth sut arr_i) len)
    | Clear arr_i -> (unit, Dynarray.clear (nth sut arr_i))
    | Iter i ->
        ( unit
        , Dynarray.iter (fun x -> ignore @@ Sys.opaque_identity (ref x)) (nth sut i) )
    | Iteri i ->
        ( unit
        , Dynarray.iteri (fun i x -> ignore @@ Sys.opaque_identity (i, x)) (nth sut i) )
    | Map i -> (unit, add_array (Dynarray.map Elem.mapping_fun (nth sut i)) sut)
    | Mapi i ->
        ( unit
        , add_array (Dynarray.mapi Elem.mapping_fun_with_index (nth sut i)) sut)
    | Fold_left (init, i) ->
        (elem , Dynarray.fold_left Elem.folding_fun init (nth sut i))
    | Fold_right (i, init) ->
        (elem, Dynarray.fold_right Elem.folding_fun (nth sut i) init)
    | Exists i -> (bool, Dynarray.exists Elem.pred (nth sut i))
    | For_all i -> (bool, Dynarray.for_all Elem.pred (nth sut i))
    | Filter i ->
        (unit, add_array (Dynarray.filter Elem.pred (nth sut i)) sut)
    | Filter_map i ->
        ( unit
        , add_array (Dynarray.filter_map Elem.filter_mapping_fun (nth sut i)) sut )
    | Of_array arr -> (unit , add_array (Dynarray.of_array arr) sut)
    | To_array i -> (array elem, Dynarray.to_array (nth sut i))
    | Of_list l -> (unit , add_array (Dynarray.of_list l) sut)
    | To_list i -> (list elem, Dynarray.to_list (nth sut i))
    | Of_seq arr -> (unit , add_array (Dynarray.of_seq (Array.to_seq arr)) sut)
    | To_seq i ->
        (* Evaluate the sequence immediately and store it as a list, otherwise
           sequence is lazily produced and later mutating operations can cause
           exceptions that are hard to model, even in a sequential setting. *)
        (list elem, Dynarray.to_seq (nth sut i) |> List.of_seq)
    | To_seq_reentrant i ->
        (list elem, Dynarray.to_seq_reentrant (nth sut i) |> List.of_seq)
    | To_seq_rev i -> (list elem, Dynarray.to_seq_rev (nth sut i) |> List.of_seq)
    | To_seq_rev_reentrant i ->
        (list elem, Dynarray.to_seq_rev_reentrant (nth sut i) |> List.of_seq)
    | Capacity i -> (int, Dynarray.capacity (nth sut i))
    | Ensure_capacity (arr_i, cap) ->
        (unit, Dynarray.ensure_capacity (nth sut arr_i) cap)
    | Ensure_extra_capacity (arr_i, extra_cap) ->
        (unit, Dynarray.ensure_extra_capacity (nth sut arr_i) extra_cap)
    | Fit_capacity arr_i -> (unit, Dynarray.fit_capacity (nth sut arr_i))
    | Set_capacity (arr_i, cap) -> (unit, Dynarray.set_capacity (nth sut arr_i) cap)
    | Reset arr_i -> (unit, Dynarray.reset (nth sut arr_i))

  let init_state = Elem.init_state

  module List = struct
    include List

    let[@tail_mod_cons] rec take n =
      function
      | [] -> []
      | _ :: _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
  end

  let get_model (I arr_i) state = List.nth state arr_i

  let update_model (I arr_i) f state =
    List.mapi (fun i arr -> if i = arr_i then f arr else arr) state

  let next_state : type r. r cmd -> state -> state =
    fun cmd state ->
    match cmd with
    | Create -> [] :: state
    | Make (l, x) -> List.init l (Fun.const x) :: state
    | Get _ -> state
    | Set (arr_i, elem_i, x) ->
        update_model
          arr_i
          (fun arr -> List.mapi (fun i y -> if i = elem_i then x else y) arr)
          state
    | Length _
    | Is_empty _
    | Get_last _
    | Find_last _
    | To_array _
    | To_list _
    | To_seq _ -> state
    | Copy arr_i ->
        get_model arr_i state :: state
    | Add_last (arr_i, x) ->
        update_model arr_i (fun arr -> arr @ [ x ]) state
    | Append_array (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Append_list (arr_i, l) ->
        update_model arr_i (fun arr -> arr @ l) state
    | Append (arr_i1, arr_i2) ->
        update_model
          arr_i1
          (fun arr -> arr @ get_model arr_i2 state)
          state
    | Append_seq (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Append_iter (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Pop_last_opt arr_i ->
        update_model arr_i (fun arr -> List.take (List.length arr - 1) arr) state
    | Remove_last arr_i ->
        update_model
          arr_i
          (fun arr -> List.take (List.length arr - 1) arr)
          state
    | Truncate (arr_i, len) ->
        update_model arr_i (List.take len) state
    | Clear arr_i ->
        update_model arr_i (Fun.const []) state
    | Iter _
    | Iteri _ -> state
    | Map i -> List.map Elem.mapping_fun (get_model i state) :: state
    | Mapi i -> List.mapi Elem.mapping_fun_with_index (get_model i state) :: state
    | Fold_left _
    | Fold_right _
    | Exists _
    | For_all _ -> state
    | Filter i -> List.filter Elem.pred (get_model i state) :: state
    | Filter_map i ->
        List.filter_map Elem.filter_mapping_fun (get_model i state) :: state
    | Of_array arr -> Array.to_list arr :: state
    | Of_list l -> l :: state
    | Of_seq arr -> Array.to_list arr :: state
    | To_seq_reentrant _
    | To_seq_rev _
    | To_seq_rev_reentrant _
    | Capacity _ -> state
    | Ensure_capacity _
    | Ensure_extra_capacity _
    | Fit_capacity _ -> state
    | Set_capacity (arr_i, cap) -> update_model arr_i (fun arr -> List.take cap arr) state
    | Reset arr_i -> update_model arr_i (Fun.const []) state

  let precond _cmd _state = true

  let postcond : type r. r cmd -> state -> r -> bool =
    fun cmd state res ->
    let valid_arr_idx (I idx) state = idx < List.length state in
    match cmd, res with
    | Create, _
    | Make _, _
    | Copy _, _
    | Add_last _, _
    | Append_array _, _
    | Append_list _, _
    | Append _, _
    | Append_seq _, _
    | Append_iter _, _
    | Remove_last _, _
    | Truncate _, _
    | Clear _, _
    | Iter _, _
    | Iteri _, _
    | Map _, _
    | Mapi _, _
    | Filter _, _
    | Filter_map _, _
    | Of_array _, _
    | Of_list _, _
    | Of_seq _, _
    | Ensure_capacity _, _
    | Ensure_extra_capacity _, _
    | Fit_capacity _, _
    | Set_capacity _, _
    | Reset _, _ -> true
    | Get (arr_i, elem_i), res ->
        valid_arr_idx arr_i state
        && (
          let arr = get_model arr_i state in
          (match List.nth arr elem_i with
           | x -> (match res with Ok x' when Elem.equal x x' -> true | _ -> false)
           | exception (Failure _) ->
               (match res with Error (Invalid_argument _) -> true | _ -> false))
        )
    | Set (arr_i, elem_i, _), res ->
        valid_arr_idx arr_i state
        && (
          let arr = get_model arr_i state in
          (match res with
           | Ok () when elem_i < List.length arr -> true
           | Error (Invalid_argument _) when elem_i >= List.length arr -> true
           | Ok () | Error _ -> false)
        )
    | Length arr_i, l ->
        valid_arr_idx arr_i state
        && l = List.length (get_model arr_i state)
    | Is_empty idx, res ->
        valid_arr_idx idx state
        && Bool.equal res (List.is_empty (get_model idx state))
    | Get_last idx, res ->
        valid_arr_idx idx state
        && (let arr = get_model idx state in
            match List.length arr, res with
            | 0, Error (Invalid_argument _) -> true
            | length, Ok res when length > 0 ->
                Elem.equal res (List.nth arr (length - 1))
            | 0, Ok _ (* unexpected absence of exception *)
            | _, Error _ -> false (* Unexpected exception type *)
            | _, _ -> assert false (* length < 0: impossible *))
    | Pop_last_opt idx, (res : elem option)
    | Find_last idx, (res : elem option) ->
        valid_arr_idx idx state
        && (let arr = get_model idx state in
            match List.length arr, res with
            | 0, None -> true
            | length, Some res when length > 0 ->
                Elem.equal res (List.nth arr (length - 1))
            | 0, Some _ (* unexpected [Some _] *)
            | _, None -> false (* unexpected [None] *)
            | _, _ -> assert false (* length < 0: impossible *))
    | Fold_left (init, i), res ->
        valid_arr_idx i state
        && Elem.equal res (List.fold_left Elem.folding_fun init (get_model i state))
    | Fold_right (i, init), res ->
        valid_arr_idx i state
        && Elem.equal res (List.fold_right Elem.folding_fun (get_model i state) init)
    | Exists i, res ->
        valid_arr_idx i state
        && Bool.equal res (List.exists Elem.pred (get_model i state))
    | For_all i, res ->
        valid_arr_idx i state
        && Bool.equal res (List.for_all Elem.pred (get_model i state))
    | To_array i, arr ->
        valid_arr_idx i state
        && (let arr' = get_model i state in
            try Array.for_all2 Elem.equal arr (Array.of_list arr')
            with Invalid_argument _ -> false)
    | To_list i, l ->
        valid_arr_idx i state
        && (let arr = get_model i state in
            try List.for_all2 Elem.equal arr l
            with Invalid_argument _ -> false)
    | To_seq i, (seq : elem list) | To_seq_reentrant i, (seq : elem list) ->
        valid_arr_idx i state
        && (let arr = get_model i state in
            List.for_all2 Elem.equal seq arr)
    | To_seq_rev i, (seq : elem list) | To_seq_rev_reentrant i, (seq : elem list) ->
        valid_arr_idx i state
        && (let arr = get_model i state in
            List.for_all2 Elem.equal seq (List.rev arr))
    | Capacity i, cap ->
        (* The model here does not contain an actual notion of capacity, so
           only check that the result is greater than the actual length. *)
        valid_arr_idx i state && cap >= List.length (get_model i state)
end

(* Same as [Dynarray_spec], but skips all model computations and checks, since
   they only slow the stress test down. The limitation is that the stress test
   *will* include run generated interleavings, without filtering them using
   [Dynarray_spec.precond]. *)
module Dynarray_spec_for_stress_test (Elem : Elem) = struct
  include Dynarray_spec (Elem)
  let next_state : type r. r cmd -> state -> state = fun cmd state ->
    match cmd with
    | Create -> [] :: state
    | Copy _
    | Make _
    | Map _
    | Mapi _
    | Filter _
    | Filter_map _
    | Of_array _
    | Of_list _
    | Of_seq _ -> [] :: state (* not accurate, just to get the length right *)
    | Get _
    | Set _
    | Length _
    | Is_empty _
    | Get_last _
    | Find_last _
    | Add_last _
    | Append_array _
    | Append_list _
    | Append _
    | Append_seq _
    | Append_iter _
    | Pop_last_opt _
    | Remove_last _
    | Truncate _
    | Clear _
    | Iter _
    | Iteri _
    | Fold_left _
    | Fold_right _
    | Exists _
    | For_all _
    | To_array _
    | To_list _
    | To_seq _
    | To_seq_reentrant _
    | To_seq_rev _
    | To_seq_rev_reentrant _
    | Capacity _
    | Ensure_capacity _
    | Ensure_extra_capacity _
    | Fit_capacity _
    | Set_capacity _
    | Reset _ -> state

  let precond _cmd _state = true
end

module Int : Elem = struct
  type t = int

  let arb = QCheck.int

  let pp = Format.pp_print_int

  let equal = Int.equal

  let ty_show = STM.int

  let init_state =
    [ [ 1; 2; 3 ]; List.init 42 Fun.id ]

  let mapping_fun = (~-)

  let mapping_fun_with_index i x = i + x

  let folding_fun = (+)

  let pred x = Int.equal 0 x

  let filter_mapping_fun x = if Int.compare x 0 < 0 then Some (-x) else None
end

module Float : Elem = struct
  type t = float

  let arb = QCheck.float

  let pp = Format.pp_print_float

  let equal = Float.equal

  let ty_show = STM.float

  let init_state =
    [ [ 1.; 2.; 3. ]; List.init 42 Float.of_int ]

  let mapping_fun = (~-.)

  let mapping_fun_with_index i x = Float.of_int i +. x

  let folding_fun = (+.)

  let pred x = Float.equal 0. x

  let filter_mapping_fun x = if Float.compare x 0. < 0 then Some (-.x) else None
end

module Test_sequential = struct
  module Int = STM_sequential.Make (Dynarray_spec (Int))
  module Float = STM_sequential.Make (Dynarray_spec (Float))
end

module Test_domain = struct
  module Int = STM_domain.Make (Dynarray_spec_for_stress_test (Int))
  module Float = STM_domain.Make (Dynarray_spec_for_stress_test (Float))
end

let () =
  QCheck_base_runner.run_tests_main
    [ Test_sequential.Int.agree_test ~count:1_000 ~name:"sequential model agreement test";
      Test_domain.Int.stress_test_par ~count:2_000 ~name:"stress test";
      Test_sequential.Float.agree_test ~count:1_000 ~name:"sequential model agreement test";
      Test_domain.Float.stress_test_par ~count:2_000 ~name:"stress test";
    ]
