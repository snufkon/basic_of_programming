open RedBlack;; 
open Metro ;;


(* 目的: RedBlack.t 型の木と ekikan_t 型の駅間を受け取り、その情報を挿入した木を返す *)
(* insert_ekikan:
   (string * (string * float) list ) RedBlack.t -> ekikan_t ->
   (string * (string * float) list ) RedBlack.t *)
let insert_ekikan tree ekikan =
  (* 目的: RedBlack.t 型の木と駅名、「その駅に直接つながっている駅名」と「その駅までの距離」の組
     を受け取り、その情報を挿入した木を返す *)
  (* insert:
     (string * (string * float) list ) RedBlack.t -> string -> (string * float) ->
     (string * (string * float) list ) RedBlack.t *)
  let insert1 tree ekimei ekimei_to_kyori = 
    let lst = try 
	search tree ekimei
      with Not_found -> [] 
    in insert tree ekimei (ekimei_to_kyori :: lst) 
  in

  match ekikan with 
  {kiten = kiten; shuten = shuten; keiyu = keiyu; kyori = kyori; jikan = jikan} -> 
    insert1 (insert1 tree shuten (kiten, kyori)) kiten (shuten, kyori)
;;


(* 目的: RedBlack.t 型の木と ekikan_t list 型の駅間リストを受け取り
   リストに含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan: (string * (string * float) list ) RedBlack.t -> ekikan_t list ->
                   (string * (string * float) list ) RedBlack.t *)
let rec inserts_ekikan tree ekikan_list =
  List.fold_left insert_ekikan tree ekikan_list
;;


(* 目的: 漢字の駅名２つ name1, name2 と RedBlack.t 型の木を受け取り、その２駅間の距離を返す *)
(* みつからないときには Not_found の例外が発生する *)
(* get_ekikan_kyori: string -> string -> (string * (string * float) list ) RedBlack.t -> float *)
let get_ekikan_kyori name1 name2 tree =
  List.assoc name2 (search tree name1)   
;;


(* 目的: 直前に確定した駅 p と未確定の駅のリスト v、使用する駅間を持つ
   RedBlack.t 型の木を受け取り更新処理を行った後に未確定の駅リストを返す *)
(* koushin: eki_t -> eki_t list -> (string * (string * float) list ) RedBlack.t -> eki_t list *)
let koushin p v ekikan_tree = match p with
    {namae=p_name; saitan_kyori=p_kyori; temae_list=p_team_list} -> 
    List.map (fun q -> match q with
                         {namae=q_name; saitan_kyori=q_kyori; temae_list=q_team_list}
                         ->
                         try
                           let ekikan_kyori = get_ekikan_kyori p_name q_name ekikan_tree in
                           let saitan_kyori = (p_kyori +. ekikan_kyori) in
                           if saitan_kyori < q_kyori then
                             {namae=q_name; saitan_kyori=saitan_kyori; temae_list= q_name :: p_team_list}
                           else
                             q
                         with Not_found -> q)
      v
;;


(* 目的: 未確定の駅のリスト eki_list, 使用する駅間を持つ RedBlack.t 型の木
         を受け取りダイクストラのアルゴリズムによって各駅について
         最短距離と最短経路が正しく入ったリストを返す
*)
let rec dijkstra_main eki_list ekikan_tree = match eki_list with
    [] -> [] 
  | first :: rest ->
     let (saitan, others) = saitan_wo_bunri first rest in
     let eki_list2 = koushin saitan others ekikan_tree in
     saitan :: dijkstra_main eki_list2 ekikan_tree
;;


(* 目的: 駅リストから指定された名前の駅のレコードを返す *)
(* find_eki: eki_t list -> string -> eki_t *)
let find_eki eki_list namae =
  let result = List.filter (fun {namae=n; saitan_kyori=sk; temae_list=tm} -> n = namae)
                 eki_list
  in
  match result with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
  | first :: rest -> first
;;


(* 目的: 始点の駅名(ローマ字の文字列)と終点の駅名(ローマ字の文字列)を受け取り、
   終点の駅のレコード(eki_t 型)を返す *)
(* dijkstra: string -> string -> eki_t *)
let dijkstra s_romaji e_romaji =
  let global_ekikan_tree = inserts_ekikan empty global_ekikan_list in
  let normalized_ekimei_list = seiretsu global_ekimei_list in
  let s_kanji = romaji_to_kanji s_romaji normalized_ekimei_list in
  let e_kanji = romaji_to_kanji e_romaji normalized_ekimei_list in
  let eki_list = make_initial_eki_list normalized_ekimei_list s_kanji in
  let result_eki_list = dijkstra_main eki_list global_ekikan_tree in
  find_eki result_eki_list e_kanji
;;


(* 21.1 *)
(* 目的: eki_t 型のレコードを受け取り結果を「きれいに」表示する *)
(* print_eki: eki_t -> unit *)
let print_eki eki = 
  (* 目的: list の要素を表示する *)
  (* print_list: string list -> unit *)
  let rec print_list lst = match lst with
      [] -> print_newline ()
    | first :: rest ->
       (print_string first;
        print_string " ";
        print_list rest
       )
  in match eki with
       {namae=namae; saitan_kyori = kyori; temae_list = lst} -> 
       (print_string "駅名: ";
        print_string namae;
        print_newline ();
        print_string "最短距離: ";
        print_float kyori;
        print_string "km";
        print_newline ();
        print_string "手前にある駅: ";
        print_list (List.rev lst);
        print_newline ()
       )
;;


(* メイン関数 *)
(* main: string -> string -> unit *)
let main s_romaji e_romaji =
  let eki = dijkstra s_romaji e_romaji in
  print_eki eki
;;


(* メイン関数の呼び出し *)
let _ = main Sys.argv.(1) Sys.argv.(2) ;;
