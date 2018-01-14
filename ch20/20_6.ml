#use "../metro/ekikan_t.ml" ;;
#use "../metro/global_ekikan_list.ml" ;;

(* make top で my-redBlack.top を作成しその上で動作させる *)

open RedBlack ;;


(* 目的: t 型の木と ekikan_t 型の駅間を受け取り、その情報を挿入した木を返す *)
(* insert_ekikan:
   (string * (string * float) list ) t -> ekikan_t ->
   (string * (string * float) list ) t *)
let insert_ekikan tree ekikan =
  (* 目的: t 型の木と駅名、「その駅に直接つながっている駅名」と「その駅までの距離」の組
     を受け取り、その情報を挿入した木を返す *)
  (* insert:
     (string * (string * float) list ) t -> string -> (string * float) ->
     (string * (string * float) list ) t *)
  let insert tree ekimei ekimei_to_kyori = 
    let lst = try 
	search tree ekimei
      with Not_found -> [] 
    in insert tree ekimei (ekimei_to_kyori :: lst) 
  in

  match ekikan with 
  {kiten = kiten; shuten = shuten; keiyu = keiyu; kyori = kyori; jikan = jikan} -> 
    insert (insert tree shuten (kiten, kyori)) kiten (shuten, kyori)
;;


(* 目的: t 型の木と ekikan_t list 型の駅間リストを受け取り
   リストに含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan: (string * (string * float) list ) t -> ekikan_t list ->
                   (string * (string * float) list ) t *)
let rec inserts_ekikan tree ekikan_list =
  List.fold_left insert_ekikan tree ekikan_list
;;


(* 目的: 漢字の駅名２つ name1, name2 と t 型の木を受け取り、その２駅間の距離を返す *)
(* みつからないときには Not_found の例外が発生する *)
(* get_ekikan_kyori: string -> string -> (string * (string * float) list ) t -> float *)
let get_ekikan_kyori name1 name2 tree =
  List.assoc name2 (search tree name1)   
;;


(* テスト *)
let global_ekikan_tree = inserts_ekikan empty global_ekikan_list ;;
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 ;;
let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_tree = 1.2 ;;
let test3 = get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_tree = 1.8 ;;
let test4 = (try
               get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree
             with Not_found -> infinity) = infinity ;;
let test5 = get_ekikan_kyori "東京" "銀座" global_ekikan_tree = 1.1 ;;
