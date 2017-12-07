#use "13_6.ml" ;;

(* 目的: 直前に確定した駅 p と未確定の駅のリスト v を受け取り
   更新処理を行った後に未確定の駅リストを返す
*)
(* koushin: eki_t -> eki_t list -> eki_t list *)
let koushin p v =
  let f q = koushin1 p q in
  List.map f v
;;


(* テスト *)
let test1 = koushin eki2 [] = [] ;;
let test2 = koushin eki3 [eki1; eki4] =
              [eki1; {namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}] ;;
let test3 = koushin eki2 [eki1; eki4] =
              [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; eki4] ;; 
