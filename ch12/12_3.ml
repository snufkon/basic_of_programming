#use "12_1.ml" ;;

(* 目的: eki_t 型のリスト lst と起点 name を受け取り、
   起点のみ下記の変更を行ったリストを返す

   - saitan_kyori に 0.0 を設定
   - temae_list は起点の駅名のみのリスト
*)
(* shokika: eki_t list -> string -> eki_t list *)
let rec shokika lst name = match lst with
    [] -> []
  | ({namae=namae; saitan_kyori=kyori; temae_list=temae_list} as first) :: rest ->
    if name = namae then
      {namae=name; saitan_kyori=0.0; temae_list=[name]} :: rest
    else
      first :: shokika rest name
;;


(* テストデータ *)
let eki1 = {namae="代々木上原"; saitan_kyori=infinity; temae_list=[]} ;;
let eki2 = {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]} ;;
let eki3 = {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[]} ;;

(* テスト *)
let test1 = shokika [] "代々木上原" = [] ;;
let test2 = shokika [eki1; eki2; eki3] "代々木上原" = [
    {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"]};
    eki2;
    eki3
  ] ;;
let test3 = shokika [eki1; eki2; eki3] "明治神宮前" = [
    eki1;
    eki2;
    {namae="明治神宮前"; saitan_kyori=0.0; temae_list=["明治神宮前"]}
  ] ;;
