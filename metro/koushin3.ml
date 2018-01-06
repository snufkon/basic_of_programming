(* 前提: eki_t, ekikan_tree_t, get_ekikan_kyori3 をロード済み *)

(* 目的: 直前に確定した駅 p と未確定の駅のリスト v、使用する駅間を持つ
   ekikan_tree_t 型の木を受け取り更新処理を行った後に未確定の駅リストを返す *)
(* koushin: eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *)
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
