#use "../metro/ekikan_t.ml" ;;
#use "../metro/ekikan_tree_t.ml" ;;
#use "../metro/insert_ekikan.ml" ;;
#use "../metro/inserts_ekikan.ml" ;;


(* 駅間の例 *) 
let ekikan1 = {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} ;;
let ekikan2 = {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} ;;
let ekikan3 = {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} ;;


(* テスト *)
let tree1 = inserts_ekikan Empty [ekikan1; ekikan2] ;;
let test1 = tree1 = Node (Empty,
                          "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
	                  Node (Empty,
                                "池袋", [("新大塚", 1.8)], 
	                        Node (Empty,
                                      "茗荷谷", [("新大塚", 1.2)],
                                      Empty))) ;;
