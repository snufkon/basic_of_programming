#use "../metro/ekikan_t.ml" ;;
#use "../metro/ekikan_tree_t.ml" ;;
#use "../metro/insert_ekikan.ml" ;;

(* 駅間の例 *) 
let ekikan1 = {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} ;;
let ekikan2 = {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} ;;
let ekikan3 = {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} ;;


(* テスト *)
let tree1 = insert_ekikan Empty ekikan1 ;;
let test1 = tree1 = Node (Empty,
                          "新大塚", [("池袋", 1.8)],
                          Node (Empty,
                                "池袋", [("新大塚", 1.8)],
                                Empty)) ;;

let tree2 = insert_ekikan tree1 ekikan2 ;;
let test2 = tree2 = Node (Empty,
                          "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
	                  Node (Empty,
                                "池袋", [("新大塚", 1.8)], 
	                        Node (Empty,
                                      "茗荷谷", [("新大塚", 1.2)],
                                      Empty))) ;;
          
let tree3 = insert_ekikan tree2 ekikan3 ;;
let test3 = tree3 = Node (Node (Empty,
                                "後楽園", [("茗荷谷", 1.8)],
                                Empty), 
	                  "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
                          Node (Empty, 
	                        "池袋", [("新大塚", 1.8)], 
	                        Node (Empty, 
		                      "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
		                      Empty))) ;;
