(* 駅間を表す木 *)
type ekikan_tree_t = Empty
                   | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t
;;
