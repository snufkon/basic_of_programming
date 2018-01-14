#use "color.ml" ;;

(* 赤黒木を表す型 *)
type ('a, 'b) rb_tree_t = Empty
                        | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t
;;                                                 
