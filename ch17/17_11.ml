#use "../metro/assoc.ml";;

(* テスト *)
let test1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8 ;;
let test1 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity ;;
