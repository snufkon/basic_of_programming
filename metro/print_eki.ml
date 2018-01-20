(* 前提: eki_t ロード済み *)

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
        print_newline ();
        print_string "手前にある駅: ";
        print_list (List.rev lst);
        print_newline ()
       )
;;
