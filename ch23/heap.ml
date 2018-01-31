
(* ヒープの添字の型 *)
type index_t = int ref ;;

(* ヒープ *)
type ('a, 'b) t = int ref * (index_t * 'a * 'b) array ;;

(* insert 時にヒープが一杯だと raise される例外 *)
exception Full ;;

(* split_top 時にヒープが空だと raise される例外 *)
exception Empty ;;

  
(* 目的: 指定された size 数の要素を持つヒープを作成する *)
(* create: int -> 'a -> 'b -> ('a, 'b) t *)
let create size key value =
  (ref (0), Array.make size (ref (-1), key, value))
;;


(* 目的: ヒープの i 番目の値と j 番目の値を入れ替える *)
(* ヒープは（破壊的に）書き換わる *)
(* exchange: (index_t * 'a * 'b) array -> int -> int -> unit *)
let exchange array i j =
  let (ref1, value1, key1) = array.(i) in
  let (ref2, value2, key2) = array.(j) in
  array.(i) <- (ref2, value2, key2) ;
  array.(j) <- (ref1, value1, key1) ;
  ref2 := i;
  ref1 := j;
  ()
;;


(* 目的: 上方向に向かってヒープ条件が満たされるまで要素の入れ替えを行う *)
(* adjust_parent: (index_t * 'a * 'b) array -> int -> unit *)
let rec adjust_parent array current_index =
  if current_index = 0 then
    ()
  else
    let (_, key, _) = array.(current_index) in
    let parent_index = (current_index - 1) / 2 in
    let (_, p_key, _) = array.(parent_index) in
    if key < p_key then
      (exchange array current_index parent_index;
       adjust_parent array parent_index;
       ())
    else
      ()
;;


(* 目的: 下方向に向かってヒープ条件が満たされるまで要素の入れ替えを行う *)
(* adjust_child: int * (index_t * 'a * 'b) array -> int -> unit *)
let rec adjust_child num array current_index =
  let c1_index = current_index * 2 + 1 in
  let c2_index = current_index * 2 + 2 in
  if c1_index >= num then
    ()
  else
    let (_, key, _) = array.(current_index) in
    let (_, c1_key, _) = array.(c1_index) in
    if c2_index >= num && key < c1_key then
      (exchange array current_index c1_index;
       adjust_child num array c1_index;
       ())        
    else
      let (_, c2_key, _) = array.(c2_index) in
      if (c1_key < c2_key) && (key > c1_key) then
        (exchange array current_index c1_index;
         adjust_child num array c1_index;
         ())
      else if key > c2_key then
        (exchange array current_index c2_index;
         adjust_child num array c2_index;
         ())
      else
        ()
;;


(* 目的: ヒープに新しい要素を追加する *)
(* ヒープは（破壊的）に書き換わる *)
(* insert: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let insert (r_num, array) key value =
  if !r_num >= (Array.length array) then
    raise Full
  else
    let index = (ref !r_num) in
    array.(!r_num) <- (index, key, value);
    adjust_parent array !r_num ;
    r_num := !r_num + 1;
    (index, (r_num, array))
;;


(* 目的: ヒープの index 番目の要素を返す *)
(* get: ('a, 'b) t -> index_t -> 'a * 'b *)
let get (r_num, array) r_index = 
  if !r_index >= 0 && !r_index < !r_num then
    let (_, key, value) =  array.(!r_index) in
    (key, value)
  else
    raise Not_found
;;


(* 目的: ヒープの index 番目の値を更新する *)
(* ヒープは（破壊的に）書き換わる *)
(* set ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t *)
let set (r_num, array) r_index key value =
  let (_, current_key, _) = array.(!r_index) in
  array.(!r_index) <- (r_index, key, value);
  if key < current_key then
    adjust_parent array !r_index
  else
    adjust_child !r_num array !r_index;
  (r_num, array)
;;


(* 目的: ヒープを最小の値と、それを取り除いたヒープに分離し、組にして返す *)
(* split_top: ('a, 'b) t -> ('a * 'b) * ('a, 'b) t *)
let split_top (r_num, array) =
  if !r_num <= 0 then
    raise Empty
  else
    r_num := !r_num-1;
  let (r_index1, key1, value1) = array.(0) in
  let (r_index2, key2, value2) = array.(!r_num) in
  array.(0) <- (r_index1, key2, value2);
  adjust_child !r_num array 0;
  ((key1, value1), (r_num, array))
;;


(* 目的: ヒープ中のデータ数を返す *)
let length (r_num, array) =
  !r_num
;;
