type 'a tree = L | T of 'a tree * 'a * 'a tree

let empty = L

let rec insert key = function
  | L -> T (L, key, L)
  | T (l, k, r) -> if key < k then T(insert key l, k, r) else T(l,k, insert key r)

let rec print_sorted = function
  | L -> ()
  | T (l, k, r) -> print_sorted l; Printf.printf "%d\n" k; print_sorted r

let rec size  = function
  | L -> 0
  | T (l, _, r) -> 1 + (size l) + (size r)

let rec member key = function
  | L -> false
  | T (l, k, r) ->
    if k = key then
      true
    else if key < k then
      member key l
    else
      member key r


let tree = empty |> insert 7 |> insert 10 |> insert 5 |> insert 3
