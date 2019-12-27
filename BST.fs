module BST

type BST<'T> =
    | Empty
    | Node of value:'T * left: BST<'T> * right: BST<'T>

let rec find value bst =
    match bst with
    | Empty -> bst
    | Node (x, left, right) ->
        if value = x then bst
        elif value < x then (find value left)
        else (find value right)

let rec insert value bst =
    match bst with
    | Empty -> Node(value, Empty, Empty)
    | Node(x, left, right) ->
        if value = x then bst
        elif value < x then Node(x, insert value left, right)
        else Node(x, left, insert value right)
