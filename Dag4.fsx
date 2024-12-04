type BinaryTree =
    | Node of BinaryTree * int * BinaryTree
    | Empty


let simpleTree = (Node(Empty, 5, Empty))

let rec insertValueInTree: BinaryTree -> int -> BinaryTree =
    fun tree n ->
        match tree with
        | Empty -> Node(Empty, n, Empty)
        | Node(left, value, right) ->
            if n < value then
                Node(insertValueInTree left n, value, right)
            else
                Node(left, value, insertValueInTree right n)


let makeTree: List<int> -> BinaryTree =
    fun list -> List.fold insertValueInTree Empty list

// Övningar
// implementera funktionerna nedan

// Ska ge en sorterad lista
let treeToList: BinaryTree -> List<int> = fun tree -> failwith "Not implemented"

let sort: List<int> -> List<int> = fun data -> failwith "Not implemented"


// Implementera en funktion som returnerar djupet på ett BinaryTree.
(*
    Empty     -> 0
    
    1         -> 1
      
     3
    / \       -> 2
   2   4
   
     3
    / \       
   2   4      -> 3
        \
         5       
         
    osv.
*)

let rec depth: BinaryTree -> int =
    fun tree ->
        match tree with
        | Empty -> 0
        | Node(left, _, right) -> 1 + max (depth left) (depth right)

// Implementera en funktion som gör en fin sträng av ett träd.
(* 

Node (Node (Empty, 2, Empty), 3, Node (Empty, 4, Node (Empty, 5, Empty))) ->

    3
   / \       
  2   4      
 / \ / \
6  6 6 5  

 *)

let t =
    Node(
        Node(Node(Empty, 1, Empty), 2, (Node(Empty, 7, Empty))),
        3,
        Node(Node(Empty, 8, Empty), 4, Node(Empty, 5, Empty))
    )

let allValuesAtDepth: int -> BinaryTree -> List<int * int> =
    fun soughtDepth tree ->
        let fullDepth = depth tree

        let rec inner: int -> int -> BinaryTree -> List<int * int> =
            fun currentDepth xpos tree ->
                if soughtDepth < 1 then
                    []
                else
                    match tree with
                    | Empty -> []
                    | Node(left, value, right) ->
                        if currentDepth = soughtDepth then
                            [ value, xpos ]
                        else
                            inner (currentDepth + 1) ((xpos - (1 + (fullDepth - currentDepth)))) left
                            @ inner (currentDepth + 1) (xpos + (1 + (fullDepth - currentDepth))) right

        inner 1 (fullDepth * 2) tree

t |> allValuesAtDepth 3

let rowToString: List<int * int> -> string =
    fun row ->
        let rec inner row i =
            match row with
            | [] -> ""
            | (value, xpos) :: T ->
                printfn $"value: %A{value} xpos: %A{xpos} i: %A{i}"
                let valLength = value.ToString().Length
                let spaces = String.replicate (xpos - (i + valLength)) " "
                let str = $"{spaces}{value}"
                str + inner T (str.Length)

        inner row 0

let prettyTree: BinaryTree -> string =
    fun tree ->
        let depth = depth tree

        [ 1..depth ]
        |> List.map (fun d -> allValuesAtDepth d tree)
        |> List.map rowToString
        |> String.concat "\n"

// t |> prettyTree
// printTree (Node (Node (Empty, 1, Empty), 2, (Node (Empty ,5, Empty))))
let printTree: BinaryTree -> unit = fun tree -> tree |> prettyTree |> printfn "%s"
printTree t
