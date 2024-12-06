type BinaryTree =
    | Node of BinaryTree * int * BinaryTree
    | Empty


let simpleTree = (Node(Empty, 5, Empty))

let rec insert: int -> BinaryTree -> BinaryTree =
    fun n tree ->
        match tree with
        | Empty -> Node(Empty, n, Empty)
        | Node(left, value, right) ->
            if n < value then
                Node(insert  n left, value, right)
            else
                Node(left, value, insert  n right)


let makeTree: List<int> -> BinaryTree =
    fun list -> List.fold (fun a b -> insert b a) Empty list

// Övningar
// implementera funktionerna nedan

// Ska ge en sorterad lista
let treeToList: BinaryTree -> List<int> = fun tree -> failwith "Not implemented"

let sort: List<int> -> List<int> = makeTree >> treeToList


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


let rowToString: List<int * int> -> string =
    fun row ->
        let rec inner row i =
            match row with
            | [] -> ""
            | (value, xpos) :: T ->
                // printfn $"value: %A{value} xpos: %A{xpos} i: %A{i}"
                let valLength = value.ToString().Length
                let spaces = String.replicate (max 0 (xpos - (i + valLength))) " "
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

let rec getBalanceFactor: BinaryTree -> int =
    fun tree ->
        match tree with
        | Empty -> 0
        | Node(left, _, right) -> depth right - depth left


let rec balanced tree = abs (getBalanceFactor tree) <= 1


// AVL LL
let rotateRight: BinaryTree -> BinaryTree =
    fun tree ->
        match tree with
        | Node(Node(a, x, b), y, c) -> Node(a, x, Node(b, y, c))
        | _ -> tree

// AVL RR
let rotateLeft: BinaryTree -> BinaryTree =
    fun tree ->
        match tree with
        | Node(a, x, Node(b, y, c)) -> Node(Node(a, x, b), y, c)
        | _ -> tree

// makeTree [3;2;1] |> rotateRight |> balanced

// AVL LR
let rotateLeftRight tree =
    match tree with
    | Node(left, value, right) ->
        let leftRotated = rotateLeft left 
        let balancedLeft = Node(leftRotated,  value, right)
        rotateRight balancedLeft // Rotate right on root
    | _ -> tree 


// AVL RL
let rotateRightLeft tree =
    match tree with
    | Node(left, value, right) ->
        let rightRotaded = rotateRight left 
        let balancedRight = Node(rightRotaded,  value, right)
        rotateRight balancedRight 
    | _ -> tree 
    
    // Denna implementering är inte korrekt
let rec balance tree =
    match tree with
    | Empty -> Empty
    | Node(left, value, right) ->
        let balanceFactor = getBalanceFactor tree
        printfn $"balanceFactor %i{balanceFactor}"
        printfn $"lefft balance %A{getBalanceFactor left}"
        
        if balanceFactor > 1 then
            if getBalanceFactor right > 0 then
                rotateLeft tree
            else
                rotateRightLeft tree
        elif balanceFactor < -1 then
            if getBalanceFactor left < 0 then
                printfn "Rotating right!"
                rotateRight tree
            else
                printfn "Rotating left right!"
                rotateLeftRight tree
        else
            Node (balance left, value, balance right)

            
let p a = printTree a; a

//makeTree [6;4;5] |> balance |> printTree
// makeTree [5;3;2] |> balance |> printTree

makeTree [10;5;3] |> rotateRight |> insert 4 |> insert 2 |> insert 0 |> rotateRight |> printTree
makeTree [10;5;3] |> rotateRight |> insert 4 |> insert 2 |> insert 0 |> getBalanceFactor 
makeTree [10;5;3] |> rotateRight |> insert 4 |> insert 2 |> insert 0 |> balance |> printTree 
makeTree [10;5;3] |> rotateRight |> insert 4 |> insert 2 |> insert 0 |> getBalanceFactor


// LL tree
makeTree [10;5;3] |> rotateRight |> insert 4 |> insert 2 |> insert 0 |> balance |> printTree

// RR tree
makeTree [10;15;20] |>  rotateLeft |> insert 16 |> insert 21 |> insert 22 |> p

// LR tree
makeTree [10;5;6] |> balance |> printTree