

type BinaryTree
    = Node of BinaryTree * int * BinaryTree 
    | Empty


let simpleTree = (Node (Empty, 5, Empty))

let rec insertValueInTree :  BinaryTree -> int -> BinaryTree =
    fun tree n ->
        match tree with
        | Empty -> Node (Empty, n, Empty)
        | Node (left, value, right) ->
            if n < value then
                Node (insertValueInTree left n, value, right)
            else
                Node (left, value, insertValueInTree right n) 


let makeTree: List<int> -> BinaryTree  =
   fun list ->
      List.fold insertValueInTree Empty list

// Övningar
// implementera funktionerna nedan

// Ska ge en sorterad lista
let treeToList: BinaryTree -> List<int> =
    fun tree ->
        failwith "Not implemented"

let sort: List<int> -> List<int>
    fun data ->
        failwith "Not implemented" 
        
        
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

// Implementera en funktion som gör en fin sträng av ett träd.
(* 

Node (Node (Empty, 2, Empty), 3, Node (Empty, 4, Node (Empty, 5, Empty))) ->

    3
   / \       
  2   4      
       \
        5  

 *)
let prettyTree : BinaryTree -> string =
    fun tree ->
        failwith "Not implemented"
        
        
let printTree :  BinaryTree -> unit =
    fun tree -> 
        tree |> prettyTree |> printfn "%s"