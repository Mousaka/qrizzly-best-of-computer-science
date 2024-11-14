let rec stackOverflow n =
    if n = 0 then 0
    else 1 + stackOverflow (n - 1)
   
let rec factorialBasic n =
    if n = 0 then 1 else factorialBasic (n - 1) * n
    

let rec factorial' n acc =
    if n = 0 then acc else factorial' (n - 1) (n * acc)

let factorial n =
    factorial' n 1


let rec fib n =
    match n with
    | 0 | 1 -> n
    | n -> fib (n-1) + fib (n-2)


let rec fibTailHelper acc1 acc2 n =
    match n with
    | 0 -> acc1
    | 1 -> acc2
    | _ ->
        fibTailHelper acc2 (acc1 + acc2) (n - 1)
let fibTail n =
    fibTailHelper 0 1 n

let runWithTimer name fn arg =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = fn arg
    stopWatch.Stop();
    printfn $"%s{name} %A{arg} result: %A{result} in %f{stopWatch.Elapsed.TotalSeconds} seconds"
    
    
    // [1,2,3] -> [2,4,6]

let rec listAddTwo list =
    match list with
    | head::tail ->
        head * 2 :: listAddTwo tail
    | [] -> []

let rec listMap fn list =
    match list with
    | head::tail ->
        fn head  :: listMap fn tail
    | [] -> []
    
let rec listMapTail' fn list acc=
    match list with
    | head::tail ->
        listMapTail' fn tail (fn head :: acc)
    | [] -> List.rev acc 
        
    
let listMapTail fn list =
    listMapTail' fn list []
    
    
// runWithTimer "fibTail" fibTail 48
// runWithTimer "fib" fib 48
    
// Övningar
// implementera listFilter som fungerar som List.filter
// Gör den svansrekursiv.

// implementera listFoldl som fungerar som List.foldl (något svårare)
// Gör den svansrekursiv.

// implementera listFilter och listMap m.h.a. listFoldl
  