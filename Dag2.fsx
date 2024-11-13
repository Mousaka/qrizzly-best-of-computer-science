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
    

runWithTimer "fibTail" fibTail 48
runWithTimer "fib" fib 48
    
    