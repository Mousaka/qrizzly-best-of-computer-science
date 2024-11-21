let runWithTimerListArg name fn (arg: List<'a>) =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = fn arg
    stopWatch.Stop();
    printfn $"%s{name} n: %A{arg.Length} result: %A{result} in %f{stopWatch.Elapsed.TotalSeconds} seconds"
    
let runWithTimer name fn arg =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = fn arg
    stopWatch.Stop();
    printfn $"%s{name} %A{arg} result: %A{result} in %f{stopWatch.Elapsed.TotalSeconds} seconds"
    

// O(1) // Konstant tid
let getFirst list =
    match list with
    | [] -> None
    | x :: _ -> Some x

let rec binarySearch target sortedList =
    let rec search low high =
        if low > high then false
        else
            let mid = (low + high) / 2
            match List.tryItem mid sortedList with
            | None -> false
            | Some midVal ->
                if midVal = target then true
                elif midVal < target then search (mid + 1) high
                else search low (mid - 1)
    search 0 (List.length sortedList - 1)

// [10, 21]          21
//        2
//        if target > 6
//         [ 1 .. 1000000 ] 

// log(x) = 100        10^x = 100       x = 2
// log(2) = 100
// log(4) = 1000
let search target list =
    List.fold (fun acc x -> acc || x = target) false list

// O(n) Linjär tid
let sumList list = List.fold (+) 0 list

// runWithTimerListArg "sumList" sumList [ for i in 1 .. 10 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 100 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 1000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 2000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 3000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 10000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 100000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 1000000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 10000000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 100000000 -> i]
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 500000000 -> i]
// Långsam
// runWithTimerListArg "sumList" sumList [ for i in 1 .. 1000000000 -> i]


// O(n^2) Kvadratisk tid
let uniquePairs list =
    [ for i in 0 .. (List.length list - 1) do
        for j in (i + 1) .. (List.length list - 1) do
            yield (list.[i], list.[j]) ]
    
[ for i in 1 .. 5 -> i] |> uniquePairs
// Börjar bli långsam här
// [ for i in 1 .. 2000 -> i] |> uniquePairs
// runWithTimerListArg "uniquePairs" uniquePairs [ for i in 1 .. 10 -> i]
// runWithTimerListArg "uniquePairs" uniquePairs [ for i in 1 .. 100 -> i]
// runWithTimerListArg "uniquePairs" uniquePairs [ for i in 1 .. 1000 -> i]
// runWithTimerListArg "uniquePairs" uniquePairs [ for i in 1 .. 2000 -> i]
// runWithTimerListArg "uniquePairs" uniquePairs [ for i in 1 .. 3000 -> i]
let bubbleSort (arr: int array) =
    let n = Array.length arr
    for j in n - 1 .. -1 .. 1 do
        for i in 0 .. j - 1 do
            if arr.[i] > arr.[i + 1] then
                // Swap elements
                let temp = arr.[i]
                arr.[i] <- arr.[i + 1]
                arr.[i + 1] <- temp

    arr
// Börjar bli långsam här
// [ for i in 1 .. 1000000 -> 1000000 - i ] |> List.toArray |> bubbleSort


// Övning 1
// a)
// Implementera en funktion som tar en lista och returnerar en lista med alla unika element från listan.
// Pseudokod är okej, det viktigaste är resonerandet kring tidskomplexiten.
// Exempel input och output:
// [1;2;3;1;2;4;5;4] -> [1;2;3;5]
// [1] -> [1]
// [1;1] -> []
// [1;1;2] -> [2]
//
// b)
// Om vi i funktionen kan anta att listan alltid är sorterad i storleksordning
// kan vi då göra funktionen snabbare? Isf hur? Och vilken tidskomplexitet kan vi få då?
