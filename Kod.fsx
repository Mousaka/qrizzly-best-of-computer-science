
let isEmpty (a: string) = (=) "" a
let contains (a: string) (b: string) = b.Contains(a)
let toUpper (a: string) = a.ToUpper()
let split (a: string) (b: string) = b.Split(a)
let replace (a: string) (b: string) (c: string) = c.Replace(a, b)
let containsHello theString = contains "Hello" theString

let stringProcessing theString =
    theString |> toUpper |> replace "a" "b" |> split " "

let stringProcessing3 =
    let composedFunction =
        fun s -> (split " " (replace "a" "b" (toUpper s)))

    composedFunction

let stringProcessing4 = toUpper >> replace "a" "b" >> split " "

stringProcessing4 "Hubba bubba!"

let notEmptyAnimal animal =
    if isEmpty animal then
        Error "No animal"
    else
        Ok animal
        
let isCoolAnimal animal =
    match animal with
    | "Kanin" -> Ok animal
    | _ -> Error "Not cool animal"

let resultMap  fn r =
    match r with
    | Ok x -> Ok (fn x)
    | Error e -> Error e

let resultBind fn r =
    match r with
    | Ok x -> fn x
    | Error e -> Error e

let validateAnimal animal=
    animal
    |> notEmptyAnimal
    |> resultBind isCoolAnimal

validateAnimal "Kanin"
validateAnimal "Hare"

// Övningar
// Implementera compose (som infix eller vanlig function)
// Implementera Result.mapError
// Implementera Option.map   
// Implementera Option.bind
// Implementera validateListOfAnimals : string list -> Result<string list, string>

// Bonus
// Implementera ResultOption.map : ('a -> 'b) -> ResultOption<'a> -> ResultOption<'b>
// Implementera ResultOption.bind : ('a -> ResultOption<'b>) -> ResultOption<'a> -> ResultOption<'b>
    
// Andra kurstillfället
// Implementera List.map : ('a -> 'b) -> 'a list -> 'b list
// Implementera List.filter : ('a -> bool) -> 'a list -> 'a list
// Implementera List.foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a


