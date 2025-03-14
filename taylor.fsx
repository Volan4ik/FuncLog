open System

let builtinExp2x (x: float) = Math.Exp(2.0 * x)

let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)

let dumbTaylorExp2x (x: float) (eps: float) =
    let mutable sum = 0.0
    let mutable term = 1.0
    let mutable n = 0
    while abs term >= eps do
        term <- Math.Pow(2.0 * x, float n) / float (factorial n)
        sum <- sum + term
        n <- n + 1
    sum, n

let smartTaylorExp2x (x: float) (eps: float) =
    let mutable sum = 1.0
    let mutable term = 1.0
    let mutable n = 1
    while abs term >= eps do
        term <- term * (2.0 * x) / float n
        sum <- sum + term
        n <- n + 1
    sum, n

let printHeader () =
    printfn "|    x    | Builtin e^(2x) | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "|---------|----------------|--------------|---------|-------------|---------|"

let printTable a b step eps =
    printHeader ()
    let mutable x = a
    while x <= b do
        let builtin = builtinExp2x x
        let smart, smartTerms = smartTaylorExp2x x eps
        let dumb, dumbTerms = dumbTaylorExp2x x eps
        printfn "| %6.3f  | %12.6f   | %12.6f | %6d  | %11.6f | %6d  |" x builtin smart smartTerms dumb dumbTerms
        x <- x + step

let a = 0.1
let b = 0.6
let step = 0.1
let eps = 1e-6

printTable a b step eps
