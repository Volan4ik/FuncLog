
open System

let dichotomy f a b epsilon maxIter =
    let rec loop a b iter =
        let c = (a + b) / 2.0
        if iter >= maxIter then c
        elif abs(b - a) < epsilon then c
        else
            if f a * f c < 0.0 then loop a c (iter + 1)
            else loop c b (iter + 1)
    if f a * f b > 0.0 then failwith "Нет корня в интервале"
    else loop a b 0


let iterations phi x0 epsilon maxIter =
    let rec loop x iter =
        let xNext = phi x
        if iter >= maxIter then xNext
        elif abs(xNext - x) < epsilon then xNext
        else loop xNext (iter + 1)
    loop x0 0

let newthon f f' x0 epsilon maxIter = 
    iterations (fun x -> x - f x / f' x) x0 epsilon maxIter

let f20 x = 0.1 * x**2.0 - x * Math.Log(x)
let f20' x = 0.2 * x - (Math.Log(x) + 1.0)
let phi20 x = Math.Sqrt(10.0 * x * Math.Log(x)) 

let f21 x = 
    let tanx = Math.Tan(x)
    tanx - (1.0/3.0)*tanx**3.0 + (1.0/5.0)*tanx**5.0 - 1.0/3.0
let f21' x = 
    let tanx = Math.Tan(x)
    1.0 + tanx**2.0 - tanx**4.0 + tanx**6.0 
let phi21 x = x - f21 x * 0.1 

let f22 x = Math.Acos(x) - Math.Sqrt(1.0 - 0.3 * x**3.0)
let f22' x = 
    -1.0 / Math.Sqrt(1.0 - x**2.0) - (0.9 * x**2.0)/(2.0 * Math.Sqrt(1.0 - 0.3 * x**3.0))
let phi22 x = Math.Cos(Math.Sqrt(1.0 - 0.3 * x**3.0))

let eps = 1e-6
let maxIter = 1000

let main = 
    printfn "var  Dichotomy  Iterations    Newthon"
    printfn "20 %10.5f  %10.5f  %10.5f" 
        (dichotomy f20 1.0 2.0 eps maxIter) 
        (iterations phi20 1.5 eps maxIter) 
        (newthon f20 f20' 1.5 eps maxIter)

    printfn "21 %10.5f  %10.5f  %10.5f" 
        (dichotomy f21 0.0 0.8 eps maxIter) 
        (iterations phi21 0.4 eps maxIter) 
        (newthon f21 f21' 0.4 eps maxIter)

    printfn "22 %10.5f  %10.5f  %10.5f" 
        (dichotomy f22 0.0 1.0 eps maxIter) 
        (iterations phi22 0.5 eps maxIter) 
        (newthon f22 f22' 0.5 eps maxIter)

main