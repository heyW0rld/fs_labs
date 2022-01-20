let calculateE =
    let eps = 0.001

    let rec calculate (f: float, i: float, n: float) =
        match n with
        | var1 when var1 < eps -> var1
        | _ -> calculate(f * i, i + 1., 1. / f) + if i > 1. then n else 0.

    let rec calculate (f: float, i: float, n: float) =
        match (n < eps) with
        | true ->n
        | false when (i > 1) -> n + calculate(f * i, i + 1., 1./f)
        | false -> calculate(f * i, i + 1., 1. / f)   

    calculate(1., 1., 1.)

let dichotomy a b f =
    let eps = 0.001

    let rec get a b f =
        let c = (a + b) * 0.5

        if abs (f c) < eps then
            c
        elif f c * f a < 0. then
            get a c f
        else
            get c b f

    let eps = 0.001

    if f a * f b > 0. then
        printfn "f(a) * f(b) > 0"
    else
        let res = get a b f
        printfn "root: %f" res

let rec quickSort arr =
  match arr with
  | [] -> []
  | x :: xs ->
      let smaller, larger = List.partition (fun e -> e <= x) xs
      quickSort smaller @ [x] @ quickSort larger

let arr = [2; 34; 56; 12; 1; 5; -15; 450; 788 ]

printfn "e: %f" calculateE
(dichotomy -100. 1. (fun x -> x ** 3. - x + 15.))
printfn "%A" (quickSort arr)