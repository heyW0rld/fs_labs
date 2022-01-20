open System

let from who =
    sprintf "from %s" who


type Matrix = { values: float[,] }
with
    static member ofArray2D (values: float [,]) = 
        { values = values }

    static member O rows cols =
        let array2d = Array2D.zeroCreate rows cols
        { values = array2d }

    static member E rows cols =
        let array2d = Array2D.init rows cols (fun x y -> if x = y then 1. else 0.)
        { values = array2d }

    static member sizes matrix =
        let rows = matrix.values.[*,0].Length
        let cols = matrix.values.[0,*].Length
        (rows, cols)
    
    static member isEquallySized matrix1 matrix2 =
        let dim1 = Matrix.sizes matrix1
        let dim2 = Matrix.sizes matrix2
        (dim1 = dim2)
    
    static member (==) (matrix1, matrix2) =
        if not (Matrix.isEquallySized matrix1 matrix2) then false
        else
            not (matrix1.values
                   |> Array2D.mapi (fun x y v -> if matrix2.values.[x, y] <> v then false else true)
                   |> Seq.cast<bool>
                   |> Seq.contains false)

    static member isMatched matrix1 matrix2 = 
        let row1Count = matrix1.values.[0,*].Length
        let col2Count = matrix2.values.[*,0].Length
    
        row1Count = col2Count

    static member (*) (matrix1, (matrix2: Matrix)) =
        if Matrix.isMatched matrix1 matrix2 then
            let row1Count = matrix1.values.[*,0].Length
            let col2Count = matrix2.values.[0,*].Length
    
            let values = Array2D.zeroCreate row1Count col2Count
    
            for r in 0..row1Count-1 do
                for c in 0..col2Count-1 do
                    let row = Array.toList matrix1.values.[r,*]
                    let col = Array.toList matrix2.values.[*,c]
    
                    let cell = List.fold2 (fun acc val1 val2 -> acc + (val1 * val2)) 0. row col
                    values.[r,c] <- cell
    
            { values = values }
    
        else failwith "matrix1 is not matched to matrix2"

    static member transpose matrix =
        let dim = Matrix.sizes matrix
        let rows = fst dim
        let cols = snd dim
    
        let tMatrix = Matrix.O cols rows
        matrix.values |> Array2D.iteri(fun x y v -> tMatrix.values.[y, x] <- v)
    
        tMatrix

    static member pow matrix value =
    
        let inRecPow m p =
    
            let rec recPow acc p =
                match p with
                | x when x > 0 ->
                    let nextAcc = acc*m
                    
                    recPow nextAcc (x-1)
                | _ -> acc
           
            let dim = Matrix.sizes matrix
            let colCount = snd dim
            let rowCount = fst dim
    
            let u = Matrix.E rowCount colCount
            
            recPow u p
    
        let powMatrix = inRecPow matrix value
        { values = powMatrix.values }

    static member minor (A: Matrix) (row: int) (col: int) =
        
        let n = A.values.[*, 0].Length
        let nn = n-1
        let values = Array2D.zeroCreate nn nn


        let mutable di = 0
        let mutable dj = 0

        for ki in 0..nn-1 do
            dj <- 0
            if ki = row then di <- 1

            for kj in 0..nn-1 do
                if kj = col then dj <- 1
                values.[ki, kj] <- A.values.[ki+di, kj+dj]
        
        {values = values}


    static member det matrix =
        
        let rec detMat A : float = 

            let n = A.values.[*,0].Length
            let mutable k = 1.
            let mutable d = 0.
        
            if n = 1 then
                d <- A.values.[0, 0]
            elif n = 2 then
                d <- A.values.[0, 0] * A.values.[1, 1] - A.values.[1, 0] * A.values.[0, 1]
            else
                for i in 0..n-1 do
                    let M = Matrix.minor A 0 i
                    d <- d + k * A.values.[0, i] * detMat M
                    k <- k * -1.

            d           

        let res = detMat matrix

        res

end

let func1 (P: Matrix) (x: List<int>) (n: int) =
    
    let nP = Matrix.pow P n
    let index = List.findIndex (fun a -> a = 1) x

    Array.toList nP.values.[index,*]

let func2 (P: Matrix) =

    let n = P.values.[*, 0].Length

    let res = [| for i in 1 .. n -> 0. |]

    let TP = Matrix.transpose P
    for i in 0..n-1 do
        TP.values.[i,i] <- TP.values.[i,i] - 1.

    for i in 0..n-1 do
        TP.values.[n-1, i] <- 1.

    printfn $"TP: {TP}"

    let mainDet = Matrix.det TP
    printfn $"Main det {mainDet}"

    let fill (arr: float [,]) (col: int) =
        let vls = Array2D.zeroCreate n n

        for i in 0..n-1 do
            for j in 0..n-1 do
                if j = col then
                    vls.[i, j] <- 0.
                else
                    vls.[i, j] <- arr.[i, j]

        vls.[n-1, col] <- 1.

        vls

    for i in 0..n-1 do
        let M = Matrix.ofArray2D (fill TP.values i)
        printfn $"{M}"
        res.[i] <- Matrix.det M

    for i in 0..n-1 do
        res.[i] <- res.[i] / mainDet

    res

[<EntryPoint>]
let main argv =

    let a = array2D [[0.; 1.0; 0.]
                     [0.; 0.5; 0.5]
                     [0.333; 0.; 0.666]]
    let x = [1; 0; 0]
    let P = Matrix.ofArray2D a
    let n = 3

    printfn $"{Matrix.sizes P}"

    let res1 = func1 P x n
    
    printfn $"Res1: {res1}"


    let a = array2D [[0.; 1.; 0.]
                     [0.3; 0.1; 0.6]
                     [1.; 0.; 0.]]

    let A = Matrix.ofArray2D a

    let res2 = func2 A
    
    printf "Res2 "
    for i in 0 .. res2.Length - 1 do
        printf "%f " (Array.get res2 i)

    0
