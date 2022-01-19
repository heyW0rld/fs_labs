type 'a heap =
    | EmptyHeap
    | HeapNode of int * 'a * 'a heap * 'a heap

 
module Heap =
    let height = function
        | EmptyHeap -> 0
        | HeapNode(h, _, _, _) -> h
    
    let makeT (x, a, b) =
        if height a >= height b then HeapNode(height b + 1, x, a, b)
        else HeapNode(height a + 1, x, b, a)
 
    let rec merge comparer = function
        | x, EmptyHeap -> x
        | EmptyHeap, x -> x
        | (HeapNode(_, x, l1, r1) as h1), (HeapNode(_, y, l2, r2) as h2) ->
            if comparer x y <= 0 then makeT(x, l1, merge comparer (r1, h2))
            else makeT (y, l2, merge comparer (h1, r2))
 
    let hd = function
        | EmptyHeap -> failwith "empty"
        | HeapNode(h, x, l, r) -> x
 
    let tl comparer = function
        | EmptyHeap -> failwith "empty"
        | HeapNode(h, x, l, r) -> merge comparer (l, r)
        
    let rec to_seq comparer = function
        | EmptyHeap -> Seq.empty
        | HeapNode(h, x, l, r) as node -> seq { yield x; yield! to_seq comparer (tl comparer node) }
 
type 'a BinaryHeap(comparer : 'a -> 'a -> int, inner : 'a heap) =
    member this.inner = inner
 
    member this.hd = Heap.hd inner
    member this.tl = BinaryHeap(comparer, Heap.tl comparer inner)
    member this.merge (other : BinaryHeap<_>) = BinaryHeap(comparer, Heap.merge comparer (inner, other.inner))
    member this.insert x = BinaryHeap(comparer, Heap.merge comparer (inner,(HeapNode(1, x, EmptyHeap, EmptyHeap))))
    
    interface System.Collections.Generic.IEnumerable<'a> with
        member this.GetEnumerator() = (Heap.to_seq comparer inner).GetEnumerator()
            
    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = (Heap.to_seq comparer inner :> System.Collections.IEnumerable).GetEnumerator()
 
    static member make(comparer) = BinaryHeap<_>(comparer, EmptyHeap)



let comp a b = b - a

let bh1 = BinaryHeap<int>(comp, HeapNode(1, 314, EmptyHeap, EmptyHeap))
let bh2 = bh1.insert 15
let bh3 = bh2.insert 92

printf "Before remove max: "
for i in bh3 do printf $"{i} "

let bh4 = bh3.tl

printf "\nAfter remove max: "
for i in bh4 do printf $"{i} "