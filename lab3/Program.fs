open System.Collections.Generic

open System

[<EntryPoint>]
let main argv = 
    let graph = Graphs.graph1 |> List.unzip |> snd
    let startV = 1
    let endV = 832

    let rec dfs visited path (v: int) = 
        let neighbor = graph.[v]
                       |> List.filter (fun x -> (not (List.contains (x) visited)) && ((x) <> v)) 
        
        if v = endV then 
            v::path |> List.rev
        elif List.isEmpty neighbor then
            if List.tryHead path |> Option.isNone then
                List.empty
            else    
                dfs (v::visited) (List.tail path) (List.head path)
        else
            dfs (v::visited) (v::path) (List.head neighbor)
    
    printfn "Path from %d to %d: %A" startV endV (dfs List.empty List.empty startV)

    let graph = Graphs.graph2 |> List.unzip |> snd
    
    let rec components visited acc =
        let rec bfs visited queue (v: int) = 
            let neighbor = graph.[v]
                           |> List.filter (fun x -> (not (List.contains (x) visited)) && ((x) <> v)) 
            
            let new_queue = queue @ neighbor
            if List.isEmpty neighbor && List.isEmpty queue then 
                v::visited
            else
                bfs (v::visited) (new_queue.Tail) (List.head new_queue)
        
        let not_visited = List.filter (fun x -> not (List.contains x visited)) [0..(List.length graph)-1]
        if List.isEmpty not_visited then
            acc
        else
            components (visited @ (bfs visited List.empty not_visited.Head)) (acc+1)
    
    printfn "Number of components: %A" (components List.empty 0)

    let graph = System.IO.File.ReadAllLines("edges2.csv")
                    |> Array.toList
                    |> List.map (fun s -> Array.map int (s.Split([|','|], StringSplitOptions.RemoveEmptyEntries)) |> Array.toList)
    
    let rec bfs (visited: Set<int>) (to_visit) (queue) =
            if Set.isEmpty to_visit then 
                visited, queue
            else
                let v = to_visit.MinimumElement
                let neighbor = graph.[v-1]
                           // |> List.filter (fun x -> (not (List.contains (x) visited) && not (List.contains (x) queue))) 
                printfn "Вершин посещено: %d" (Set.count visited)
                bfs (visited.Add(v)) (to_visit.Remove(v)) (Set.union queue (Set.ofList neighbor))

    let rec calculate (visited2) (queue2) (stage: int) (acc: int) = 
        printfn "Вершин посещено: %d" (Set.count visited2)
        printfn "Вершин нужно посетить на след. шаге: %d" (Set.count queue2)
        printfn "stage: %d" stage
        printfn "acc: %d" acc

        let visited3, queue3 = (bfs visited2 queue2 Set.empty)
        if Set.isEmpty queue3 then
            acc / (Set.count visited3)
        else
            calculate visited3 queue3 (stage+1) acc+stage*(Set.count queue3)
    
    let rec avg_num_handshakes (v: int) (sum: int) =
        if v = 11316812 then
            sum / 11316811
        else
            avg_num_handshakes (v+1) (calculate Set.empty (Set.empty.Add(v)) 0 0)
        
    printfn "Average number of handshake: %A" (avg_num_handshakes 1 0)
 
    0
