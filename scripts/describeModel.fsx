#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
open System.Collections.Generic

type bmaModel = JsonProvider<"../data/models/BMAReconModel_v1_minF.json">

let model = bmaModel.Load("../data/models/BMAReconModel251120.json")

let nodes = Seq.map (fun (v:bmaModel.Variable) -> v.Id) model.Model.Variables |> Array.ofSeq
let names = 
    let d = new Dictionary<int,string>()
    Seq.iter (fun (v:bmaModel.Variable2) -> d.Add(v.Id,v.Name) ) model.Layout.Variables 
    d
let edges = Seq.map (fun (r:bmaModel.Relationship) -> (r.FromVariable, r.ToVariable) ) model.Model.Relationships |> Array.ofSeq

let incomingCount = Array.map (fun id -> Array.filter (fun (f,_) -> f=id) edges |> Array.length ) nodes
let outgoingCount = Array.map (fun id -> Array.filter (fun (_,t) -> t=id) edges |> Array.length ) nodes

let totalEdges = Array.map3 (fun i o n -> (n,i,o,i+o) ) incomingCount outgoingCount nodes
                 |> Array.sortByDescending (fun (_,e,_,_) -> e )


//Duplicate names
let nameArr = Array.map (fun n -> names.[n]) nodes
let dupes = nameArr 
            |> Array.groupBy id
            |> Array.map ( fun (key, set) -> 
                if Array.length set > 1
                then Some (key, Array.length set)
                else None )
printfn "Duplicate genes (%d)" <| (Array.length <| Array.filter ((<>) None) dupes)
Array.iter (fun n -> match n with 
                     | Some(k,n) -> printfn "%s %d" k n
                     | None -> () ) dupes


//Edge ranks
printfn "Top connections (sorted by incoming edges)"  
for i=0 to 10 do
    let n,t,s,e = totalEdges.[i]
    printfn "%-20s\t%d\t(%d\t%d)" names.[n] e t s

//Self loops
let selfLoops = Array.filter (fun (i,o) -> i=o) edges 
printfn "Self-loop variables (total = %d)" (Array.length selfLoops)
Array.iter (fun (v,_) -> printfn "%s" names.[v] ) selfLoops

//Reversible reactions
let reverse = Array.filter (fun (s,t) -> Array.contains (t,s) edges ) edges
              |> Array.filter (fun (s,t) -> s < t) //removes duplications
printfn "Reversible reactions (total = %d)" (Array.length reverse)
Array.iter (fun (s,t) -> printfn "%s <=> %s" names.[s] names.[t]) reverse

