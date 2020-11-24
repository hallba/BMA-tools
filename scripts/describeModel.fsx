#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
open System.Collections.Generic

type bmaModel = JsonProvider<"../data/models/BMAReconModel_v1_minF.json">

let model = bmaModel.Load("../data/models/BMAReconModel_v1_minF.json")

let nodes = Seq.map (fun (v:bmaModel.Variable) -> v.Id) model.Model.Variables |> Array.ofSeq
let names = 
    let d = new Dictionary<int,string>()
    Seq.iter (fun (v:bmaModel.Variable2) -> d.Add(v.Id,v.Name) ) model.Layout.Variables 
    d
let edges = Seq.map (fun (r:bmaModel.Relationship) -> (r.FromVariable, r.ToVariable) ) model.Model.Relationships |> Array.ofSeq

let incomingCount = Array.map (fun id -> Array.filter (fun (f,_) -> f=id) edges |> Array.length ) nodes
let outgoingCount = Array.map (fun id -> Array.filter (fun (_,t) -> t=id) edges |> Array.length ) nodes

let totalEdges = Array.map3 (fun i o n -> (n,i+o) ) incomingCount outgoingCount nodes
                 |> Array.sortByDescending (fun (_,e) -> e )



//printfn "%A" names

for i=0 to 5 do
    let n,e = totalEdges.[i]
    printfn "%-20s\t%d" names.[n] e

