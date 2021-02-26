#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
open System.Collections.Generic
open System.Windows.Forms

type cex =  JsonProvider<"../data/output/cex_fp_example.json">
type proof = JsonProvider<"../tmp/proof_output">
type bmaModel = JsonProvider<"../data/models/BMAReconModel_v1_minF.json">
//type sbmlModel = XmlProvider<"ReconMap-2.01-SBML3-Layout-Render.xml">

type metabolite = { name: string ; location: string} 

let cexFile = "../tmp/proof_output_cex"
let proofFile = "../tmp/proof_output251120"
let modelFile = "../data/models/BMAReconModel251120.json"
//let originalRecon = "ReconMap-2.01-SBML3-Layout-Render.xml"

let cexResult = cex.Load(cexFile)
let model = bmaModel.Load(modelFile)
//let proofResult = proof.Load(proofFile)
//let recon = sbmlModel.Parse(originalRecon)

let variableToKV (v:bmaModel.Variable2 ) =
    (int(v.Id),v.Name)

(*let speciesToKV (s:sbmlModel.Species ) =
    //(s.id,s.name)
    s*)

let variableIDLookup =
    let d = new Dictionary<int,string>()
    Seq.iter (fun v -> d.Add(variableToKV v) ) model.Layout.Variables
    d
(*
let reconNameLookup =
    let d = new Dictionary<string,string>()
    Seq.iter (fun s -> d.Add(("a","b")) recon.
    d 
    *)

//small molecules that are abundant
let common = [|"atp";"h2o";"adp";"gdp";"gtp";"o2";"nadph";"nadp";"h";
                "co2";"nad";"nadh";"nh4";"hco3";"pi";"fad";"fadh2";
                "ppi";"na1";"o2s";"amp";"damp";"utp";"dadp";"dutp";"dgtp";"dadp";
                "dcdp";"dgdp";"dudp";"dtdp";"datp";"ctp";"dctp";
                "h2o2"|]

let nameParse (n: string) = 
    n.Split('_') |> fun (x: string []) -> {name=x.[1];location= x.[2]}

let processID (s:string) =
    s.Split('^')
    |> fun (x: string []) -> x.[0] 
    |> int
    |> fun x -> variableIDLookup.[x]

let instability = Seq.filter (fun (v:cex.Variable)  -> v.Fix1 <> v.Fix2 ) cexResult.Variables

let unstableSpace = proof.Load(proofFile)
                    |> fun proofResult -> Seq.take 1 proofResult.Ticks 
                    |> Array.ofSeq  |> fun x -> x.[0] 
                    |> fun (x:proof.Tick) -> Array.filter (fun (v:proof.Variable) -> v.Lo <> v.Hi  ) x.Variables
                    |> Array.map (fun (v:proof.Variable) -> string v.Id)
                    |> Seq.ofArray

//instability |> Seq.iter (fun (v:cex.Variable) ->  printfn "%s" (processID v.Id) )
//instability |> Seq.length  |> printfn "%d variables" 

let cexID (v:cex.Variable) =
    v.Id

let idIsNotCommon i =
    processID i 
    |> nameParse 
    |> fun x -> Array.contains x.name common
    |> not 

let reduction = Seq.filter idIsNotCommon unstableSpace// <| Seq.map cexID instability

reduction |> Seq.iter (fun v ->  printfn "%s" (processID v) )
reduction |> Seq.length  |> printfn "%d variables" 

let variableIDfromTimedVariable (a: string) =
    a.Split('^').[0] |> int

let reductionID = Seq.map (fun (v) -> variableIDfromTimedVariable v) reduction


let reductionGraph _ = 
    let m = Seq.filter (fun (v:bmaModel.Variable) -> Seq.exists ((=) v.Id) reductionID ) model.Model.Variables |> Array.ofSeq
    let l = Seq.filter (fun (v:bmaModel.Variable2) -> Seq.exists ((=) v.Id) reductionID ) model.Layout.Variables |> Array.ofSeq
    let r = Seq.filter (fun (v:bmaModel.Relationship) -> Seq.exists ((=) v.ToVariable) reductionID && Seq.exists ((=) v.FromVariable) reductionID ) model.Model.Relationships |> Array.ofSeq
    let revisedModel = bmaModel.Model(m,r)
    let revisedLayout = bmaModel.Layout(l,Array.empty)
    
    let resultFile = "../tmp/isolatedIssue.json"

    let model = 
        bmaModel.Root
            (
                name = "ReconProblem",
                model = revisedModel,
                layout = revisedLayout
            )
    let result = model.JsonValue.ToString()
    printfn "%s" result
    Clipboard.SetText(result)
    result