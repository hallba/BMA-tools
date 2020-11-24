#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
open System.Collections.Generic

type cex =  JsonProvider<"../data/output/cex_fp_example.json">
type bmaModel = JsonProvider<"../data/models/BMAReconModel_v1_minF.json">
//type sbmlModel = XmlProvider<"ReconMap-2.01-SBML3-Layout-Render.xml">

type metabolite = { name: string ; location: string} 

let resultFile = "../data/output/proof_output_cex"
let modelFile = "../data/models/BMAReconModel.json"
//let originalRecon = "ReconMap-2.01-SBML3-Layout-Render.xml"

let result = cex.Load(resultFile)
let model = bmaModel.Load(modelFile)
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
                "ppi";"na1";"o2s";"amp";"utp";"dadp";"dutp";"dgtp";"dadp";
                "dcdp";"dgdp";"dudp";"dtdp";"datp";"ctp";"dctp";
                "h2o2"|]

let nameParse (n: string) = 
    n.Split('_') |> fun (x: string []) -> {name=x.[1];location= x.[2]}

let processID (s:string) =
    s.Split('^')
    |> fun (x: string []) -> x.[0] 
    |> int
    |> fun x -> variableIDLookup.[x]

let instability = Seq.filter (fun (v:cex.Variable)  -> v.Fix1 <> v.Fix2 ) result.Variables

//instability |> Seq.iter (fun (v:cex.Variable) ->  printfn "%s" (processID v.Id) )
//instability |> Seq.length  |> printfn "%d variables" 

let idIsNotCommon (v:cex.Variable) =
    processID v.Id 
    |> nameParse 
    |> fun x -> Array.contains x.name common
    |> not 

let reduction = Seq.filter idIsNotCommon instability

reduction |> Seq.iter (fun (v:cex.Variable) ->  printfn "%s" (processID v.Id) )
reduction |> Seq.length  |> printfn "%d variables" 

let reductionID = Seq.map (fun (i:cex.Variable) -> i.Id |> int ) reduction


let reductionGraph _ = 
    printfn "m"
    let m = Seq.filter (fun (v:bmaModel.Variable) -> Seq.exists ((=) v.Id) reductionID ) model.Model.Variables |> Array.ofSeq
    printfn "l"
    let l = Seq.filter (fun (v:bmaModel.Variable2) -> Seq.exists ((=) v.Id) reductionID ) model.Layout.Variables |> Array.ofSeq
    printfn "r"
    let r = Seq.filter (fun (v:bmaModel.Relationship) -> Seq.exists ((=) v.ToVariable) reductionID && Seq.exists ((=) v.FromVariable) reductionID ) model.Model.Relationships |> Array.ofSeq
    let revisedModel = bmaModel.Model(m,r)
    let revisedLayout = bmaModel.Layout(l,Array.empty)
    
    bmaModel.Root
        (
            name = "ReconProblem",
            model = revisedModel,
            layout = revisedLayout
        )