#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#r "../packages/System.Xml.Linq/lib/net20/System.Xml.Linq.dll"
#r "../packages/Microsoft.Z3.x64/lib/netstandard1.4/Microsoft.Z3.dll"
#r "../packages/XPlot.Plotly/lib/netstandard2.0/XPlot.Plotly.dll"
#r "../packages/Accord/lib/netstandard2.0/Accord.dll"
#r "../packages/Accord.Math/lib/netstandard2.0/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/netstandard2.0/Accord.Statistics.dll"
#r "../packages/Accord.MachineLearning/lib/netstandard2.0/Accord.MachineLearning.dll"

open FSharp.Data
open System.Collections.Generic
open System.IO
open Microsoft.Z3
open XPlot.Plotly
open Accord.Statistics
open Accord.MachineLearning

type bmaModel = JsonProvider<"../data/models/BMAReconModel_v1_minF.json">
type svgImage = XmlProvider<"../data/images/circles.svg">
//BMAReconModel251120
let model = bmaModel.Load("../data/models/MetSmall.json")

let digraphCluster nodes edges (number:int) =
        let ctx = new Context()
        let nodeCluster = Array.map (fun (nodeName:string) -> ctx.MkIntConst(nodeName)) nodes
        let nodesGeZero = ctx.MkAnd(Array.map (fun n -> ctx.MkGe(n,ctx.MkInt(0))) nodeCluster)
        let nodesLtMax = ctx.MkAnd(Array.map (fun n -> ctx.MkLt(n,ctx.MkInt(number))) nodeCluster)
        let mutable minimumClusters = ctx.MkTrue()
        for i=0 to (number-1) do
                let nZ = ctx.MkInt(i)
                minimumClusters <- ctx.MkAnd(minimumClusters,(ctx.MkOr(Array.map (fun n -> ctx.MkEq(nZ,n)) nodeCluster)))

        let edgeDic = new Dictionary<string,_>()
        let edgeName source target = sprintf "%s>%s" source target
        Array.iter (fun (s,t) -> edgeDic.Add((edgeName s t),()) ) edges
        let setEdge source target =
                let edgeId = edgeName source target
                let e = ctx.MkBoolConst(edgeId)
                if edgeDic.ContainsKey(edgeId) then ctx.MkEq(e,ctx.MkTrue()) else ctx.MkEq(e,ctx.MkFalse())
        let mutable zEdges = ctx.MkTrue()// ctx.MkAnd( Array.map (fun source -> ctx.MkAnd(Array.map (fun target -> setEdge source target) nodes ) ) nodes )

        let cuts = ctx.MkIntConst("^Cuts")
        let mutable counter = ctx.MkInt(0) :> ArithExpr

        let maxIndex =  (Array.length nodes) - 1
        for i=0 to maxIndex do
                for j=0 to maxIndex do
                        let source = nodes.[i]
                        let target = nodes.[j]
                        zEdges <- ctx.MkAnd(setEdge source target, zEdges)
                        let ncS = nodeCluster.[i]
                        let ncT = nodeCluster.[j]
                        let differentClusters = ctx.MkNot(ctx.MkEq(ncS,ncT))
                        let edgeId = edgeName source target
                        let connected = ctx.MkBoolConst(edgeId)
                        let cut = ctx.MkITE(ctx.MkAnd(connected,differentClusters),ctx.MkInt(1),ctx.MkInt(0)) :?> ArithExpr
                        let total = [|cut;counter|]
                        counter <- ctx.MkAdd(total)
        let s = ctx.MkOptimize()
        s.Add(minimumClusters)
        s.Add(zEdges)
        s.Add(ctx.MkEq(cuts,counter))
        s.MkMinimize(cuts) |> ignore
        s.Add(nodesLtMax)
        s.Add(nodesGeZero)
        let getC (m : Model) (node:string) =
                sprintf "%O" (s.Model.Eval(ctx.MkIntConst(node)))
        match s.Check() with
        | Status.SATISFIABLE -> 
                printf "got clusters"
                let m = s.Model
                let result = Array.map (fun n -> (n,(getC m n) ) ) nodes
                             |> Array.sortBy snd
                result
        | _ -> failwith "something went wrong"

let processDataAndZCluster clusterNumber =
        let nodes = Array.map (fun (v:bmaModel.Variable2) -> string v.Id ) model.Layout.Variables
        let edges = Array.map (fun (r:bmaModel.Relationship) -> (string r.FromVariable, string r.ToVariable)) model.Model.Relationships
        let result = digraphCluster nodes edges clusterNumber
        result


let rec nest n f x =
        let f_x = f x
        if f_x=x || n=0 then x else nest (n-1) f f_x

let random (data: _ [] []) k =
        let rand = System.Random().NextDouble
        let gen i _ =
          let xs = data |> Seq.map (fun u -> u.[i])
          let x = rand()
          x * Seq.min xs + (1.0 - x) * Seq.max xs
        Array.init k (fun _ -> Array.mapi gen data.[0])


let kmeans (data: _ [] []) distance k =
        let nearestCentroid centroids u = Array.minBy (distance u) centroids
        let iteration (centroids: float [] []) =
          [|for _, us in Seq.groupBy (nearestCentroid centroids) data do
              let n = Seq.length us
              if n <> 0 then
                yield Array.mapi (fun i _ -> us |> Seq.averageBy (fun u -> u.[i])) data.[0]|]
        random data k
        |> nest 100 iteration
        |> fun centroids -> Seq.groupBy (nearestCentroid centroids) data 

let data = Array.map (fun (v:bmaModel.Variable2) -> [|float v.PositionX;float v.PositionY|] ) model.Layout.Variables

let euclidean (a: float []) (b: float []) =
        let mutable result = 0.
        for i in 0..((-) (Array.length a) 1) do
            result <- result + pown (a.[i] - b.[i]) 2
        //Array.fold2 (fun x u v -> x + pown (u - v) 2) 0.0
        result

let clusters k =
        seq { while true do yield kmeans data euclidean k }
        |> Seq.find (fun xs -> Seq.length xs = k) 

//let views = Array.init 3 (fun i ->  clusters (pown 10 (i+1)) )

fsi.ShowDeclarationValues <- false


let image = svgImage.Load("../data/images/circles.svg")
image.ViewBox

let genSym =
        let x = ref 0
        fun _ ->
                let result = !x
                x := !x + 1
                result

type circleStyle =
        {
                colour : string
        }
        member this.ToStyle() = sprintf "fill:#%s;fill-opacity:1;stroke:none;stroke-width:1;stroke-linecap:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" this.colour

let circle (style:circleStyle) (p,d) =
        let id = genSym () |> string
        let dd = decimal d
        let pd = Array.map decimal p
        svgImage.Circle(style=style.ToStyle(), id=id, cx=pd.[0], cy=pd.[1], r=dd)


let dataPlot name size =
        let dataX = Array.map (fun (x: _ []) -> x.[0]) data
        let dataY = Array.map (fun (x: _ []) -> x.[1]) data

        let minX = Array.min dataX |> int
        let minY = Array.min dataY |> int
        let maxX = Array.max dataX |> int
        let maxY = Array.max dataY |> int
        let midX = (maxX - minX) / 2
        let midY = (maxY - minY) / 2
        
        let norm (c: _ []) = 
                [|c.[0];c.[1]|]

        let cSize = Seq.map (fun d -> (norm d,size)) data |> Array.ofSeq

        let width = (sprintf "%d" (maxX-minX))
        let height = (sprintf "%d" (maxY-minY))
        let view = sprintf "%d %d %d %d" minX minY minX maxY

        
        let newCircles = Array.map (circle {colour="000000"}) cSize 
        let newG = svgImage.G("Layer 1","layer","layer1",newCircles)
        
        let newSVG = svgImage.Svg(width, height, view, image.Version, image.Id, image.Version2, image.Docname, image.Defs, image.Namedview, image.Metadata, newG)
        let result = newSVG.XElement.ToString()
        
        let filename = sprintf "./tmp/data-%s.svg" name

        File.WriteAllText (filename, result)

let cPlot cl name size =
        let dataX = Array.map (fun (x: _ []) -> x.[0]) data
        let dataY = Array.map (fun (x: _ []) -> x.[1]) data

        let minX = Array.min dataX |> int
        let minY = Array.min dataY |> int
        let maxX = Array.max dataX |> int
        let maxY = Array.max dataY |> int
        let midX = (maxX - minX) / 2
        let midY = (maxY - minY) / 2
        
        let norm (c: _ []) = 
                [|c.[0];c.[1]|]

        let cSize = Seq.map (fun (c,d) -> (norm c,size*(Seq.length d))) cl |> Array.ofSeq

        let width = (sprintf "%d" (maxX-minX))
        let height = (sprintf "%d" (maxY-minY))
        let view = sprintf "%d %d %d %d" minX minY minX maxY

        
        let newCircles = Array.map (circle {colour="00c9ff"}) cSize 
        let newG = svgImage.G("Layer 1","layer","layer1",newCircles)
        
        let newSVG = svgImage.Svg(width, height, view, image.Version, image.Id, image.Version2, image.Docname, image.Defs, image.Namedview, image.Metadata, newG)
        let result = newSVG.XElement.ToString()
        
        let filename = sprintf "./tmp/clusters-%s.svg" name

        File.WriteAllText (filename, result)

let kdePlot _ =
    let kernel = new Distributions.DensityKernels.GaussianKernel(2)
    let dist = new Distributions.Multivariate.MultivariateEmpiricalDistribution(kernel, data)
    let granularity = 100
    let fGran = float(granularity)
    let translate minN maxN n =
        minN + (float(n)/fGran)*(maxN-minN)
    let dataX = Array.map (fun (x: _ []) -> x.[0]) data
    let dataY = Array.map (fun (x: _ []) -> x.[1]) data

    let minX = Array.min dataX 
    let minY = Array.min dataY 
    let maxX = Array.max dataX 
    let maxY = Array.max dataY

    let width = int <| (maxX-minX)
    let height = int <| (maxY-minY)

    let translateX = translate minX maxX
    let translateY = translate minY maxY
    let hm = Array.init granularity (fun x -> 
                Array.init granularity (fun y -> 
                    dist.ProbabilityDensityFunction([| translateX x ; translateY y|]) ) )
    Heatmap(
        z = hm,
        x = (List.init granularity translateX ),
        y = (List.init granularity translateY ) 
    )
    |> Chart.Plot
    |> Chart.WithWidth width
    |> Chart.WithHeight height
    |> Chart.Show

let c10 = clusters 10
let c25 = clusters 25
//let c100 = clusters 100 //44.5 - 9.0 (mutable euclidrsn)
cPlot c10 "full10" 10
cPlot c25 "full25" 25

dataPlot "met" 20