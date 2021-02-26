#r "../packages/System.Windows.Forms/lib/System.Windows.Forms.dll"
#r "../packages/System.Windows.Forms.DataVisualization/lib/netcoreapp3.0/System.Windows.Forms.DataVisualization.dll"


let rec nest n f x =
        let f_x = f x
        if f_x=x || n=0 then x else nest (n-1) f f_x

let random (data: _ [] []) k =
        let rand = System.Random().NextDouble
        let gen i _ =
          let xs = data |> Seq.map (fun u -> u.[i])
          let x = rand()
          x * Seq.min xs + (1.0 - x) * Seq.max xs
        Array.init k (fun _ -> Array.mapi gen data.[0]);;


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

type species =
        | Setosa
        | Versicolor
        | Virginica

let irisData =
        use client = new System.Net.WebClient()
        let url = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
        let (|Float|_|) string = try Some(Float(float string)) with _ -> None
        let (|Species|_|) = function
          | "Iris-setosa" -> Some(Species Setosa)
          | "Iris-versicolor" -> Some(Species Versicolor)
          | "Iris-virginica" -> Some(Species Virginica)
          | _ -> None
        [ for line in (client.DownloadString url).Split[|'\n'|] do
            match line.Split[|','|] with
            | [|Float w; Float x; Float y; Float z; Species species|] -> yield [|w; x; y; z|], species
            | _ -> () ]

let euclidean =
        Array.fold2 (fun x u v -> x + pown (u - v) 2) 0.0

let clusters =
        let k = 3
        seq { while true do yield kmeans [|for u, _ in irisData -> u|] euclidean k }
        |> Seq.find (fun xs -> Seq.length xs = k)

clusters |> Seq.map (snd >> Seq.countBy (Map irisData |> fun m k -> m.[k]))
