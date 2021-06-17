open System


let qModel = ref "input.xml"
let bModel = ref "output.json"

let rec parse_args args = 
    match args with 
    | [] -> ()
    | "-i" :: m :: rest -> qModel := m; parse_args rest  
    | "-o" :: m :: rest -> bModel := m; parse_args rest
    | _ -> failwith "Bad command line args" 

[<EntryPoint>]
let main argv =
    printfn "BMA Exchange"

    List.ofArray argv |> parse_args

    printfn "Reading filename %s" !qModel
    let qRead = ReadQual.readQualFile !qModel

    WriteBMA.writeBMAJson !bModel qRead

    0 // return an integer exit code