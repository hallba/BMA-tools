module WriteBMA

open System.Collections.Generic
open System.IO

open Types

let getFormula (d:Dictionary<string,string>) lookup maxGran =
    let fExists,f = d.TryGetValue(lookup)
    if not fExists then string(maxGran) else f

let nodeToBmaVar formulaDictionary (n:QualSpecies) (g:Glyph) (vidMap:Dictionary<string,int>)  = 
    assert(n.qid=g.gid)
    let name = n.name
    {   name = name
        x = g.x
        y = g.y
        id = vidMap.[n.qid]
        formula = getFormula formulaDictionary n.qid n.max
        granularity = n.max+1
        description = ""
    }

let bmaVariableModel bmaVar =
    sprintf "{\"Name\":\"%s\",\"Id\":%d,\"RangeFrom\":0,\"RangeTo\":%d,\"Formula\":\"%s\"}" bmaVar.name bmaVar.id bmaVar.granularity bmaVar.formula 

let bmaVariableLayout angle (bmaVar: BmaVariable) =
    let x' = bmaVar.x*cos(angle) - bmaVar.y*sin(angle)
    let y' = bmaVar.x*sin(angle) + bmaVar.y*cos(angle)
    sprintf "{\"Id\":%d,\"Name\":\"%s\",\"Type\":\"Constant\",\"ContainerId\":0,\"PositionX\":%f,\"PositionY\":%f,\"CellX\":0,\"CellY\":0,\"Angle\":0,\"Description\":\"%s\"}" bmaVar.id bmaVar.name x' y' bmaVar.description

let bmaRelationshipText bmaRel =
    sprintf "{\"Id\":%d,\"FromVariable\":%d,\"ToVariable\":%d,\"Type\":\"%s\"}" bmaRel.id bmaRel.source bmaRel.target bmaRel.kind

let edgeToBmaRel (e:relationship) (nameToID:Dictionary<string,int>) i =
    let source = e.Source
    let target = e.Target
    let kind =  match e.EdgeType with
                | Activator -> "Activator"
                | Inhibitor -> "Inhibitor"
    {
        kind = kind
        id = i
        source = nameToID.[source]
        target = nameToID.[target]
    }

let writeBMAJson outputFileName qualModel =
    let vidMap = new Dictionary<string,int>()
    let nameMap = new Dictionary<string,string>()
    Array.iteri (fun i v -> vidMap.Add(v.qid,i)) qualModel.variables
    Array.iter (fun v -> nameMap.Add(v.qid,v.name)) qualModel.variables
    
    let nodeCount = Array.length qualModel.variables

    let formulaMap = new Dictionary<string,string>()
    Array.iter (fun (f:QualFormula) ->
        let f' = TFGeneration.calculateTargetFunction vidMap nameMap f qualModel.variables f.name
        formulaMap.Add(f.name,f') ) qualModel.formulae


    //find the interactions first, to build up the description dictionary, then build the variables
    
    let bmaVariables = Array.map2 (fun q g -> nodeToBmaVar formulaMap q g vidMap) qualModel.variables qualModel.glyphs
    let varModel = Array.map bmaVariableModel bmaVariables |> fun x -> System.String.Join(",",x) 
    let varLayout = Array.map (bmaVariableLayout 0.) bmaVariables |> fun x -> System.String.Join(",",x) 
    let bmaRelationships = Array.mapi (fun i r -> edgeToBmaRel r vidMap (i+nodeCount)) qualModel.relationships |> Array.map bmaRelationshipText |> fun x -> System.String.Join(",",x)

    
    
    
    
    let result = sprintf "{\"Model\": {\"Name\": \"BMAExchange model\",\"Variables\":[%s],\"Relationships\":[%s]},\"Layout\":{\"Variables\":[%s],\"Containers\":[]}}\n" varModel bmaRelationships varLayout

    File.WriteAllText(outputFileName, result)
