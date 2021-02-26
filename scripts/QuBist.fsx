#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open FSharp.Data
open System.Collections.Generic

type SbmlQual = XmlProvider<"../data/models/SBML/Test_system3.sbmlqual.xml">
type BMAModel = JsonProvider<"../data/models/SBML/test.json">

let model = SbmlQual.Load("../data/models/SBML/Test_system3.sbmlqual.xml")

type Glyph = { X: float; Y: float; SbmlName: string}

type Variable = { Granularity: int; SbmlName: string; VarName: string}

let genSym =
    let x = ref 0
    fun _ -> 
        let result = !x
        x := !x + 1
        result
    


let relationshipOut  source target sign =
    let id = genSym ()
    let bmaSign = if sign = "negative" then "Inhibitor" else "Activator"
    let result  = BMAModel.Relationship(target,bmaSign,source,id)
    result





let main _ =
    //Create dictionaries of variable names and SBML ids and names and unique identifiers
    let idLookup = new Dictionary<string,int>()
    let qualNameLookup = new Dictionary<string,string>()

    Array.iter (fun (species:SbmlQual.QualitativeSpecies) -> printfn "%s %s" species.Id species.Name ; qualNameLookup.Add(species.Id,species.Name)) model.Model.ListOfQualitativeSpecies.QualitativeSpecies

    let glyphUnpack (v:SbmlQual.GeneralGlyph) =
        let id = genSym ()
        let result = BMAModel.Variable2(id,qualNameLookup.[v.Reference],"Constant",0,v.BoundingBox.Position.X,v.BoundingBox.Position.Y,0,0,0,Some(""))
        result 

    let variableCollect qualGlyph =
        let layoutVariable = glyphUnpack qualGlyph
        idLookup.Add(qualGlyph.Reference, layoutVariable.Id)
        printfn "%s %d" qualGlyph.Reference layoutVariable.Id
        layoutVariable

    let layoutVariables =
        model.Model.ListOfLayouts.Layout.ListOfAdditionalGraphicalObjects.GeneralGlyphs
        |> Array.map variableCollect

    let transitionUnpack (t:SbmlQual.Transition) = 
        let id = genSym ()
        //Is this always one for qual? or just qual from casq
        let target = t.ListOfOutputs.Output 
        let targetId = idLookup.[target.QualitativeSpecies]
        let r = Array.map (fun (source:SbmlQual.Input) -> relationshipOut idLookup.[source.QualitativeSpecies] targetId source.Sign ) t.ListOfInputs.Inputs 
        r
    let modelRelationships = 
        model.Model.ListOfTransitions.Transitions 
        |> Array.collect transitionUnpack
    
    let qualvarUnpack (v:SbmlQual.QualitativeSpecies) =
        let id = idLookup.[v.Id]
        let formula = ""
        let result = BMAModel.Variable(v.Name,id,0,v.MaxLevel,Some(formula))
        result

    let modelVariables = 
        model.Model.ListOfQualitativeSpecies.QualitativeSpecies
        |> Array.map qualvarUnpack

    let m = BMAModel.Model("name",modelVariables,modelRelationships)
    let l = BMAModel.Layout(layoutVariables, [||], "")
    let ltl = BMAModel.Ltl([||],[||])
    
    let complete = BMAModel.Root(m,l,ltl)
    
    complete