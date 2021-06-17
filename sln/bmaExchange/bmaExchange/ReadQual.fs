module ReadQual

open Types
open libsbmlcs

let getSpecies (p:QualModelPlugin) s =
    let qs = p.getQualitativeSpecies (int64(s))
    //Some models dont include a separate name, and just use the id
    let name =
        if qs.getName() = "" then qs.getId() else qs.getName()

    {qid=qs.getId();name=name;max=qs.getMaxLevel()}

let getGlyphs (l:Layout) s =
    let ls = l.getAdditionalGraphicalObject (int64(s))
    let b = ls.getBoundingBox()
    let p = b.getPosition()
    //Ugly hack- I cannot access the "reference" attribute which contains the QS id
    let glyphName = ls.getId()
    let x = ls.toXML()
    let mutable refNum = 0
    let mutable unfound = true
    while unfound do
        let name = x.getAttrName refNum
        if name = "reference" then
            unfound <- false
        else
            refNum <- 1+refNum
    
    let name = x.getAttrValue refNum
    //let name = (glyphName.Split [|'_'|]).[0]
    {gid=name;x=p.x();y=p.y()}

let inputOutputToRelationship (outputs:Output []) (input:Input) = 
    let t = if input.getSign () = 1 then Inhibitor else Activator 
    let makeEdge (output:Output) =
        let target = output.getQualitativeSpecies()
        {Source=input.getQualitativeSpecies();EdgeType=t;Target=target} 
    Array.map makeEdge outputs

let getTransitions (q:QualModelPlugin) s =
    let t = q.getTransition (int64(s))
    //How can there be more than one output for a logical "transition"? For petri nets...
    let targets = Array.init (t.getNumOutputs () |> int) (fun i -> t.getOutput(int64(i)))
    let relationships = Array.init (t.getNumInputs () |> int) (fun i -> t.getInput(int64(i)) |> inputOutputToRelationship targets)
                        |> Array.concat
    relationships

let getFormula (q:QualModelPlugin) s =
    let t = q.getTransition (int64(s))
    //Assert that there is only one output. 
    //Not sure how else it would be in a pure-logical model
    assert (=) (t.getNumOutputs ()) 1L
    let vid = t.getOutput(0L) |> fun (x:Output) -> x.getQualitativeSpecies()
    let nTerms = t.getNumFunctionTerms() |> int
    let defValue = t.getDefaultTerm () |> fun (x:DefaultTerm) -> x.getResultLevel ()
    //
    let rec readFormula (mathTerm:ASTNode) cont =
        match mathTerm.getName() with
        | "or" ->   readFormula (mathTerm.getLeftChild()) (fun left ->
                        readFormula (mathTerm.getRightChild()) (fun right ->
                            cont(Or(left,right)) ) )
        | "and"->   readFormula (mathTerm.getLeftChild()) (fun left ->
                        readFormula (mathTerm.getRightChild()) (fun right ->
                            cont(And(left,right)) ) )
        | "not"->   readFormula (mathTerm.getLeftChild()) (fun child -> 
                        cont(Not(child)))
        | "implies" ->readFormula (mathTerm.getLeftChild()) (fun left ->
                        readFormula (mathTerm.getRightChild()) (fun right ->
                            cont(Implies(left,right)) ) )
        | "xor"->   readFormula (mathTerm.getLeftChild()) (fun left ->
                        readFormula (mathTerm.getRightChild()) (fun right ->
                            cont(Xor(left,right)) ) )
        | "eq" ->   let name = mathTerm.getLeftChild() |> fun x -> x.getName()
                    let value = mathTerm.getRightChild() |> fun x -> x.getInteger()
                    cont(Value({qfid=name;value=value;operator=Equal}))
        | x -> failwithf "[Error] Operator %s unrecognised" x

    let terms = Array.init nTerms (fun i -> readFormula (t.getFunctionTerm(int64(i)).getMath()) id)
    let outputValues = Array.init nTerms (fun i -> t.getFunctionTerm(int64(i)).getResultLevel())
    //let humanMath = libsbmlcs.libsbml.formulaToString(aloneMath)
    let formulae = Array.map2 (fun f v -> (f,v) ) terms outputValues
    let result:QualFormula = {name=vid;formula=formulae;defaultState=defValue}
    result

let readQualFile inputFile =
    let reader = new libsbmlcs.SBMLReader()

    let sbmlDoc = reader.readSBML(inputFile)

    if sbmlDoc.setPackageRequired("qual", true) <> 0 then failwith "Not a qual file"
    if sbmlDoc.setPackageRequired("layout", true) <> 0 then failwith "No layout information"

    let m = sbmlDoc.getModel ()

    let qual:QualModelPlugin = m.getPlugin("qual") :?> QualModelPlugin
    let layout:LayoutModelPlugin = m.getPlugin("layout") :?> LayoutModelPlugin
    let nV = qual.getNumQualitativeSpecies () |> int

    let nC = m.getNumCompartments ()
    
    let comp = m.getCompartment (nC-1L)
    
    //Assume one layout    
    let layoutOne = layout.getLayout 0L
    
    let nG = layoutOne.getNumAdditionalGraphicalObjects () |> int
    
    let nT = qual.getNumTransitions () |> int
    
    let relationships = Array.init nT (getTransitions qual) |> Array.concat
    let formulae = Array.init nT (fun i -> getFormula qual i)
    let glyphs = Array.init nG (getGlyphs layoutOne)
    let variables = Array.init nV (getSpecies qual)

    {relationships=relationships;formulae=formulae;glyphs=glyphs;variables=variables}
