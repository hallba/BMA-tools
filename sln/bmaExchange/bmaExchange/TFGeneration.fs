module TFGeneration

open System.Collections.Generic

open Microsoft.Z3

open Types

let varStateToBool  (qsVars:string []) (state:int []) (value:QualVariableState) =
    let location = Array.findIndex (fun (qsv) -> qsv=value.qfid) qsVars
    let currentState = state.[location] 
    match value.operator with 
    | Equal -> currentState = value.value 
    | GreaterThan -> currentState > value.value 
    | GreaterThanOrEqual -> currentState >= value.value 
    | LessThan -> currentState < value.value 
    | LessThanOrEqual -> currentState <= value.value 
    | NotEqual -> currentState <> value.value 

let varStateToBoolExpr (ctx:Context) (qsVars:string []) (state:int []) (value:QualVariableState) =
    let result = varStateToBool  qsVars state value
    if result then ctx.MkFalse() else ctx.MkTrue()

let termToExpr (ctx:Context) (term:LogicalFunction) qsVars state =
    let evaluateValue = varStateToBoolExpr ctx qsVars state
    let rec inner f cont =
        match f with
        | Value(v) -> cont(evaluateValue v)
        | Not(f')       -> inner f' (fun acc ->
                                        cont(ctx.MkNot(acc)))
        | And(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(ctx.MkAnd(left,right))) )
        | Or(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(ctx.MkOr(left,right))) )
        | Implies(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(ctx.MkImplies(left,right))) )
        | Xor(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(ctx.MkXor(left,right))) )
    inner term id

let termToBool (term:LogicalFunction) qsVars state =
    let evaluateValue = varStateToBool qsVars state
    let rec inner f cont =
        match f with
        | Value(v) -> cont(evaluateValue v)
        | Not(f')       -> inner f' (fun acc ->
                                        cont(not acc))
        | And(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(left && right)) )
        | Or(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(left || right)) )
        | Implies(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont(left || not right)) )
        | Xor(f',f'')   -> inner f' (fun left ->
                                        inner f'' (fun right ->
                                                        cont((left || right) && not (left && right))) )
    inner term id

let nv (n:int) (radix:int) =
    if radix>10 then failwith "maximum radix 10"
    let rec core n radix acc = 
        if n = 0 then acc else core (n/radix) radix ( (n%radix)::acc)
    core n radix []

let nvPad max n radix =
    let bvM = nv max radix
    let bvN = nv n radix
    let rec pad current length =
        if List.length current <> length then pad (0::current) length else current
    pad bvN (List.length bvM) |> Array.ofList

let generateTruthTable depth granularity = 
    let lineCount = pown granularity depth
    Array.init lineCount (fun i -> nvPad (lineCount-1) i granularity)

let makeUpdateName (names: string []) specificName (inputState: int []) =
    Array.fold2 (fun acc name state -> acc+"/"+name+"="+string(state) ) (sprintf "UF-%s-" specificName) names inputState

let makeUpdate (ctx:Context) (names: string []) specificName (inputState: int [])= 
    let title = makeUpdateName names specificName inputState
    ctx.MkIntConst(title)

let functionPrint (ctx:Context) (s:Solver) names specificVariable granularity =
    printf "Update function %s\n" specificVariable
    for n in names do
        printf "%s\t" n
    printf "| %s'\n" specificVariable
    printf "-------------------------------\n"
    
    let tt = generateTruthTable <| Array.length names <| granularity
    
    for line in tt do
        for bool in line do
            printf "%d\t" bool
        printf "| %O\n" (s.Model.Eval(makeUpdate ctx names specificVariable line))

let getFormulaValues (formulae:(LogicalFunction * int) []) =
    //get all names of variables in values from the formulae
    let justFormula = Array.map (fun f -> fst(f)) formulae |> List.ofArray
    let rec readAndAddValueNames f acc =
        //recurse the formula until you find a value, adding just the name to the acc
        //Not tail recursive
        match f with
        | Value(v) -> v.qfid::acc
        | Not(f') -> readAndAddValueNames f' acc
        | And(f',f'') -> readAndAddValueNames f' acc
                         |> readAndAddValueNames f''
        | Or(f',f'') -> readAndAddValueNames f' acc
                        |> readAndAddValueNames f''
        | Xor(f',f'') -> readAndAddValueNames f' acc
                         |> readAndAddValueNames f''
        | Implies(f',f'') -> readAndAddValueNames f' acc
                             |> readAndAddValueNames f''                
    let rec inner f acc =
        match f with
        | [] -> acc
        | topFormula::rest ->
            let acc' = readAndAddValueNames topFormula acc
            inner rest acc'
    inner justFormula []

let calculateTargetFunction (vidMap:Dictionary<string,int>) (nMap:Dictionary<string,string>) (qf:QualFormula) (qsVars:QualSpecies []) specificVariable =
    //Do I need to know the inputs for each? Can I implicitly use an unmentioned variable?
    //Assuming not for now
    let ctx = new Context()
    //Inputs is going to be the qual id
    let inputs = getFormulaValues qf.formula |> List.distinct |> Array.ofList
    //If there are no inputs, set the formula to the default state
    if Array.length inputs = 0 then sprintf "%d" qf.defaultState else
        let inputNames = Array.map (fun i -> nMap.[i]) inputs
        //Can't cope with mixed granularities atm
        let inputMaxGranularity = Array.map (fun i -> 
                                    Array.find (fun (qs:QualSpecies) -> qs.qid=i) qsVars
                                    |> fun qs -> qs.max+1 ) inputs
                                    |> Array.max
        let possibleInputs = generateTruthTable (Array.length inputs) inputMaxGranularity
    
        let inputToConstraint inputVals =
            let zvar = makeUpdate ctx inputs specificVariable inputVals
            let rec makeConstraints form acc =
                match (form:(LogicalFunction*int) list) with
                | [] -> acc
                | head::tail -> 
                    let zform = termToExpr ctx (fst(head)) inputs inputVals 
                    let acc' = ctx.MkITE(zform,ctx.MkEq(zvar,ctx.MkInt(snd(head))),acc) :?> BoolExpr
                    makeConstraints tail acc'
            let condition = makeConstraints ( List.ofArray qf.formula) (ctx.MkEq(zvar,ctx.MkInt(qf.defaultState)))
            condition

        let values = Array.map inputToConstraint possibleInputs
    
        let s = ctx.MkSolver()
        s.Add(ctx.MkAnd(values))

        match s.Check() with
        | Status.SATISFIABLE -> 
            let getTFString (input: int []) (names:string []) output =
                let terms = Array.map2 (fun name valueInt -> sprintf "max(0,min(var(%d)+%d,1)*max(%d-var(%d),0))" vidMap.[name] (1-valueInt) (valueInt+1) vidMap.[name]) names input
                (*let mutable result = "0"
                for i in 0..((Array.length input)-1) do
                    let name = names.[i]
                    let valueInt = input.[i]
                    let current = sprintf "max(0,min(var(%s)+%d,1)*max(%d-var(%s),0))" name (1-valueInt) (valueInt+1) name
                    result <- current + "*" + string(output) + "+" + result*)
                let result = Array.fold (fun acc t -> t + "*" + acc ) (string(output)) terms
                //printfn "tfc:%s" result
                result
            //sprintf "max(0,min(var(%s)+%d,1)*max(%d-var(%s),0))" name (1-valueInt) (valueInt+1) name
            let functionComponents = Array.map (fun v -> (s.Model.Eval(makeUpdate ctx inputs specificVariable v).ToString() |> int),v) possibleInputs
                                     |> Array.filter (fun v -> fst(v) <> 0)
                                     |> Array.map (fun (output,input) -> (getTFString input inputNames output) )
                                     |> Array.fold (fun acc line -> acc + "+" + line ) "0"
            functionComponents

        | Status.UNSATISFIABLE -> failwith "terms contradict"
        | _ -> failwith "Unknown solver result"