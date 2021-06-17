module Types

type QualSpecies = {
    qid: string
    name: string
    max: int
}

type Glyph = {
    gid: string
    x: float
    y: float
}

type edgeType = Activator | Inhibitor

type relationship = 
    {
        Source: string
        Target: string
        EdgeType: edgeType
    }

type algebraicComparison = Equal | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual | NotEqual

type QualVariableState = {
    qfid: string
    value: int
    operator: algebraicComparison
}

//How many of these operators if included in the file would work in ginsim etc?
type LogicalFunction = 
    And of LogicalFunction * LogicalFunction 
    | Or of LogicalFunction * LogicalFunction
    | Not of LogicalFunction
    | Implies of LogicalFunction * LogicalFunction
    | Xor of LogicalFunction * LogicalFunction
    | Value of QualVariableState

type QualFormula = {
    name: string
    formula: (LogicalFunction * int) []
    defaultState: int
}

type Qualmodel = {
    glyphs: Glyph []
    formulae: QualFormula []
    relationships: relationship []
    variables: QualSpecies []
}

type BmaVariable = {
    id: int
    name: string
    x: float
    y: float
    formula: string
    granularity: int
    description: string
}
type BmaRelationship = {
        id: int
        source: int
        target: int
        kind: string
    }