#load "lexer.fsx"
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

open CSCI374.Lexer

(*
Grammar:
    S -> F
    S -> ( S + F )
    F -> a

Use http://zaa.ch/jison/try/usf/ to construct parsetable

    S : F;
    S : LPAR S PLUS F RPAR;
    F : a ;
*)
let grammar = [|
    (NonTerminal S, [NonTerminal F]);
    (NonTerminal S, [Terminal LPAR; NonTerminal S; Terminal PLUS; NonTerminal F; Terminal RPAR]);
    (NonTerminal F, [Terminal A])
|]

// Top-down parsing: LL(1)

// LL(1) parse table
let table = [((S,A),1); ((S,LPAR),2); ((F,A),3)]

CSCI374.LL1.parse grammar table "(a+a)";;
printfn "Done!!!\n"

// Bottom-up parsing: LR(0)

let action = [|
    Map.ofList [(LPAR, Shift 3); (A, Shift 4)]; // 0
    Map.ofList [(END, Accept)]; // 1
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> (t, Reduce 1) }); // 2
    Map.ofList [(LPAR, Shift 3); (A, Shift 4)]; // 3
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> (t, Reduce 3) }); // 4
    Map.ofList [(PLUS, Shift 6)]; // 5
    Map.ofList [(A, Shift 4)];    // 6
    Map.ofList [(RPAR, Shift 8)]; // 7
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> (t, Reduce 2) }); // 8
|]

let goto = [|
    Map.ofList [(F, 2); (S, 1)]; // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.ofList [(F, 2); (S, 5)]; // 3
    Map.empty;              // 4
    Map.empty;              // 5
    Map.ofList [(F, 7)];    // 6
    Map.empty;              // 7
    Map.empty;              // 8
|]

CSCI374.LR0.parse grammar action goto "(a+a)";;