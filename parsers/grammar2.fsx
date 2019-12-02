#load "lexer.fsx"
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

open CSCI374.Lexer

(*
Grammar:
    F → F + ( F )
    F → a

Use http://zaa.ch/jison/try/usf/ to construct parsetable

    F : F PLUS LPAR F RPAR;
    F : a ;
*)
let grammar = [|
    (NonTerminal F, [NonTerminal F; Terminal PLUS; Terminal LPAR; NonTerminal F; Terminal RPAR]);
    (NonTerminal F, [Terminal A])
|]

// Top-down parsing: LL(1)

let table = [((F,A),1); ((F,A),2)]

CSCI374.LL1.parse grammar table "a+(a+(a))";;
printfn "Done!!!\n"

// Bottom-up parsing: LR(0)

let action = [|
    Map.ofList [(A, Shift 2)]; // 0
    Map.ofList [(PLUS, Shift 3); (END, Accept)]; // 1
    Map.ofSeq (seq {for t in [A; PLUS; LPAR; RPAR; END] -> (t, Reduce 2) }); // 2
    Map.ofList [(LPAR, Shift 4)]; // 3
    Map.ofList [(A, Shift 2)]; // 4
    Map.ofList [(PLUS, Shift 3); (RPAR, Shift 6)]; // 5
    Map.ofSeq (seq {for t in [A; PLUS; LPAR; RPAR; END] -> (t, Reduce 1) }); // 6
|]

let goto = [|
    Map.ofList [(F, 1)];    // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.empty;              // 3
    Map.ofList [(F, 5)];    // 4
    Map.empty;              // 5
    Map.empty;              // 6
|]

CSCI374.LR0.parse grammar action goto "a+(a+(a))";;
