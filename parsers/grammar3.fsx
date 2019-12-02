#load "lexer.fsx"
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

open CSCI374.Lexer

(*
Grammar:
    S -> S * F | S + F | F
    F -> a | b
*)


// Top-down parsing: LL(1)

(*
    S -> S * F | S + F | F
    F -> a | b

Remove left recursion:

    S  -> F S1
    S1 -> * F | Ɛ
    S  -> F S2
    S2 -> + F | Ɛ
    S  -> F
    F  -> a
    F  -> b

Use http://zaa.ch/jison/try/usf/ to construct parse table:

S : F S1
  | F S2
  | F ;

S1 : MULT F
   | EPS ;

S2 : PLUS F
   | EPS ;

F : a | b ;
*)

let grammar_ll = [|
    (NonTerminal S, [NonTerminal F; NonTerminal T]);
    (NonTerminal T, [Terminal EPS]);
    (NonTerminal T, [NonTerminal S1]);
    (NonTerminal T, [NonTerminal S2]);
    (NonTerminal S1, [Terminal MULT; NonTerminal F]);
    (NonTerminal S1, [Terminal EPS]);
    (NonTerminal S2, [Terminal PLUS; NonTerminal F]);
    (NonTerminal S2, [Terminal EPS]);
    (NonTerminal F, [Terminal A]);
    (NonTerminal F, [Terminal B])
|]

let table = [((S,A),1); ((S,B),1);  ((T,MULT),3); ((T,PLUS),4); ((T,EPS),2);  ((S1,EPS),6); ((S1,MULT),5);  ((S2,EPS),8); ((S2,PLUS),7);  ((F,A),9); ((F,B),10) ]

CSCI374.LL1.parse grammar_ll table "b+b";;
printfn "Done!!!\n"

(* Bottom-up parsing: LR(0)

Use http://zaa.ch/jison/try/usf/ to construct ACTION and GOTO tables

S : S MULT F
  | S PLUS F
  | F;

F : a | b ;

*)
let grammar = [|
    (NonTerminal S, [NonTerminal S; Terminal MULT; NonTerminal F]);
    (NonTerminal S, [NonTerminal S; Terminal PLUS; NonTerminal F]);
    (NonTerminal S, [NonTerminal F]);
    (NonTerminal F, [Terminal A]);
    (NonTerminal F, [Terminal B])
|]

let action = [|
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 0
    Map.ofList [(END, Accept); (MULT, Shift 5); (PLUS, Shift 6)]; // 1
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 3) }); // 2
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 4) }); // 3
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 5) }); // 4
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 5
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 6
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 1) }); // 7
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 2) }); // 8
|]

let goto = [|
    Map.ofList [(F, 2); (S, 1)]; // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.empty;              // 3
    Map.empty;              // 4
    Map.ofList [(F, 7)];    // 5
    Map.ofList [(F, 8)];    // 6
    Map.empty;              // 7
    Map.empty;              // 8
|]

CSCI374.LR0.parse grammar action goto "b+b";;
