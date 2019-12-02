namespace CSCI374

module LR0 =

    open Lexer

    // syntactic analyser
    let parser (grammar: (SYMBOL * SYMBOL list)[]) (action: Map<TOKEN,ACTION>[]) (goto: Map<RULE,int>[]) input verbose =
        if grammar.Length = 0 then
            failwith "Grammmar is empty!"
        // Push a 0 on the stack
        let stack = [0]

        let rec analyse stack (token, input) =
            if verbose then (printfn "Input: %A, Token: %A, Stack: %A" input token stack) else ()
            if not (List.isEmpty stack) then
                // current state is taken from top of stack
                let st = top stack
                let act = action.[st].[token]
                if verbose then (printfn "State: %A, Action: %A" st act)
                // get next token
                match act with
                // shift and go to a new state
                | Shift state -> analyse (push state stack) (tokenize input) // advance input
                // reduce by rule: X ::= A1...An
                | Reduce rule ->
                    begin
                        let (NonTerminal lhs, rhs) = grammar.[rule-1]
                        if verbose then
                            printfn "Reduce Rule %A: %A → %A" rule lhs rhs
                        else
                            let srhs = String.concat "" [for x in rhs do yield x.str]
                            printfn "%d: %s → %s" rule (lhs.str) srhs
                        // restore state before reduction from top of stack
                        let newstack = List.skip (List.length rhs) stack
                        if verbose then (printfn "Stack top: %A" (top newstack))
                        let state = goto.[(top newstack)].[lhs] // state after reduction
                        analyse (push state newstack) (token, input)
                    end
                | Accept -> printfn "Accepted!!!"
            else ()

        analyse stack (tokenize input)

    let parse grammar action goto input = parser grammar action goto input false
