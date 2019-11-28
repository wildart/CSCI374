namespace CSCI374

module LL1 =

    open Lexer

    // syntactic analyser
    let parser (grammar: (SYMBOL * SYMBOL list)[]) (table:Map<(RULE * TOKEN),int>) input =
        if grammar.Length = 0 then
            failwith "Grammmar is empty!"
        // Push a $ on the stack
        // Initialize the stack to the start symbol.
        let stack = List.empty<SYMBOL> |>
                    push (Terminal TOKEN.END) |>
                    push (fst grammar.[0])

        let rec analyse stack (token, input) =
            printfn "Token: %A, Stack: %A" token stack
            if not (List.isEmpty stack) then
                // take element from top of the stack
                let sym = top stack
                // get next token
                match sym with
                | Terminal term ->
                    if term = token then // input symbol matches terminal
                        // pop stack
                        let t, newstack = pop stack
                        printfn "pop %A" t
                        analyse newstack (tokenize input) // advance input
                    else
                        failwith (sprintf "bad term on input: %A" token)
                | NonTerminal nterm ->
                    begin
                        // Use nonterminal and current input symbol to find correct production in table.
                        printfn "svalue: %A, token: %A" nterm token
                        let ruleIdx =
                            try
                                table.[(nterm, token)]
                            with
                                | :? System.Collections.Generic.KeyNotFoundException -> failwith (sprintf "No rule found for %A → %A" nterm token)
                        let rule = grammar.[ruleIdx-1]
                        printfn "%d: %A → %A" ruleIdx (fst rule) (snd rule)

                        // Pop stack
                        // Push right-hand side of production from table onto stack, last symbol first.
                        let newstack = List.append (snd rule) (stack |> pop |> snd)
                        analyse newstack (token, input)
                    end
                | _ -> failwith "error"
            else ()

        analyse stack (tokenize input)
