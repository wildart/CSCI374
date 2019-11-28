namespace CSCI374

module Lexer =

    type TOKEN = LPAR | RPAR | PLUS | MINUS | MULT | DIV | A | B | C | INVALID | END | EPS with
         member this.str =
             match this with
             | LPAR -> "("
             | RPAR -> ")"
             | PLUS -> "+"
             | MINUS -> "-"
             | MULT -> "*"
             | DIV -> "/"
             | A -> "a"
             | B -> "b"
             | C -> "c"
             | END -> "$"
             | EPS -> "ε"
             | _ ->""

    type RULE = S | F | T | S1 | S2 with
         member this.str =
             match this with
             | S -> "S"
             | F -> "F"
             | T -> "T"
             | S1 -> "S1"
             | S2 -> "S2"

    type SYMBOL =
        | Terminal of TOKEN
        | NonTerminal of RULE
        | Error with
        member this.str =
            match this with
            | Terminal t -> t.str
            | NonTerminal n -> n.str
            | Error -> "ERROR"


    type ACTION =
        | Shift of int
        | Reduce of int
        | Accept

    // lexical analyser
    let tokenize input =
        if Seq.isEmpty input then
            TOKEN.END, Seq.empty<char>
        else
            let c = Seq.head input
            let s = Seq.tail input
            let t = match c with
                    | '(' -> TOKEN.LPAR
                    | ')' -> TOKEN.RPAR
                    | '+' -> TOKEN.PLUS
                    | '-' -> TOKEN.MINUS
                    | '*' -> TOKEN.MULT
                    | '/' -> TOKEN.DIV
                    | 'a' -> TOKEN.A
                    | 'b' -> TOKEN.B
                    | 'c' -> TOKEN.C
                    | _   -> TOKEN.INVALID
            t, s

    // stack
    let push sym stk = sym::stk
    let top stk = List.head stk
    let pop stk =
        match stk with
        | top::tl -> top, tl
        | _ -> SYMBOL.Error, List.empty<SYMBOL>
