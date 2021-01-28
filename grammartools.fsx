// -*- coding: utf-8 -*-
namespace CSCI374

module Reflection =

    open Microsoft.FSharp.Reflection

    let toString (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

    let getUnionTypes<'T> () =
        let cases = FSharpType.GetUnionCases(typeof<'T>)
        Array.map (fun (uc:UnionCaseInfo) -> uc.Name) cases

    let getTypeByName<'T> tname =
        let cases = FSharpType.GetUnionCases(typeof<'T>)
        let typ = Array.find (fun (uc:UnionCaseInfo) -> uc.Name = tname) cases
        FSharpValue.MakeUnion(typ, [| |]) :?> 'T

module ParserTypes =

    let (|IntLit|_|) (str:string) =
       match System.Int32.TryParse str with
       | (true,i) -> Some(i)
       | _ -> None

    type TOKEN = LPAR | RPAR | PLUS | MINUS | MULT | DIV | A | B | C | D | E | F | INVALID | END | EPS | INT of int with
        override this.ToString() =
            match this with
            | LPAR -> "("
            | RPAR -> ")"
            | PLUS -> "+"
            | MINUS -> "-"
            | MULT -> "*"
            | DIV -> "/"
            | END -> "$"
            | EPS -> "ε"
            | INT n -> string n
            | _ -> (Reflection.toString this).ToLower()
        static member FromChar (c:char) =
            let strtkn =
                match c with
                | '(' -> "LPAR"
                | ')' -> "RPAR"
                | '+' -> "PLUS"
                | '-' -> "MINUS"
                | '*' -> "MULT"
                | '/' -> "DIV"
                | '$' -> "END"
                | c when int c = 949 -> "EPS"
                | _ -> string c
            TOKEN.FromString strtkn
        static member FromString s =
            match s with
            | IntLit num -> INT num
            | _  -> match Reflection.fromString<TOKEN> (s.ToUpper()) with
                    | Some(t) -> t
                    | _ -> INVALID

    type RULE = S | T | U | V | W | X | Y | Z with
        override this.ToString() = Reflection.toString this
        static member FromString s = Reflection.fromString<RULE> s

    type SYMBOL = Terminal of TOKEN | NonTerminal of RULE | Error with
        override this.ToString() =
            match this with
            | Terminal t -> t.ToString()
            | NonTerminal n -> n.ToString()
            | Error -> "ERROR"

    type SYMBOLS = SYMBOL list
    type PRODUCTION = SYMBOL * SYMBOLS

module GrammarTools =

    open ParserTypes

    type ACTION =
        | Shift of int
        | Reduce of int
        | Accept

    let rng = System.Random()
    let (=>) s ss = (s,ss)

    let charsToString (cs:char list) =
        List.fold (fun a c -> a + string c) "" cs

    let isNumber (c:char) :bool = c >= '0' && c <= '9'

    let isBinOp = function
        | '+' | '-' | '*' | '/'  -> true
        | _ -> false

    let SymbolsToStrDlm dlm (xs:'a list) :string =
        List.fold (fun a (s:'a) -> a + (s.ToString())+dlm) "" xs

    let SymbolsToStr (xs:'a list) :string =
        SymbolsToStrDlm "" xs

    let RuleToStr ((lhs,rhs):'a *'b list) :string =
        sprintf "%s → %s" (lhs.ToString()) (SymbolsToStr rhs)

    let strGrammarRule verbose (grammar:PRODUCTION []) ruleIdx =
        let lhs, rhs = grammar.[ruleIdx-1]
        if verbose then
            sprintf "%d: %A → %A" ruleIdx lhs rhs
        else
            sprintf "%d: %s → %s" ruleIdx (lhs.ToString()) (SymbolsToStr rhs)

    let printGrammarRule verbose (grammar:PRODUCTION []) ruleIdx =
        (strGrammarRule verbose grammar ruleIdx) |> printfn "%s"

    let strGrammar (grammar:PRODUCTION []) =
        seq {
            for i in seq {1 .. (Array.length grammar)} ->
            strGrammarRule false grammar i
        } |> Seq.fold (fun a v -> a + v + "\n") ""

    let strGrammarCompact (grammar:PRODUCTION []) =
        seq {
            for nt in grammar |> Array.map fst |> Array.distinct ->
                let rhs = grammar
                        |> Array.filter (fun p -> (fst p) = nt)
                        |> Array.map (snd >> SymbolsToStr)
                        |> Array.fold (fun a s -> a + s + " | ") ""
                        |> fun s -> s.TrimEnd([|'|'; ' '|])
                sprintf "%s → %s" (nt.ToString()) rhs
        } |> Seq.fold (fun a v -> a + v + "\n") ""

    let printGrammar (grammar:PRODUCTION []) =
        printfn "Grammar:\n%s" (strGrammar grammar)

    let printGrammarCompact (grammar:PRODUCTION []) =
        printfn "Grammar:\n%s" (strGrammarCompact grammar)

    let findProduction grammar sym =
        Array.filter (fun (lhs,rhs) -> lhs = sym) grammar

    let gensentence (grammar:PRODUCTION []) sym debug maxdepth =
        let rnd = System.Random()
        let rec expand sym depth :(string * int) =
            if debug then printfn "D:%d, SYM: %A" depth sym else ()
            let prods = findProduction grammar sym
            let prodIdx =
                if prods.Length = 1 then 0
                else System.Random().Next() % prods.Length
            processRHS (snd prods.[prodIdx]) (depth+1)
        and processRHS rhs depth :(string * int) =
            if debug then printfn "D:%d, RHS: %A" depth rhs else ()
            if depth = maxdepth then failwithf "Reached maximum depth: %d" maxdepth else ()
            match rhs with
            | [] -> ("", depth)
            | h::xs ->
                let cstf =
                    match h with
                    | Terminal s -> (s.ToString(), depth)
                    | NonTerminal s -> expand h depth
                    | _ -> failwith "ERROR"
                let estf = processRHS xs depth
                let reached = max (snd cstf) (snd estf)
                ((fst cstf) + (fst estf), reached)
        expand sym 0

    let grammarSentence (grammar:PRODUCTION []) =
        gensentence grammar (fst grammar.[0]) false 100

    let grammarSentences grammar mindepth maxdepth =
        let mutable c = 0
        seq {
            while true do
                c <- c+1
                let sentence, depth =
                    try
                        gensentence grammar (fst grammar.[0]) false maxdepth
                    with
                        | Failure a -> "", 9
                if sentence.Length > 0 && mindepth <= depth then
                    yield sentence
                    c <- 0
                else ()
                if c = 1000 then failwith "Cannot generate sentences with specified parameters"
        }

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rng.Next(i, Array.length a))) a
        a

    let sample n xs =
        xs
        |> Array.map (fun x -> rng.Next(),x)
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.take n

    // Returns number of nonterminals and terminals in the rule
    let getRuleSize rsize =
        let curRuleSize = rng.Next(1,rsize+1)
        let ntermCount = (float curRuleSize) *rng.NextDouble() |> floor |> int
        let termCount = curRuleSize - ntermCount
        ntermCount, termCount

    // Checks if collection of rules has nonterminals
    let hasNonterminals rules =
        let ntCount = List.fold (fun a e -> a + (fst e)) 0 rules
        ntCount > 0

    let genRules rhsRules ruleSize =
        [for i in 1 .. rng.Next(1,rhsRules+1) -> getRuleSize ruleSize]

    let genProdParams (nterms:string []) (terms:string []) rhsRules ruleSize =
        seq {
            for (i,nt) in Array.zip [|0 .. nterms.Length-1|] nterms do
                let mutable generate = true
                while generate do
                    let rhs = genRules rhsRules ruleSize
                    // first production should have nonterminals
                    if hasNonterminals rhs || i <> 0 then
                        generate <- false
                        let rhsSym = [
                            for (ntc, tc) in rhs ->
                                let ntsym =
                                    (Array.filter (fun s -> s <> nt) nterms |> sample ntc)
                                    |> Array.map (Reflection.getTypeByName<RULE> >>  NonTerminal)
                                let tsym =
                                    (terms |> sample tc)
                                    |> Array.map (Reflection.getTypeByName<TOKEN> >> Terminal)
                                Array.append ntsym tsym |> shuffle |> Array.toList
                        ]
                        yield ((Reflection.getTypeByName<RULE> >>  NonTerminal) nt, rhsSym)
        }

    let genGrammar prodNumber termsNumber rhsRules ruleSize specialSym = [|
        let nterms = Reflection.getUnionTypes<RULE> () |> sample prodNumber
        let tkns = Reflection.getUnionTypes<TOKEN> ()
        let terms =
            if specialSym then tkns else (Array.skip 6 tkns)
            |> Array.takeWhile (fun t -> t <> "INVALID")
            |> sample termsNumber
        let rules = genProdParams nterms terms rhsRules ruleSize
        for (nt,prods) in rules do
            for p in prods -> (nt,p)
        |]

    let makeLeftRecursive grammar =
        let i = rng.Next(0, Array.length grammar)
        let lhs, rhs = grammar.[i]
        let newrhs =
            match rhs with
            | (NonTerminal s)::xs -> lhs::xs
            | xs -> lhs::xs
        grammar.[i] <- lhs, newrhs
        grammar

    let parseGrammarString (sgrammar:string) =
        let trim (s:string) = s.Trim([|' '|])
        let split (c:char) (s:string) = s.Split(c)
        let astuple (arr:'a []) = arr.[0], arr.[1]
        let charToRule (c:char) = (string c) |> Reflection.getTypeByName<RULE> |> NonTerminal
        let strToToken (s:string) = s.ToUpper () |>  Reflection.getTypeByName<TOKEN> |> Terminal
        let str chs = Seq.fold (fun str x -> str + x.ToString()) "" chs
        let srules = sgrammar |> split '\n' |> Array.filter (fun s -> s.Length <> 0)
        [|
            for srule in srules do
                let slhs, srhs = srule |> split '→' |> Array.map trim |> astuple
                let lhs = Reflection.getTypeByName<RULE> slhs |> NonTerminal
                yield! [
                    for sprod in (srhs |> Seq.filter (fun c -> c <> ' ') |> str |> split '|' |> Array.map trim) -> lhs, [
                        for c in sprod ->
                            match c with
                            | '(' -> strToToken "LPAR"
                            | ')' -> strToToken "RPAR"
                            | '+' -> strToToken "PLUS"
                            | '-' -> strToToken "MINUS"
                            | '*' -> strToToken "MULT"
                            | '/' -> strToToken "DIV"
                            | '$' -> strToToken "END"
                            | c when int c = 949 -> strToToken "EPS"
                            | c when (System.Char.IsUpper c) -> charToRule c
                            | _ -> strToToken (string c)
                    ]
                ]
        |]

module FSMLexer =

    open ParserTypes
    open GrammarTools

    let rec tokenize (input:char list) =
        match input with
        | c::str when isNumber c -> (number [c] str)
        | c::str when isBinOp c -> (operation c str)
        | [] -> [END]
        | _  -> [INVALID]

    and number (nums:char list) (input:char list) =
        let makeIntLit cs = cs |> List.rev |> charsToString |> int |> INT
        match input with
        | [] -> (makeIntLit nums)::[END]
        | ' '::str -> (makeIntLit nums)::(space str)
        | c::str when isNumber c -> (number (c::nums) str)
        | _  -> [INVALID]

    and space (input:char list) =
        match input with
        | c::str when isNumber c -> (number [c] str)
        | c::str when isBinOp c -> (operation c str)
        | [] -> [END]
        | _  -> [INVALID]

    and operation (c:char) input =
        match input with
        | []  -> (TOKEN.FromChar c)::[END]
        | ' '::str -> (TOKEN.FromChar c)::(space str)
        | _  -> [INVALID]

module Lexer =

    open ParserTypes
    open GrammarTools

    let rec makeInt (cs:char list) =
        cs |> List.rev |> charsToString |> int |> INT

    // read input until number is tokenized
    let rec parseInt (cs:char list) input =
        match input with
        | [] -> (makeInt cs), []
        | c::t when isNumber c -> parseInt (c::cs) t
        | _  -> (makeInt cs), input

    // lexical analyser
    let rec token (input:char list) =
        match input with
        | c::t when isNumber c -> parseInt [c] t
        | ' '::t -> token t
        | c::t -> TOKEN.FromChar c, t
        | []   -> END, []
        | _   -> INVALID, []

    let rec tokenize input =
        match token input with
        | END, [] -> [END]
        | t, cs -> t::(tokenize cs)
