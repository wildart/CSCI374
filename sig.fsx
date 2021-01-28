namespace CSCI374

module ExtraReflection =

    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System.Text.RegularExpressions

    ///Regex extensions
    module Regex =
        type ActiveMatch =
            {
                Match: Match
                MatchValue: string
                Groups: Group list
                OptionalGroups: (Group option) list
                GroupValues: string list
                OptionalGroupValues: (string option) list
            }

        ///<summary>
        ///Test an input string against a regex pattern using the given RegexOptions flags.
        ///If the match succeeds, returns an ActiveMatch instance, which can be used for further pattern matching.
        ///Note that the implementation takes advantage of the .NET Regex cache.
        ///</summary>
        ///<param name="flags">
        ///The first argument allows you pass in RegexOptions flags.
        ///</param>
        ///<param name="pattern">
        ///The second argument is the regex pattern. Cannot be null.
        ///</param>
        ///<param name="input">
        ///The last argument is the input string to test. The input
        ///may be null which would result in a no-match.
        ///</param>
        let (|Match|_|) flags pattern input =
            match input with
            | null -> None //Regex.Match will throw with null input, we return None instead
            | _ ->
                //using the static Regex.Match takes advantage of Regex caching
                match Regex.Match(input, pattern, flags) with
                | m when m.Success ->
                    //n.b. the head value of m.Groups is the match itself, which we discard
                    //n.b. if a group is optional and doesn't match, it's Value is ""
                    let groups = [for x in m.Groups -> x].Tail
                    let optionalGroups = groups |> List.map (fun x -> if x.Success then Some(x) else None)
                    let groupValues = groups |> List.map (fun x -> x.Value)
                    let optionalGroupValues = optionalGroups |> List.map (function None -> None | Some(x) -> Some(x.Value))

                    Some({ Match=m
                           MatchValue=m.Value
                           Groups=groups
                           OptionalGroups=optionalGroups
                           GroupValues=groupValues
                           OptionalGroupValues=optionalGroupValues })
                | _ -> None

        ///Convenience versions of our regex active patterns using RegexOptions.Compiled flag.
        module Compiled =
            ///When silverlight mode is None, else is Compiled
            let private compiledRegexOption =
                RegexOptions.Compiled

            let (|Match|_|) = (|Match|_|) compiledRegexOption

        ///Convenience versions of our regex active patterns using RegexOptions.None flag
        module Interpreted =
            let (|Match|_|) = (|Match|_|) RegexOptions.None

    let inline private applyParensForPrecInContext context prec s = if prec > context then s else sprintf "(%s)" s

    let staticOrInstanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly

    let sprintSig (outerTy:Type) =
        //list of F# type abbrs: http://207.46.16.248/en-us/library/ee353649.aspx
        ///Get the type abbr name or short name from the "clean" name
        let displayName = function
            | "System.Object"   -> "obj"
            | "System.String"   -> "string"
            | "System.Char"     -> "char"
            | "System.Boolean"  -> "bool"
            | "System.Decimal"  -> "decimal"

            | "System.Int16"    -> "int16"
            | "System.Int32"    -> "int"//int32
            | "System.Int64"    -> "int64"

            | "System.UInt16"   -> "uint16"
            | "System.UInt32"   -> "uint32"
            | "System.UInt64"   -> "uint64"

            | "System.Single"   -> "float32"//single
            | "System.Double"   -> "float"//double

            | "System.Byte"     -> "byte"//uint8
            | "System.SByte"    -> "sbyte"//int8

            | "System.IntPtr"   -> "nativeint"
            | "System.UIntPtr"  -> "unativeint"

            | "System.Numerics.BigInteger"  -> "bigint"
            | "Microsoft.FSharp.Core.Unit"  -> "unit"
            | "Microsoft.FSharp.Math.BigRational"   -> "BigNum"
            | "Microsoft.FSharp.Core.FSharpRef"     -> "ref"
            | "Microsoft.FSharp.Core.FSharpOption"  -> "option"
            | "Microsoft.FSharp.Collections.FSharpList" -> "list"
            | "Microsoft.FSharp.Collections.FSharpMap"  -> "Map"
            | "System.Collections.Generic.IEnumerable"  -> "seq"
            | Regex.Compiled.Match @"[\.\+]?([^\.\+]*)$" { GroupValues=[name] }-> name //short name
            | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

        let rec getGenericArgumentsArrayInclusive (ty:System.Type) =
            if ty.IsArray then
                getGenericArgumentsArrayInclusive (ty.GetElementType()) //todo: verify the recursive case
            else
                ty.GetGenericArguments()

        let rec sprintSig context (ty:Type) =
            let applyParens = applyParensForPrecInContext context
            let cleanName, arrSig =
                //if is generic type, then doesn't have FullName, need to use just Name
                match (if String.IsNullOrEmpty(ty.FullName) then ty.Name else ty.FullName) with
                //long name type encoding left of `, array encoding at end
                | Regex.Compiled.Match @"^([^`\[]*)`?.*?(\[[\[\],]*\])?$" { GroupValues=[cleanName;arrSig] } ->
                    cleanName, arrSig
                | _ ->
                    failwith ("failed to parse type name: " + ty.FullName)

            match (getGenericArgumentsArrayInclusive ty) with
            | args when args.Length = 0 ->
                if FSharpType.IsFunction ty then
                    let ity = ty.GetMethods() |> Seq.filter (fun x -> x.Name = "Invoke") |> Seq.head
                    let ps = ity.GetParameters()
                    let args = ps |> Seq.map (fun p -> sprintf "%s:%s" p.Name (p.ParameterType |> sprintSig 2)) |> String.concat " -> "
                    sprintf "%s -> %s" args (sprintSig 1 ity.ReturnType)
                else
                    (if outerTy.IsGenericTypeDefinition then "'" else "") + (displayName cleanName) + arrSig
            | args when cleanName = "System.Tuple" ->
                (applyParens (if arrSig.Length > 0 then 0 else 3) (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
            | [|lhs;rhs|] when cleanName = "Microsoft.FSharp.Core.FSharpFunc" -> //right assoc, binding not as strong as tuples
                (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s -> %s" (sprintSig 2 lhs) (sprintSig 1 rhs))) + arrSig
            | args ->
                sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig

        sprintSig 0 outerTy

    let sgn (f:'a) =
        let ty = f.GetType()
        let typ = if FSharpType.IsFunction ty then "Function" else "Value"
        sprintf "%s: %s" typ (sprintSig ty)
             
    // function call timer
    let duration f = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let returnValue = f()
        stopWatch.Stop()
        printfn "Elapsed Time: %f ms" stopWatch.Elapsed.TotalMilliseconds
        returnValue
        
