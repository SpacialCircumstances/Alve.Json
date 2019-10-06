﻿module Json

open System
open System.Text.Json

module Decode =
    type Decoder<'a> = JsonElement -> Result<'a, string>

    let decodeElement (dec: Decoder<'a>) (element: JsonElement): Result<'a, string> = dec element

    let decodeDocument (dec: Decoder<'a>) (document: JsonDocument): Result<'a, string> = decodeElement dec document.RootElement

    let decodeString (dec: Decoder<'a>) (json: string): Result<'a, string> =
        let doc = JsonDocument.Parse json
        decodeDocument dec doc

    let private expectationFailed (expected: string) (got: 'a) = Error (sprintf "Expected: %s, but got: %O" expected got)

    let jstring: Decoder<string> = fun json ->
        match json.ValueKind with
            | JsonValueKind.String -> Ok (json.GetString ())
            | other -> expectationFailed "String" other

    let jbool: Decoder<bool> = fun json ->
        match json.ValueKind with
            | JsonValueKind.True -> Ok true
            | JsonValueKind.False -> Ok false
            | other -> expectationFailed "Boolean" other

    let jint: Decoder<int64> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Number -> 
                try
                    Ok (json.GetInt64())
                with
                    | :? FormatException as e -> Error e.Message
            | other -> expectationFailed "Int" other

    let jfloat: Decoder<float> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Number ->
                try
                    Ok (json.GetDouble())
                with
                    | :? FormatException as e -> Error e.Message
            | other -> expectationFailed "Float" other

    let jdecimal: Decoder<decimal> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Number ->
                try
                    Ok (json.GetDecimal())
                with
                    | :? FormatException as e -> Error e.Message
            | other -> expectationFailed "Decimal" other

    let success (a: 'a): Decoder<'a> = fun _ -> Ok a

    let failed (msg: string): Decoder<'a> = fun _ -> Error msg

    let jnull (a: 'a): Decoder<'a> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Null -> Ok a
            | other -> expectationFailed "Null" other

    let nullable (dec: Decoder<'a>): Decoder<'a option> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Null -> Ok None
            | _ -> dec json |> Result.map Some

    let jlist (dec: Decoder<'a>): Decoder<'a list> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Array ->
                let arr = seq {
                    for el in json.EnumerateArray() do
                        yield dec el
                }
                Seq.fold (fun (results, errors) el -> 
                    match el with
                        | Ok elem -> elem :: results, errors
                        | Error err -> results, err :: errors) ([], []) arr
                    |> (fun (vals, errs) ->
                        if List.isEmpty errs then
                            Ok vals
                        else 
                            let (errorDescription, _) = List.fold (fun (errStr, i) err -> (sprintf "%s (Index: %i, Error: %s)" errStr i err, i + 1)) ("Errors decoding array: ", 0) errs
                            Error errorDescription)

            | other -> expectationFailed "Array" other

    let orElse (d1: Decoder<'a>) (d2: Decoder<'a>) = fun json ->
        let r = d1 json
        match r with
            | Ok a -> Ok a
            | Error _ -> d2 json

    let jtry (dec: Decoder<'a>) (a: 'a) = orElse dec (success a)

    let oneOf (ds: Decoder<'a> list): Decoder<'a> = List.reduce orElse ds

    let bind (binder: 'a -> Decoder<'b>) (dec: Decoder<'a>): Decoder<'b> = fun json ->
        let r = dec json
        match r with
            | Ok a -> (binder a) json
            | Error e -> Error e

    let apply (d: Decoder<'a>) (df: Decoder<'a -> 'b>): Decoder<'b> =
        bind (fun f -> bind (fun x -> success (f x)) d) df

    let map1 (d: Decoder<'a>) (mapper: 'a -> 'b): Decoder<'b> = fun o -> Result.map mapper (d o)
    
    let map2 (d1: Decoder<'a>) (d2: Decoder<'b>) (mapper: 'a -> 'b -> 'c): Decoder<'c> = map1 d1 mapper |> apply d2
    
    let map3 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (mapper: 'a -> 'b -> 'c -> 'd): Decoder<'d> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
    
    let map4 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (mapper: 'a -> 'b -> 'c -> 'd -> 'e): Decoder<'e> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
                |> apply d4
    
    let map5 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f): Decoder<'f> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5

    let map6 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g): Decoder<'g> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6

    let map7 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>) (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h): Decoder<'h> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6
                |> apply d7

    let map8 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>) (d8: Decoder<'h>) (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i): Decoder<'i> =
        map1 d1 mapper
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6
                |> apply d7
                |> apply d8

module Encode = 
    ()