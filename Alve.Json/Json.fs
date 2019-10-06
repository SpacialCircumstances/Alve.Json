module Json

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
            | other -> dec json |> Result.map Some

    let orElse (d1: Decoder<'a>) (d2: Decoder<'a>) = fun json ->
        let r = d1 json
        match r with
            | Ok a -> Ok a
            | Error _ -> d2 json

    let jtry (dec: Decoder<'a>) (a: 'a) = orElse dec (success a)

    let oneOf (ds: Decoder<'a> seq): Decoder<'a> = Seq.reduce orElse ds

    let bind (binder: 'a -> Decoder<'b>) (dec: Decoder<'a>): Decoder<'b> = fun json ->
        let r = dec json
        match r with
            | Ok a -> (binder a) json
            | Error e -> Error e

    let apply (d: Decoder<'a>) (df: Decoder<'a -> 'b>): Decoder<'b> =
        bind (fun f -> bind (fun x -> success (f x)) d) df

module Encode = 
    ()