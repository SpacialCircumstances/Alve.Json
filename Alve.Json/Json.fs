namespace Alve.Json

open System
open System.Collections.Generic
open System.Text.Json

module Decode =
    module private Tuples =
        let inline tuple2 a b = a, b

        let inline tuple3 a b c = a, b, c

        let inline tuple4 a b c d = a, b, c, d

        let inline tuple5 a b c d e = a, b, c, d, e

        let inline tuple6 a b c d e f = a, b, c, d, e, f

        let inline tuple7 a b c d e f g = a, b, c, d, e, f, g

        let inline tuple8 a b c d e f g h = a, b, c, d, e, f, g, h

    type [<StructuredFormatDisplay("{AsString}")>] JsonError = 
        | DecodingError of string //For wrapping exceptions etc.
        | JsonTypeError of string * string
        | NotFoundError
        | FieldError of string * JsonError
        | ElementError of int * JsonError
        | MultiError of JsonError list
        | UserError of string
    with
        override x.ToString() = match x with
                                    | DecodingError e -> e
                                    | UserError e -> e
                                    | NotFoundError -> "Not found"
                                    | JsonTypeError (exp, got) -> sprintf "Type error: Expected %s, got %s" exp got
                                    | FieldError (field, err) -> sprintf "Error decoding field %s: %O" field err
                                    | ElementError (index, err) -> sprintf "Error decoding element %i: %O" index err
                                    | MultiError errors -> 
                                        let strErrors = Seq.map (fun e -> e.ToString()) errors
                                        String.Join(Environment.NewLine, strErrors)

        member self.AsString = self.ToString ()

    type Decoder<'a> = JsonElement -> Result<'a, JsonError>

    let decodeElement (dec: Decoder<'a>) (element: JsonElement): Result<'a, JsonError> = dec element

    let decodeDocument (dec: Decoder<'a>) (document: JsonDocument): Result<'a, JsonError> = decodeElement dec document.RootElement

    let decodeString (dec: Decoder<'a>) (json: string): Result<'a, JsonError> =
        let doc = JsonDocument.Parse json
        decodeDocument dec doc

    let private expectationFailed (expected: string) (got: 'a) = Error (JsonTypeError (expected, got.ToString()))

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
                    | :? FormatException as e -> DecodingError e.Message |> Error
            | other -> expectationFailed "Int" other

    let jfloat: Decoder<float> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Number ->
                try
                    Ok (json.GetDouble())
                with
                    | :? FormatException as e -> DecodingError e.Message |> Error
            | other -> expectationFailed "Float" other

    let jdecimal: Decoder<decimal> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Number ->
                try
                    Ok (json.GetDecimal())
                with
                    | :? FormatException as e -> DecodingError e.Message |> Error
            | other -> expectationFailed "Decimal" other

    let success (a: 'a): Decoder<'a> = fun _ -> Ok a

    let failed (msg: string): Decoder<'a> = fun _ -> DecodingError msg |> Error

    let jnull (a: 'a): Decoder<'a> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Null -> Ok a
            | other -> expectationFailed "Null" other

    let nullable (dec: Decoder<'a>): Decoder<'a option> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Null -> Ok None
            | _ -> dec json |> Result.map Some

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

    let bindResult (binder: 'a -> Result<'b, string>) (dec: Decoder<'a>): Decoder<'b> = fun json ->
        dec json |> Result.bind (fun a -> binder a |> Result.mapError UserError)

    let apply (d: Decoder<'a>) (df: Decoder<'a -> 'b>): Decoder<'b> =
        bind (fun f -> bind (fun x -> success (f x)) d) df

    let map1 (mapper: 'a -> 'b) (d: Decoder<'a>): Decoder<'b> = fun o -> Result.map mapper (d o)
    
    let map2 (mapper: 'a -> 'b -> 'c) (d1: Decoder<'a>) (d2: Decoder<'b>): Decoder<'c> = map1 mapper d1 |> apply d2
    
    let map3  (mapper: 'a -> 'b -> 'c -> 'd) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>): Decoder<'d> =
        map1 mapper d1
                |> apply d2
                |> apply d3
    
    let map4 (mapper: 'a -> 'b -> 'c -> 'd -> 'e) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>): Decoder<'e> =
        map1 mapper d1
                |> apply d2
                |> apply d3
                |> apply d4
    
    let map5 (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>): Decoder<'f> =
        map1 mapper d1
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5

    let map6 (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>): Decoder<'g> =
        map1 mapper d1
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6

    let map7 (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>): Decoder<'h> =
        map1 mapper d1
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6
                |> apply d7

    let map8 (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>) (d8: Decoder<'h>): Decoder<'i> =
        map1 mapper d1
                |> apply d2
                |> apply d3
                |> apply d4
                |> apply d5
                |> apply d6
                |> apply d7
                |> apply d8

    let tuple2 (d1: Decoder<'a>) (d2: Decoder<'b>)= map2 Tuples.tuple2 d1 d2

    let tuple3 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) = map3 Tuples.tuple3 d1 d2 d3

    let tuple4 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) = map4 Tuples.tuple4 d1 d2 d3 d4

    let tuple5 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) = map5 Tuples.tuple5 d1 d2 d3 d4 d5

    let tuple6 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) = map6 Tuples.tuple6 d1 d2 d3 d4 d5 d6

    let tuple7 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>) = map7 Tuples.tuple7 d1 d2 d3 d4 d5 d6 d7

    let tuple8 (d1: Decoder<'a>) (d2: Decoder<'b>) (d3: Decoder<'c>) (d4: Decoder<'d>) (d5: Decoder<'e>) (d6: Decoder<'f>) (d7: Decoder<'g>) (d8: Decoder<'h>) = map8 Tuples.tuple8 d1 d2 d3 d4 d5 d6 d7 d8

    let optional (dec: Decoder<'a>): Decoder<'a option> = fun json ->
        match dec json with
            | Ok a -> Ok (Some a)
            | Error _ -> Ok None

    let keyValuePairs (dec: Decoder<'a>): Decoder<(string * 'a) list> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Object ->
                let elements = seq {
                    for el in json.EnumerateObject() do
                        yield (el.Name, dec el.Value)
                }
                let values, errors = Seq.fold (fun (results, errors) (key, el) -> 
                    match el with
                        | Ok elem -> (key, elem) :: results, errors
                        | Error err -> results, (FieldError (key, err)) :: errors) ([], []) elements
                
                if List.isEmpty errors then
                    Ok values
                else 
                    MultiError errors |> Error
            | other -> expectationFailed "Object" other

    let jdict (dec: Decoder<'a>): Decoder<IReadOnlyDictionary<string, 'a>> = map1 readOnlyDict (keyValuePairs dec)

    let jmap (dec: Decoder<'a>): Decoder<Map<string, 'a>> = map1 Map.ofList (keyValuePairs dec)

    let field (fieldname: string) (dec: Decoder<'a>): Decoder<'a> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Object ->
                match json.TryGetProperty(fieldname) with
                    | true, el -> Result.mapError (fun err -> FieldError (fieldname, err)) (dec el)
                    | _ -> FieldError (fieldname, NotFoundError) |> Error
            | other -> expectationFailed "Object" other

    let at (fields: string list) (dec: Decoder<'a>): Decoder<'a> = (List.foldBack field fields dec)

    let index (idx: int) (dec: Decoder<'a>): Decoder<'a> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Array ->
                if idx >= 0 && idx < json.GetArrayLength() then
                    Result.mapError (fun err -> ElementError (idx, err)) (dec (json.Item idx))
                else
                    ElementError (idx, NotFoundError) |> Error
            | other -> expectationFailed "Array" other

    let jlist (dec: Decoder<'a>): Decoder<'a list> = fun json ->
        match json.ValueKind with
            | JsonValueKind.Array ->
                let arr = seq {
                    for el in json.EnumerateArray() do
                        yield dec el
                }
                let values, errors, _ = Seq.foldBack (fun el (results, errors, idx) -> 
                    match el with
                        | Ok elem -> elem :: results, errors, idx - 1
                        | Error err -> results, (ElementError (idx, err) :: errors), idx - 1) arr ([], [], (Seq.length arr) - 1)
                
                if List.isEmpty errors then
                    Ok values
                else 
                    MultiError errors |> Error

            | other -> expectationFailed "Array" other

    type JsonDecodeBuilder() =
        member this.Bind(x: Decoder<'a>, f) = bind f x

        member this.Return(x) = success x

    let jsonDecode = JsonDecodeBuilder()

module Encode = 
    open System.IO
    
    type JsonValue =
        | JsonNull
        | JsonString of string
        | JsonFloat of float
        | JsonInteger of int64
        | JsonDecimal of decimal
        | JsonArray of JsonValue list
        | JsonObject of IDictionary<string, JsonValue>

    let rec private encodeObj (obj: IDictionary<string, JsonValue>) (writer: Utf8JsonWriter) encodeArr =
        for entry in obj do
            let key = entry.Key
            let value = entry.Value
            match value with
                | JsonString str -> writer.WriteString(key, str)
                | JsonFloat f -> writer.WriteNumber(key, f)
                | JsonInteger i -> writer.WriteNumber(key, i)
                | JsonDecimal d -> writer.WriteNumber(key, d)
                | JsonNull -> writer.WriteNull(key)
                | JsonArray arr ->
                    writer.WriteStartArray (key)
                    List.iter (fun el -> encodeArr el writer) arr
                    writer.WriteEndArray ()
                | JsonObject obj2 ->
                    writer.WriteStartObject(key)
                    encodeObj obj2 writer encodeArr
                    writer.WriteEndObject()

    let rec private encodeArr (jv: JsonValue) (writer: Utf8JsonWriter) =
        match jv with
            | JsonObject obj ->
                writer.WriteStartObject ()
                encodeObj obj writer encodeArr
                writer.WriteEndObject ()
            | JsonArray arr ->
                writer.WriteStartArray ()
                List.iter (fun el -> encodeArr el writer) arr
                writer.WriteEndArray ()
            | JsonNull ->
                writer.WriteNullValue ()
            | JsonString str ->
                writer.WriteStringValue(str)
            | JsonInteger i ->
                writer.WriteNumberValue(i)
            | JsonFloat f ->
                writer.WriteNumberValue(f)
            | JsonDecimal d ->
                writer.WriteNumberValue(d)
    
    let encodeToWriter (jv: JsonValue) (writer: Utf8JsonWriter) =
        do encodeArr jv writer
        do writer.Flush ()
    
    let encodeToStream (jv: JsonValue) (stream: Stream) (options: JsonWriterOptions) = 
        use writer = new Utf8JsonWriter(stream, options)
        do encodeToWriter jv writer
    
    let encodeToStringOpt (jv: JsonValue) (options: JsonWriterOptions) =
        use stream = new MemoryStream()
        do encodeToStream jv stream options
        stream.Position <- 0L
        use reader = new StreamReader(stream)
        reader.ReadToEnd ()

    let encodeToString (jv: JsonValue) = encodeToStringOpt jv ( JsonWriterOptions())