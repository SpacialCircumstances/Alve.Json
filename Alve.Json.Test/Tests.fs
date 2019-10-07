module Tests

open Alve.Json.Decode
open Xunit

let decodeFail (decoder: Decoder<'a>) (str: string) =
    try
        let dec = decodeString decoder str
        match dec with
            | Ok a -> Assert.False(true, sprintf "Expected failure, but got: %A" a)
            | Error _ -> Assert.True(true)
    with
        | :? System.Text.Json.JsonException -> Assert.True(true)

let decodeEq (expected: 'a) (decoder: Decoder<'a>) (str: string) =
    let dec = decodeString decoder str
    match dec with
        | Ok res -> Assert.Equal<'a>(expected, res)
        | Error e -> Assert.True(false, e)

let readFile (path: string) = System.IO.File.ReadAllText path

[<Fact>]
let ``String decoder`` () =
    decodeEq "test" jstring @"""test"""
    decodeEq "" jstring @""""""
    decodeFail jstring "null"
    decodeFail jstring ""
    decodeFail jstring "{}"
    decodeFail jstring "test"

[<Fact>]
let ``Boolean decoder`` () =
    decodeEq true jbool "true"
    decodeEq false jbool "false"
    decodeFail jbool "test"
    decodeFail jbool @"""true"""
    decodeFail jbool "null"
    decodeFail jbool "True"

[<Fact>]
let ``Int decoder`` () =
    decodeEq 10L jint "10"
    decodeEq 999912L jint "999912"
    decodeFail jint "123.0"
    decodeFail jint "123.1"
    decodeFail jint @"""123"""
    decodeFail jint "null"
    decodeFail jint "{}"

[<Fact>]
let ``Float decoder`` () =
    decodeEq 10.0 jfloat "10.0"
    decodeEq 10.0 jfloat "10"
    decodeEq -32.1 jfloat "-32.1"
    decodeFail jfloat "null"
    decodeFail jfloat @"""10.0"""

[<Fact>]
let ``Decimal decoder`` () =
    decodeEq 19.3m jdecimal "19.3"
    decodeEq 12m jdecimal "12"

[<Fact>]
let ``Null decoder`` () =
    let d = jnull "test"
    decodeEq "test" d "null"
    decodeFail d ""
    decodeFail d @"""null"""

[<Fact>]
let ``OrElse and try decoder`` () =
    let d1 = orElse jint (jnull 2L)
    let d2 = jtry jint 2L
    decodeEq 2L d1 "null"
    decodeEq 2L d1 "2"
    decodeEq 3L d1 "3"
    decodeEq 0L d2 "0"
    decodeEq 2L d2 "null"

[<Fact>]
let ``All as string decoder`` () =
    let d1 = jstring
    let d2 = map1 jint string
    let d3 = map1 jfloat string
    let d4 = map1 jbool string
    let d = oneOf [ d1; d2; d3; d4 ]
    decodeEq "test" d @"""test"""
    decodeEq "123" d "123"
    decodeEq "123" d @"""123"""
    decodeEq "True" d "true"
    decodeEq "True" d @"""True"""

[<Fact>]
let ``Deep value access`` () =
    let json = readFile "test1.json"
    let glossary = field "glossary"
    let title = glossary (field "title" jstring)
    let glossDiv = fun d -> glossary (field "GlossDiv" d)
    let gdTitle = glossDiv (field "title" jstring)
    let glossEntry = fun d -> glossDiv (field "GlossList" (field "GlossEntry" d))
    let glossEntryId = glossEntry (field "ID" jstring)
    let glossEntryDef = fun d -> glossEntry (field "GlossDef" d)
    let glossSeeAlso = fun d -> glossEntryDef (field "GlossSeeAlso" d)
    let glossSeeAlso0 = glossSeeAlso (index 0 jstring)
    let glossSeeAlso1 = glossSeeAlso (index 1 jstring)
    let acronym = at [ "glossary"; "GlossDiv"; "GlossList"; "GlossEntry"; "Acronym" ] jstring
    let gdTitle2 = at [ "glossary"; "GlossDiv"; "title" ] jstring
    decodeEq "example glossary" title json
    decodeEq "S" gdTitle json
    decodeEq "S" gdTitle2 json
    decodeEq "SGML" glossEntryId json
    decodeEq "GML" glossSeeAlso0 json
    decodeEq "XML" glossSeeAlso1 json
    decodeEq "SGML" acronym json
    let f1 = at [ "glossary"; "GlossaryDiv"; "test" ] jstring
    decodeFail f1 json

type Item = {
    id: string
    label: string option
}

type MenuItem =
    | Item of Item
    | Separator

type Menu = {
    header: string
    items: MenuItem list
}

type Config = {
    menu: Menu
}

let config = {
    menu = {
        header = "SVG Viewer"
        items = [
            Item { 
                id = "Open"
                label = None
            }
            Item {
                id = "OpenNew"
                label = Some "Open New"
            }
            Separator
            Item {
                id = "ZoomIn"
                label = Some "Zoom In"
            }
            Item {
                id = "ZoomOut"
                label = Some "Zoom Out"
            }
            Item {
                id = "OriginalView"
                label = Some "Original View"
            }
            Separator
            Item {
                id = "Quality"
                label = None
            }
            Item {
                id = "Pause"
                label = None
            }
            Item {
                id = "Mute"
                label = None
            }
            Separator
            Item {
                id = "Help"
                label = None
            }
            Item {
                id = "About"
                label = Some "About Adobe CVG Viewer..."
            }
        ]
    }
}

[<Fact>]
let ``Map json to data structures`` () = 
    let jsonText = readFile "test2.json"
    let itemDecoder = map2 (field "id" jstring) (field "label" jstring |> optional) (fun id label -> { id = id; label = label })
    let menuItemDecoder = map1 (nullable itemDecoder) (fun item -> match item with
                                                                        | None -> Separator
                                                                        | Some item -> Item item)
    let menuItemListDecoder = jlist menuItemDecoder
    let menuDecoder = map2 (field "header" jstring) (field "items" menuItemListDecoder) (fun header items -> { header = header; items = items })
    let configDecoder = map1 (field "menu" menuDecoder) (fun menu -> { menu = menu })
    decodeEq config configDecoder jsonText

[<Fact>]
let ``Map json to data structures with ok computation expression`` () =
    let jsonText = readFile "test2.json"
    let itemDecoder = fun json -> ok {
        let! id = field "id" jstring json
        let! label = optional (field "label" jstring) json
        return {
            id = id
            label = label
        }
    }
    let menuItemDecoder = fun json -> ok {
        let! item = nullable itemDecoder json
        return match item with
                | None -> Separator
                | Some item -> Item item
    }
    let menuDecoder = fun json -> ok {
        let itemsDecoder = jlist menuItemDecoder
        let! items = (field "items" itemsDecoder) json
        let! header = (field "header" jstring) json
        return {
            items = items
            header = header
        }
    }
    let configDecoder = fun json -> ok {
        let! menu = json |> field "menu" menuDecoder
        return {
            menu = menu
        }
    }
    decodeEq config configDecoder jsonText

[<Fact>]
let ``Map json to data structures with computation expression`` () =
    let jsonText = readFile "test2.json"
    let itemDecoder = jsonDecode {
        let! id = field "id" jstring
        let! label = optional (field "label" jstring)
        return {
            id = id
            label = label
        }
    }
    let menuItemDecoder = jsonDecode {
        let! item = nullable itemDecoder
        return match item with
                | None -> Separator
                | Some item -> Item item
    }
    let menuDecoder = jsonDecode {
        let itemsDecoder = jlist menuItemDecoder
        let! items = (field "items" itemsDecoder)
        let! header = (field "header" jstring)
        return {
            items = items
            header = header
        }
    }
    let configDecoder = jsonDecode {
        let! menu = field "menu" menuDecoder
        return {
            menu = menu
        }
    }
    decodeEq config configDecoder jsonText