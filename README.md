# Alve.Json

![Nuget](https://img.shields.io/nuget/v/Alve.Json?style=plastic)

A simple F# library for decoding and encoding JSON values, inspired by Elm's Json.Decode. It is based on the [`System.Text.Json`](https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=netcore-3.0) APIs.

## Installation

TODO

## Usage

Assume we have this JSON (adapted from <https://json.org/example.html>):

```json
{
  "menu": {
    "header": "SVG Viewer",
    "items": [
      { "id": "Open" },
      {
        "id": "OpenNew",
        "label": "Open New"
      },
      null,
      {
        "id": "ZoomIn",
        "label": "Zoom In"
      },
      {
        "id": "ZoomOut",
        "label": "Zoom Out"
      },
      {
        "id": "OriginalView",
        "label": "Original View"
      },
      null,
      { "id": "Quality" },
      { "id": "Pause" },
      { "id": "Mute" },
      null,
      { "id": "Help" },
      {
        "id": "About",
        "label": "About Adobe CVG Viewer..."
      }
    ]
  }
}
```

And these F# types:

```fsharp
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
```

Then we can write datatype decoders in the following way:

```fsharp
open Alve.Json.Decode

let itemDecoder = map2 (field "id" jstring) (field "label" jstring |> optional) (fun id label -> { id = id;label = label })

let menuItemDecoder = map1 (nullable itemDecoder) (fun item -> match item with
                                                                    | None -> Separator
                                                                    | Some item -> Item item)
let menuItemListDecoder = jlist menuItemDecoder
let menuDecoder = map2 (field "header" jstring) (field "items" menuItemListDecoder) (fun header items -> {header = header; items = items })
let configDecoder = map1 (field "menu" menuDecoder) (fun menu -> { menu = menu })

//Now, decode our JSON:

printfn "%A" (decodeString configDecoder json)
```

Alternatively, we can use the `jsonDecode` computation expression to simplify the mapping operation:

```fsharp
open Alve.Json.Decode

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

//Now, decode our JSON:

printfn "%A" (decodeString configDecoder json)
```

## Performance

Alve.Json requires the entire JSON to be present in memory for decoding. Therefore, it is unsuited for processing enormous amounts of data. Also, the functional programming style (bind, apply etc.) is not very good for performance and may generate some garbage, so do not use this in performance-sensitive applications.

## License

Alve.Json is licensed under the MIT License.
