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