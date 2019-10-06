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
    decodeFail jstring "test"

[<Fact>]
let ``Boolean decoder`` () =
    decodeEq true jbool "true"
    decodeEq false jbool "false"
    decodeFail jbool "test"
    decodeFail jbool @"""true"""
    decodeFail jbool "null"