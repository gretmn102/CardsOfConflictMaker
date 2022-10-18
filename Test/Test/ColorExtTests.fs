module ColorExtTests
open Fuchu
open System.Drawing

[<Tests>]
let fromHexOrNameTests =
    let testColors exp act =
        match act with
        | Ok act ->
            if not <| ColorExt.equalsByValue exp act then
                failtestf "Expected: %A\nActual: %A" exp act
        | Error(errorValue) ->
            failtestf "Expected: %A\nActual: %A" exp errorValue

    testList "fromHexOrNameTests" [
        testCase "red" <| fun () ->
            let exp = Color.Red
            let act = ColorExt.fromHexOrName "red"

            Assert.Equal("", Ok exp, act)
        testCase "#00" <| fun () ->
            let act = ColorExt.fromHexOrName "#00"
            match act with
            | Error _ -> ()
            | Ok x ->
                failtestf "Expected: Error _\nActual: %A" x
        testCase "#7FFFD4 = Aquamarine" <| fun () ->
            let exp = Color.Aquamarine
            let act = ColorExt.fromHexOrName "#7FFFD4"

            testColors exp act
        testCase "#FF7FFFD4 = Aquamarine" <| fun () ->
            let exp = Color.Aquamarine
            let act = ColorExt.fromHexOrName "#FF7FFFD4"

            testColors exp act
    ]
