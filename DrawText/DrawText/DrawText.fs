module DrawText

#if INTERACTIVE
#load "Grid.fs"
#load "DrawingTextCard.fs"
#endif
open DrawingTextCard
open System.Drawing
let f () =
    start "answers" Color.Black Color.White
    start "questions" Color.White Color.Black
// f()
[<EntryPoint>]
let main argv =
    try
        f ()
        0
    with e ->
        printfn "%A" e
        System.Console.ReadKey() |> ignore
        -1 // return an integer exit code
