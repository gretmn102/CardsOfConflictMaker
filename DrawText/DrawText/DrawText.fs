module DrawText
open Argu

open DrawingTextCard

let start (settings: Settings) destinationFolder destinationFileName cardsDescriptionsPath =
    let destinationFolder = destinationFolder |> Option.defaultValue "output"
    let destinationFileName = destinationFileName |> Option.defaultValue "card"

    System.IO.Directory.CreateDirectory destinationFolder |> ignore

    start
        settings
        (System.IO.File.ReadAllText cardsDescriptionsPath)
    |> Seq.iteri (fun i img ->

        img.Save(sprintf "%s\\%s%d.png" destinationFolder destinationFileName i)
        img.Dispose()
    )

type CliArguments =
    | [<Unique; EqualsAssignmentOrSpaced>] Width of int
    | [<Unique; EqualsAssignmentOrSpaced>] Height of int
    | [<Unique; EqualsAssignmentOrSpaced>] Text_Color of string
    | [<Unique; EqualsAssignmentOrSpaced>] Background_Color of string
    | [<Unique; EqualsAssignmentOrSpaced>] Columns_Count of int
    | [<Unique; EqualsAssignmentOrSpaced>] Rows_Count of int
    | [<Unique; EqualsAssignmentOrSpaced>] Font_Name of string
    | [<Unique; EqualsAssignmentOrSpaced>] Font_Size of float32
    | [<Unique; EqualsAssignmentOrSpaced>] Max_Characters_In_Line of int
    | [<Unique; EqualsAssignmentOrSpaced>] Grid_Color of string
    | [<Unique; EqualsAssignmentOrSpaced>] Destination_FileName of string
    | [<Unique; EqualsAssignmentOrSpaced>] Destination_Folder of string
    | [<MainCommand; ExactlyOnce; Last>] Descriptions_Path of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Width(_) -> "specify width of card."
            | Height(_) -> "specify height of card."
            | Descriptions_Path(_) -> "specify a file with card descriptions."
            | Text_Color(_) -> "specify text color."
            | Background_Color(_) -> "specify background color."
            | Destination_FileName(_) -> "specify destination file name for output cards."
            | Destination_Folder(_) -> "specify destination file folder for output cards."
            | Columns_Count(_) -> "specify columns count."
            | Rows_Count(_) -> "specify rows count."
            | Font_Name(_) -> "specify font name."
            | Font_Size(_) -> "specify font size."
            | Max_Characters_In_Line(_) -> "specify max characters in line."
            | Grid_Color(_) -> "specify grid color."

let argsToSettings (res: ParseResults<CliArguments>) =
    let update opt updater (settings: Settings) =
        match opt with
        | None -> Ok settings
        | Some x -> updater x settings

    let bind nexter res =
        match res with
        | Ok x -> nexter x
        | Error errMsg -> Error errMsg

    let tryApply opt fn = bind (update opt fn)
    let apply opt fn = bind (update opt (fun opt settings -> Ok (fn opt settings)))

    Ok Settings.Default
    |> tryApply (res.TryGetResult Text_Color) (fun opt settings ->
        ColorExt.fromHexOrName opt
        |> Result.map (fun color ->
            { settings with TextColor = color }
        )
    )
    |> tryApply (res.TryGetResult Background_Color) (fun opt settings ->
        ColorExt.fromHexOrName opt
        |> Result.map (fun color ->
            { settings with BackgroundColor = color }
        )
    )
    |> apply (res.TryGetResult Width) (fun opt settings ->
        { settings with Width = opt }
    )
    |> apply (res.TryGetResult Height) (fun opt settings ->
        { settings with Height = opt }
    )
    |> apply (res.TryGetResult Columns_Count) (fun opt settings ->
        { settings with ColumnsCount = opt }
    )
    |> apply (res.TryGetResult Rows_Count) (fun opt settings ->
        { settings with RowsCount = opt }
    )
    |> apply (res.TryGetResult Font_Name) (fun opt settings ->
        { settings with FontName = opt }
    )
    |> apply (res.TryGetResult Font_Size) (fun opt settings ->
        { settings with FontSize = opt }
    )
    |> apply (res.TryGetResult Max_Characters_In_Line) (fun opt settings ->
        { settings with MaxCharactersInLine = opt }
    )
    |> tryApply (res.TryGetResult Grid_Color) (fun opt settings ->
        ColorExt.fromHexOrName opt
        |> Result.map (fun color ->
            { settings with GridColor = color }
        )
    )

[<EntryPoint>]
let main argv =
    let argParser = ArgumentParser<CliArguments>(programName = "TextCardsMaker.exe")
    let res =
        try
            Ok (argParser.ParseCommandLine(argv))
        with e ->
            Error e.Message

    match res with
    | Error errMsg ->
        printfn "%s" errMsg
        1
    | Ok res ->
        match argsToSettings res with
        | Error errMsg -> printfn "%s" errMsg
        | Ok settings ->
            start
                settings
                (res.TryGetResult Destination_Folder)
                (res.TryGetResult Destination_FileName)
                (res.GetResult Descriptions_Path)

        0
