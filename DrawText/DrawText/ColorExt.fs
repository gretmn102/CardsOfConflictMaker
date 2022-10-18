module ColorExt
open System.Drawing

module HexOrNameParser =
    open FParsec

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let phexColor: _ Parser =
        let p8 =
            (regex "[0-9a-fA-F]{8}" .>> eof)
            |>> fun hex ->
                System.Convert.ToInt32(hex, 16)

        let p6 =
            (regex "[0-9a-fA-F]{6}" .>> eof)
            |>> fun hex ->
                0xff000000 + System.Convert.ToInt32(hex, 16)

        pchar '#'
        >>. (p8 <|> p6)
        |>> Color.FromArgb

    let tryGetNameColor (rawColor: string) =
        match rawColor.ToLower() with
        | "empty" -> Result.Ok Color.Black
        | colorName ->
            let color = Color.FromName colorName
            if color.A = 0uy && color.R = 0uy && color.G = 0uy && color.B = 0uy then
                Result.Error (sprintf "unknown '%s' color" colorName)
            else
                Result.Ok color

    let ptryGetNameColor =
        many1Satisfy (fun _ -> true)
        >>= fun x ->
            match tryGetNameColor x with
            | Result.Ok x -> preturn x
            | Result.Error errMsg -> fail errMsg

    module FParsecExt =
        let runResult p str =
            match run p str with
            | Success(res, _, _) -> Result.Ok res
            | Failure(errMsg, _, _) -> Result.Error errMsg

    let parse str =
        FParsecExt.runResult (phexColor <|> ptryGetNameColor) str

let fromHexOrName rawHexOrName =
    HexOrNameParser.parse rawHexOrName

let equalsByValue (c1: Color) (c2: Color) =
    c1.A = c2.A && c1.R = c2.R && c1.G = c2.G && c1.B = c2.B
