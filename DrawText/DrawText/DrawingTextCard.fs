module DrawingTextCard
open FsharpMyExtension
open FsharpMyExtension.Either
open System.Drawing

let defineTextSize =
    let img = new Bitmap(1, 1)
    fun font str ->
        use draw = Graphics.FromImage(img)
        draw.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        draw.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        draw.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
        draw.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality
        // let f = new StringFormat()
        // f.FormatFlags <- StringFormatFlags.MeasureTrailingSpaces
        // draw.MeasureString(str, font, System.Int32.MaxValue, f)
        draw.MeasureString(str, font)
let format = new StringFormat()
format.Alignment <- StringAlignment.Center

/// Возвращает изображение с указаным текстом
let drawText2 (g:Graphics) font textColor (pos: float32 * float32) text =
    // use g = Graphics.FromImage(img)
    g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
    g.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
    g.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality
    // g.
    use txtBrush = new SolidBrush(textColor)
    g.DrawString(text, font, txtBrush, fst pos, snd pos)
    // g.Save() |> ignore // TODO: обязательно ли?

let drawText font textColor backColor text =
    //measure the string to see how big the image needs to be
    let txtSize = defineTextSize font text

    let img = new Bitmap(int txtSize.Width, int txtSize.Height)
    use g = Graphics.FromImage(img)
    g.Clear(backColor)
    drawText2 g font textColor (0.f, 0.f) text
    img

let drawText3 font textColor backColor (text : string list) =
    let xs =
        text |> List.map (defineTextSize font)
    let width = xs |> List.maxBy (fun x -> x.Width) |> fun x -> x.Width
    let height = xs |> List.sumBy (fun x -> x.Height ) |> int
    //measure the string to see how big the image needs to be
    // let txtSize = defineTextSize font text
    let img = new Bitmap(int width, height)
    use g = Graphics.FromImage(img)
    g.Clear(backColor)

    List.zip text xs
    |> List.mapFold (
        let widthHalf = width / 2.f
        fun h (str, size) ->
            let w = widthHalf - size.Width / 2.f
            drawText2 g font textColor (w, h) str
            (), h + size.Height)
        0.f
    |> ignore
    img

let drawText4 width (shiftW, shiftH) textColor g xs =
    xs
    |> List.mapFold (
        let widthHalf = width / 2.f
        fun h ((w', h'), xs) ->
            let w = widthHalf - w' / 2.f
            xs
            |> List.mapFold (fun w ((str, font), (size:SizeF)) ->
                let h = h + (h' - size.Height) // если шрифт разного размера
                drawText2 g font textColor (w + shiftW, h + shiftH) str
                (), w + size.Width
                ) w
            |> ignore
            (), h + h'
            )
        0.f
    |> ignore
let gen (fonts: Map<FontStyle, Font>) text =
    let xs =
        text |> List.map (
            List.map (fun (str, style) ->
                let font = fonts.[style]
                (str, font), defineTextSize font str)
            >> fun xs ->
                let width = xs |> List.sumBy (snd >> fun x -> x.Width) // TODO: optimize
                let height =
                    xs |> List.maxBy (fun (_, x) -> x.Height)
                    |> fun (_, x) -> x.Height
                (width, height), xs
            )
    let width = xs |> List.maxBy (fun ((w, _),_) -> w) |> fun ((w, _),_) -> w
    let height = xs |> List.sumBy (fun ((_, h),_) -> h)
    (width, height), xs
let drawText5 (fonts: Map<FontStyle, Font>) textColor backColor (text : (string * FontStyle) list list) =
    let (width, height), xs = gen fonts text
    let img = new Bitmap(int width, int height)
    use g = Graphics.FromImage(img)
    g.Clear(backColor)
    drawText4 width (0.f, 0.f) textColor g xs
    img
let drawText6 (dstWidth, dstHeight) (fonts: Map<FontStyle,Font>) textColor backColor (text : (string * FontStyle) list list) =
    let (width, height), xs = gen fonts text
    let x = dstWidth / 2.f - width / 2.f // TODO: а если отрицательное?
    let y = dstHeight / 2.f - height / 2.f

    let img = new Bitmap(int dstWidth, int dstHeight)
    use g = Graphics.FromImage(img)
    g.Clear(backColor)
    drawText4 width (x, y) textColor g xs
    img


let test () =
    let font = new System.Drawing.Font("Fira code", 20.f, FontStyle.Bold)

    // сумма частей равна общему?
    let str1 = "a"
    let str2 = "a"
    // let exp = defineTextSize fontBold (sprintf "%s\n%s" str1 str2)
    let exp = defineTextSize font (sprintf "%s%s" str1 str2)
    let act =
        let line1 = defineTextSize font str1
        let line2 = defineTextSize font str2
        line1.Width + line2.Width
        exp.Height - (line1.Height + line2.Height)
        1.f / 6.f
    ()
let test2 () =
    let font = new System.Drawing.Font("Times New Roman", 30.f)
    let text =
        [" Не волнуйся, когда он вернется ко"; "мне с Восточного побережья, я";
         "непременно отучу его от этого. А"; "что до твоей змеи, я совсем не";
         "удивлюсь, если она имеет особенные"; "виды на твою грудь, раз так ее";
         "тискает."
         "a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a"
        ]
    let img = text |> drawText3 font Color.Black Color.White
    // let img = drawText "draw\nand draw" font Color.Black Color.White
    img.Save("output\\img.png")
    let img2 =
        text |> String.concat "\n" |> drawText font Color.Black Color.White
    img2.Save("output\\img2.png")
let test3 () =
    let text =
        [
            [ "Не волнуйся, когда он вернется ко", FontStyle.Regular ]
            [ "мне с Восточного побережья, я", FontStyle.Regular ]
            [ "непременно отучу его от этого. А", FontStyle.Regular ]
            [ "что до твоей змеи, я совсем не", FontStyle.Regular ]
            [ "удивлюсь, если она ", FontStyle.Regular; "имеет ", FontStyle.Bold; "особенные", FontStyle.Regular ]
            [ "виды на ", FontStyle.Regular; "твою грудь, ", FontStyle.Strikeout;  "раз так ее", FontStyle.Regular ]
            [ "тискает.", FontStyle.Regular ]
            [ "a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a", FontStyle.Regular ]
        ]
    let fonts =
        let f (fontStyle:FontStyle) =
            fontStyle, new System.Drawing.Font("Times New Roman", 30.f, fontStyle)

        Map [
            f FontStyle.Regular
            f FontStyle.Bold
            f FontStyle.Strikeout
        ]
    let act =
        text |> drawText5 fonts Color.Black Color.White
        // |> Array2D.ofBitmap
    // let img = drawText "draw\nand draw" font Color.Black Color.White
    act.Save("output\\img.png")
    // let img2 =
    //     text |> String.concat "\n" |> drawText font Color.Black Color.White
    // img2.Save("output\\img2.png")

module Slice =
    open FsharpMyExtension
    // let p : Parser<_,unit> =
    //     Inline.SepBy(
    //         stateFromFirstElement   = (fun x -> failwith ""),
    //         foldState               = (fun st sep x -> failwith ""),
    //         resultFromState         = (fun st -> failwith ""),
    //         elementParser           = failwith "",
    //         separatorParser         = spaces1,
    //         // firstElementParser      = Parser<'T,'U> option,
    //         resultForEmptySequence  = fun () -> []
    //         // separatorMayEndSequence = bool option
    //     )
    // ['а'..'я'] @ ['А'..'Я'] |> System.String.Concat
    // "абвгдежзийклмнопрстуфхцчшщъыьэюяАБВГДЕЖЗИЙ"
    // |> fun x -> x.Length
    // let count = 42
    // предложение
    type T = {
        Word:char list * int
        Line:char list * int
        Lines:string list
    }

    // TODO: переносы по слогам
    // TODO: пренебречь служебными тегам, вроде `\C[4]sometext\C[0]` — на изменение цвета.
    // TODO: Убрать в начале пробел (хотя с пробелом вначале выглядит эстетичнее. Так что так задумано, а не у кого-то руки из...)
    // TODO: Учесть теги подстановок, например `\N[0]` — по-умолчанию подставляет "Davon", но кто знает, какова длина окажется подстановки.
    /// Слово — совокупность любых символов, кроме пробела.
    let loop count (prevIsLetter, (acc:T)) = function
        | ' ' ->
            if prevIsLetter then
                let acc =
                    let word, wordLength = acc.Word
                    let line, lineLength = acc.Line
                    let count' = lineLength + wordLength + 1
                    let acc = { acc with Word = [], 0 }
                    if count' > count then
                        {
                            acc with
                                Lines =
                                    List.rev line |> System.String.Concat
                                    |> fun x -> x::acc.Lines
                                Line = word, wordLength
                        }
                    else
                        {
                            acc with
                                Line =
                                    let line = word @ (' ' :: line)
                                        // ' ' :: List.fold List.consFlip (line) (List.rev word)
                                    line, count'
                        }
                false, acc
            else
                false, acc
        | y ->
            let acc =
                { acc with Word = acc.Word |> mapPair (List.cons y) succ}
            true, acc
            // loop (n, acc)

    let start count str =
        str
        |> Seq.fold (loop count) (false, { Word = [], 0; Line = [], 0; Lines = []})
        |> flip (loop count) ' '
        |> fun (_, x) ->
            List.rev (fst x.Line) |> System.String.Concat
            |> fun y -> List.rev (y::x.Lines)


module SliceText =
    open FParsec
    open FsharpMyExtension.Either

    type 'a Parser = Parser<'a, unit>
    let word : _ Parser = many1SatisfyL (isNoneOf "\t \\*_\n~") "word"
    let spacesOnly : _ Parser = manySatisfy (fun x -> x = ' ' || x = '\t')

    let excpts : _ Parser =
        choice [
            pchar '\\' >>? notFollowedBy (pchar 'n')
            >>. choice [
                pstring "\\"
                pstring "*"
                pstring "_"
                pstring "~"
            ]
            pstring "_" .>>? notFollowedBy (pchar '_')
        ]
    let ptext : _ Parser =
        many1 (
            excpts <|> word
            .>> spacesOnly
        )
    // createParserForwardedToRef
    let p : _ Parser =
        let punctuations =
            // System.Char.IsPunctuation
            many1Strings (excpts <|> many1Satisfy (isAnyOf "!@#$%^&*()[];',.—-?:"))
        let p str style =
            pstring str >>. (spacesOnly >>. ptext) .>> pstring str
            .>>. opt punctuations
            .>> spacesOnly
            // |>> fun xs -> xs, style
            |>> fun (xs, punctsOpt) ->
                match punctsOpt with
                | None ->
                    xs, style
                | Some puncts ->
                    let rec f acc = function
                        | [x] -> f ((x + puncts) :: acc) []
                        | x::xs -> f (x::acc) xs
                        | [] -> List.rev acc
                    f [] xs, style
        choice [
            p "**" FontStyle.Bold
            p "*" FontStyle.Italic
            p "~" FontStyle.Strikeout
            p "__" FontStyle.Underline

            ptext
            |>> fun xs -> xs, FontStyle.Regular
        ]
    let mainParser =
        let nl = skipString "\\n" >>. spacesOnly
        many1 (many1 p <|> (nl >>. many p))

    let start str =
        let comment = pstring "//" >>. manySatisfy ((<>) '\n') .>> spaces
        let p =
            mainParser .>> spaces .>> skipMany comment
        match run (spaces >>. skipMany comment >>. many p .>> eof) str with
        | Success(x, _, _) -> Right x
        | x -> Left x

    // start "asdf \n sd f"
    // start "kjsadf skdjf ksd*italic,* ** bold — ** kjs ~kj~ __as__ jh"


    // start "kjsadf skdjf ksd sdfkj sdf k" |> Either.get |> List.head |> fst

    let splitSimple count xss =
        let f fn (st:'State) xs =
            List.unfold (fun (st, xs) ->
                match xs with
                | [] -> None
                | x::xs ->
                    let st, x = fn st x
                    Some(x, (st, xs)) ) (st, xs)
        let splitSimple count = function
            | [] -> []
            | (x:string, y)::xs ->
                xs
                |> List.fold (fun (count', acc, accs) (x,y) ->
                    let c = count' + 1 + x.Length
                    if c > count then
                        let accs = List.rev acc :: accs
                        let acc = [x, y]
                        x.Length, acc, accs
                    else
                        c, (x,y)::acc, accs
                ) (x.Length, [x, y], [])
                |> fun (_, acc, accs) ->
                    List.rev acc :: accs |> List.rev
        xss
        |> List.collect (fun (xs, style) ->
            xs |> List.map (flip comma style) )
        |> splitSimple count
        |> List.map (
            let rec g acc = function
                | (x, style)::xs ->
                    let f (x, style) = function
                        | [] -> (([x], style)::acc) |> List.rev
                        | xs ->
                            let ys, xs = List.takeWhileRest (snd >> (=) style) xs
                            let acc = ((x::List.map fst ys), style) :: acc
                            g acc xs
                    f (x, style) xs
                | [] -> acc |> List.rev
            g [])
        |> List.map (List.map (mapFst (String.concat " ")))


    // let count = 20
    // start "12345 1234 1234*italic,* ** bold — ** kjs ~kj~ __as__ jh" |> Either.get
    // |> simple count
    // // |> List.find (fun xs ->
    // //     let count' = xs |> List.sumBy (fun (x,_) -> x.Length)
    // //     count < count'
    // //     )

    // start "dsf * boo!*" |> Either.get
    // |> simple
    let final count str =
        start str
        |> Either.map (List.map (List.collect (splitSimple count)))
    // final 20 "12345\\n 5234\n1234*italic,* ** bold — ** kjs ~kj~ __as__ jh"

type Settings =
    {
        Width: int
        Height: int
        TextColor: Color
        BackgroundColor: Color
        ColumnsCount: int
        RowsCount: int
        FontName: string
        FontSize: float32
        MaxCharactersInLine: int
        GridColor: Color
        GridEdgeWidth: int
    }
    static member Default =
        {
            Width = 325
            Height = 325
            TextColor = Color.Black
            BackgroundColor = Color.White
            ColumnsCount = 4
            RowsCount = 5
            FontName = "Times New Roman"
            FontSize = 20.f
            MaxCharactersInLine = 22
            GridColor = Color.Black
            GridEdgeWidth = 3
        }

let start (settings: Settings) descriptionCardsRaw =
    let fonts =
        let f (fontStyle: FontStyle) =
            fontStyle, new Font(settings.FontName, settings.FontSize, fontStyle)

        Map [
            f FontStyle.Regular
            f FontStyle.Italic
            f FontStyle.Bold
            f FontStyle.Strikeout
            f FontStyle.Underline
        ]

    let xs =
        descriptionCardsRaw
        |> SliceText.final settings.MaxCharactersInLine
        |> Either.get
    xs
    |> List.map (
        drawText6 (float32 settings.Width, float32 settings.Height) fonts settings.TextColor settings.BackgroundColor
        >> fun img ->
            let r = Rectangle(0, 0, img.Width, img.Height)
            img, r
        )
    |> Grid.drawImagesOnGrids settings.GridColor settings.GridEdgeWidth false (settings.Width, settings.Height) (settings.ColumnsCount, settings.RowsCount)
