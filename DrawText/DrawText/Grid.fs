module Grid
open System.Drawing
open FsharpMyExtension

let gen' step count width =
    List.unfold (fun (st, i) ->
        if i > 0 then
            let st' = st + width - 1
            Some ((st,st'), (st' + 1 + step, i - 1))
        else None) (0, count)
// gen' 0 10 10
let gen step count width =
    List.unfold (fun (st, i) ->
        if i > 0 then
            let st' = st + width - 1
            Some (st, (st' + 1 + step, i - 1))
        else None) (0, count)
// assert
    // gen 3 4 4

let fromBmp numWidth numHeight (path:string) =
    let bmpSource = new Bitmap(path)
    let wDest = bmpSource.Width / numWidth
    let hDest = bmpSource.Height / numHeight
    let ws = gen 0 numWidth wDest
    let hs = gen 0 numHeight hDest
    let m =
        hs |> List.fold (fun st h ->
            ws |> List.fold (fun (m,i) w ->
                let r = Rectangle(w, h, wDest, hDest)
                Map.add i r m, i + 1) st ) (Map.empty, 0)
    bmpSource, (wDest, hDest), fst m
let draw2 numWidth numHeight path =
    let bmpSrc, (wDest, hDest), m = fromBmp numWidth numHeight path
    let r = Rectangle(0, 0, wDest, hDest)
    m |> Seq.iter (fun (KeyValue(i,v)) ->
        use bmp = new Bitmap(wDest, hDest)
        use g = Graphics.FromImage bmp
        g.DrawImage(bmpSrc, r, v, GraphicsUnit.Pixel)
        bmp.Save(sprintf "Output\\0%d.png" i) // bmp.Save(sprintf "Output\\0%d %dx%d.png" i w h)
    )
    bmpSrc.Dispose()
let sizeCellDef widthGridEdge lenDst count =
    let x = lenDst + widthGridEdge - count * widthGridEdge
    match x % count with 0 -> () | x -> failwithf "%d" x
    x / count
let lenDstDef widthGridEdge sizeCell count =
    sizeCell * count + (count - 1) * widthGridEdge + widthGridEdge * 2
let drawGrid (gridColor:Color) widthGridEdge cellW cellH (bmp:Bitmap) =
    // let size = 3
    // let step = 5
    // let x0 = size + step/2
    // let x1 = x0 + size + step
    // let x2 = x1 + size + step
    // |0|1|2|3|4|5|6|7|8|9|0|1|2|3|4|5
    // |_|_|^|_|_|^|_|_|^|               // 2; 5
    // |_|_|+|^|+|_|_|+|^|+|_|_|+|^|     // 3; 8; 13
    // |_|_|+|+|^|+|+|_|_|+|+|^|+|+|_|_| // 4; 11; 18
    // |_|+|+|^|+|+|_|+|+|^|+|+|_|       // 3; 9;

    // let size = 3
    // let step = 5
    // let x0 = size / 2
    // let x1 = x0 + size + step
    // let x2 = x1 + size + step
    // |0|1|2|3|4|5|6|7|8|9|0|1|2|3|4|5|6|7|8|9|0
    // |^|_|_|^|_|_|^|_|_|^|                       // size = 1; step = 2; [0; 3; 6; 9]
    // |+|^|+|_|_|+|^|+|_|_|+|^|+|_|_|+|^|         // size = 3; step = 2; [1; 6; 11; 16]
    // |+|+|^|+|+|_|_|+|+|^|+|+|_|_|+|+|^|+|+|_|_| // size = 5; step = 2; [2; 9; 16; 23]
    // |+|+|^|+|+|_|+|+|^|+|+|_|+|+|^|+|+|_|+|+|^| // size = 5; step = 1; [2; 8; 14; 20]
    let lines size step count =
        Seq.unfold (fun (st,count) ->
                if count > 0 then
                    Some(st, (st + size + step, count - 1))
                else None)
            // (size + step/2, count)
            (size / 2, count)
    // lines 5 1 4
    use g = Graphics.FromImage bmp
    use pen = new Pen(gridColor, float32 widthGridEdge)

    lines widthGridEdge cellW (bmp.Width / cellW + 1)
    |> Seq.iter (fun x -> g.DrawLine(pen, x, 0, x, bmp.Height))
    lines widthGridEdge cellH (bmp.Height / cellH + 1)
    |> Seq.iter (fun x -> g.DrawLine(pen, 0, x, bmp.Width, x))
assert
    let _test () =
        let widthGridEdge = 3
        let w, h = 40, 30
        let numWidth, numHeight = 10, 5

        use bmp = new Bitmap(
                    lenDstDef widthGridEdge w numWidth,
                    lenDstDef widthGridEdge h numHeight)
        // use g = Graphics.FromImage bmp
        // g.Clear Color.Black
        drawGrid Color.Black widthGridEdge w h (bmp:Bitmap)
        bmp.Save("Output\\all.png")
    // _test()
    true
/// растягивает изображения в ячейке.
let drawed gridColor flipByHor cellW cellH numWidth numHeight (imgs: (Bitmap * Rectangle) seq) =
    // количество пикселей между изображениями (по высоте и по ширине).
    // должно быть нечетным (чтобы не смущать пиксельное пространство)
    let widthGridEdge = 3
    if widthGridEdge % 2 = 0 then failwith "step must be not even"

    let ws = gen widthGridEdge numWidth cellW |> List.map ((+) widthGridEdge)
    let hs = gen widthGridEdge numHeight cellH |> List.map ((+) widthGridEdge)
    let xs =
        let f =
            if flipByHor then
                fun h -> ws |> List.map (fun w -> w, h) |> List.rev
            else
                fun h -> ws |> List.map (fun w -> w, h)
        hs |> List.collect f

    let f rSrcs =
        let bmpAll = new Bitmap(
                        lenDstDef widthGridEdge cellW numWidth,
                        lenDstDef widthGridEdge cellH numHeight)
        use g = Graphics.FromImage bmpAll
        g.Clear(Color.White)
        drawGrid gridColor widthGridEdge cellW cellH bmpAll
        Seq.zip xs rSrcs
        |> Seq.iter (fun (x, (bmpSrc, r)) ->
            let create (w0, h0) r =
                g.DrawImage(bmpSrc,
                    Rectangle(w0, h0, cellW, cellH),
                    r,
                    GraphicsUnit.Pixel)
            create x r
        )
        bmpAll
    Seq.chunkBySize (numWidth * numHeight) imgs
    |> Seq.map f

let drawed' flipByHor cellW cellH numWidth numHeight imgs dir fileName frm =
    let ext =
        if frm = Imaging.ImageFormat.Jpeg then ".jpeg"
        elif frm = Imaging.ImageFormat.Png then ".png"
        else failwithf "support only 'jpeg' or 'png' image format"

    let change =
        let path = System.IO.Path.Combine(dir, System.IO.Path.ChangeExtension(fileName, ext))
        fun i ->
            path
            |> Path.changeFileNameWithoutExt (fun s -> sprintf "%s%s" s i)
    drawed Color.Black flipByHor cellW cellH numWidth numHeight imgs
    |> List.ofSeq
    |> List.numerate (Some (fun _ -> ""))
    |> List.iter (fun (bmp, s) ->
        bmp.Save (change s, frm) )
