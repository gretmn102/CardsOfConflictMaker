module Grid
open System.Drawing
open FsharpMyExtension

let generateCellCoords gridEdgeWidth cellsCount cellLength =
    List.unfold
        (fun (st, i) ->
            if i > 0 then
                let st' = st + cellLength - 1
                Some (st, (st' + 1 + gridEdgeWidth, i - 1))
            else
                None
        )
        (0, cellsCount)

let getCellLength gridEdgeWidth gridLength cellsCount =
    (gridLength - (cellsCount * gridEdgeWidth) - gridEdgeWidth) / cellsCount

/// returns `(cellWidth, cellHeight) * cellLocation [] []`
let getCellsFromGrid gridEdgeWidth (columnsCount, rowsCount) (imageWithGridWidth, imageWithGridHeight) =
    let cellWidth = getCellLength gridEdgeWidth imageWithGridWidth columnsCount
    let cellHeight = getCellLength gridEdgeWidth imageWithGridHeight rowsCount
    let columnLocations = generateCellCoords gridEdgeWidth columnsCount cellWidth
    let rowLocations = generateCellCoords gridEdgeWidth rowsCount cellHeight
    let cells =
        rowLocations
        |> List.fold
            (fun st y ->
                columnLocations
                |> List.fold
                    (fun (m, i) x ->
                        let r = Rectangle(x + gridEdgeWidth, y + gridEdgeWidth, cellWidth, cellHeight)
                        Map.add i r m, i + 1)
                    st
            )
            (Map.empty, 0)
    (cellWidth, cellHeight), fst cells

let getImagesFromGridAndSave gridEdgeWidth (columnsCount, rowsCount) (outputDir: string) (path: string) =
    use imageWithGrid = new Bitmap(path)
    let (cellWidth, cellHeight), m =
        getCellsFromGrid gridEdgeWidth (columnsCount, rowsCount) (imageWithGrid.Width, imageWithGrid.Height)

    let cell = Rectangle(0, 0, cellWidth, cellHeight)

    System.IO.Directory.CreateDirectory outputDir |> ignore

    m
    |> Seq.iter (fun (KeyValue(i, v)) ->
        use bmp = new Bitmap(cellWidth, cellHeight)
        use g = Graphics.FromImage bmp
        g.DrawImage(imageWithGrid, cell, v, GraphicsUnit.Pixel)
        let path = System.IO.Path.Combine(outputDir, sprintf "%d.png" i)
        bmp.Save(path)
    )

let getGridLength gridEdgeWidth cellLength cellsCount =
    cellLength * cellsCount + (cellsCount - 1) * gridEdgeWidth + gridEdgeWidth * 2

let drawGrid (gridColor: Color) gridEdgeWidth (cellWidth, cellHeight) (image: Bitmap) =
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
    let lines gridEdgeWidth cellWidth linesCount =
        Seq.unfold
            (fun (st, count) ->
                if count > 0 then
                    Some(st, (st + gridEdgeWidth + cellWidth, count - 1))
                else
                    None
            )
            (gridEdgeWidth / 2, linesCount)

    use g = Graphics.FromImage image
    use pen = new Pen(gridColor, float32 gridEdgeWidth)

    lines gridEdgeWidth cellWidth (image.Width / cellWidth + 1)
    |> Seq.iter (fun x -> g.DrawLine(pen, x, 0, x, image.Height))
    lines gridEdgeWidth cellHeight (image.Height / cellHeight + 1)
    |> Seq.iter (fun x -> g.DrawLine(pen, 0, x, image.Width, x))

let drawImagesOnGrids gridColor gridEdgeWidth flipByHor (cellWidth, cellHeight) (columnsCount, rowsCount) (imgs: (Bitmap * Rectangle) seq) =
    let columnLocations = generateCellCoords gridEdgeWidth columnsCount cellWidth |> List.map ((+) gridEdgeWidth)
    let rowLocations = generateCellCoords gridEdgeWidth rowsCount cellHeight |> List.map ((+) gridEdgeWidth)

    let cellLocations =
        let f =
            if flipByHor then
                fun y -> columnLocations |> List.map (fun x -> x, y) |> List.rev
            else
                fun y -> columnLocations |> List.map (fun x -> x, y)
        rowLocations |> List.collect f

    let drawImagesOnGrid srcImageAndRectangles =
        let gridWidth = getGridLength gridEdgeWidth cellWidth columnsCount
        let gridHeight = getGridLength gridEdgeWidth cellHeight rowsCount

        let imageGrid = new Bitmap(gridWidth, gridHeight)

        use g = Graphics.FromImage imageGrid

        drawGrid gridColor gridEdgeWidth (cellWidth, cellHeight) imageGrid

        Seq.zip cellLocations srcImageAndRectangles
        |> Seq.iter (fun ((cellX, cellY), (srcImage, srcRectangle)) ->
            g.DrawImage(srcImage,
                Rectangle(cellX, cellY, cellWidth, cellHeight),
                srcRectangle,
                GraphicsUnit.Pixel
            )
        )
        imageGrid

    Seq.chunkBySize (columnsCount * rowsCount) imgs
    |> Seq.map drawImagesOnGrid

let drawImagesOnGridsAndSave flipByHor gridColor (cellWidth, cellHeight) (numWidth, numHeight) outputDir fileName imageFormat imgs =
    let ext =
        if imageFormat = Imaging.ImageFormat.Jpeg then ".jpeg"
        elif imageFormat = Imaging.ImageFormat.Png then ".png"
        else failwithf "support only 'jpeg' or 'png' image format"

    let createPath =
        let path = System.IO.Path.Combine(outputDir, System.IO.Path.ChangeExtension(fileName, ext))
        fun i ->
            path
            |> Path.changeFileNameWithoutExt (fun s -> sprintf "%s%s" s i)

    drawImagesOnGrids Color.Black flipByHor gridColor (cellWidth, cellHeight) (numWidth, numHeight) imgs
    |> List.ofSeq
    |> List.numerate (Some (fun _ -> ""))
    |> List.iter (fun (bmp, s) ->
        bmp.Save (createPath s, imageFormat)
    )
