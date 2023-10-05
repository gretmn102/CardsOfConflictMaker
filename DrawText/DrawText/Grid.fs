module Grid
open System.Drawing
open FsharpMyExtension

type CellsMatrixSize =
    {
        RowsCount: int
        RowHeight: int
        ColumnsCount: int
        ColumnWidth: int
    }

type Grid =
    {
        CellsMatrixSize: CellsMatrixSize
        LineWidth: int
        LineColor: Color
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Grid =
    let generateCellLocations lineWidth cellsCount cellLength =
        List.unfold
            (fun (st, i) ->
                if i > 0 then
                    let st' = st + cellLength - 1
                    Some (st, (st' + 1 + lineWidth, i - 1))
                else
                    None
            )
            (0, cellsCount)

    let generateCellsLocations (grid: Grid) =
        let cellsMatrixSize = grid.CellsMatrixSize
        let columnLocations =
            generateCellLocations grid.LineWidth cellsMatrixSize.ColumnsCount cellsMatrixSize.ColumnWidth
        let rowLocations =
            generateCellLocations grid.LineWidth cellsMatrixSize.RowsCount cellsMatrixSize.RowHeight
        rowLocations, columnLocations

    let calcSize (grid: Grid) =
        let calcLength lineWidth cellLength cellsCount =
            cellLength * cellsCount + (cellsCount - 1) * lineWidth + lineWidth * 2
        let cellsMatrixSize = grid.CellsMatrixSize
        let gridWidth =
            calcLength grid.LineWidth cellsMatrixSize.ColumnWidth cellsMatrixSize.ColumnsCount
        let gridHeight =
            calcLength grid.LineWidth cellsMatrixSize.RowHeight cellsMatrixSize.RowsCount
        gridWidth, gridHeight

    let calcCellLength lineWidth gridLength cellsCount =
        (gridLength - (cellsCount * lineWidth) - lineWidth) / cellsCount

    /// returns `(cellWidth, cellHeight) * cellLocation [] []`
    let getCells lineWidth (columnsCount, rowsCount) (gridWidth, gridHeight) =
        let columnWidth = calcCellLength lineWidth gridWidth columnsCount
        let columnLocations = generateCellLocations lineWidth columnsCount columnWidth
        let rowHeight = calcCellLength lineWidth gridHeight rowsCount
        let rowLocations = generateCellLocations lineWidth rowsCount rowHeight
        let cells =
            rowLocations
            |> List.fold
                (fun st y ->
                    columnLocations
                    |> List.fold
                        (fun (m, i) x ->
                            let r = Rectangle(x + lineWidth, y + lineWidth, columnWidth, rowHeight)
                            Map.add i r m, i + 1)
                        st
                )
                (Map.empty, 0)
        (columnWidth, rowHeight), fst cells

    let draw (image: Bitmap) (grid: Grid) =
        let lines lineWidth cellWidth linesCount =
            Seq.unfold
                (fun (st, count) ->
                    if count > 0 then
                        Some(st, (st + lineWidth + cellWidth, count - 1))
                    else
                        None
                )
                (lineWidth / 2, linesCount)

        use g = Graphics.FromImage image
        use pen = new Pen(grid.LineColor, float32 grid.LineWidth)

        let cellsMatrixSize = grid.CellsMatrixSize

        lines grid.LineWidth cellsMatrixSize.RowHeight (cellsMatrixSize.RowsCount + 1)
        |> Seq.iter (fun x -> g.DrawLine(pen, 0, x, image.Width, x))
        lines grid.LineWidth cellsMatrixSize.ColumnWidth (cellsMatrixSize.ColumnsCount + 1)
        |> Seq.iter (fun x -> g.DrawLine(pen, x, 0, x, image.Height))

let getImagesFromGridAndSave gridLineWidth (columnsCount, rowsCount) (outputDir: string) (path: string) =
    use imageWithGrid = new Bitmap(path)
    let (cellWidth, cellHeight), m =
        Grid.getCells gridLineWidth (columnsCount, rowsCount) (imageWithGrid.Width, imageWithGrid.Height)

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

let drawImagesOnGrids flipByHor (grid: Grid) (imgs: (Bitmap * Rectangle) seq) =
    let rowLocations, columnLocations = Grid.generateCellsLocations grid

    let rowLocations, columnLocations =
        Pair.mapBoth (List.map ((+) grid.LineWidth)) (rowLocations, columnLocations)

    let cellLocations =
        let f =
            if flipByHor then
                fun y -> columnLocations |> List.map (fun x -> x, y) |> List.rev
            else
                fun y -> columnLocations |> List.map (fun x -> x, y)
        rowLocations |> List.collect f

    let cellsMatrixSize = grid.CellsMatrixSize

    let drawImagesOnGrid srcImageAndRectangles =
        let gridWidth, gridHeight = Grid.calcSize grid

        let imageGrid = new Bitmap(gridWidth, gridHeight)

        use g = Graphics.FromImage imageGrid

        Grid.draw imageGrid grid

        Seq.zip cellLocations srcImageAndRectangles
        |> Seq.iter (fun ((cellX, cellY), (srcImage, srcRectangle)) ->
            g.DrawImage(srcImage,
                Rectangle(cellX, cellY, cellsMatrixSize.ColumnWidth, cellsMatrixSize.RowHeight),
                srcRectangle,
                GraphicsUnit.Pixel
            )
        )
        imageGrid

    Seq.chunkBySize (cellsMatrixSize.ColumnsCount * cellsMatrixSize.RowsCount) imgs
    |> Seq.map drawImagesOnGrid

let drawImagesOnGridsAndSave flipByHor (grid: Grid) outputDir fileName imageFormat imgs =
    let ext =
        if imageFormat = Imaging.ImageFormat.Jpeg then ".jpeg"
        elif imageFormat = Imaging.ImageFormat.Png then ".png"
        else failwithf "support only 'jpeg' or 'png' image format"

    let createPath =
        let path = System.IO.Path.Combine(outputDir, System.IO.Path.ChangeExtension(fileName, ext))
        fun i ->
            path
            |> Path.changeFileNameWithoutExt (fun s -> sprintf "%s%s" s i)

    drawImagesOnGrids flipByHor grid imgs
    |> List.ofSeq
    |> List.numerate (Some (fun _ -> ""))
    |> List.iter (fun (bmp, s) ->
        bmp.Save (createPath s, imageFormat)
    )
