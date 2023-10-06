module Grid
open System.Drawing
open FsharpMyExtension

type CellsMatrixSize =
    {
        ColumnsCount: int
        ColumnWidth: int
        RowsCount: int
        RowHeight: int
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
        Array.unfold
            (fun (st, i) ->
                if i > 0 then
                    Some (st + lineWidth, (st + cellLength + lineWidth, i - 1))
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
        columnLocations, rowLocations

    let calcSize (grid: Grid) =
        let calcLength lineWidth cellLength cellsCount =
            cellLength * cellsCount + (cellsCount - 1) * lineWidth + lineWidth * 2
        let cellsMatrixSize = grid.CellsMatrixSize
        let gridWidth =
            calcLength grid.LineWidth cellsMatrixSize.ColumnWidth cellsMatrixSize.ColumnsCount
        let gridHeight =
            calcLength grid.LineWidth cellsMatrixSize.RowHeight cellsMatrixSize.RowsCount
        gridWidth, gridHeight

    let calcColumnRowSize lineWidth (columnsCount, rowsCount) (gridWidth, gridHeight) =
        let calcCellLength lineWidth gridLength cellsCount =
            (gridLength - (cellsCount * lineWidth) - lineWidth) / cellsCount
        let rowHeight = calcCellLength lineWidth gridHeight rowsCount
        let columnWidth = calcCellLength lineWidth gridWidth columnsCount
        columnWidth, rowHeight

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

        lines grid.LineWidth cellsMatrixSize.ColumnWidth (cellsMatrixSize.ColumnsCount + 1)
        |> Seq.iter (fun x -> g.DrawLine(pen, x, 0, x, image.Height))
        lines grid.LineWidth cellsMatrixSize.RowHeight (cellsMatrixSize.RowsCount + 1)
        |> Seq.iter (fun x -> g.DrawLine(pen, 0, x, image.Width, x))

let getImagesFromGridAndSave gridLineWidth (columnsCount, rowsCount) (outputDir: string) (path: string) =
    use imageWithGrid = new Bitmap(path)

    let columnWidth, rowHeight =
        Grid.calcColumnRowSize gridLineWidth (columnsCount, rowsCount) (imageWithGrid.Width, imageWithGrid.Height)

    let grid =
        {
            CellsMatrixSize =
                {
                    ColumnWidth = columnWidth
                    ColumnsCount = columnsCount
                    RowHeight = rowHeight
                    RowsCount = rowsCount
                }
            LineWidth = gridLineWidth
            LineColor = Color.Black
        }

    let columnLocations, rowLocations = Grid.generateCellsLocations grid

    let cell = Rectangle(0, 0, columnWidth, rowHeight)

    System.IO.Directory.CreateDirectory outputDir |> ignore

    rowLocations
    |> Array.iteri (fun i y ->
        columnLocations
        |> Array.iteri (fun j x ->
            use bmp = new Bitmap(columnWidth, columnWidth)
            use g = Graphics.FromImage bmp
            let r = Rectangle(x, y, columnWidth, rowHeight)
            g.DrawImage(imageWithGrid, cell, r, GraphicsUnit.Pixel)
            let path = System.IO.Path.Combine(outputDir, sprintf "%d.png" (j + columnWidth * i))
            bmp.Save(path)
        )
    )

type CellsMatrix = (Bitmap * Rectangle) [,]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CellsMatrix =
    let draw (grid: Grid) dstImage (cellsMatrix: CellsMatrix) =
        let columnLocations, rowLocations = Grid.generateCellsLocations grid
        use g = Graphics.FromImage dstImage
        let cellsMatrixSize = grid.CellsMatrixSize
        cellsMatrix
        |> Array2D.iteri (fun y x (srcImage, srcRectangle) ->
            let cellX, cellY = columnLocations[x], rowLocations[y]
            g.DrawImage(srcImage,
                Rectangle(cellX, cellY, cellsMatrixSize.ColumnWidth, cellsMatrixSize.RowHeight),
                srcRectangle,
                GraphicsUnit.Pixel
            )
        )

let drawImagesOnGrids flipByHor (grid: Grid) (imgs: (Bitmap * Rectangle) seq) : Bitmap seq =
    let cellsMatrixSize = grid.CellsMatrixSize

    let drawImagesOnGrid (srcImageAndRectangles: (Bitmap * Rectangle) []) =
        let gridWidth, gridHeight = Grid.calcSize grid
        let imageGrid = new Bitmap(gridWidth, gridHeight)
        Grid.draw imageGrid grid
        let cellsMatrix =
            let arrayArray =
                srcImageAndRectangles
                |> Array.chunkBySize cellsMatrixSize.ColumnsCount

            let arrayArray =
                if flipByHor then
                    Array.map Array.rev arrayArray
                else
                    arrayArray

            arrayArray
            |> Array2D.ofArAr

        CellsMatrix.draw grid imageGrid cellsMatrix
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
