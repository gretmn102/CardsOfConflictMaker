module Grid
open System.Drawing
open FsharpMyExtension
open SimpleTable

let getImagesFromGridAndSave gridLineWidth (columnsCount, rowsCount) (outputDir: string) (path: string) =
    use imageWithGrid = new Bitmap(path)
    let grid, cellsMatrix =
        CellsMatrix.ofImage
            gridLineWidth
            Color.Black
            (columnsCount, rowsCount)
            imageWithGrid

    System.IO.Directory.CreateDirectory outputDir |> ignore

    let columnWidth, rowHeight =
        grid.CellsMatrixSize.ColumnWidth, grid.CellsMatrixSize.RowHeight

    let dstRect = Rectangle(0, 0, columnWidth, rowHeight)

    cellsMatrix
    |> Array2D.iteri (fun i j (srcBitmap, srcRect) ->
        use bmp = new Bitmap(columnWidth, rowHeight)
        use g = Graphics.FromImage bmp
        g.DrawImage(srcBitmap, dstRect, srcRect, GraphicsUnit.Pixel)
        let path = System.IO.Path.Combine(outputDir, sprintf "%d.png" (j + columnWidth * i))
        bmp.Save(path)
    )

let drawImagesOnGrids flipByHor (grid: Grid) (imgs: (Bitmap * Rectangle) seq) : Bitmap seq =
    let cellsMatrixSize = grid.CellsMatrixSize

    let drawImagesOnGrid (srcImageAndRectangles: (Bitmap * Rectangle) []) =
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

        let gridWidth, gridHeight = Grid.calcSize grid
        let imageGrid = new Bitmap(gridWidth, gridHeight)

        Table.draw imageGrid grid cellsMatrix

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
