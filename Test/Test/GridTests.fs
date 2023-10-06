module GridTests
open FsharpMyExtension
open Fuchu
open System.Drawing
open SimpleTable

module Data1 =
    let grid =
        {
            CellsMatrixSize =
                {
                    ColumnWidth = 10
                    ColumnsCount = 3
                    RowHeight = 20
                    RowsCount = 2
                }

            LineWidth = 1
            LineColor = Color.Red
        }

    let (gridWidth, gridHeight) as size = Grid.calcSize grid

module Data2 =
    let grid =
        {
            CellsMatrixSize =
                {
                    ColumnWidth = 15
                    ColumnsCount = 2
                    RowHeight = 30
                    RowsCount = 3
                }

            LineWidth = 2
            LineColor = Color.Red
        }

    let (gridWidth, gridHeight) as size = Grid.calcSize grid

[<Tests>]
let drawImagesOnGridTests =
    testList "drawImagesOnGridTests" [
        testCase "base 1" <| fun () ->
            let images =
                System.IO.Directory.GetFiles("mocks/1/numbers")
                |> Seq.map (fun path ->
                    let img = new Bitmap(path)
                    img, Rectangle(0, 0, img.Width, img.Height)
                )

            let act =
                use img =
                    Grid.drawImagesOnGrids
                        false
                        Data1.grid
                        images
                    |> Seq.exactlyOne
                img.Save("mocks/1/gridMockAct.png")
                Bitmap.toArray img

            let exp =
                use img = new Bitmap("mocks/1/gridMock.png")
                img |> Bitmap.toArray

            Assert.Equal("", exp, act)

        testCase "base 2" <| fun () ->
            let images =
                System.IO.Directory.GetFiles("mocks/2/numbers")
                |> Seq.map (fun path ->
                    let img = new Bitmap(path)
                    img, Rectangle(0, 0, img.Width, img.Height)
                )

            let act =
                use img =
                    Grid.drawImagesOnGrids
                        false
                        Data2.grid
                        images
                    |> Seq.exactlyOne

                Bitmap.toArray img

            let exp =
                use img = new Bitmap("mocks/2/gridMock.png")
                img |> Bitmap.toArray

            Assert.Equal("", exp, act)
    ]
