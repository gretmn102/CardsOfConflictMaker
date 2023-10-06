module GridTests
open FsharpMyExtension
open Fuchu
open System.Drawing

open Grid

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
let drawGridTests =
    testList "drawGridTests" [
        testCase "base" <| fun () ->
            let act =
                use image = new Bitmap(Data1.gridWidth, Data1.gridHeight)
                Grid.draw image Data1.grid
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/1/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)

        testCase "base 2" <| fun () ->
            let act =
                use image = new Bitmap(Data2.gridWidth, Data2.gridHeight)
                Grid.draw image Data2.grid
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/2/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)

        testCase "base 4" <| fun () ->
            let act =
                let grid =
                    {
                        CellsMatrixSize =
                            {
                                RowHeight = 30
                                RowsCount = 3
                                ColumnWidth = 15
                                ColumnsCount = 2
                            }

                        LineWidth = 4
                        LineColor = Color.Red
                    }

                let gridWidth, gridHeight = Grid.calcSize grid

                use image = new Bitmap(gridWidth, gridHeight)
                Grid.draw image grid
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/4/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let generateCellsLocationsTests =
    testList "generateCellsLocationsTests" [
        testCase "grid.LineWidth = 1" <| fun () ->
            let act =
                Grid.generateCellsLocations Data1.grid
            let exp =
                [| 1; 12; 23 |], [| 1; 22 |]

            Assert.Equal("", act, exp)

        testCase "grid.LineWidth = 2" <| fun () ->
            let act =
                Grid.generateCellsLocations Data2.grid
            let exp =
                [| 2; 19 |], [| 2; 34; 66 |]

            Assert.Equal("", act, exp)
    ]

[<Tests>]
let calcColumnRowSizeTests =
    let test (grid: Grid) =
        let cellsMatrixSize = grid.CellsMatrixSize
        let columnRowSizeAct =
            Grid.calcColumnRowSize
                grid.LineWidth
                (cellsMatrixSize.ColumnsCount, cellsMatrixSize.RowsCount)
                (Grid.calcSize grid)

        Assert.Equal("", (cellsMatrixSize.ColumnWidth, cellsMatrixSize.RowHeight), columnRowSizeAct)

    testList "calcColumnRowSizeTests" [
        testCase "grid.LineWidth = 1" <| fun () ->
            test Data1.grid

        testCase "grid.LineWidth = 2" <| fun () ->
            test Data2.grid
    ]

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
