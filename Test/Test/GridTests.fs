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
                    RowHeight = 20
                    RowsCount = 2
                    ColumnWidth = 10
                    ColumnsCount = 3
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
                    RowHeight = 30
                    RowsCount = 3
                    ColumnWidth = 15
                    ColumnsCount = 2
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
let getCellsFromGridTests =
    testList "getCellsFromGridTests" [
        testCase "grid.LineWidth = 1" <| fun () ->
            let cellsMatrixSize = Data1.grid.CellsMatrixSize
            let (columnWidthAct, rowHeigthAct) as columnRowAct, coordsAct =
                Grid.getCells
                    Data1.grid.LineWidth
                    (cellsMatrixSize.ColumnsCount, cellsMatrixSize.RowsCount)
                    Data1.size

            Assert.Equal("", (cellsMatrixSize.ColumnWidth, cellsMatrixSize.RowHeight), columnRowAct)

            let coordsExp =
                let create x y =
                    Rectangle(x, y, columnWidthAct, rowHeigthAct)
                [(0, create 1 1); (1, create 12 1); (2, create 23 1);
                 (3, create 1 22); (4, create 12 22); (5, create 23 22)]
                |> Map

            Assert.Equal("", coordsExp, coordsAct)

        testCase "grid.LineWidth = 2" <| fun () ->
            let cellsMatrixSize = Data2.grid.CellsMatrixSize
            let (columnWidthAct, rowHeigthAct) as columnRowAct, coordsAct =
                Grid.getCells
                    Data2.grid.LineWidth
                    (cellsMatrixSize.ColumnsCount, cellsMatrixSize.RowsCount)
                    Data2.size

            Assert.Equal("", (cellsMatrixSize.ColumnWidth, cellsMatrixSize.RowHeight), columnRowAct)

            let coordsExp =
                let create x y =
                    Rectangle(x, y, columnWidthAct, rowHeigthAct)
                [(0, create 2 2); (1, create 19 2);
                 (2, create 2 34); (3, create 19 34);
                 (4, create 2 66); (5, create 19 66)]
                |> Map

            Assert.Equal("", coordsExp, coordsAct)
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
