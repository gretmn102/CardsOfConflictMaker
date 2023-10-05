module GridTests
open FsharpMyExtension
open Fuchu
open System.Drawing

module Data1 =
    let gridEdgeColor = Color.Red
    let gridEdgeWidth = 1
    let cellWidth, cellHeight = 10, 20
    let columnsCount, rowsCount = 3, 2
    let gridWidth = Grid.Grid.calcLength gridEdgeWidth cellWidth columnsCount
    let gridHeight = Grid.Grid.calcLength gridEdgeWidth cellHeight rowsCount

module Data2 =
    let gridEdgeColor = Color.Red
    let gridEdgeWidth = 2
    let cellWidth, cellHeight = 15, 30
    let columnsCount, rowsCount = 2, 3
    let gridWidth = Grid.Grid.calcLength gridEdgeWidth cellWidth columnsCount
    let gridHeight = Grid.Grid.calcLength gridEdgeWidth cellHeight rowsCount

module Data4 =
    let gridEdgeColor = Color.Red
    let gridEdgeWidth = 4
    let cellWidth, cellHeight = 15, 30
    let columnsCount, rowsCount = 2, 3
    let gridWidth = Grid.Grid.calcLength gridEdgeWidth cellWidth columnsCount
    let gridHeight = Grid.Grid.calcLength gridEdgeWidth cellHeight rowsCount

[<Tests>]
let drawGridTests =
    testList "drawGridTests" [
        testCase "base" <| fun () ->
            let act =
                use image = new Bitmap(Data1.gridWidth, Data1.gridHeight)
                Grid.Grid.draw Data1.gridEdgeColor Data1.gridEdgeWidth (Data1.cellWidth, Data1.cellHeight) image
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/1/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)
        testCase "base 2" <| fun () ->
            let act =
                use image = new Bitmap(Data2.gridWidth, Data2.gridHeight)
                Grid.Grid.draw Data2.gridEdgeColor Data2.gridEdgeWidth (Data2.cellWidth, Data2.cellHeight) image
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/2/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)
        testCase "base 4" <| fun () ->
            let act =
                use image = new Bitmap(Data4.gridWidth, Data4.gridHeight)
                Grid.Grid.draw Data4.gridEdgeColor Data4.gridEdgeWidth (Data4.cellWidth, Data4.cellHeight) image
                Bitmap.toArray image

            let exp =
                use img = Bitmap.FromFile("mocks/4/emptyGridMock.png") :?> Bitmap
                Bitmap.toArray img

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let getCellsFromGridTests =
    testList "getCellsFromGridTests" [
        testCase "gridEdgeWidth = 1" <| fun () ->
            let (cellWidthAct, cellHeightAct), coordsAct =
                Grid.Grid.getCells Data1.gridEdgeWidth (Data1.columnsCount, Data1.rowsCount) (Data1.gridWidth, Data1.gridHeight)

            Assert.Equal("", Data1.cellWidth, cellWidthAct)
            Assert.Equal("", Data1.cellHeight, cellHeightAct)

            let coordsExp =
                let create x y =
                    Rectangle(x, y, cellWidthAct, cellHeightAct)
                [(0, create 1 1); (1, create 12 1); (2, create 23 1);
                 (3, create 1 22); (4, create 12 22); (5, create 23 22)]
                |> Map

            Assert.Equal("", coordsExp, coordsAct)

        testCase "gridEdgeWidth = 2" <| fun () ->
            let (cellWidthAct, cellHeightAct), coordsAct =
                Grid.Grid.getCells Data2.gridEdgeWidth (Data2.columnsCount, Data2.rowsCount) (Data2.gridWidth, Data2.gridHeight)

            Assert.Equal("", Data2.cellWidth, cellWidthAct)
            Assert.Equal("", Data2.cellHeight, cellHeightAct)

            let coordsExp =
                let create x y =
                    Rectangle(x, y, cellWidthAct, cellHeightAct)
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
                        Data1.gridEdgeColor
                        Data1.gridEdgeWidth
                        false
                        (Data1.cellWidth, Data1.cellHeight)
                        (Data1.columnsCount, Data1.rowsCount)
                        images
                    |> Seq.exactlyOne

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
                        Data2.gridEdgeColor
                        Data2.gridEdgeWidth
                        false
                        (Data2.cellWidth, Data2.cellHeight)
                        (Data2.columnsCount, Data2.rowsCount)
                        images
                    |> Seq.exactlyOne

                Bitmap.toArray img

            let exp =
                use img = new Bitmap("mocks/2/gridMock.png")
                img |> Bitmap.toArray

            Assert.Equal("", exp, act)
    ]
