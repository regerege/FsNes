// Learn more about F# at http://fsharp.org

open System
open System.Windows.Controls
open System.Windows.Media

[<STAThread>]
[<EntryPoint>]
let main argv =
    let app = new System.Windows.Application()
    let wpf = new System.Windows.Window()

    wpf.Title <- "てすと"
    let scaling n = float <| n * 3
    let parentCanv = new Canvas()
    parentCanv.Width <- scaling 256
    parentCanv.Height <- scaling 240
    let canv = [|
        for a in 1..256 do
            for b in 1..240 ->
                [|
                    let c = new Canvas()
                    c.Background <- new SolidColorBrush(Color.FromArgb(255uy, 0uy, byte(a), byte(b)))
                    c.Width <- scaling 1
                    c.Height <- scaling 1
                    Canvas.SetTop(c, scaling(b-1))
                    Canvas.SetLeft(c, scaling(a-1))
                    yield c
                |]
    |]
    canv |> Array.collect id |> Array.iter (parentCanv.Children.Add >> ignore)
    wpf.Content <- parentCanv
    

    app.Run(wpf) |> ignore

    printfn "Hello World from F#!"
    0 // return an integer exit code
