#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

open Plotly.NET
open System

let drawChart time (series:(string * 'a) list) =
    let line (x:list<DateTime>) y name = 
        Chart.Line(x, y)
        |> Chart.withTraceName(Name=name)
        |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid)

    series 
    |> Seq.map (fun (name, data) -> line time data name)
    |> Chart.combine
    |> Chart.withXAxisStyle("Time")
    |> Chart.withYAxisStyle("Collateral")