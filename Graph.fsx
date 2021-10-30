#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

open Plotly.NET
open Plotly.NET.TraceObjects
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
    |> Chart.withYAxisStyle("Excess Collateral")

let drawSurface x y z =
    Trace3D.initMesh3d 
        (fun mesh3d ->
            mesh3d?x <- x
            mesh3d?y <- y
            mesh3d?z <- z
            mesh3d?flatshading <- false
            mesh3d?contour <- Contours.initXyz(Show=true)
            mesh3d
            )
    |> GenericChart.ofTraceObject 