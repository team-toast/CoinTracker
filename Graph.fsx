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

let drawLine time (series:(string * 'a) list) =
    let line (x:list<'b>) y name = 
        Chart.Line(x, y)
        |> Chart.withTraceName(Name=name)
        |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid)

    series 
    |> Seq.map (fun (name, data) -> line time data name)
    |> Chart.combine
    |> Chart.withXAxisStyle("X")
    |> Chart.withYAxisStyle("Y")

let drawSurface x y surface =
    Chart.Surface(X=x, Y=y, zData=surface)

let drawSurfaceNoAxis surface =
    Chart.Surface(zData = surface)

let drawMesh x y z =
    Trace3D.initMesh3d 
        (fun mesh3d ->
            mesh3d?x <- x
            mesh3d?y <- y
            mesh3d?z <- z
            mesh3d?flatshading <- false
            mesh3d?colorscale <- StyleParam.Colorscale.Hot 
            mesh3d?opacity <- 0.75
            mesh3d?contour <- Contours.initXyz(Show=true)
            mesh3d
            )
    |> GenericChart.ofTraceObject 