open System
open System.Drawing
open System.Diagnostics
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type chartControl(area1 : ChartArea, area2 : ChartArea, s : Series, s' : Series) = 
    let mutable colC = Color.LightBlue
    let mutable colM = Color.LightBlue
    let mutable linesC = ChartDashStyle.Dot
    let mutable linesM = ChartDashStyle.Dot
    let mutable serCPUGraph = Color.RoyalBlue
    let mutable serMemGraph = Color.RoyalBlue
    member this.CPULinesColor
        with get () = colC
        and set (value) = colC <- value
                          area1.AxisX.MajorGrid.LineColor <- colC
                          area1.AxisY.MajorGrid.LineColor <- colC
    member this.CPULinesStyle
        with get () = linesC
        and set (value) = linesC <- value
                          area1.AxisX.MajorGrid.LineDashStyle <- linesC
                          area1.AxisY.MajorGrid.LineDashStyle <- linesC
    member this.MemLinesStyle
        with get () = linesM
        and set (value) = linesM <- value
                          area2.AxisX.MajorGrid.LineDashStyle <- linesM
                          area2.AxisY.MajorGrid.LineDashStyle <- linesM
    member this.MemLinesColor
        with get () = colM
        and set (value) = colM <- value
                          area2.AxisX.MajorGrid.LineColor <- colM
                          area2.AxisY.MajorGrid.LineColor <- colM
    member this.CPUGraphColor
        with get () = serCPUGraph
        and set (value) = serCPUGraph <- value; s.Color <- serCPUGraph
    member this.MemGraphColor
        with get () = serMemGraph
        and set (value) = serMemGraph <- value; s'.Color <- serMemGraph

Application.EnableVisualStyles()

let getCpuUsage = 
    let counter = 
        new PerformanceCounter(CounterName = "% Processor Time", 
                               CategoryName = "Processor", 
                               InstanceName = "_Total")
    (fun () -> counter.NextValue())

let getFreeMem = 
    let counter = 
        new PerformanceCounter("Memory", "Available MBytes")
    (fun () -> counter.NextValue())

let form = new Form(Visible = true, 
                        Size = new Size (800, 600))
let properties = new PropertyGrid(Dock = DockStyle.Right,
                                  Size = new Size (250, 30))
let chart = new Chart(Dock = DockStyle.Fill,
                      Palette = ChartColorPalette.Excel,
                      BackColor = Color.WhiteSmoke)
let areaC = new ChartArea("CPU")
let areaM = new ChartArea("Memory")

for area in [areaC; areaM] do
    area.BackColor <- Color.White
    for axis in [area.AxisX; area.AxisY] do 
        axis.MajorGrid.LineDashStyle <- ChartDashStyle.Dot
        axis.MajorGrid.LineColor <- Color.LightBlue
    area.AxisX.Interval <- 5.
    area.AxisX.Maximum <- 60.
    area.AxisX.Title <- "Time"
    if area = areaC then 
        area.AxisY.Interval <- 20.
        area.AxisY.Maximum <- 100.
        area.AxisY.Title <- "CPU Usage, %"
    else
        area.AxisY.Interval <- 500.
        area.AxisY.Maximum <- 9000.
        area.AxisY.Title <- "Free memory, MB"        

chart.ChartAreas.Add(areaC)
chart.ChartAreas.Add(areaM)

let addSeries typ  =
    let series = new Series(ChartType = typ)
    chart.Series.Add(series)
    series

let series = addSeries SeriesChartType.FastLine
series.ChartArea <- "CPU"
series.Color <- Color.RoyalBlue

let series' = addSeries SeriesChartType.FastLine
series'.ChartArea <- "Memory"
series'.Color <- Color.RoyalBlue

let updateCPULoop = async { 
    while not chart.IsDisposed do
        let v = float (getCpuUsage())
        series.Points.Add(v) |> ignore
        if series.Points.Count >= 60 then
            series.Points.RemoveAt 0
        do! Async.Sleep(250) 
    }

let updateMemLoop = async { 
    while not chart.IsDisposed do
        let v = float (getFreeMem())
        series'.Points.Add(v) |> ignore
        if series'.Points.Count >= 60 then
            series'.Points.RemoveAt 0
        do! Async.Sleep(250) 
    }

let start =     
    Async.StartImmediate updateCPULoop
    Async.StartImmediate updateMemLoop

let ctrl = new chartControl(areaC, areaM, series, series')
properties.SelectedObject <- ctrl 

form.Controls.Add chart
form.Controls.Add properties
form.Show()
Application.Run(form)
