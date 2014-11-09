open System.Drawing
open System.Diagnostics
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

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

let mainForm = new Form(Visible = true, 
                        Size = new Size (800, 600))
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

mainForm.Controls.Add(chart)
mainForm.Show()
Application.Run(mainForm)
