namespace WindowsPhoneApp

open System
open System.Net
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Ink
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Animation
open System.Windows.Shapes
open System.Windows.Navigation
open Microsoft.Phone.Controls
open Microsoft.Phone.Shell
open Microsoft.Devices.Sensors
open Visifire.Charts

//#nowarn "40"
[<AutoOpen>]
module private Utilities = 
    /// This is an implementation of the dynamic lookup operator for binding
    /// Xaml objects by name.
    let (?) (source:obj) (s:string) =
        match source with 
        | :? ResourceDictionary as r ->  r.[s] :?> 'T
        | :? Control as source -> 
            match source.FindName(s) with 
            | null -> invalidOp (sprintf "dynamic lookup of Xaml component %s failed" s)
            | :? 'T as x -> x
            | _ -> invalidOp (sprintf "dynamic lookup of Xaml component %s failed because the component found was of type %A instead of type %A"  s (s.GetType()) typeof<'T>)
        | _ -> invalidOp (sprintf "dynamic lookup of Xaml component %s failed because the source object was of type %A. It must be a control or a resource dictionary" s (source.GetType()))

module Acc =
    let accMeter = new Accelerometer()
    

/// This type implements the main page of the application
type MainPage() as this =
    inherit PhoneApplicationPage()

    // Load the Xaml for the page.
    do Application.LoadComponent(this, new System.Uri("/WindowsPhoneApp;component/MainPage.xaml", System.UriKind.Relative))

    // Bind named Xaml components relevant to this page.
    let results: TextBlock = this?Results
    let xTextBlock: TextBlock = this?X
    let yTextBlock: TextBlock = this?Y
    let zTextBlock: TextBlock = this?Z

    let graphStackPanel: StackPanel = this?GraphStackPanel

    let start = DateTime.Now
    let rand = new Random()
    let charts =
        [| for _ in 1 .. 3 do
            let chart = new Chart(Width = 480., Height = 160.)
            let axis = new Axis(IntervalType = IntervalTypes.Seconds);
            chart.AxesX.Add(axis)

            let dataSeries = new DataSeries();

            // Set DataSeries properties
            dataSeries.RenderAs <- RenderAs.Line;
            //dataSeries.XValueType <- ChartValueTypes.DateTime;
//            for x in 0 .. 10 do
//                let dataPoint = new DataPoint();
//                dataPoint.XValue <- x
//                dataPoint.YValue <- float (rand.Next(10, 100));
//                dataSeries.DataPoints.Add(dataPoint);

            chart.Series.Add dataSeries

            graphStackPanel.Children.Add(chart) 
            
            yield chart |]

    let addPointToChart (chart: Chart) y =
        let dataPoint = new DataPoint(YValue = y)
        chart.Series.[0].DataPoints.Add(dataPoint); // Changing the dataPoint YValue at runtime
        if (chart.Series.[0].DataPoints.Count > 20) then
            chart.Series.[0].DataPoints.RemoveAt(0);
    let receiveDataService = new Proxy.ServiceReference1.ReceiveDataClient()
    let every n (ev:IEvent<_>) =
       let out = new Event<_>()
       let count = ref 0
       ev.Add (fun arg -> incr count; if !count % n = 0 then out.Trigger arg)
       out.Publish
    let loaded _ =
        let doUpdateBoxes (x, y, z) =
            xTextBlock.Text <- x.ToString()
            yTextBlock.Text <- y.ToString()
            zTextBlock.Text <- z.ToString()
        let doUpdateChart (x, y, z) =
            addPointToChart charts.[0] x
            addPointToChart charts.[1] y
            addPointToChart charts.[2] z
        let doSendExternalMessage (xs, ys, zs) =
            try
                let points = new Proxy.ServiceReference1.ReceivePointsRequest(xs, ys, zs)
                receiveDataService.ReceivePointsAsync(points)
            with _ -> () // TODO ignore failure for now, need to signal this in UI

        let mappedEvent =
            Acc.accMeter.ReadingChanged
                |> Event.map(fun ea -> ea.X, ea.Y, ea.Z)

        let doInvoke func (x, y, z) = this.Dispatcher.BeginInvoke(new Action(fun _ -> func (x, y, z))) |> ignore

        do mappedEvent |> Event.add(doInvoke(doUpdateBoxes))

        do mappedEvent
            |> every 50
            |> Event.add (doInvoke(doUpdateBoxes))

        let updateFreq = 100
        do mappedEvent
            |> Event.scan (fun (xs:List<_>, ys, zs) (x, y, z) ->  if xs.Length <= updateFreq then x::xs, y::ys, z::zs else [x], [y], [z]) ([], [], [])
            |> Event.filter (fun (xs:List<_>, _, _) -> xs.Length = updateFreq)
            |> Event.add (fun (xs, ys, zs) -> doSendExternalMessage (Array.ofList xs, Array.ofList ys, Array.ofList zs))

    do this.Loaded.Add loaded
    

/// One instance of this type is created in the application host project.
type App(app:Application) = 
    // Global handler for uncaught exceptions. 
    // Note that exceptions thrown by ApplicationBarItem.Click will not get caught here.
    do app.UnhandledException.Add(fun e -> 
            if (System.Diagnostics.Debugger.IsAttached) then
                // An unhandled exception has occurred, break into the debugger
                System.Diagnostics.Debugger.Break();
     )

    let rootFrame = new PhoneApplicationFrame();

    do app.RootVisual <- rootFrame;

    // Handle navigation failures
    do rootFrame.NavigationFailed.Add(fun _ -> 
        if (System.Diagnostics.Debugger.IsAttached) then
            // A navigation has failed; break into the debugger
            System.Diagnostics.Debugger.Break())

    // Navigate to the main page 
    do rootFrame.Navigate(new Uri("/WindowsPhoneApp;component/MainPage.xaml", UriKind.Relative)) |> ignore

    // Required object that handles lifetime events for the application
    let service = PhoneApplicationService()
    // Code to execute when the application is launching (eg, from Start)
    // This code will not execute when the application is reactivated
    do service.Launching.Add(fun _ -> Acc.accMeter.Start())
    // Code to execute when the application is closing (eg, user hit Back)
    // This code will not execute when the application is deactivated
    do service.Closing.Add(fun _ -> Acc.accMeter.Stop())
    // Code to execute when the application is activated (brought to foreground)
    // This code will not execute when the application is first launched
    do service.Activated.Add(fun _ -> Acc.accMeter.Start())
    // Code to execute when the application is deactivated (sent to background)
    // This code will not execute when the application is closing
    do service.Deactivated.Add(fun _ -> Acc.accMeter.Stop())

    do app.ApplicationLifetimeObjects.Add(service) |> ignore
