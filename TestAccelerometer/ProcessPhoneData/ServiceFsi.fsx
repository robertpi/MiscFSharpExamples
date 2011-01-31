#I @"..\..\Dependencies\visifire_v3.6.8\WPF Binaries";; 
#r "System";;
#r "System.Xaml";;
#r "PresentationCore.dll";;
#r "PresentationFramework.dll";;
#r "WindowsBase.dll";;
#r "WPFToolkit.dll";;
#r "WPFVisifire.Charts.dll";;
#r "System.ServiceModel";;
#r "System.Configuration";;
#r "System.ServiceModel.Activation";;

open System
open System.Threading
open System.Configuration
open System.ServiceModel
open System.ServiceModel.Channels
open System.ServiceModel.Activation
open System.ServiceModel.Configuration
open System.ServiceModel.Description
open System.Runtime.Serialization
open Visifire.Charts
open System.Windows
open System.Windows.Controls

[<AutoOpen>]
module SynchronizationContextExt =
    type SynchronizationContext with
        /// A standard helper extension method to raise an event on the GUI thread
        member syncContext.RaiseEvent (event: Event<_>) args =
            syncContext.Post((fun _ -> event.Trigger args),state=null)
 
        /// A standard helper extension method to capture the current synchronization context.
        /// If none is present, use a context that executes work in the thread pool.
        static member CaptureCurrent () =
            match SynchronizationContext.Current with
            | null -> new SynchronizationContext()
            | ctxt -> ctxt

[<ServiceContract(Namespace = "")>]
[<AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)>]
type IReceiveData =
    [<OperationContract>]
    abstract ReceiveData: data:byte[] -> unit
    [<OperationContract>]
    abstract ReceivePoint: x:float -> y:float -> z:float -> unit
    [<OperationContract>]
    abstract ReceivePoints: xs:float[] -> ys:float[] -> zs:float[] ->  unit

let graphStackPanel = new StackPanel()
let window = new Window(Content = graphStackPanel)
window.Show()

let charts =
    [| for _ in 1 .. 3 do
        let chart = new Chart(Height = 300.)
        let yAxis = new Axis()
        yAxis.AxisMaximum <- 3.0
        yAxis.AxisMinimum <- -3.0
        chart.AxesY.Add(yAxis)

        let dataSeries = new DataSeries(RenderAs = RenderAs.Line);

        // Set DataSeries properties
        chart.Series.Add dataSeries

        graphStackPanel.Children.Add(chart) |> ignore
            
        yield chart |]

let addPointToChart (chart: Chart) ys =
    ys |> Seq.iteri (fun i y ->
        if i % 4 = 0 then
            let dataPoint = new DataPoint(YValue = y)
            chart.Series.[0].DataPoints.Add(dataPoint); 
            if (chart.Series.[0].DataPoints.Count > 120) then
                chart.Series.[0].DataPoints.RemoveAt(0))

let context = SynchronizationContext.CaptureCurrent()

type ReceiveDataService =
    new() = {}
    interface IReceiveData with
        member this.ReceiveData( data ) = ()
        member this.ReceivePoint x y z = 
            printfn "%f %f %f" x y z
        member this.ReceivePoints xs ys zs  =
            context.Post(new SendOrPostCallback(fun _ ->
                                    printfn "Entery gui context call back, threadid: %i" Thread.CurrentThread.ManagedThreadId
                                    addPointToChart charts.[0] xs
                                    addPointToChart charts.[1] ys
                                    addPointToChart charts.[2] zs
                                    printfn "Leaving gui context call back, threadid: %i" Thread.CurrentThread.ManagedThreadId), null)


let myServiceHost = 
    let baseAddress = new Uri("http://localhost:8080/service")

    let temp = new ServiceHost((typeof<ReceiveDataService>), baseAddress)


    let binding = new BasicHttpBinding(Name = "binding1")

    let hTransport = new HttpTransportBindingElement(MaxBufferSize = Int32.MaxValue,MaxReceivedMessageSize = int64 Int32.MaxValue);

    //Create the message encoding binding element - we'll specify binary encoding
    let binaryMessageEncoding = new BinaryMessageEncodingBindingElement();


    //Add the binding elements into a Custom Binding            
    let customBinding = new CustomBinding(binaryMessageEncoding, hTransport);

    let smb = new ServiceMetadataBehavior(HttpGetEnabled = true)
    temp.Description.Behaviors.Add(smb)
    temp.AddServiceEndpoint((typeof<IReceiveData>), customBinding, baseAddress) |> ignore;
    temp.AddServiceEndpoint(ServiceMetadataBehavior.MexContractName, MetadataExchangeBindings.CreateMexHttpBinding(), "mex") |> ignore;
    temp

myServiceHost.Open()

myServiceHost.Close()
