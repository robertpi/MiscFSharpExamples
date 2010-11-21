open System
open System.Reflection
open System.Windows
open System.Windows.Markup
open Strangelights.FsViewModels

let mainWindowStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("MainWindow.xaml")
let mainWindow = 
    let win = XamlReader.Load(mainWindowStream) :?> Window
    let viewModel = new StockViewerViewModel()
    win.DataContext <- viewModel
    win.Loaded.Add(fun _ -> viewModel.OnLoaded())
    win

let app = new Application()

[<STAThread>]
app.Run mainWindow |> ignore