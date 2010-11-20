namespace Strangelights.FsViewModels
open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Net
open System.IO
open System.ComponentModel

module DataAccess =

    let djia =
        [ "3M", "MMM", "Diversified industrials"
          "Alcoa", "AA", "Aluminum"
          "American Express", "AXP", "Consumer finance"
          "AT&T", "T", "Telecommunication"
          "Bank of America", "BAC", "Institutional and retail banking"
          "Boeing", "BA", "Aerospace & defense"
          "Caterpillar", "CAT", "Construction and mining equipment"
          "Chevron Corporation", "CVX", "Oil and gas"
          "Citigroup", "C", "Banking"
          "Coca-Cola", "KO", "Beverages"
          "DuPont", "DD", "Commodity chemicals"
          "ExxonMobil", "XOM", "Integrated oil & gas"
          "General Electric", "GE", "Conglomerate"
          "General Motors", "GM", "Automobiles"
          "Hewlett-Packard", "HPQ", "Diversified computer systems"
          "Home Depot", "HD", "Home improvement retailers"
          "Intel", "INTC", "Semiconductors"
          "IBM", "IBM", "Computer services"
          "Johnson & Johnson", "JNJ", "Pharmaceuticals"
          "JPMorgan Chase", "JPM", "Banking"
          "Kraft Foods", "KFT", "Food processing"
          "McDonald's", "MCD", "Restaurants & bars"
          "Merck", "MRK", "Pharmaceuticals"
          "Microsoft", "MSFT", "Software"
          "Pfizer", "PFE", "Pharmaceuticals"
          "Procter & Gamble", "PG", "Non-durable household products"
          "United Technologies Corporation", "UTX", "Aerospace, heating/cooling, elevators"
          "Verizon Communications", "VZ", "Telecommunication"
          "Walmart", "WMT", "Broadline retailers"
          "Walt Disney", "DIS", "Broadcasting & entertainment" ]

    let baseUrl symb sMonth sDay sYear fMonth fDay fYear = 
        Printf.sprintf "http://ichart.finance.yahoo.com/table.csv?s=%s&a=%i&b=%i&c=%i&d=%i&e=%i&f=%i&g=d&ignore=.csv" 
            symb sMonth sDay sYear fMonth fDay fYear

    let getStockInfo symbol (start: DateTime) (finish: DateTime) =
        let parseStream data =
            new CsvReader<DateTime * float * float * float * float * int * float>(data, 1, "yyyy-MM-dd") :> seq<_>
        let wb = new WebClient()
        let url = baseUrl symbol (finish.Month - 1) finish.Day finish.Year (start.Month - 1) start.Day start.Year
        async { let! data = wb.AsyncDownloadString(new Uri(url))
                return parseStream data }

type Quote =
    { Date: DateTime
      Value: float }

type StockViewerViewModel() =
    let mutable isBusy = true
    let mutable selectedCompany = ""

    let dataDict = new Dictionary<string, ObservableCollection<Quote>>()
    let companies = new ObservableCollection<string>()

    do for (name, symbol, _) in DataAccess.djia do
        companies.Add(name)

    let cecEvent = new Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = cecEvent.Publish

    member x.TriggerPropertyChanged(name)=
        cecEvent.Trigger (x, new PropertyChangedEventArgs(name))
    member x.IsBusy
        with get() = isBusy
        and set value = 
            isBusy <- value
            x.TriggerPropertyChanged("IsBusy")
    member x.Companies = companies
    member x.SelectedCompany
        with get() = selectedCompany
        and set value = 
            selectedCompany <- value
            x.TriggerPropertyChanged("SelectedCompany")
            x.TriggerPropertyChanged("CurrentQuotes")

    member x.CurrentQuotes = 
        if dataDict.ContainsKey selectedCompany then dataDict.[selectedCompany] 
        else new ObservableCollection<_>()

    member x.OnLoaded() =
        let syncContext = System.Threading.SynchronizationContext()
        let workflows = DataAccess.djia |> Seq.map (fun (_, symbol, _) -> DataAccess.getStockInfo symbol DateTime.Now (DateTime.Now.AddDays(-28.)))
    
        let workflow =
            async { x.IsBusy <- true
                    let! data = Async.Parallel workflows
                    do! Async.SwitchToContext(syncContext)
                    let symbolData = Seq.zip DataAccess.djia data
                    for ((name, _, _), data) in symbolData do
                        let quotes = data |> Seq.map(fun (d, p, _, _, _, _, _) -> { Date = d; Value = p})
                        dataDict.Add(name, new ObservableCollection<Quote>(quotes))
                    do! Async.SwitchToContext(syncContext)
                    x.IsBusy <- false }
        Async.StartImmediate workflow
