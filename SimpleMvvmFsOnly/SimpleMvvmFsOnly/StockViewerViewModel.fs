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

// A view model for a simple stock price history viewing app
type StockViewerViewModel() =
    // indicates if the screen is busy or not
    let mutable isBusy = true
    // contains the currect company
    let mutable selectedCompany = ""

    // diction of company name * quote data
    let dataDict = new Dictionary<string, ObservableCollection<Quote>>()
    // list of companies to drive drop down
    let companies = new ObservableCollection<string>()

    // prepopulate the list of companies and we know which ones we're fetching
    do for (name, symbol, _) in DataAccess.djia do
        companies.Add(name)

    // create an event object
    let cecEvent = new Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    // implement the interface INotifyPropertyChanged
    interface INotifyPropertyChanged with
        // PropertyChanged is raised each time a property changes, 
        // it's INotifyPropertyChanged's only memeber
        [<CLIEvent>]
        member x.PropertyChanged = cecEvent.Publish

    // this method provides a convient way of raising the PropertyChanged event
    member x.TriggerPropertyChanged(name)=
        cecEvent.Trigger (x, new PropertyChangedEventArgs(name))

    // exposes the busy indicator
    member x.IsBusy
        with get() = isBusy
        and set value = 
            isBusy <- value
            x.TriggerPropertyChanged("IsBusy")
    // exposes the list of companies
    member x.Companies = companies
    // will be bound to the combo box, so we can know which company's selected
    member x.SelectedCompany
        with get() = selectedCompany
        and set value = 
            selectedCompany <- value
            x.TriggerPropertyChanged("SelectedCompany")
            x.TriggerPropertyChanged("CurrentQuotes")
    // exposes the quote data of the currently selected company
    member x.CurrentQuotes = 
        if dataDict.ContainsKey selectedCompany then dataDict.[selectedCompany] 
        else new ObservableCollection<_>()

    // initalises the GUI, so should be called once it is loaded
    member x.OnLoaded() =
        // grab the synchronization context, this will be used later to call back to the GUI thread
        let syncContext = System.Threading.SynchronizationContext()

        // 
        let workflows = 
            DataAccess.djia 
            |> Seq.map (fun (_, symbol, _) ->
                // get stock infos is a little function that returns an async workflow 
                // that will connect to Yahoo! Finance return a list of dates and stock values
                DataAccess.getStockInfo symbol DateTime.Now (DateTime.Now.AddDays(-28.)))
    
        let workflow =
            async { // notify the gui that we're busy
                    x.IsBusy <- true
                    // make the async calls
                    let! data = Async.Parallel workflows
                    // switch back to the gui thread
                    do! Async.SwitchToContext(syncContext)
                    // write the data to the objects that are bound to the GUI
                    let symbolData = Seq.zip DataAccess.djia data
                    for ((name, _, _), data) in symbolData do
                        let quotes = data |> Seq.map(fun (d, p, _, _, _, _, _) -> { Date = d; Value = p})
                        dataDict.Add(name, new ObservableCollection<Quote>(quotes))
                    x.IsBusy <- false }
        // the start immediately will start the workflow on the gui thread meaning before the
        // first async call we can interact with gui objects
        Async.StartImmediate workflow
