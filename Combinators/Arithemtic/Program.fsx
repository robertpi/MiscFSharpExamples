#load "FSharpChart.fsx"

type Expression =
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Constant of int
    | Parameter of string
    with
        static member (+) (x, y) = Add(x, y)
        static member (-) (x, y) = Subtract(x, y)
        static member (*) (x, y) = Multiply(x, y)

module NumericLiteralN = 
    let FromZero() = Constant 0
    let FromOne() = Constant 1
    let FromInt32 = Constant

let param = Parameter


module Example1 =
    let expr = (1N + 2N) * (5N - 2N)

module Example2 =
    let evaluateExpression parameters =
        let rec innerEval tree =
            match tree with
            | Multiply (x, y) -> innerEval x * innerEval y
            | Add (x, y) -> innerEval x + innerEval y
            | Subtract (x, y) -> innerEval x - innerEval y
            | Constant value -> value
            | Parameter key -> Map.find key parameters
        innerEval


    let expr = (1N + 2N) * (5N - 2N)

    evaluateExpression Map.empty expr

module Example3 =
    let rec simplifyExpression exp =
        let simpIfPoss op exp1 exp2 =
            let exp' = op (simplifyExpression exp1, 
                           simplifyExpression exp2)
            if exp' = exp then exp' else simplifyExpression exp'
        match exp with
        | Multiply(Constant 0, Constant _) -> Constant 0
        | Multiply(Constant _, Constant 0) -> Constant 0
        | Multiply(Constant n1, Constant n2) -> Constant (n1 * n2)
        | Add(Constant n1, Constant n2) -> Constant (n1 + n2)
        | Subtract(Constant n1, Constant n2) -> Constant (n1 - n2)
        | Multiply(exp1, exp2) -> simpIfPoss Multiply exp1 exp2
        | Add(exp1, exp2) -> simpIfPoss Add exp1 exp2
        | Subtract(exp1, exp2) -> simpIfPoss Add exp1 exp2
        | Constant _ | Parameter _ -> exp


module Example4 =
    open System
    open System.Numerics

    /// Given a list of parameters evaluate the tree
    let evaluateExpression parameters =
        let rec innerEval tree =
            match tree with
            | Multiply (x, y) -> innerEval x * innerEval y
            | Add (x, y) -> innerEval x + innerEval y
            | Subtract (x, y) -> innerEval x - innerEval y
            | Constant value -> value
            | Parameter key -> 
                if Map.containsKey key parameters then Map.find key parameters 
                else failwithf "Key not found: %s" key
        innerEval

    let simplifyExpression =
        let rec innerEval tree =
            match tree with
            | Multiply (Constant 0, _) -> Constant (0)
            | Multiply (_, Constant 0) -> Constant (0)
            | Multiply (Constant x, Constant y) -> Constant (x * y)
            | Add (Constant x, Constant y) -> Constant (x + y)
            | Subtract (Constant x, Constant y) -> Constant (x - y)
            | x -> x
        let rec loop tree =
            let tree' = innerEval tree
            if tree' = tree then
                tree
            else
                loop tree
        loop

    /// print the expression to the console
    let printExpression =
        let rec innerPrint ppf tree =
            match tree with
            | Multiply (x, y) -> Printf.fprintf ppf "(%a * %a)" innerPrint x innerPrint y
            | Add (x, y) -> Printf.fprintf ppf "(%a + %a)" innerPrint x innerPrint y
            | Subtract (x, y) -> Printf.fprintf ppf "(%a - %a)" innerPrint x innerPrint y
            | Constant value -> Printf.fprintf ppf "%i" value
            | Parameter pos -> Printf.fprintf ppf "(param %s)" pos
        innerPrint System.Console.Out

    let rand = new Random()

    /// build a random expression with limited depth, a maximum constants value,
    /// and a limited number of parameters
    let buildRandomExpression maxDepth maxConst parameters =
        let getRandParameter() = List.nth parameters (rand.Next parameters.Length)
        let rec innerBuild curDepth =
            if curDepth < maxDepth then
                let nextDepth = curDepth + 1
                match rand.Next(4) with
                | 0 -> Multiply (innerBuild nextDepth, innerBuild nextDepth)
                | 1 -> Add (innerBuild nextDepth, innerBuild nextDepth)
                | 2 -> Subtract (innerBuild nextDepth, innerBuild nextDepth) 
                | 3 -> Constant (rand.Next(maxConst))
                | 4 -> Parameter (getRandParameter())
                | _ -> failwith "assert false"
            else 
                match rand.Next(2) with
                | 0 -> Constant (rand.Next(maxConst))
                | 1 -> Parameter (getRandParameter())
                | _ -> failwith "assert false"
        innerBuild 0

    /// make a change to an existing tree by replace a node
    /// with a randomly generated tree
    let mutateExpression maxConst parameters rate =
        let rec innerMutate currDepth tree =
            let mutate node = 
                let newNode =
                    if rand.NextDouble() < rate then 
                        buildRandomExpression maxConst (currDepth + 1) parameters 
                    else node
                innerMutate (currDepth + 1) node
            match tree with
            | Multiply (x, y) -> Multiply (mutate x, mutate y)
            | Add (x, y) -> Add(mutate  x, mutate  y)
            | Subtract (x, y) -> Subtract (mutate  x, mutate  y)
            | Constant value -> Constant( value )
            | Parameter pos -> Parameter ( pos )
        innerMutate 0


    let (|Binary|Nullary|) = function 
        | Add(x,y) -> Binary((fun(x,y) -> Add(x,y)),x,y)
        | Subtract(x,y) -> Binary((fun(x,y) -> Subtract(x,y)),x,y)
        | Multiply(x,y) -> Binary((fun(x,y) -> Multiply(x,y)),x,y)
        | x -> Nullary(x)

    type HoleTree =
      | LeftHole of (Expression * Expression -> Expression) * HoleTree * Expression
      | RightHole of (Expression * Expression -> Expression) * Expression * HoleTree
      | Hole

    let rec plug = function
      | LeftHole(con,h,r),t -> con(plug(h,t), r)
      | RightHole(con,l,h),t -> con(l, plug(h,t))
      | Hole,t -> t


    let rec descendTree top p = function
      | Nullary(x) -> Hole, x
      | t when not top && rand.NextDouble() < p -> Hole, t
      | Binary(con,l,r) -> 
          if rand.NextDouble() < 0.5 then
            let h,t = descendTree false p l
            LeftHole(con,h,r),t
          else
            let h,t = descendTree false p r
            RightHole(con,l,h),t

    let crossOverExpressions p t1 t2 =
        let h,_ = descendTree true p t1
        let _,t = descendTree true p t2
        plug(h,t)

    type EvolutionData =
        { ScoreFunction: Expression -> BigInteger;
          MutRate: float
          CrossRate: float
          //BreedChance: float 
          Population: int 
          StudPopulation: int 
          MaxGen: int
          MaxDepth: int
          MaxConst: int 
          ParameterNames: list<string>
          ReceiveUpdate: Option<int*BigInteger*Expression -> unit> }
        with
            member x.NullHypeTest() =
                let initExpr = buildRandomExpression x.MaxDepth x.MaxConst x.ParameterNames
                let totalTest = x.Population * x.MaxGen
                // the inner loop which will handle each generation 
                let rec innerLoop currWinner currScore count =
        
                    // calculate score sort list to find the winner
                    let newExpre = buildRandomExpression x.MaxDepth x.MaxConst x.ParameterNames
                    let newScore = x.ScoreFunction newExpre

                    let winner, score =
                        if newScore < currScore then newExpre, newScore
                        else currWinner, currScore

            
                    // if we've found winner or reached the maxium gens return
                    if newScore = 0I || count = totalTest then
                        winner, score
                    else
                        // loop recursively
                        innerLoop winner score (count + 1)
                // start the loop
                innerLoop initExpr (x.ScoreFunction initExpr) 0
            member x.Evolve() =
                let initPop = List.init x.Population (fun _ -> buildRandomExpression x.MaxDepth x.MaxConst x.ParameterNames)

                // the inner loop which will handle each generation 
                let rec innerGenEvolve currPop currGen =
        
                    // calculate score sort list to find the winner
                    let res =
                        [ for expr in currPop ->
                            x.ScoreFunction expr, expr ]
                    let res = List.sortBy (fun (score,_) -> score) res
                    let score,winner = List.head res
                    
                    let invokeFunc() =
                        match x.ReceiveUpdate with
                        | Some func ->
                            func (currGen, score, winner)
                        | None -> ()
                    
                    invokeFunc()

                    // print the winner ... just for info
                    printfn "\nGen:%i score:%A" currGen score
                    printExpression winner
            
                    // if we've found winner or reached the maxium gens return
                    if score = 0I || currGen = x.MaxGen then
                        printfn "Winner! Gen:%i score:%A" currGen score 
                        winner, score
                    else
                        // get rid of scores, no longer needed
                        let res = List.map snd res
                
                        // always keep winner and second
                        let winner, second = 
                            match res with 
                            | winner :: second :: _ -> winner, second 
                            | _ -> failwith "assert false"
                        let newpop = winner :: second :: []
                
                        // select an expression no bias
                        let selectExpr() =
                            //let n =  int(log(rand.NextDouble()) / log(x.BreedChance))
                            //printfn "%i" n
                            let n =  rand.Next(x.Population - 1)
                            List.nth res n

                        let selectStudExpr() =
                            let n =  rand.Next(x.StudPopulation - 1)
                            List.nth res n
                
                        // loop to calculate the new population
                        let rec addExpress acc=
                            if  List.length acc = x.Population then
                                acc
                            else
                                // cross two expressions then mutate
                                let crossExpress = (crossOverExpressions x.CrossRate (selectStudExpr()) (selectExpr()))
                                let newExp = mutateExpression x.MaxConst x.ParameterNames x.MutRate crossExpress
                                addExpress (newExp :: acc)
                        
                        let newpop = addExpress newpop
                        // loop recursively
                        innerGenEvolve newpop (currGen + 1)
                // start the loop
                innerGenEvolve initPop 0

    // define a secret funtion we're trying to find
    let secertFunction = fun x y -> (x * x) + (2 * y) + (3 * x + 5)

    // calculate some data from the secret function
    let data = [ for x in [0 .. 200] -> 
                    let x = rand.Next(40)
                    let y = rand.Next(40)
                    (x,y), secertFunction x y ]

    // evaluate the an expression to see how close to the secret function it is
    let scoreFunction expr =
        let results =
            [ for (x,y),res in data ->
                let parameters = ["x", x; "y", y] |> Map.ofList
                res - evaluateExpression parameters expr ]
        results |> List.fold (fun acc x -> BigInteger.Abs (BigInteger x) + acc) 0I

    open System
    open System.Drawing
    open System.Windows.Forms
    open System.Windows.Forms.DataVisualization.Charting

    open MSDN.FSharp.Charting
    open MSDN.FSharp.Charting.ChartTypes

    let secertFunctionChart = 
        [ -200 .. 200 ] 
        |> List.map (fun x -> secertFunction x 0) 
        |> FSharpChart.Line 
        |> FSharpChart.WithTitle "Secert Function Chart"
        |> FSharpChart.Create

    let resultChart = 
        [ for x in  -200  .. 200 do yield x, 0 ]
        |> FSharpChart.Line 
        |> FSharpChart.WithTitle "Results Chart"
        |> FSharpChart.Create
    
    let chartExpress (currGen, score, expr) =
        let results =
            [ for x in -200 .. 200 ->
                let parameters = ["x", x; "y", 0] |> Map.ofList
                x, evaluateExpression parameters expr ]
        resultChart.SetData results
        Application.DoEvents()

    let evolutionData =
        { ScoreFunction = scoreFunction
          MutRate = 0.3
          CrossRate = 0.7
          //BreedChance = 0.7
          Population = 500
          StudPopulation = 50
          MaxGen = 100
          MaxDepth = 8
          MaxConst = 10
          ParameterNames = ["x"; "y"]
          ReceiveUpdate = None }
    
    evolutionData.Evolve()

    let avgScoreEvolve = List.init 10 (fun _ -> snd (evolutionData.Evolve()) |> float) |> Seq.average
    // val avgScoreEvolve : float = 2097.3
    let avgScoreNullHype = List.init 10 (fun _ -> snd (evolutionData.NullHypeTest()) |> float) |> Seq.average
    // val avgScoreNullHype : float = 7005.8
