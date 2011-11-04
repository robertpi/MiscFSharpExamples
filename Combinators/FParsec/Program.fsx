#if INTERACTIVE
#r "FParsec.dll"
#r "FParsecCS.dll"
#endif

open FParsec

// define a little function to excute our parse on a string and display the out put
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A %O" result (result.GetType())
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


// parse the numer 1
let one = pstring "1"

// test our one parser
test one "1"
// note: this fails
test one "0"

// parse zero
let zero = pstring "0"
// combine with one to parse zero or one
let zeroOrOne = zero <|> one 

// test our parse
test zeroOrOne "1"
test zeroOrOne "0"
// note: this only parse first value
test zeroOrOne "01"

// make a parse that will parse many zero and ones
let zerosAndOnes = many (zeroOrOne) 

// test our parser
test zerosAndOnes "01"
// note: this will work, but may not be desirable
test zerosAndOnes ""

// parse at least one zero or one
let zerosAndOnes' = many1 (zeroOrOne) 

// test our parser
test zerosAndOnes' "01"
// note: this will fail
test zerosAndOnes' ""

// parse any digit
let num = pstring "1" <|> pstring "2" <|> pstring "3" <|> 
          pstring "4" <|> pstring "5" <|> pstring "6" <|> 
          pstring "7" <|> pstring "8" <|> pstring "9" <|> 
          pstring "0"

// parse a list of digits
let nums = many1 num 

// test our parser
test nums "375993"

// alternative (better) implementation of "num" parser
let num' = satisfy isDigit

// test our parser, note: all first number parsed
test num' "375993"

// alternative (better) implementation of "nums" parser
let nums' = many1Satisfy isDigit

// test our parser
test nums' "375993"

// create a parser that parses a digit sequence and returns an int
let int' = many1Satisfy isDigit |>> int

// test our parser
test int' "375993"

// create a parser that parses a int between square brackets
let numBetweenBrackets = pstring "[" >>. int' .>> pstring "]"

// test our parser
test numBetweenBrackets "[375993]"

// create (and test) a parser that parses a sequence of 
// ints between square brackets
test (many numBetweenBrackets) "[375993][828295]"
