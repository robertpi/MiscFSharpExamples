// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.
open System;
open System.Reflection;

//typeof<Type>.GetMethods(BindingFlags.Public ||| BindingFlags.Instance) |> Seq.filter(fun x -> x.IsAbstract) |> Seq.iter (fun x -> printfn "%O" x)

#r "bin\Debug\ExampleTypeProvider.dll";;

type t = ExampleTypeProvider.Atype

