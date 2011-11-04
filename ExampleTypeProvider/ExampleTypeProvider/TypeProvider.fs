namespace ExampleTypeProvider.DesignTime

open System
open System.Collections.Generic
open System.Linq.Expressions
open System.Globalization
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Core.CompilerServices

[<assembly: TypeProviderAssembly>]
do()

[<AutoOpen>]
module Helper =
    let debug = true
    let trace() =
        if debug then
            let st = new StackTrace()
            let frame = st.GetFrame(1)
            let meth = frame.GetMethod()
            printfn "%s.%s" meth.DeclaringType.Name meth.Name
    let fulltrace() =
        if debug then
            let st = new StackTrace()
            printfn "%O" st

    let IsErased = 1073741824


type ProvidedType(name, ns, assembly) =
    inherit Type()
    let mutable attributes = (TypeAttributes.Public ||| TypeAttributes.Sealed ||| enum IsErased) 
    do trace() 
    override x.GetConstructorImpl(flags: BindingFlags, binder: Binder, cc: CallingConventions, paramTypes: System.Type[], modifiers: ParameterModifier[]) = 
        trace() 
        null
    override x.GetMethodImpl(name: System.String, flags: BindingFlags, binder: Binder, cc: CallingConventions, paramTypes: System.Type[], modifiers: ParameterModifier[]) =
        trace() 
        null
    override x.GetPropertyImpl(name: System.String, flags: BindingFlags, binder: Binder, t: System.Type, paramTypes: System.Type[], modifiers: ParameterModifier[]) =
        trace() 
        null
    override x.GetAttributeFlagsImpl() = 
        trace() 
        attributes
    override x.IsArrayImpl() = 
        trace() 
        false
    override x.IsByRefImpl() = 
        trace() 
        false
    override x.IsPointerImpl() = 
        trace() 
        false
    override x.IsPrimitiveImpl() = 
        trace() 
        false
    override x.IsCOMObjectImpl() = 
        trace() 
        false
    override x.HasElementTypeImpl() = 
        trace() 
        false
    override x.GetMembers(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GUID = 
        trace() 
        Guid.Empty
    override x.InvokeMember(name: System.String, flags: BindingFlags, binder: Binder, instance: System.Object, parameters: System.Object[], modifiers: ParameterModifier[], culture: CultureInfo, names: System.String[]) = 
        trace() 
        null
    override x.Module = 
        trace() 
        null: Module
    override x.Assembly = 
        trace() 
        assembly
    override x.FullName = 
        trace() 
        sprintf "%s.%s" ns name
    override x.Namespace = 
        trace() 
        ns
    override x.AssemblyQualifiedName = 
        trace() 
        sprintf "%s.%s, %s" ns name assembly.FullName
    override x.BaseType = 
        trace() 
        null
    override x.GetConstructors(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetMethods(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetField(name: System.String, flags: BindingFlags) = 
        trace() 
        null
    override x.GetFields(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetInterface(name: System.String, inherited: Boolean) = 
        trace() 
        null
    override x.GetInterfaces() = 
        trace() 
        [||]
    override x.GetEvent(name: System.String, flags: BindingFlags) = 
        trace() 
        null
    override x.GetEvents(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetProperties(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetNestedTypes(flags: BindingFlags) = 
        trace() 
        [||]
    override x.GetNestedType(name: System.String, flags: BindingFlags) = 
        trace() 
        null
    override x.GetElementType() = 
        trace() 
        null
    override x.UnderlyingSystemType = 
        trace() 
        null
    override x.Name = 
        trace() 
        name
    override x.GetCustomAttributes(inherited: Boolean) = 
        trace() 
        [||]
    override x.GetCustomAttributes(t: System.Type, inherited: Boolean) = 
        trace() 
        [||]
    override x.GetCustomAttributesData() = 
        trace() 
        new ResizeArray<_>() :> IList<_>
    override x.IsDefined(t: System.Type, inherited: Boolean) = 
        trace() 
        false
    override x.IsSecurityCritical
        with get() =
            trace() 
            false
    override x.IsSecuritySafeCritical
        with get() =
            trace() 
            false
    override x.IsSecurityTransparent
        with get() =
            trace() 
            false
    override x.GetDefaultMembers() =
        trace() 
        [||]
    override x.GetEnumValues() = 
        trace() 
        [||] :> Array
    member x.IsErased
        with get() = (attributes &&& enum IsErased) <> TypeAttributes.NotPublic
        and set value =
            if value then
                attributes <- attributes ||| (enum IsErased)
            else
                attributes <- attributes &&& ~~~ (enum IsErased)


type TypeProvidedNamespace(name, assembly) =
    do trace() 
    interface IProvidedNamespace with
        member x.NamespaceName 
            with get() = 
                trace() 
                name
        member x.GetNestedNamespaces() = 
            trace() 
            [||]
        member x.GetTypes() = 
            trace() 
            [|new ProvidedType("Atype", name, assembly, IsErased = true)|]

        member x.ResolveTypeName(typeName: string) = 
            trace() 
            null


[<TypeProvider>]
type TypeProviderRoot() =
    do trace() 
    let theAssembly = typeof<TypeProviderRoot>.Assembly
    let invalidate = new Event<EventHandler,EventArgs>()
    interface ITypeProvider with
        [<CLIEvent>]
        member x.Invalidate = invalidate.Publish
        member x.GetNamespaces() = 
            trace() 
            [|new TypeProvidedNamespace("ExampleTypeProvider", theAssembly)|] 
        member x.GetStaticParameters(typeWithoutArguments: Type) = 
            trace() 
            [||]
        member x.ApplyStaticArguments(typeWithoutArguments: Type, typeNameWithArguments: string,  staticArguments: obj[]) = 
            trace() 
            null: Type
        member x.GetInvokerExpression(syntheticMethodBase: MethodBase, parameters: ParameterExpression[]) = 
            trace() 
            null: Expression 
        member x.Dispose() = 
            trace() 
            ()
