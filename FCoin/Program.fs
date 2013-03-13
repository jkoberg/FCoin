// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Base58

[<EntryPoint>]
let main argv = 
    printfn "Base 58 string: %s" (Base58.toBase58check 0uy ("Hello there guys!"B |> Digest.sha256 |> Digest.ripemd160))
    
    printfn "Verify string: %A" (Base58.verifyBase58check "93VYUMzRG9DdbRP72uQXjaWibbQwygnvaCu9DumcqDjGybD864T")
    printfn "%A" argv
    0 // return an integer exit code
