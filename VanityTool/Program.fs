// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Convert.BigEndian
open EcDsa.Arith

[<EntryPoint>]
let main argv = 
    let step_1_privkey = EcDsa.secp256k1.newPrivKey()
    let step_1_pubkey = EcDsa.secp256k1.getPubKey step_1_privkey 
    let step_2_pubkey_for_pool = 
      match step_1_pubkey with 
      | PointO -> failwith "Somehow got pointO"
      | Point(x,y) -> 
        Array.concat [
          [| Magic.EcDsaUncompressedPubKey |]
          x |> fromBigInt |> pad 32
          y |> fromBigInt |> pad 32
          ]

    printfn "Give pool this pubkey: %s" (step_2_pubkey_for_pool |> Conv.Hex.fromBytes)

    printfn "Paste back the solution: "
    let x = System.Console.ReadLine()
    let solution_privkey = Conv.UnsignedBig.fromHex x
    let new_privkey = step_1_privkey + solution_privkey % EcDsa.secp256k1.n
    printfn "Your new private key: %s" (Conv.Bitcoin.toWalletImportFormat secret_vanity_privkey)
    printfn "And Address: %s" (Conv.Bitcoin.toAddressFormat secret_vanity_privkey)

    0
