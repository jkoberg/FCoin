// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Conv
open Conv.Bytes

[<EntryPoint>]
let main argv =

    let step_1_privkey =
      // Read privkey from command line if present
      // User must paste in the previous run's hex privkey to redeem the solution
      if argv.Length > 0
      then UnsignedBig.fromHex argv.[0]
      else EcDsa.secp256k1.newPrivKey()

    printfn "Your step 1 private key: %s" (UnsignedBig.toHex step_1_privkey)

    let step_1_pubkey = EcDsa.secp256k1.getPubKey step_1_privkey

    let step_2_pubkey_for_pool = Conv.Bitcoin.toUncompressedPubKey step_1_pubkey

    printfn "Give pool this pubkey: %s" (Hex.fromBytes step_2_pubkey_for_pool)

    printfn "Paste back the solution: "
    let step_3_solution_privkey = UnsignedBig.fromHex (System.Console.ReadLine())

    let secret_vanity_privkey = (step_1_privkey + step_3_solution_privkey) % EcDsa.secp256k1.n
    let vanity_pubkey = EcDsa.secp256k1.getPubKey secret_vanity_privkey

    printfn "Your new private key: %s" (Bitcoin.toWalletImportFormat secret_vanity_privkey)
    printfn "And Address: %s" (Bitcoin.toAddressFormat vanity_pubkey)

    0 // program return code to OS
