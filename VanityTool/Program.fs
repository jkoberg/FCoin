open Conv
open Conv.Bytes
open EcDsa

[<EntryPoint>]
let main argv =

    let step_1_privkey =
      if argv.Length > 0 then
        UnsignedBig.fromHex argv.[0]
      else
        secp256k1.newPrivKey()

    printfn "Your step 1 private key: %s" (UnsignedBig.toHex step_1_privkey)

    let step_1_pubkey = secp256k1.getPubKey step_1_privkey

    let step_2_pubkey_for_pool = Bitcoin.toUncompressedPubKey step_1_pubkey

    printfn "Give pool this pubkey: %s" (Hex.fromBytes step_2_pubkey_for_pool)

    printfn "Paste back the solution: "
    let step_3_solution_privkey = UnsignedBig.fromHex (System.Console.ReadLine())

    let secret_vanity_privkey = (step_1_privkey + step_3_solution_privkey) % secp256k1.n
    let vanity_pubkey = secp256k1.getPubKey secret_vanity_privkey

    printfn "Your new private key: %s" (Bitcoin.toWalletImportFormat secret_vanity_privkey)
    
    printfn "And Address: %s" (Bitcoin.toAddressFormat vanity_pubkey)

    printfn "\nHit enter to continue."
    
    ignore (System.Console.ReadLine())

    0

