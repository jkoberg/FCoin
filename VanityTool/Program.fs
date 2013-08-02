open Conv
open Conv.Bytes
open Conv.Bitcoin
open Ecc.Curves

[<EntryPoint>]
let main argv =

    let step_1_privkey =
      if argv.Length > 0 then
        argv.[0] |> Privkey.fromWalletImportFormat
      else
        secp256k1.newPrivKey false

    printfn "Your step 1 private key: %s" (step_1_privkey |> Privkey.toHex)

    let step_1_pubkey = secp256k1.getPubKey step_1_privkey

    let step_2_pubkey_for_pool = Pubkey.toHex step_1_pubkey

    printfn "Give pool this pubkey: %s" step_2_pubkey_for_pool

    printfn "Paste back the solution: "
    let step_3_solution_privkey = UnsignedBig.fromHex (System.Console.ReadLine())

    let (cmp, privkey) = step_1_privkey
    let secret_vanity_privkey = cmp, (privkey + step_3_solution_privkey) % secp256k1.Curve.n
    let vanity_pubkey = secp256k1.getPubKey secret_vanity_privkey

    printfn "Your new private key: %s" (Privkey.toWalletImportFormat secret_vanity_privkey)
    
    printfn "And Address: %s" (Pubkey.toAddress vanity_pubkey)

    printfn "\nHit enter to continue."
    
    ignore (System.Console.ReadLine())

    0

