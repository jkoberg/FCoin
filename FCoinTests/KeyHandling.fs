module KeyHandling

open NUnit.Framework

module KeyHandlingTests = 
  open Ecc.Curves
  open Conv.Bitcoin

  let compressedHex = ""


  let [<Test>] fromCompressed () =
    let privkey, pubkey, r = secp256k1.newKeypair false

    let uncompressed = Pubkey.toBytes pubkey
    let newpub2 = Pubkey.fromBytes uncompressed
    Assert.AreEqual(pubkey, newpub2, "didn't encode or decode to uncompressed correctly")

    
    let privkey, pubkey, r = secp256k1.newKeypair true

    let compressed = Pubkey.toBytes pubkey
    let newpub1 = Pubkey.fromBytes compressed
    if newpub1 <> pubkey
      then Assert.Fail(sprintf "didn't decode %s\nto %s\ninstead got %s\n privkey %s"
                          (compressed |> Conv.Hex.fromBytes)
                          (pubkey |> Pubkey.toHex)
                          (newpub1 |> Pubkey.toHex )
                          (privkey |> Privkey.toHex )
          )
      else Assert.Pass(sprintf "unpacked compressed pubkey %s to %s" (compressed |> Conv.Hex.fromBytes) (newpub1 |> Pubkey.toHex))



        
