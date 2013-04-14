module KeyHandling

open NUnit.Framework

module KeyHandlingTests = 
  open EcDsa.secp256k1
  open Conv.Bitcoin

  let compressedHex = ""


  let [<Test>] fromCompressed () =
    let privkey, pubkey, r = newKeypair false

    let uncompressed = encodePubkey pubkey
    let newpub2 = fromEncodedPubkey uncompressed
    Assert.AreEqual(pubkey, newpub2, "didn't encode or decode to uncompressed correctly")

    
    let privkey, pubkey, r = newKeypair true

    let compressed = encodePubkey pubkey
    let newpub1 = fromEncodedPubkey compressed
    if newpub1 <> pubkey
      then Assert.Fail(sprintf "didn't decode %s\nto %s\ninstead got %s\n privkey %s"
                          (compressed |> Conv.Hex.fromBytes)
                          (pubkey |> encodePubkey |> Conv.Hex.fromBytes)
                          (newpub1 |> encodePubkey |> Conv.Hex.fromBytes)
                          (privkey |> encodePrivKey |> Conv.Bytes.toHex)

          )
      else Assert.Pass(sprintf "unpacked compressed pubkey %s to %s" (compressed |> Conv.Hex.fromBytes) (newpub1 |> encodePubkey |> Conv.Hex.fromBytes))



        
