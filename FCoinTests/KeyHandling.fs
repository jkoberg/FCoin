module KeyHandling

open NUnit.Framework

module KeyHandlingTests = 
  open EcDsa.secp256k1
  open Conv.Bitcoin

  let compressedHex = ""


  let [<Test>] fromCompressed () =
    let privkey, pubkey, r = newKeypair()

    let uncompressed = toUncompressedPubKey pubkey
    let newpub2 = fromEncodedPubkey uncompressed
    Assert.AreEqual(pubkey, newpub2, "didn't encode or decode to uncompressed correctly")

    let compressed = toCompressedPubkey pubkey
    let newpub1 = fromEncodedPubkey compressed
    if newpub1 <> pubkey
      then Assert.Fail(sprintf "didn't decode %s\nto %s\ninstead got %s\n privkey %s"
                          (compressed |> Conv.Hex.fromBytes)
                          (pubkey |> toUncompressedPubKey |> Conv.Hex.fromBytes)
                          (newpub1 |> toUncompressedPubKey |> Conv.Hex.fromBytes)
                          (privkey |> Conv.UnsignedBig.toHex)

          )
      else Assert.Pass(sprintf "unpacked compressed pubkey %s to %s" (compressed |> Conv.Hex.fromBytes) (newpub1 |> toUncompressedPubKey |> Conv.Hex.fromBytes))



        
