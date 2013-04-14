module SignatureTests

open NUnit.Framework

open Conv.Bytes
 

module SignatureTests = 
  open EcDsa.secp256k1

  let myPrivkey, myPubkey, r = newKeypair()
  let message = "Hello There!"B
  let hash = Crypto.sha256 message |> Conv.UnsignedBig.fromBytes

  let [<Test>] sign () =
    let r,s = sign myPrivkey hash
    match verify myPubkey hash (r,s) with
    | EcDsa.Error msg -> Assert.Fail(msg)
    | EcDsa.Valid payload -> Assert.Pass(sprintf "good payload of hash %A" (payload))
    

  let [<Test>] example1 () =
    let addr = "1joekaZPA1QZtg8kH99Fak4FP6JZ17nZi"
    let msg = "Hello There!"B
    let sigstr = "G9XC1ARSjDSXtXf2c1UDpWd+1ghkHPANnClKi83dIKNYBTxvrkDJEmnkLXWwrg9T9rClZztpQ89uH2Nn08c6BYQ="
    let sigbytes = System.Convert.FromBase64String sigstr
    None
