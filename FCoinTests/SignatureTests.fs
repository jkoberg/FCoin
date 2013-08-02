module SignatureTests

open NUnit.Framework

open Conv.Bitcoin
open Bitcoin.Signature
open Conv.Bytes
open Crypto
open Ecc
open Ecc.Curves

module SignatureTests = 

  let myPrivkey, myPubkey, r = secp256k1.newKeypair false
  let message = "Hello There!"B
  let hash = message |> sha256 |> sha256 |> Conv.UnsignedBig.fromBytes

  let [<Test>] sign () =
    let _, privkey = myPrivkey
    let _, pubkey = myPubkey
    let r,s = secp256k1.ecdsa_sign privkey hash
    match secp256k1.ecdsa_verify pubkey hash (r,s) with
    | Error msg -> Assert.Fail(msg)
    | Valid payload -> Assert.Pass(sprintf "good payload of hash %A" (payload))

  let [<Test>] example1 () =
    let addr = "1joekaZPA1QZtg8kH99Fak4FP6JZ17nZi"
    let msg = "Hello There!"B
    let sigstr = "G9XC1ARSjDSXtXf2c1UDpWd+1ghkHPANnClKi83dIKNYBTxvrkDJEmnkLXWwrg9T9rClZztpQ89uH2Nn08c6BYQ="
    match verify addr msg sigstr with
    | Error err -> Assert.Fail(err)
    | Valid address -> Assert.AreEqual(addr, address)

  let [<Test>] example11 () =
    let addr = "1joekaZPA1QZtg8kH99Fak4FP6JZ17nZi"
    let msg = "Hello There!"B // was actually "zebras"
    let sigstr = "HLRtl5xKh8ipGQ7kIMM1OVzJeHvZwUhilNACGs+ZyWnlWC4DrZimxXfJktPXL7uC1MeX46HZVcIEfSeZYjmFVLc="
    match verify addr msg sigstr with
    | Error err -> Assert.Pass(err)
    | Valid address -> Assert.AreEqual(addr, address)

  let [<Test>] example2 () =
    let addr = "1joekaZPA1QZtg8kH99Fak4FP6JZ17nZi"
    let msg = "Hello Therd!"B
    let sigstr = "G9XC1ARSjDSXtXf2c1UDpWd+1ghkHPANnClKi83dIKNYBTxvrkDJEmnkLXWwrg9T9rClZztpQ89uH2Nn08c6BYQ="
    match verify addr msg sigstr with
    | Error err -> Assert.Pass(err)
    | Valid address -> Assert.AreNotEqual(addr, address, "Didn't properly fail sig")

  let [<Test>] example3 () =
    let msg = "test 12345"B
    let signature = create myPrivkey msg 
    let myAddr = Pubkey.toAddress myPubkey
    printfn "My address %s" myAddr
    printfn "message: test 12345"
    printfn "sig: %s" signature
    match verify myAddr msg signature with
    | Error err -> Assert.Fail(err)
    | Valid address ->
        Assert.AreEqual(myAddr, address, "Didn't properly read sig")
