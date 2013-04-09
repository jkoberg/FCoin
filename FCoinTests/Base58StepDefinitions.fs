module Base58StepDefinitions

open TickSpec
open NUnit.Framework

let toUTF8 (s:string) = System.Text.Encoding.UTF8.GetBytes(s)
let fromUTF8 (b:byte[]) = System.Text.Encoding.UTF8.GetString(b)

module ``Encoding a byte array`` =
    let mutable version = 0x00uy;
    let mutable payload : byte[] = [||]
    let mutable encoded : string = ""

    let [<Given>] ``a version byte (.*) and payload "(.*)"`` (ver:byte, pyld:string) = 
        version <- ver
        payload <- pyld |> toUTF8
       
    let [<When>] ``base58check is applied`` () =  
        encoded <- Conv.Base58.toBase58check version payload

    let [<Then>] ``the result is "(.*)"`` (s:string) =
        let matched = encoded = s  
        Assert.True(matched, "Encoded base58check did not match example")
             

module ``Verifiying an encoded string`` =
    let mutable encstr = ""
    let mutable result : (byte * byte[]) option = None

    let [<Given>] ``a base8check string "(.*)"`` (b58:string) = 
        encstr <- b58
       
    let [<When>] ``the string is verified`` () =  
        result <- Conv.Base58.verifyBase58check encstr

    let [<Then>] ``it verifies and has version (.*) and payload "(.*)"`` (ver:byte, s:string) =
        match result with
        | None -> Assert.Fail("Couldn't decode base58check string")
        | Some (version, payload) -> 
            Assert.True((ver = version), "version mismatch")
            Assert.True((s = (fromUTF8 payload)), "payload mismatch")
             
    let [<Then>] ``no result is obtained`` () =
        Assert.True((result = None))
             
