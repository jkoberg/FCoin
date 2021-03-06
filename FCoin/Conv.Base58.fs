﻿module Conv.Base58

open Conv.Bytes

type RadixEncoder(digits:string)  =
  let radix = bigint digits.Length
  let zero = digits.[0]

  let rec toRadix (representation:char list) (num:bigint) =
    if num.IsZero then representation else
    let num, remainder = bigint.DivRem(num, radix)
    let nextdigit = digits.[int remainder]
    toRadix (nextdigit::representation) num

  let rec fromRadix (num:bigint) (representation:char list) = 
    match representation with
    | [] -> num
    | msd :: lsds -> 
      let digitvalue = digits.IndexOf msd
      if digitvalue < 0 then failwith (sprintf "Couldn't parse character '%c' in Base58 string" msd)
      else fromRadix ((num * radix) + (bigint digitvalue)) lsds
      
  let fromBigInt num = 
    new string(Array.ofList(toRadix [] num))
    
  let toBigInt (rep:string) =
    fromRadix (bigint 0) (List.ofSeq rep)
  
  member this.FromBytes (bytes:byte[]) =
    bytes |> UnsignedBig.fromBytes |> fromBigInt

  member this.ToBytes (repr:string) =
    repr |> toBigInt |> UnsignedBig.toBytes

  member this.zeroDigit = zero


let radix58 = RadixEncoder("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

let countLeading item = Seq.takeWhile ((=) item) >> Seq.length

let hashOf = Crypto.sha256 >> Crypto.sha256

let checksum x = (hashOf x).[..3]


let toBase58check (version:byte) (payload:byte[]) =
  let body = [|version|] ++ payload
  let checksummed = body ++ (checksum body)
  let zeroCount = checksummed |> countLeading 0uy
  let strippedB58 = radix58.FromBytes checksummed
  let zeros = new string(Array.create zeroCount radix58.zeroDigit) 
  zeros + strippedB58


let verify (encoded:string) =
  let zeroCount = encoded |> countLeading radix58.zeroDigit
  let strippedB58 = encoded.[zeroCount..]
  let checksummed = (Array.zeroCreate zeroCount) ++ (radix58.ToBytes strippedB58)
  let csumidx = checksummed.Length - 4
  let body, csum = checksummed.[..(csumidx-1)], checksummed.[csumidx..]
  if csum <> (checksum body) then None else
  let version = body.[0]
  let payload = body.[1..]
  Some (version, payload)
