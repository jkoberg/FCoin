module Conv.Base58

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
      if digitvalue < 0 then 
        failwith (sprintf "Couldn't parse character '%c' in Base58 string" msd)
      else 
        fromRadix ((num * radix) + (bigint digitvalue)) lsds
      
  let fromBigInt num = 
    new string(Array.ofList(toRadix [] num))
    
  let toBigInt (rep:string) =
    fromRadix (bigint 0) (List.ofSeq rep)

  member this.zeroDigit = zero

  member this.FromBytes (bytes:byte[]) =
    bytes |> Convert.BigEndian.toBigInt |> fromBigInt
  
  member this.ToBytes (repr:string) =
    repr |> toBigInt |> Convert.BigEndian.fromBigInt


let chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let encode = RadixEncoder(chars)
let countLeading item = Seq.takeWhile ((=) item) >> Seq.length
let hashOf = Digest.sha256 >> Digest.sha256

let toBase58check (version:byte) (payload:byte[]) =
  let body = Array.append [|version|] payload
  let checksum = (hashOf body).[..3]
  let withchecksum = Array.append body checksum
  let zeroCount = withchecksum |> countLeading 0uy
  let strippedB58 = encode.FromBytes withchecksum
  let encoded = new string(Array.create zeroCount encode.zeroDigit) + strippedB58
  encoded

let verifyBase58check (encoded:string) =
  let zeroCount = encoded |> countLeading encode.zeroDigit
  let strippedB58 = encoded.[zeroCount..]
  let stripped = encode.ToBytes strippedB58
  let withchecksum = Array.append (Array.zeroCreate zeroCount) stripped
  let checksumidx = withchecksum.Length - 4
  let body, checksum = withchecksum.[..(checksumidx-1)], withchecksum.[checksumidx..]
  if checksum <> (hashOf body).[..3] then
    None
  else 
    let version = body.[0]
    let payload = body.[1..]
    Some (version, payload)

