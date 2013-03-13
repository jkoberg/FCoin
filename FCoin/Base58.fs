module Base58

let chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let encode = Radix.Encoder(chars)

let countLeading item = Seq.takeWhile ((=) item) >> Seq.length

let hash = Digest.sha256 >> Digest.sha256

let toBase58check (version:byte) (payload:byte[]) =
  let body = Array.append [|version|] payload
  let checksum = (hash body).[..3]
  let checksummed = Array.append body checksum
  let zeroCount = checksummed |> countLeading 0uy
  let strippedB58 = encode.FromBytes checksummed
  let encoded = new string(Array.create zeroCount encode.zeroDigit) + strippedB58
  encoded

let verifyBase58check (encoded:string) =
  let zeroCount = encoded |> countLeading encode.zeroDigit
  let strippedB58 = encoded.[zeroCount..]
  let stripped = encode.ToBytes strippedB58
  let checksummed = Array.append (Array.zeroCreate zeroCount) stripped
  let checksumidx = checksummed.Length - 4
  let body, checksum = checksummed.[..(checksumidx-1)], checksummed.[checksumidx..]
  if checksum <> (hash body).[..3] then
    None
  else 
    let version = body.[0]
    let payload = body.[1..]
    Some (version, payload)

let encodeAddress = Digest.sha256 >> Digest.ripemd160 >> toBase58check 0uy 
