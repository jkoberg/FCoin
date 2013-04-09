module Convert


module BigEndian =
  let fromHex s = //bigint.Parse(s, System.Globalization.NumberStyles.HexNumber)
    let bytes = System.Runtime.Remoting.Metadata.W3cXsd2001.SoapHexBinary.Parse(s).Value
    let littleEndian = Array.append (Array.rev bytes) "\x00"B
    (bigint littleEndian)

  let toHex (bi:bigint) = bi.ToString("X")    

  let toBigInt bytes = 
    let littleEndian = Array.rev bytes
    let padded = Array.append littleEndian [|0uy|]
    bigint padded

  let fromBigInt (num:bigint) =
    num.ToByteArray() 
    |> Array.rev  
    |> Seq.skipWhile ((=) 0uy) 
    |> Array.ofSeq

  let pad len (arr:byte[]) = 
    let needed = len - arr.Length
    if needed < 1 then arr 
    else Array.append (Array.zeroCreate needed) arr

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
    bytes |> BigEndian.toBigInt |> fromBigInt
  
  member this.ToBytes (repr:string) =
    repr |> toBigInt |> BigEndian.fromBigInt

