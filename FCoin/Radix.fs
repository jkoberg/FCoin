module Radix

module BigEndian =
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

type Encoder(digits:string)  =
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

