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


