namespace Conv

module Hex =
  open System.Runtime.Remoting.Metadata.W3cXsd2001
  let makeEvenLenHex (s:string) = 
    if s.Length % 2 = 1 then "0" + s
    else s
  let onlyPositive bi = if bi < 0I then failwith "No negative bigints permitted" else bi
  let fromBytes bytes = SoapHexBinary(bytes).ToString()
  let toBytes hexstr = SoapHexBinary.Parse(hexstr).Value
  let fromBigInt bi = (onlyPositive bi).ToString("X").TrimStart('0') |> makeEvenLenHex
  let toBigInt hexstr = bigint.Parse("0"+hexstr, System.Globalization.NumberStyles.HexNumber)

module Bytes =
  let fromHex = Hex.toBytes
  let toHex = Hex.fromBytes
  let fromBigInt = Hex.fromBigInt >> Hex.toBytes
  let toBigInt = Hex.fromBytes >> Hex.toBigInt

module UnsignedBig =
  let toBytes = Bytes.fromBigInt
  let fromBytes = Bytes.toBigInt
  let toHex = Hex.fromBigInt 
  let fromHex = Hex.toBigInt