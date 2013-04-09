module EcDsa.bpP160r1

open EcDsa.Arith
open Convert.BigEndian

let size = 256
let a = fromHex "340E7BE2 A280EB74 E2BE61BA DA745D97 E8F7C300"
let b = fromHex "1E589A85 95423412 134FAA2D BDEC95C8 D8675E58"
let p = fromHex "E95E4A5F 737059DC 60DFC7AD 95B3D813 9515620F"
let G = Point(fromHex "BED5AF16 EA3F6A4F 62938C46 31EB5AF7 BDBCDBC3",
              fromHex "1667CB47 7A1A8EC3 38F94741 669C9763 16DA6321")
let n = fromHex "E95E4A5F 737059DC 60DF5991 D4502940 9E60FC09"
let h = 1I
let curve = {a=a; b=b; p=p}

let double = double curve
let add = add curve
let multiply = multiply curve
let onCurve = onCurve curve 
let getPubKey = getPubKey curve G
let newPrivKey () = newPrivKey n
