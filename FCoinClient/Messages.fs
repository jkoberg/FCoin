module Messages


type MagicValues =
  | main = 0xD9B4BEF9
  | testnet = 0xDAB5BFFA
  | testnet3 = 0x0709110B

let longToBytes (l:uint64) = 
   use ms = new System.IO.MemoryStream()
   let writer = new System.IO.BinaryWriter(ms)
   writer.Write(l)
   ms.ToArray()

let bytesToLong (bs:byte[]) =
   let bs = Array.append bs (Array.zeroCreate 8)
   use ms = new System.IO.MemoryStream(bs)
   let reader = new System.IO.BinaryReader(ms)
   reader.ReadUInt64()

module VarInt =
  open Conv.Bytes

  type VarLenInt = byte[]

  let encode num : VarLenInt =
      if num < 0xfdUL then (longToBytes num).[..0] else
      if num < 0xffffUL then (longToBytes num).[..1] else
      if num < 0xffffffffUL then (longToBytes num).[..3] else
      (longToBytes num).[..7]

  let decode (bs:byte[]) = 
    if bs.[0] < 0xfduy then uint64 bs.[0], bs.[1..] else
    if bs.[0] = 0xfduy then bytesToLong bs.[..1], bs.[2..] else
    if bs.[0] = 0xfeuy then bytesToLong bs.[..3], bs.[4..] else
    bytesToLong bs.[..7], bs.[8..]


module VarStr =
  type VarLenStr = byte[]

  let encode (bs:byte[]) : VarLenStr = 
     Array.append (VarInt.encode(uint64 bs.Length)) bs

  let decode (input:byte[]) =
     let len, remain = VarInt.decode input
     let len = int len
     remain.[..len-1], remain.[len..]

type Message = {
  Magic : uint32
  Command : byte[]
  Length : uint32
  Checksum : uint32
  Payload: byte[]
}

type NetworkAddress = {
  Time : uint32
  Services: uint64
  Ip : byte[]
  Port : uint16
  }

type InventoryVectorType = 
  | ERROR = 0u
  | MSG_TX = 1u
  | MSG_BLOCK = 2u

type InventoryVector = {
  Type : uint32
  Hash : byte[]
  }

type BlockHeader = {
  Version : uint32
  PrevBlock : byte[]
  MerkleRoot : byte[]
  Timestamp : uint32
  Bits : uint32
  Nonce : uint32
  TxnCount : uint8
  }



