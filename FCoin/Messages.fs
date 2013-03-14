module Messages

type Message = {
  magic: uint32
  command: byte[]
  length: uint32
  checksum: uint32
  payload: byte[]
  }

type MagicValues =
  | main = 0xD9B4BEF9
  | testnet = 0xDAB5BFFA
  | testnet3 = 0x0709110B


type VariableLengthInt = VarInt of byte[]

module VariableLengthInteger =
  let EncodeFromBigInt (num:bigint) =
    VarInt [| 0uy |]

  let DecodeToBigInt (VarInt representation) =
    (bigint 0)

[<Struct>]
type VariableLengthStr(len:bigint, arr:byte[]) =
    member this.length = len
    member this.string = arr



  let ToString (vs:VariableLengthStr) = 
    System.Text.Encoding.UTF8.GetString(vs.string)

type InventoryRecord = {
    type_: uint32
    hash: byte[]
}

type InventoryVector = InventoryRecord []

type InventoryRecordType = 
  | ERROR = 1
  | MSG_TX = 2
  | MSG_BLOCK = 3

