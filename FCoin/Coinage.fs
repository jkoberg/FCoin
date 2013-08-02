module Coinage

type BTC = decimal

module NumericLiteralG =
  let FromOne () : BTC = 1.00000000M
  let FromZero () : BTC = 0.00000000M
  let FromInt32 (i:int32) : BTC = (decimal i) * 1.000000000M
  let FromInt64 i : BTC = (decimal i) * 1.00000000M
  let FromString (s:string) : BTC =  (decimal s) * 1.00000000M



