
namespace Ep133.Types


private instance : Repr ByteArray where
  reprPrec := reprPrec ∘ ByteArray.toList


inductive ParseResult (a : Type) where
| parsed : a → ParseResult a
| throw : String → ParseResult a
deriving Repr, BEq

instance : Monad ParseResult where
  pure := ParseResult.parsed
  bind
  | ParseResult.parsed x, f => f x
  | ParseResult.throw s, _ => ParseResult.throw s

instance : Alternative ParseResult where
  failure := ParseResult.throw "Parsing failed."
  orElse
  | ParseResult.parsed x, _ => ParseResult.parsed x
  | ParseResult.throw _, f => f ()


inductive Effect where
| OFF
| DLY
| RVB
| DST
| CXO
| FLT
| CMP
deriving Repr, BEq

namespace Effect

  open ParseResult

  def ofUInt8 : UInt8 → ParseResult Effect
  | 0 => parsed OFF
  | 1 => parsed DLY
  | 2 => parsed RVB
  | 3 => parsed DST
  | 4 => parsed CXO
  | 5 => parsed FLT
  | 6 => parsed CMP
  | i => throw $ "Effect " ++ toString i ++ " is not defined."

end Effect


structure Effects where
  raw : ByteArray
  effect : Effect
  param1 : Float32
  param2 : Float32
deriving Repr


structure Project where
  pads : ByteArray
  scenes : ByteArray
  settings : ByteArray
  effects : ByteArray
  scenesSettings  : ByteArray
  sounds : ByteArray
deriving Repr


end Ep133.Types
