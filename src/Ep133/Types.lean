
import Std.Data.HashMap.Basic


namespace Ep133.Types


private instance : Repr ByteArray where
  reprPrec := reprPrec ∘ ByteArray.toList


inductive ParseResult (a : Type) where
| parsed : a → ParseResult a
| throw : String → ParseResult a
deriving Repr, BEq
open ParseResult (throw)

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


inductive EffectLabel where
| OFF
| DLY
| RVB
| DST
| CXO
| FLT
| CMP
deriving Repr, BEq, Hashable

namespace EffectLabel

  def ofUInt8 : UInt8 → ParseResult EffectLabel
  | 0 => pure OFF
  | 1 => pure DLY
  | 2 => pure RVB
  | 3 => pure DST
  | 4 => pure CXO
  | 5 => pure FLT
  | 6 => pure CMP
  | i => throw $ "Effect " ++ toString i ++ " is not defined."

end EffectLabel


structure EffectParams where
  param1 : Float32
  param2 : Float32
deriving Repr, BEq


structure Effects where
  raw : ByteArray
  active : EffectLabel
  effects : Std.HashMap EffectLabel EffectParams
deriving Repr


structure TimeSignature where
  numerator : Nat
  denominator : Nat
deriving Repr, BEq


structure PatternLabel where
  letter : Fin 4
  number : Fin 99
deriving Repr, BEq

instance : ToString PatternLabel where
  toString
  | ⟨ l , n ⟩ =>
    let letter := l.toNat + 'A'.toNat |> Char.ofNat
    let number := if n.toNat < 9 then "0" ++ toString (n.toNat + 1) else toString (n.toNat + 1)
    letter.toString ++ number


structure Scene where
  patternA : PatternLabel
  patternB : PatternLabel
  patternC : PatternLabel
  patternD : PatternLabel
  timeSignature : TimeSignature
deriving Repr, BEq

instance : Inhabited Scene where
  default :=
    {
      timeSignature := {numerator := 4, denominator := 4}
    , patternA := {letter := 0, number := 0}
    , patternB := {letter := 1, number := 0}
    , patternC := {letter := 2, number := 0}
    , patternD := {letter := 3, number := 0}
    }


structure Scenes where
  raw : ByteArray
  scenes : Array Scene
deriving Repr, BEq


inductive TimeStretch where
| OFF
| BPM
| BARS
deriving Repr, BEq

namespace TimeStretch

  def ofUInt8 : UInt8 → ParseResult TimeStretch
  | 0 => pure OFF
  | 1 => pure BPM
  | 2 => pure BARS
  | i => throw $ "Time stretch " ++ toString i ++ " is not defined."

end TimeStretch


inductive TimeStretchBars where
| WHOLE
| DOUBLE
| QUADRUPLE
| HALF
| QUARTER
deriving Repr, BEq

namespace TimeStretchBars

  def ofUInt8 : UInt8 → ParseResult TimeStretchBars
  | 0 => pure WHOLE
  | 1 => pure DOUBLE
  | 2 => pure QUADRUPLE
  | 255 => pure HALF
  | 254 => pure QUARTER
  | i => throw $ "Time stretch bars " ++ toString i ++ " is not defined."

end TimeStretchBars


inductive PlayMode where
| ONE
| KEY
| LEG
deriving Repr, BEq

namespace PlayMode

  def ofUInt8 : UInt8 → ParseResult PlayMode
  | 0 => pure ONE
  | 1 => pure KEY
  | 2 => pure LEG
  | i => throw $ "Play mode " ++ toString i ++ " is not defined."

end PlayMode


inductive ChokeGroup where
| TRUE
| FALSE
deriving Repr, BEq

namespace ChokeGroup

  def ofUInt8 : UInt8 → ParseResult ChokeGroup
  | 0 => pure TRUE
  | 1 => pure FALSE
  | i => throw $ "Choke group " ++ toString i ++ " is not defined."

end ChokeGroup


def Volume := Fin 201
deriving Repr, BEq


def Attack := UInt8
deriving Repr, BEq


def Release := UInt8
deriving Repr, BEq


structure Project where
  pads : ByteArray
  scenes : ByteArray
  settings : ByteArray
  effects : ByteArray
  scenesSettings  : ByteArray
  sounds : ByteArray
deriving Repr


end Ep133.Types
