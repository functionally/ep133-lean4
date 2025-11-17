
import Ep133.Util
import Lean.Data.Json.FromToJson
import Std.Data.HashMap.Basic
import Std.Data.TreeMap.Raw.Basic

open Ep133.Util (require)
open Lean (Json ToJson)

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
deriving Repr, BEq, Hashable, ToJson

instance : ToString EffectLabel where
  toString
  | EffectLabel.OFF => "OFF"
  | EffectLabel.DLY => "DLY"
  | EffectLabel.RVB => "RVB"
  | EffectLabel.DST => "DST"
  | EffectLabel.CXO => "CXO"
  | EffectLabel.FLT => "FLT"
  | EffectLabel.CMP => "CMP"

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
  param1 : Float
  param2 : Float
deriving Repr, BEq, ToJson


structure Effects where
  raw : ByteArray
  active : EffectLabel
  effects : Std.HashMap EffectLabel EffectParams
deriving Repr

instance : ToJson Effects where
  toJson x :=
    open Lean.ToJson (toJson) in
    ToJson.toJson
      $ Json.mkObj
      [
        ⟨ "active" , toJson x.active ⟩
      , ⟨ "effects", Json.mkObj $ x.effects.toList.map (fun ⟨ k , v ⟩ ↦ ⟨ toString k , toJson v ⟩) ⟩
      ]


structure TimeSignature where
  numerator : Nat
  denominator : Nat
deriving Repr, BEq

instance : ToString TimeSignature where
  toString x := toString x.numerator ++ "/" ++ toString x.denominator

instance : ToJson TimeSignature where
  toJson := ToJson.toJson ∘ toString


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

instance : ToJson PatternLabel where
  toJson := ToJson.toJson ∘ toString


structure Scene where
  patternA : PatternLabel
  patternB : PatternLabel
  patternC : PatternLabel
  patternD : PatternLabel
  timeSignature : TimeSignature
deriving Repr, BEq, ToJson

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

instance : ToJson Scenes where
  toJson := ToJson.toJson ∘ Scenes.scenes


inductive TimeStretchMode where
| OFF
| BPM
| BARS
deriving Repr, BEq, ToJson

namespace TimeStretchMode

  def ofUInt8 : UInt8 → ParseResult TimeStretchMode
  | 0 => pure OFF
  | 1 => pure BPM
  | 2 => pure BARS
  | i => throw $ "Time stretch " ++ toString i ++ " is not defined."

end TimeStretchMode


inductive TimeStretchBars where
| WHOLE
| DOUBLE
| QUADRUPLE
| HALF
| QUARTER
deriving Repr, BEq, ToJson

namespace TimeStretchBars

  def ofUInt8 : UInt8 → ParseResult TimeStretchBars
  | 0 => pure WHOLE
  | 1 => pure DOUBLE
  | 2 => pure QUADRUPLE
  | 255 => pure HALF
  | 254 => pure QUARTER
  | i => throw $ "Time stretch bars " ++ toString i ++ " is not defined."

end TimeStretchBars


structure TimeStretch where
  mode : TimeStretchMode
  bars : TimeStretchBars
  bpm : Float
deriving Repr, BEq, ToJson


inductive PlayMode where
| ONE
| KEY
| LEG
deriving Repr, BEq, ToJson

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
deriving Repr, BEq, ToJson

namespace ChokeGroup

  def ofUInt8 : UInt8 → ParseResult ChokeGroup
  | 0 => pure TRUE
  | 1 => pure FALSE
  | i => throw $ "Choke group " ++ toString i ++ " is not defined."

end ChokeGroup


def Volume := Fin 201
deriving Repr, BEq

instance : ToJson Volume where
  toJson := ToJson.toJson ∘ Fin.toNat

namespace Volume

  def ofUInt8 (i : UInt8) : ParseResult Volume :=
    do
      require (i <= 200)
        $ throw ("Volume " ++ toString i ++ " is too large.")
      pure $ Fin.ofNat 201 i.toNat

end Volume


structure Pitch where
  integral : Fin 25
  decimal : UInt8
deriving Repr, BEq

instance : ToString Pitch where
  toString x := toString x.integral ++ "." ++ toString x.decimal

instance : ToJson Pitch where
  toJson := ToJson.toJson ∘ toString

namespace Pitch

  def ofUInt8s (i d : UInt8) : ParseResult Pitch :=
    do
      require (i <= 12 || i >= 244)
        $ throw ("Pitch " ++ toString i ++ " is out of range.")
      pure
        {
          integral := Fin.ofNat 25 $ if i <= 12 then i.toNat + 13 else 256 - i.toNat
        , decimal := d
        }

end Pitch


def SoundId := Fin 999
deriving Repr, BEq

instance : ToString SoundId where
  toString i :=
    let j := i.toNat + 1
    if j < 10
      then "00" ++ toString j
      else if j < 99
        then "0" ++ toString j
        else toString j

instance : ToJson SoundId where
  toJson := ToJson.toJson ∘ toString

namespace SoundId

  def ofUInt8s (i j : UInt8) : ParseResult SoundId :=
    do
      let n := (i.toNat <<< 8) + j.toNat
      require (n > 0 && n < 1000)
        $ throw ("Sound ID " ++ toString n ++ " out of range.")
      pure $ Fin.ofNat 999 (n - 1)

end SoundId


def Pan := Fin 33
deriving Repr, BEq

instance : ToString Pan where
  toString i := (i.toNat.toFloat / 16 - 1) |> toString

instance : ToJson Pan where
  toJson i := ToJson.toJson (i.toNat.toFloat / 16 - 1)

namespace Pan

  def ofUInt8 (i : UInt8) : ParseResult Pan :=
    do
      require (i <= 16 || i >= 240)
        $ throw ("Pan " ++ toString i ++ " is out of range.")
      if i <= 16
        then pure ∘ Fin.ofNat 33 $ i.toNat + 17
        else pure ∘ Fin.ofNat 33 $ 255 - i.toNat

end Pan



structure Trim where
  left : Nat
  right : Nat
deriving Repr, BEq, ToJson


def Attack := UInt8
deriving Repr, BEq

instance : ToJson Attack where
  toJson := ToJson.toJson ∘ UInt8.toNat


def Release := UInt8
deriving Repr, BEq

instance : ToJson Release where
  toJson := ToJson.toJson ∘ UInt8.toNat


structure Pad where
  raw : ByteArray
  soundId : SoundId
  volume : Volume
  attack : Attack
  release : Release
  playMode : PlayMode
  timeStretch : TimeStretch
  pitch : Pitch
  trim : Trim
  pan : Pan
  chokeGroup : ChokeGroup
  midiChannel : UInt8
deriving Repr, BEq

instance : ToJson Pad where
  toJson x :=
    open Lean.ToJson (toJson) in
    ToJson.toJson
      $ Json.mkObj
      [
        ⟨ "soundId" , toJson x.soundId              ⟩
      , ⟨ "volume"  , toJson x.volume               ⟩
      , ⟨ "attack"  , toJson x.attack               ⟩
      , ⟨ "release" , toJson x.release              ⟩
      , ⟨ "playMode", toJson x.playMode             ⟩
      , ⟨ "timeStretch", toJson x.timeStretch       ⟩
      , ⟨ "pitch"      , toJson x.pitch             ⟩
      , ⟨ "trim"       , toJson x.trim              ⟩
      , ⟨ "pan"        , toJson x.pan               ⟩
      , ⟨ "chokeGroup" , toJson x.chokeGroup        ⟩
      , ⟨ "midiChannel", toJson x.midiChannel.toNat ⟩
      ]


end Ep133.Types
