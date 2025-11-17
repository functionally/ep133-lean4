
import Ep133.Types
import Ep133.Util


open Ep133.Types
open Ep133.Util (getFloat32BE getUInt32 require)

open ParseResult (throw)


-- See <https://github.com/phones24/ep133-export-to-daw/blob/99454368f8da44b8b5bf495df0f89c72603b8dc4/docs/EP133_FORMATS.md> for reference.

namespace Ep133.Parsing


def parseEffects (raw : ByteArray) : ParseResult Effects :=
  do
   require (raw.size >= 100)
      $ throw ("Effects only contains " ++ toString raw.size ++ " bytes.")
    let effectType := raw[4]!
    let active ← EffectLabel.ofUInt8 effectType
    let effects ←
      (List.range 7).mapM
        (fun i ↦
          do
            let label ← EffectLabel.ofUInt8 i.toUInt8
            let param1 := getFloat32BE raw $ 8 + 4 * i
            let param2 := getFloat32BE raw $ 72 + 4 * i
            pure ⟨ label , {param1, param2} ⟩
        )
    pure
      {
        raw
      , active
      , effects := Std.HashMap.ofList effects
      }


def makePatternLabel (l : Fin 4) (n : UInt8) : ParseResult PatternLabel :=
  do
    require (n > 0 && n < 100)
      $ throw ("Pattern " ++ toString n ++ " is out of range.")
    pure {letter := l, number := Fin.ofNat 99 $ n.toNat - 1}


def parseScene (raw : ByteArray) (offset : Nat) : ParseResult Scene :=
  do
    let patternA ← makePatternLabel 0 $ raw[offset + 0]!
    let patternB ← makePatternLabel 1 $ raw[offset + 1]!
    let patternC ← makePatternLabel 2 $ raw[offset + 2]!
    let patternD ← makePatternLabel 3 $ raw[offset + 3]!
    pure
      {
        patternA, patternB, patternC, patternD
      , timeSignature := {numerator := raw[offset + 4]!.toNat, denominator := raw[offset + 5]!.toNat}
      }


def parseScenes (raw : ByteArray) : ParseResult Scenes :=
  do
    require (raw.size >= 605)
      $ throw ("Scenes only contains " ++ toString raw.size ++ " bytes.")
    let sceneCount := raw[604]! |> UInt8.toNat
    let scenes : List Scene ← (List.range sceneCount).mapM (fun n ↦ parseScene raw (1 + 6 * n))
    pure
      {
        raw
      , scenes := scenes.toArray
      }


def parsePad (raw : ByteArray) : ParseResult Pad :=
  do
    let playMode ← PlayMode.ofUInt8 raw[23]!
    let volume ← Volume.ofUInt8 raw[16]!
    let timeStretchMode ← TimeStretchMode.ofUInt8 raw[21]!
    let timeStretchBars ← TimeStretchBars.ofUInt8 raw[25]!
    let chokeGroup ← ChokeGroup.ofUInt8 raw[22]!
    let soundId ← SoundId.ofUInt8s raw[2]! raw[1]!
    let pan ← Pan.ofUInt8 raw[18]!
    let trim :=
      {
        left := getUInt32 0 raw[6]! raw[5]! raw[4]! |> UInt32.toNat
        right := getUInt32 0 raw[10]! raw[9]! raw[8]! |> UInt32.toNat
      }
    let pitch ← Pitch.ofUInt8s raw[17]! raw[26]!
    pure
      {
        raw
      , soundId
      , volume
      , attack := raw[19]!
      , release := raw[20]!
      , playMode
      , timeStretch :=
          {
            mode := timeStretchMode
          , bars:= timeStretchBars
          , bpm := getFloat32BE raw 12
          }
      , pitch
      , trim
      , pan
      , chokeGroup
      , midiChannel := raw[3]!
      }


end Ep133.Parsing
