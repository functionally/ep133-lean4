
import Ep133.Types
import Ep133.Util


open Ep133.Types
open Ep133.Util (getFloat32BE require)

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


end Ep133.Parsing
