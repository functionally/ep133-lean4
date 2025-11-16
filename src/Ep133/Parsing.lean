
import Ep133.Types
import Ep133.Util


open Ep133.Types
open Ep133.Util (getFloat32BE require)

open ParseResult (throw)


namespace Ep133.Parsing


def parseEffects (raw : ByteArray) : ParseResult Effects :=
  do
   require (raw.size = 144)
      $ throw "Effects not 144 bytes."
    let effectType := raw[4]!
    let offsetBase := (effectType.toNat - 1) * 4
    let effect ← Effect.ofUInt8 effectType
    pure
      {
        raw
      , effect
      , param1 := getFloat32BE raw $ offsetBase + 12
      , param2 := getFloat32BE raw $ offsetBase + 76
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
      $ ParseResult.throw "Scene does not contain at least 605 bytes."
    let sceneCount := raw[604]! |> UInt8.toNat
    let scenes : List Scene ← (List.range sceneCount).mapM (fun n ↦ parseScene raw (1 + 6 * n))
    pure
      {
        raw
      , scenes := scenes.toArray
      }


end Ep133.Parsing
