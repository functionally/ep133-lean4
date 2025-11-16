
import Ep133.Types
import Ep133.Util


open Ep133.Types
open Ep133.Util (getFloat32BE require)


namespace Ep133.Parsing


def parseEffects (raw : ByteArray) : ParseResult Effects :=
  do
   require (raw.size = 144)
      $ ParseResult.throw "Effects not 144 bytes."
    let effectType := raw.get! 4
    let offsetBase := (effectType.toNat - 1) * 4
    let effect ← Effect.ofUInt8 effectType
    pure
      {
        raw
      , effect
      , param1 := getFloat32BE raw $ offsetBase + 12
      , param2 := getFloat32BE raw $ offsetBase + 76
      }


end Ep133.Parsing
