
import Cli
import Ep133


open Cli
open Ep133.Parsing (parseEffects parsePad parseScenes)
open Ep133.Types (ParseResult)
open Lean (ToJson)


def dump {α : Type} [ToJson α] (filepathFlag : String) (parse : ByteArray → ParseResult α) (p : Parsed) : IO UInt32 :=
  do
    let filepath : String := p.flag! filepathFlag |>.as! String
    let raw ← IO.FS.readBinFile filepath
    match parse raw with
    | ParseResult.parsed x =>
      IO.println (ToJson.toJson x).pretty
      pure 0
    | ParseResult.throw message =>
      IO.println message
      pure 1


def dumpEffects : Parsed → IO UInt32 := dump "effects-file" parseEffects

def dumpEffectsCmd := `[Cli|
  "dump-effects" VIA dumpEffects;
  "Dump the contents of effects."
  FLAGS:
    "effects-file" : String ; "The effects file."
  EXTENSIONS:
    defaultValues! #[
      ("effects-file", "/dev/stdin")
    ]
]


def dumpPad : Parsed → IO UInt32 := dump "pad-file" parsePad

def dumpPadCmd := `[Cli|
  "dump-pad" VIA dumpPad;
  "Dump the contents of a pad."
  FLAGS:
    "pad-file" : String ; "The pad file."
  EXTENSIONS:
    defaultValues! #[
      ("pad-file", "/dev/stdin")
    ]
]


def dumpScene : Parsed → IO UInt32 := dump "scenes-file" parseScenes

def dumpSceneCmd := `[Cli|
  "dump-scenes" VIA dumpScene;
  "Dump the contents of scenes."
  FLAGS:
    "scenes-file" : String ; "The scenes file."
  EXTENSIONS:
    defaultValues! #[
      ("scenes-file", "/dev/stdin")
    ]
]


def ep133 : Cmd := `[Cli|
  ep133 NOOP; ["0.0.1"]
  "Manipulate EP-133 K.O. II backup files."
  SUBCOMMANDS:
    dumpEffectsCmd;
    dumpPadCmd;
    dumpSceneCmd
]


def main (args : List String) : IO UInt32 :=
  ep133.validate args
