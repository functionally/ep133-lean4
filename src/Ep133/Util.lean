
namespace Ep133.Util


def getFloat32BE (bytes : ByteArray) (offset : Nat) : Float32 :=
  let getUInt32BE (bytes : ByteArray) (offset : Nat) : UInt32 :=
    let b1 := bytes[offset    ]! |> UInt8.toUInt32
    let b2 := bytes[offset + 1]! |> UInt8.toUInt32
    let b3 := bytes[offset + 2]! |> UInt8.toUInt32
    let b4 := bytes[offset + 3]! |> UInt8.toUInt32
    (b1 <<< 24) ||| (b2 <<< 16) ||| (b3 <<< 8) ||| b4
  Float32.ofBits $ getUInt32BE bytes offset


def require {m : Type → Type} [Monad m] (cond : Bool) (action : m Unit) : m Unit :=
  if cond then
    pure ()
  else
    action


end Ep133.Util
