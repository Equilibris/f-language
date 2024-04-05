module Objects = Objects
module Mat = Mat
include Quasi_arctic
include Foetus
module IntRig : Objects.Rig with type t = int = Core.Int
