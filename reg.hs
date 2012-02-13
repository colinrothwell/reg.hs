import qualified Data.Map as Map
import qualified Data.Array as Array

type Register = String

data Instruction = Halt 
                 | Add Register Integer
                 | Sub Register Integer Integer

type Program = Array.Array Integer Instruction
type Store = Map.Map Register Integer 
type ProgramCounter = Integer

data State = Continue Store ProgramCounter
           | Stop

execute :: Store -> Instruction -> State
execute _ Halt = Stop
execute s (Add r pc) = Continue (Map.insert r (Map.findWithDefault 0 r s + 1) s) pc
execute s (Sub r pcnb pcb)
    | (s Map.! r) == 0 = Continue s pcb
    | otherwise = Continue (Map.insert r (Map.findWithDefault 0 r s - 1) s) pcnb

run :: Program -> Store -> ProgramCounter -> Store
run prog store pc =
    case ns of Stop -> store
               (Continue s' pc') -> run prog s' pc'
    where ins = prog Array.! pc
          ns = execute store ins

main = 
    let pl = [Sub "R1" 1 2, Add "R0" 0, Sub "R2" 3 4, Add "R0" 2, Halt]
        s = Map.fromList [("R0", 0), ("R1", 5), ("R2", 10)]
    in print $ run (Array.listArray (0, 4) pl) s 0 Map.! "R0"
