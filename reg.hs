import qualified Data.Map as Map
import qualified Data.Array as Array

type Register = String

data Instruction = Halt 
                 | Add Register Integer
                 | Sub Register Integer Integer

type Program = Array.Array Integer Instruction
type Store = Map.Map Register Integer 
type ProgramCounter = Integer

add :: Store -> Register -> Store
add s r = Map.insert r (Map.findWithDefault 0 r s + 1) s

sub :: Store -> Register -> Integer -> Integer -> (Store, Integer)
sub s r pcnb pcb
    | (s Map.! r) == 0 = (s, pcb)
    | otherwise = (Map.insert r (Map.findWithDefault 0 r s - 1) s, pcnb)

run :: Program -> Store -> ProgramCounter -> Integer
run prog store pc =
    case ins of Halt -> store Map.! "R0"
                (Add r pc') -> run prog (add store r) pc'
                (Sub r pcnb pcb) -> 
                    let (s', pc') = sub store r pcnb pcb
                    in run prog s' pc'
    where ins = prog Array.! pc

main = 
    let pl = [Sub "R1" 1 2, Add "R0" 0, Sub "R2" 3 4, Add "R0" 2, Halt]
        s = Map.fromList [("R0", 0), ("R1", 5), ("R2", 10)]
    in print $ run (Array.listArray (0, 4) pl) s 0
