import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Char (toLower)
import System (getArgs)

type Register = String
type ProgramCounter = Int

data Instruction = Halt 
                 | Add Register ProgramCounter
                 | Sub Register ProgramCounter ProgramCounter
                 deriving (Show)

type Program = Array.Array ProgramCounter Instruction
type Store = Map.Map Register Integer

data State = Continue Store ProgramCounter
           | Stop

readBranch :: [String] -> Int -> ProgramCounter
readBranch ss i = read $ ss !! i

parseInstruction :: String -> Instruction
parseInstruction s 
    | op == '+' = Add reg (b 1)
    | op == '-' = Sub reg (b 1) (b 2)
    | lf == "halt" = Halt
    | otherwise = error $ "Invalid Instruction Format " ++ s
    where parts = words s
          f = head parts
          lf = map toLower f
          b = readBranch parts
          reg = init f
          op = last f

parseProgram :: String -> Program
parseProgram s = Array.listArray (0, length il - 1) il
    where il = map parseInstruction (lines s)
        
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
    let s = Map.fromList [("R0", 0), ("R1", 5), ("R2", 10)]
    in getArgs >>= 
       return . head >>=
       readFile >>= 
       \x -> print $ run (parseProgram x) s 0 Map.! "R0"
