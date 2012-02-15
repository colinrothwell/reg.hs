import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Char (toLower)
import System (getArgs)
import Debug.Trace (trace)

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

type LabelMap = Map.Map String ProgramCounter

extractLabels :: String -> (LabelMap, String)
extractLabels p = (lm, unlines pl)
    where ls = lines p
          extractLabel :: LabelMap -> ProgramCounter -> String -> (LabelMap, String)
          extractLabel lm pc isn
              | islbl = (Map.insert lbl pc lm, isn')
              | otherwise = (lm, isn)
              where wi = words isn
                    cl = head wi
                    islbl = (last cl == ':')
                    lbl = init cl
                    isn' = unwords $ tail wi
          transform :: LabelMap -> ProgramCounter -> [String] -> (LabelMap, [String])
          transform lm pc (i:is) = (lm'', il') 
              where (lm', i') = extractLabel lm pc i
                    (lm'', is') = transform lm' (pc + 1) is
                    il' = i' : is'
          transform lm _ [] = (lm, [])
          (lm, pl) = transform Map.empty 0 ls

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
       {-\x -> print $ run (parseProgram x) s 0 Map.! "R0"-}
       \x -> print $ extractLabels x
