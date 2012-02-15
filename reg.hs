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
extractLabels p = (rdlbls pl Map.empty 0, unlines $ rmlbls pl)
    where pl = lines p
          rdlbls :: [String] -> LabelMap -> ProgramCounter -> LabelMap
          rdlbls (l:ls) lm pc
            | islbl = rdlbls ls (Map.insert lbl pc lm) (pc + 1)
            | otherwise = rdlbls ls lm (pc + 1)
            where candlbl = head $ words l
                  islbl = last candlbl == ':'
                  lbl = init candlbl
          rdlbls [] lm _ = lm
          rmlbls :: [String] -> [String]
          rmlbls (l : ls)
              | islbl = (unwords $ tail wl) : (rmlbls ls)
              | otherwise = l : (rmlbls ls)
              where wl = words l
                    islbl = (last $ head wl) == ':'
          rmlbls [] = []

readBranch :: LabelMap -> [String] -> Int -> ProgramCounter
readBranch lm ss i = Map.findWithDefault (read lbl) lbl lm
    where lbl = ss !! i

parseInstruction :: LabelMap -> String -> Instruction
parseInstruction lm s 
    | op == '+' = Add reg (b 1)
    | op == '-' = Sub reg (b 1) (b 2)
    | lf == "halt" = Halt
    | otherwise = error $ "Invalid Instruction Format " ++ s
    where parts = words s
          f = head parts
          lf = map toLower f
          b = readBranch lm parts
          reg = init f
          op = last f

parseProgram :: String -> Program
parseProgram s = Array.listArray (0, length il - 1) il
    where (lm, s') = extractLabels s
          il = map (parseInstruction lm) (lines s')
        
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
