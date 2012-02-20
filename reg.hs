import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Char (toLower)
import System (getArgs)
import Debug.Trace (trace)

type Register = String
type ProgramCounter = Int

data Branch = NextLine
            | Line ProgramCounter
            deriving (Show)

data Instruction = Halt 
                 | Add Register Branch
                 | Sub Register Branch Branch
                 deriving (Show)

type Program = Array.Array ProgramCounter Instruction
type Store = Map.Map Register Integer

data State = Continue Store Branch
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
          rmlbls (l:ls)
              | islbl = (unwords $ tail wl) : (rmlbls ls)
              | otherwise = l : (rmlbls ls)
              where wl = words l
                    islbl = (last $ head wl) == ':'
          rmlbls [] = []

readBranch :: LabelMap -> String -> Branch
readBranch lm lbl = Line $ Map.findWithDefault (read lbl) lbl lm

parseInstruction :: LabelMap -> String -> Instruction
parseInstruction lm s 
    | op == '+' = buildAdd reg bs
    | op == '-' = buildSub reg bs
    | lf == "halt" = Halt
    | otherwise = error $ "Invalid Instruction Format " ++ s
    where parts = words s
          bs = map (readBranch lm) $ tail parts
          nb = length bs
          f = head parts
          lf = map toLower f
          reg = init f
          op = last f
          buildAdd :: String -> [Branch] -> Instruction
          buildAdd reg bs
            | nb == 0 = Add reg NextLine
            | otherwise = Add reg (bs !! 0)
          buildSub :: String -> [Branch] -> Instruction
          buildSub reg bs
            | nb == 1 = Sub reg NextLine (bs !! 0)
            | otherwise = Sub reg (bs !! 0) (bs !! 1)

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

nextPC :: ProgramCounter -> Branch -> ProgramCounter
nextPC _ (Line n) = n
nextPC pc NextLine = pc + 1

run :: Program -> Store -> ProgramCounter -> Store
run prog store pc =
    case ns of Stop -> store
               (Continue s' b) -> run prog s' (nextPC pc b)
    where ins = prog Array.! pc
          ns = execute store ins

parseArgs :: [String] -> ([Register], [(Register, Integer)])
parseArgs [] = ([], [])
parseArgs (a : as)
    | isi = let (r, v) = break (== '=') a in (os, (r, read $ tail v) : is)
    | otherwise = (a : os, is)
    where pr = parseArgs as
          os = fst pr
          is = snd pr
          isi = '=' `elem` a

getValues :: Store -> [Register] -> [String]
getValues s [] = []
getValues s (r : rs) = (r ++ " = " ++ show (s Map.! r)) : getValues s rs

main = do
    args <- getArgs
    prog <- readFile $ head args
    let (os, is) = parseArgs $ tail args
     in putStr $ unlines $ getValues (run (parseProgram prog) (Map.fromList is) 0) os
    
