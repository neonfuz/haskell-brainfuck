import Tape
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

bfCompile :: String -> Tape Char
bfCompile program = tapeFromList $ filter (`elem` "+-<>[],.") program ++ "E"

run :: String -> String -> String
run program stdin = bf (bfCompile program) newMem stdin ""
  where newMem = Tape (repeat 0) 0 (repeat 0)

bf :: Tape Char -> Tape Int -> String -> String -> String
bf prog mem stdIn stdOut =
  case current prog of
    'E' -> reverse stdOut
    '+' -> bf (next prog) (increment mem) stdIn stdOut
    '-' -> bf (next prog) (decrement mem) stdIn stdOut
    '<' -> bf (next prog) (prev mem) stdIn stdOut
    '>' -> bf (next prog) (next mem) stdIn stdOut
    ',' -> bf (next prog) (set (ord . head $ stdIn) mem) (tail stdIn) stdOut
    '.' -> bf (next prog) mem stdIn (chr (current mem) : stdOut)
    '[' -> bf (if current mem == 0 then (loop forward (next prog) 1) else (next prog)) mem stdIn stdOut
    ']' -> bf (if current mem /= 0 then (loop backward (prev prog) 1) else (next prog)) mem stdIn stdOut
  where apply func mem = case mem of (Tape b c e) -> Tape b (func c) e
        set val = apply (const val)
        increment = apply (limit . (+1))
        decrement = apply (limit . (subtract 1))
        limit = (`mod` 256) . (+256)
        forward  = (next, [('[', 1), (']', -1)])
        backward = (prev, [('[', -1), (']', 1)])
        loop _ prog 0 = next prog
        loop dir@(next,table) prog@(Tape _ c _) level =
          loop dir (next prog) (level + (fromMaybe 0 $ lookup c table))
