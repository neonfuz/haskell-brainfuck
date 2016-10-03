import Tape
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

bfCompile :: String -> Tape Char
bfCompile program = tapeFromList $ filter (`elem` "+-<>[],.") program ++ "E"

newMem = Tape (repeat 0) 0 (repeat 0)

run program = bf (bfCompile program) newMem "" ""

bf :: Tape Char -> Tape Int -> String -> String -> String
bf prog mem stdIn stdOut =
  case current prog of
    'E' -> reverse stdOut
    '+' -> bf (next prog) (increment mem) stdIn stdOut
    '-' -> bf (next prog) (decrement mem) stdIn stdOut
    '<' -> bf (next prog) (prev mem) stdIn stdOut
    '>' -> bf (next prog) (next mem) stdIn stdOut
    ',' -> bf (next prog) (set mem $ ord $ head stdIn) (tail stdIn) stdOut
    '.' -> bf (next prog) mem stdIn (chr (current mem) : stdOut)
    '[' -> bf (if current mem == 0 then (loop forward (next prog) 1) else (next prog)) mem stdIn stdOut
    ']' -> bf (if current mem /= 0 then (loop backward (prev prog) 1) else (next prog)) mem stdIn stdOut
    _ -> "unhandled"
  where set mem val = case mem of (Tape b c e) -> Tape b val e
        increment mem = set mem $ (current mem + 1 + 256) `mod` 256
        decrement mem = set mem $ (current mem - 1 + 256) `mod` 256
        forward  = (next, [('[', 1), (']', -1)])
        backward = (prev, [('[', -1), (']', 1)])
        loop direction prog count = case (prog, count) of
          (prog, 0) -> next prog
          (prog@(Tape _ c _), count) ->
            loop direction (fst direction prog) (count + (fromMaybe 0 $ lookup c $ snd direction))
