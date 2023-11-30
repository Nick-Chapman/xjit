
module BfExample (bf1) where

import Prelude hiding (drop)
import Execution (Execution(..))
import Data.Word (Word8)
import Data.ByteString.Internal (c2w,w2c)

bf1 :: Execution
bf1 = do
  readProgString $ \progString -> do
    let ops = parse progString
    execFromStart ops

readProgString :: (String -> Execution) -> Execution
readProgString k = collect ""
  where
    collect acc =
      Xget $ \c -> if c == '!' then k (reverse acc) else collect (c:acc)

data Op
  = Plus
  | Minus
  | Rarrow
  | Larrow
  | Block [Op]
  | Dot
  | Comma

parse :: String -> [Op]
parse s = loop [] [] s
  where
    loop :: [Op] -> [[Op]] -> [Char] -> [Op]
    loop acc nest = \case
      [] ->
        case nest of
          [] -> reverse acc
          _:_ -> error "unclosed["
      x:xs -> do
        case x of
          '+' -> loop (Plus:acc) nest xs
          '-' -> loop (Minus:acc) nest xs
          '<' -> loop (Larrow:acc) nest xs
          '>' -> loop (Rarrow:acc) nest xs
          '.' -> loop (Dot:acc) nest xs
          ',' -> loop (Comma:acc) nest xs
          '[' -> loop [] (acc:nest) xs
          ']' ->
            case nest of
              [] -> error "unexpected]"
              acc1:nest -> loop (Block (reverse acc) : acc1) nest xs
          _ ->
            loop acc nest xs

execFromStart :: [Op] -> Execution
execFromStart prog = exec prog tape0

exec :: [Op] -> Tape -> Execution
exec ops0 t =
  case ops0 of
    [] -> Xhalt
    Minus:ops -> exec ops (doMinus t)
    Plus:ops -> exec ops (doPlus t)
    Rarrow:ops -> exec ops (doRight t)
    Larrow:ops -> exec ops (doLeft t)
    Dot:ops -> Xput (w2c (wordAtPoint t)) (exec ops t)
    Comma:ops -> Xget (\c -> exec ops (setWordAtPoint (c2w c) t))
    bl@(Block ops1) : ops2 ->
      if isZeroAtPoint t
      then exec ops2 t
      else exec (ops1 ++ bl : ops2) t

setWordAtPoint :: Word8 -> Tape -> Tape
setWordAtPoint w Tape{left,right} = Tape {left,point=w,right}

wordAtPoint :: Tape -> Word8
wordAtPoint Tape{point} = point

isZeroAtPoint :: Tape -> Bool
isZeroAtPoint Tape{point} = (point == 0)

doMinus :: Tape -> Tape
doMinus Tape{left,point=old,right} = Tape {left, point = old-1, right}

doPlus :: Tape -> Tape
doPlus Tape{left,point=old,right} = Tape {left, point = old+1, right}

doRight :: Tape -> Tape
doRight Tape{left,point,right} =
  case right of
    []       -> Tape { left = point : left , point = 0 , right }
    r1:right -> Tape { left = point : left , point = r1, right }

doLeft :: Tape -> Tape
doLeft Tape{left,point,right} =
  case left of
    []      -> Tape { left = []   , point = 0 , right = point : right }
    l1:left -> Tape { left = left , point = l1, right = point : right }

tape0 :: Tape
tape0 = Tape { left = [], point = 0, right = [] }

data Tape = Tape { left :: [Word8], point :: Word8, right :: [Word8] }
