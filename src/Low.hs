module Low
  ( Image, emulateImage
  , Asm, runAsm
  , halt,get,put,dup,drop,equal,invert,lit,label,branch,branch0
  ) where

import Prelude hiding (drop)

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Data.Char (chr,ord)
import Execution (Execution(..))

-- monadic assembler with locations
halt,get,put,dup,drop,equal,invert :: Asm ()
lit :: Value -> Asm ()
label :: Asm Loc
branch,branch0 :: Loc -> Asm ()

instance MonadFix Asm where mfix = Amfix
instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Apure; (<*>) = ap
instance Monad Asm where (>>=) = Abind

halt = Aemit [Ohalt]
get = Aemit [Oget]
put = Aemit [Oput]
dup = Aemit [Odup]
drop = Aemit [Odrop]
equal = Aemit [Oequal]
invert = Aemit [Oinvert]
lit v = Aemit [Olit v]
label = Alabel
branch loc = Aemit [Obranch loc]
branch0 loc = Aemit [Obranch0 loc]

type Loc = Int -- should be abstract
type Value = Int -- ?? or Char

data Asm a where
  Apure :: a -> Asm a
  Abind :: Asm a -> (a -> Asm b) -> Asm b
  Amfix :: (a -> Asm a) -> Asm a
  Aemit :: [Op] -> Asm ()
  Alabel :: Asm Loc

runAsm :: Asm () -> Image
runAsm asm = Image ops
  where
    ((),ops) = loop 0 asm
    loop :: Int -> Asm a -> (a,[Op])
    loop n = \case
      Apure x -> (x,[])
      Abind asm f -> do
        let (v1,xs1) = loop n asm
        let (v2,xs2) = loop (n + length xs1) (f v1)
        (v2, xs1 ++ xs2)
      Amfix f -> do
        let x@(a,_) = loop n (f a)
        x
      Aemit xs -> ((),xs)
      Alabel -> (n,[])

-- Low level ops
data Image = Image [Op] deriving Show

data Op
  = Ohalt
  | Oget
  | Oput
  | Olit Value
  | Odup
  | Odrop
  | Oequal
  | Oinvert
  | Obranch Abs
  | Obranch0 Abs
  deriving Show

type Abs = Int -- TODO: use rel instead?

emulateImage :: Image -> Execution

data State = State { pc :: PC, ps :: [Value] } deriving Show
type PC = Int

emulateImage (Image ops) = go state0
  where
    state0 = State { pc = 0, ps = [] }
    mem :: PC -> Op
    mem pc = ops !! pc -- TODO: use array!
    go :: State -> Execution
    go s@State{pc} = do
      let op = mem pc
      emulateOp op s { pc = pc + 1 } go

    emulateOp :: Op -> State -> (State -> Execution) -> Execution
    emulateOp op s@State{ps} k = Xdebug (show (op,s)) $ do
      let
        p1 = case ps of p:_ -> p; _ -> error "p1"
        p2 = case ps of _:p:_ -> p; _ -> error "p2"
        ps' = case ps of _:ps -> ps; _ -> error "ps'"
        ps'' = case ps of _:_:ps -> ps; _ -> error "ps''"
      case op of
        Ohalt -> Xhalt
        Oget -> Xget $ \c -> k s { ps = ord c : ps }
        Oput -> Xput (chr p1) $ k s { ps = ps' }
        Olit v -> k s { ps = v : ps }
        Odup -> k s { ps = p1 : ps }
        Odrop -> k s { ps = ps' }
        Oequal -> k s { ps = vEqual p1 p2 : ps'' }
        Oinvert -> k s { ps = vInvert p1 : ps' }
        Obranch abs -> k s { pc = abs }
        Obranch0 abs ->
          if vIsZero p1
          then k s { ps = ps', pc = abs }
          else k s { ps = ps' }

    vEqual :: Value -> Value -> Value
    vEqual v1 v2 = if (v1==v2) then vTrue else vFalse

    vInvert :: Value -> Value
    vInvert v = if vIsZero v then -1 else 0

    vIsZero :: Value -> Bool
    vIsZero v = (v == 0)

    vTrue = -1
    vFalse = 0
