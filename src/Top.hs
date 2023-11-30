module Top (main) where

import Prelude hiding (drop)

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import qualified Data.Char (chr,ord)

chr :: Int -> Char
chr = Data.Char.chr

ord :: Char -> Int
ord = Data.Char.ord

main :: IO ()
main = do
  putStrLn "*xjit*"
  -- Start with "cat", before attempting BF.
  let input = "abc"

  putStrLn "cat1 (direction execution)..."
  let x1 :: Execution = cat1
  runX x1 input

  putStrLn "cat2 (via asm/image)..."
  let asm :: Asm () = cat2
  let image :: Image = runAsm asm
  print image
  let x2 = emulateImage image
  runX x2 input
  pure ()

cat1 :: Execution
cat1 = Xget $ \c -> if c == chr 0 then Xput '!' $ Xhalt else Xput c cat1

cat2 :: Asm ()
cat2 = mdo
  loop <- label
  get
  dup
  lit 0
  equal
  invert
  branch0 done
  put
  branch loop
  done <- label
  drop
  lit (ord '!')
  put
  halt
  pure ()

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

-- program execution

data Execution where
  Xdebug :: String -> Execution -> Execution
  Xhalt :: Execution
  Xget :: (Char -> Execution) -> Execution
  Xput :: Char -> Execution -> Execution

runX :: Execution -> String -> IO ()
runX x input =
  case x of
    Xdebug s x -> do
      let _ = putStrLn ("debug: " ++ s)
      runX x input
    Xhalt -> pure ()
    Xget f ->
      case input of
        [] -> runX (f (chr 0)) []
        char1:input -> runX (f char1) input
    Xput char x -> do
      print ("XPut",char)
      runX x input
