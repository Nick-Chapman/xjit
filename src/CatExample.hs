module CatExample (cat1,cat2) where

import Prelude hiding (drop)
import Data.Char (chr,ord)
import Execution (Execution(..))

import Low ( Asm
           , halt,get,put,dup,drop,equal,invert,lit,label,branch,branch0
           )

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
