module Top (main) where

import Prelude hiding (drop)
--import CatExample (cat1,cat2)
import BfExample (bf1)

--import Low (Asm,runAsm,Image,emulateImage)
import Execution (Execution(..),runX)

main :: IO ()
main = do
  putStrLn "*xjit*"

  prog <- readFile "../bf/b/factor.b"
  let input = prog ++ "!12345\n"

  putStrLn "bf1..."
  let x1 :: Execution = bf1
  runX x1 input

  -- Start with "cat", before attempting BF.
  {-putStrLn "cat1 (direction execution)..."
  let x1 :: Execution = cat1
  runX x1 input

  putStrLn "cat2 (via asm/image)..."
  let asm :: Asm () = cat2
  let image :: Image = runAsm asm
  print image
  let x2 = emulateImage image
  runX x2 input-}

  pure ()
