module Top (main) where

import Prelude hiding (drop)
import CatExample (cat1,cat2)
import Low (Asm,runAsm,Image,emulateImage)
import Execution (Execution(..),runX)

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
