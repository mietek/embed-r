{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Utils (toBool)


foreign import ccall safe "altR_initR" initR :: IO ()
foreign import ccall safe "altR_endR" endR :: IO ()
foreign import ccall safe "altR_do1LineR" c_do1LineR :: IO CInt


do1LineR :: IO Bool
do1LineR =
    toBool <$> c_do1LineR


runR :: IO ()
runR = do
    putStrLn "-----> Haskell: Starting R"
    initR
    loop
    endR
    putStrLn "-----> Haskell: Exiting R"
  where
    loop =
      do1LineR >>= \case
        True  -> loop
        False -> return ()


main :: IO ()
main = runR
