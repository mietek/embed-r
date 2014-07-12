{-# LANGUAGE CPP #-}

module Main where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Utils (toBool)

#ifdef AUX
import Control.Concurrent (forkOS, threadDelay)
#endif


run :: IO () -> IO ()
run action = do
#ifdef AUX
    _ <- forkOS action
    threadDelay 1000000
#else
    action
#endif


while :: (Monad m) => m Bool -> m ()
while a = do
    b <- a
    if b
      then while a
      else return ()


foreign import ccall safe "altR_initR" initR :: IO ()
foreign import ccall safe "altR_endR" endR :: IO ()
foreign import ccall safe "altR_do1LineR" c_do1LineR :: IO CInt


do1LineR :: IO Bool
do1LineR =
    toBool <$> c_do1LineR


runR :: IO ()
runR = run $ do
    putStrLn "-----> Haskell: Starting R..."
    initR
    while do1LineR
    endR
    putStrLn "-----> Haskell: Exiting R..."


main :: IO ()
main = runR
