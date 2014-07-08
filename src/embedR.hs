{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (void)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr)

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


foreign import ccall safe "Rf_initEmbeddedR" c_initEmbeddedR :: CInt -> Ptr CString -> IO CInt
foreign import ccall safe "Rf_endEmbeddedR" c_endEmbeddedR :: CInt -> IO ()
foreign import ccall safe "altR_do1Line" c_do1Line :: IO CInt


initEmbeddedR :: [String] -> IO ()
initEmbeddedR args =
    void . withMany withCString args $ \c_args ->
      withArrayLen c_args $ \argc c_argv ->
        c_initEmbeddedR (fromIntegral argc) c_argv


endEmbeddedR :: IO ()
endEmbeddedR =
    c_endEmbeddedR 0


do1Line :: IO Bool
do1Line =
    fmap (> 0) c_do1Line


runR :: IO ()
runR = run $ do
    putStrLn "-----> Haskell: Starting R..."
    initEmbeddedR ["embedR", "--interactive", "--silent", "--vanilla"]
    while do1Line
    endEmbeddedR
    putStrLn "-----> Haskell: Exiting R..."


main :: IO ()
main = runR
