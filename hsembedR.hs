{-# LANGUAGE CPP #-}

module Main where

#ifdef FORK
import Control.Concurrent (forkOS, threadDelay)
#endif

import Control.Monad (void)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr)

foreign import ccall safe "Rf_initEmbeddedR" c_initEmbeddedR :: CInt -> Ptr CString -> IO CInt
foreign import ccall safe "R_ReplDLLinit" c_replDllInit :: IO ()
foreign import ccall safe "R_ReplDLLdo1" c_replDllDo1 :: IO CInt
foreign import ccall safe "Rf_endEmbeddedR" c_endEmbeddedR :: CInt -> IO ()

initEmbeddedR :: [String] -> IO ()
initEmbeddedR args =
    void . withMany withCString args $ \c_args ->
      withArrayLen c_args $ \argc c_argv ->
        c_initEmbeddedR (fromIntegral argc) c_argv

replDllInit :: IO ()
replDllInit =
    c_replDllInit

replDllDo1 :: IO Bool
replDllDo1 =
    fmap (> 0) c_replDllDo1

endEmbeddedR :: IO ()
endEmbeddedR =
    c_endEmbeddedR 0

run :: IO () -> IO ()
run action = do
#ifdef FORK
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

main :: IO ()
main =
    run $ do
      initEmbeddedR ["hsembedR", "--interactive", "--silent", "--vanilla"]
      replDllInit
      while replDllDo1
      endEmbeddedR
