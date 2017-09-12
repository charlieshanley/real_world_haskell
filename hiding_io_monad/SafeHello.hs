module SafeHello where

import MonadHandle
import System.IO (IOMode(..))


safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path =
    openFile path WriteMode   >>= \h ->
    hPutStrLn h "hello world" >>
    hClose h