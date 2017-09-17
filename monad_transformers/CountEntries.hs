module CountEntries (doesDirectoryExist, countEntries) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT(..), tell)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."


-- Sugar-free version. Lesson from debugging this:
-- ($) can cause problems in de-sugared do notation; it's precedence is lower
-- than (>>=) and other operators
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path =
    liftIO (listDirectory path)    >>= \contents ->
    tell [(path, length contents)] >>
    forM_ contents (\name ->
        let newName = (path </> name)
        in liftIO (doesDirectoryExist newName) >>= \isDir ->
            when isDir (countEntries newName))

-- Sugary version
countEntries' :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries' path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries' newName