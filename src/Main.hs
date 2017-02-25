{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Lens
import Data.Aeson.Lens
import Network.Wreq
import System.Directory
import System.FilePath
import System.IO

data WanikaniToolbar = WanikaniToolbar
  { userToken :: String
  } deriving (Read, Show, Eq)

main = do
  hSetEncoding stdout utf8
  wkToolbarDir <- getAppUserDataDirectory "wanikani-toolbar"
  exists <- doesDirectoryExist wkToolbarDir
  if exists
    then do
      conf <- readIO =<< readFile (wkToolbarDir </> "config")
      r <- get ("https://www.wanikani.com/api/user/" ++ userToken conf ++ "/study-queue")
      let info = responseBody . key "requested_information"
          lessons = (r ^? info . key "lessons_available" . _Integral) :: Maybe Int
          reviews = (r ^? info . key "reviews_available" . _Integral) :: Maybe Int
      case (lessons, reviews) of
        (Just 0, Just 0) -> putStr "\x1f389"
        _ -> do
          putStr "\x1f40a \x1f980  "
          forM_ lessons $ \l -> when (l /= 0) $ putStr ("Lessons: " ++ show l ++ "  ")
          forM_ reviews $ \r -> when (r /= 0) $ putStr ("Reviews: " ++ show r)
      putStrLn "\n---"
      forM_ lessons $ \c -> when (c /= 0) $ putStrLn "\x1f530 Start lessons | href=https://www.wanikani.com/lesson/session"
      forM_ reviews $ \c -> when (c /= 0) $ putStrLn "\x231b Start reviews | href=https://www.wanikani.com/review/session"
      case (lessons, reviews) of
        (Just 0, Just 0) -> putStrLn "Take it easy, speedy!"
        _ -> return ()
    else do
      createDirectoryIfMissing False wkToolbarDir
      putStrLn "What is your WaniKani public API key? (https://www.wanikani.com/account):"
      userKey <- getLine
      writeFile (wkToolbarDir </> "config") $ show $ WanikaniToolbar userKey

