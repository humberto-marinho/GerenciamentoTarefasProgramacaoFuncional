module Persistence where

import Types
import Data.Maybe (mapMaybe)

taskToString :: Task -> String
taskToString (Task id name (Category cat) status) =
  show id ++ "," ++ name ++ "," ++ cat ++ "," ++ show status

stringToTask :: String -> Maybe Task
stringToTask str =
  case wordsWhen (== ',') str of
    [idStr, name, cat, statusStr] ->
      let id = read idStr
          status = if statusStr == "Completed" then Completed else Pending
      in Just (Task id name (Category cat) status)
    _ -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

saveTasks :: FilePath -> TaskList -> IO ()
saveTasks filePath tasks = writeFile filePath (unlines (map taskToString tasks))

loadTasks :: FilePath -> IO TaskList
loadTasks filePath = do
  content <- readFile filePath
  let taskLines = lines content
  return (mapMaybe stringToTask taskLines)
