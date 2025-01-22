module TaskOperations where

import Types

addTask :: Task -> TaskList -> TaskList
addTask task tasks = task : tasks

removeTask :: Int -> TaskList -> TaskList
removeTask id tasks = filter (\task -> taskId task /= id) tasks

listTasks :: TaskList -> [String]
listTasks = map (\task -> taskName task ++ " (" ++ show (taskStatus task) ++ ")")
