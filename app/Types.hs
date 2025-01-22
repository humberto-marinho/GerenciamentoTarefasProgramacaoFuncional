module Types where

data Status = Pending | Completed
  deriving (Show, Eq)

data Category = Category { catName :: String }
  deriving (Show, Eq)

data Task = Task
  { taskId :: Int
  , taskName :: String
  , taskCategory :: Category
  , taskStatus :: Status
  } deriving (Show, Eq)

type TaskList = [Task]