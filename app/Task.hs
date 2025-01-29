module Task (
    Task(..), 
    createTask, 
    saveTasks, 
    loadTasks, 
    Status(..), 
    filterTasksByStatus, 
    sortTasksByName,
    filterTasksByCategory,
    countTasksByStatus,
    contarTarefasPorStatus,
    markTaskAsDone,
    markDone
) where


import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import System.IO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Category (Category(..), showCategories)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (UTCTime)
import Data.Maybe (maybe)

-- Tipo Status
data Status = EmProgresso | Concluida | Cancelada
  deriving (Show, Read, Eq)

-- Tipo Task
data Task = Task
  { taskId      :: Int        -- ID único da tarefa
  , nome        :: String     -- Nome da tarefa
  , descricao   :: String     -- Descrição da tarefa
  , categoria   :: Category   -- Categoria da tarefa
  , status      :: Status     -- Status da tarefa
  , dataInicial :: UTCTime    -- Data inicial
  , dataFinal   :: UTCTime    -- Data final
  } deriving (Show, Read)

-- Função para salvar as tarefas no arquivo
saveTasks :: FilePath -> [Task] -> IO ()
saveTasks filePath tasks = do
    let content = show tasks -- Converte a lista para String usando `show`
    writeFile filePath content -- Salva no arquivo

loadTasks :: FilePath -> IO [Task]
loadTasks filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            content <- readFile filePath
            if null content
                then return []  -- Retorna lista vazia se o arquivo estiver vazio
                else case readMaybe content :: Maybe [Task] of
                    Just tasks -> return tasks
                    Nothing -> do
                        putStrLn "Erro ao ler o conteúdo do arquivo de tarefas!"
                        return [] -- Retorna uma lista vazia se o conteúdo for inválido
        else return [] -- Retorna uma lista vazia se o arquivo não existir



-- Função para criar uma nova tarefa e persistir no arquivo
createTask :: FilePath -> [Category] -> [Task] -> IO [Task]
createTask filePath categories tasks = do
    -- Nome da tarefa
    putStrLn "Digite o nome da tarefa:"
    nome <- getLine

    -- Descrição da tarefa
    putStrLn "Digite a descrição da tarefa:"
    descricao <- getLine

    -- Categoria
    putStrLn "Categorias disponíveis:"
    putStrLn $ showCategories categories
    putStrLn "Digite o ID da categoria:"
    categoryIdInput <- getLine
    let maybeCategoria = findCategory (readMaybe categoryIdInput) categories
    categoria <- case maybeCategoria of
        Just c  -> return c
        Nothing -> fail "Categoria inválida!"

    -- Status
    putStrLn "Digite o status da tarefa (EmProgresso, Concluida, Cancelada):"
    statusInput <- getLine
    let maybeStatus = readMaybe statusInput :: Maybe Status
    status <- case maybeStatus of
        Just s  -> return s
        Nothing -> fail "Status inválido!"

    -- Data Inicial
    putStrLn "Digite a data inicial no formato YYYY-MM-DD HH:MM:SS:"
    dataInicialInput <- getLine
    dataInicial <- parseDate dataInicialInput

    -- Data Final
    putStrLn "Digite a data final no formato YYYY-MM-DD HH:MM:SS:"
    dataFinalInput <- getLine
    dataFinal <- parseDate dataFinalInput

    -- Criação da Tarefa com ID
    let newTaskId = length tasks + 1 -- ID da tarefa será baseado no tamanho do vetor
    let newTask = Task newTaskId nome descricao categoria status dataInicial dataFinal
    let updatedTasks = tasks ++ [newTask]

    -- Salvar tarefas no arquivo
    saveTasks filePath updatedTasks
    putStrLn $ "Tarefa criada e salva: " ++ show newTask

    return updatedTasks

-- Função auxiliar para buscar categoria por ID
findCategory :: Maybe Int -> [Category] -> Maybe Category
findCategory (Just cid) categories = lookup cid $ map (\c -> (categoryId c, c)) categories
findCategory Nothing _ = Nothing

-- Função auxiliar para validar e converter data e hora
parseDate :: String -> IO UTCTime
parseDate input =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" input of
    Just date -> return date
    Nothing   -> fail "Formato de data inválido! Use o formato YYYY-MM-DD HH:MM:SS."

-- Função para filtrar tarefas por status
filterTasksByStatus :: Status -> [Task] -> [Task]
filterTasksByStatus st = filter (\task -> status task == st)

-- Função para ordenar tarefas por nome
sortTasksByName :: [Task] -> [Task]
sortTasksByName = sortBy (comparing nome)

filterTasksByCategory :: Int -> [Task] -> [Task]
filterTasksByCategory catId tasks =
    filter (\task -> categoryId (categoria task) == catId) tasks

countTasksByStatus :: [Task] -> Status -> Int
countTasksByStatus tasks targetStatus = go tasks 0
  where
    go [] acc = acc
    go (t:ts) acc
      | status t == targetStatus = go ts (acc + 1)
      | otherwise = go ts acc



contarTarefasPorStatus :: [Task] -> IO ()
contarTarefasPorStatus tasks = do
    putStrLn "\nEscolha o status para contar:"
    putStrLn "1 - Cancelada"
    putStrLn "2 - Em Progresso"
    putStrLn "3 - Concluído"
    putStr "Digite o número do status: "
    hFlush stdout
    statusOpcao <- getLine
    let status = case statusOpcao of
            "1" -> Cancelada 
            "2" -> EmProgresso  
            "3" -> Concluida   
            _   -> EmProgresso  
    let quantidade = countTasksByStatus tasks status
    putStrLn $ "\nNúmero de tarefas com status " ++ show status ++ ": " ++ show quantidade

markTaskAsDone :: FilePath -> Int -> [Task] -> IO [Task]
markTaskAsDone activitiesFilePath taskId tasks = do
    let updatedTasks = markDone taskId tasks
    case updatedTasks of
        Nothing -> return tasks  -- Retorna a lista original se não encontrar a tarefa
        Just newTasks -> do
            -- Persiste as tarefas atualizadas no arquivo
            saveTasks activitiesFilePath newTasks
            return newTasks

markDone :: Int -> [Task] -> Maybe [Task]
markDone taskId tasks = 
    let (before, after) = span (\task -> taskId /= taskId task) tasks
    in case after of
        (task:rest) -> Just (before ++ [task { status = Concluida }] ++ rest)
        []          -> Nothing

