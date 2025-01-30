module Task (
    Task(..), 
    createTask, 
    saveTasks, 
    loadTasks, 
    Status(..), 
    filterTasksByStatus, 
    sortTasksByName,
    filterTasksByCategory
) where


import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import System.IO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Category (Category(..), showCategories)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (UTCTime)

-- Tipo Status
data Status = Agendada | EmProgresso | Cancelada | Concluida
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


createTask :: FilePath -> [Category] -> [Task] -> IO [Task]
createTask filePath categories tasks = do
    -- Nome da tarefa
    putStrLn "Digite o nome da tarefa:"
    nome <- getLine
    if null nome then do
        putStrLn "Erro: O nome da tarefa não pode ser vazio!"
        return tasks
    else do
        -- Descrição da tarefa
        putStrLn "Digite a descrição da tarefa:"
        descricao <- getLine
        if null descricao then do
            putStrLn "Erro: A descrição da tarefa não pode ser vazia!"
            return tasks
        else do
            -- Categoria
            putStrLn "Categorias disponíveis:"
            putStrLn $ showCategories categories
            putStrLn "Digite o ID da categoria:"
            categoryIdInput <- getLine
            let maybeCategoria = findCategory (readMaybe categoryIdInput) categories
            case maybeCategoria of
                Just categoria -> do
                    -- Status
                    putStrLn "Digite o status da tarefa (Agendada | EmProgresso | Cancelada | Concluida):"
                    statusInput <- getLine
                    let maybeStatus = readMaybe statusInput :: Maybe Status
                    case maybeStatus of
                        Just status -> do
                            -- Data Inicial
                            putStrLn "Digite a data inicial no formato YYYY-MM-DD HH:MM:SS:"
                            dataInicialInput <- getLine
                            maybeDataInicial <- parseDate dataInicialInput

                            -- Data Final
                            putStrLn "Digite a data final no formato YYYY-MM-DD HH:MM:SS:"
                            dataFinalInput <- getLine
                            maybeDataFinal <- parseDate dataFinalInput

                            -- Validação das datas
                            case (maybeDataInicial, maybeDataFinal) of
                                (Just dataInicial, Just dataFinal) -> do
                                    if dataInicial >= dataFinal then do
                                        putStrLn "Erro: A data inicial deve ser anterior à data final!"
                                        return tasks  -- Retorna a lista original de tarefas
                                    else do
                                        -- Criação da Tarefa com ID
                                        let newTaskId = length tasks + 1 -- ID da tarefa será baseado no tamanho do vetor
                                        let newTask = Task newTaskId nome descricao categoria status dataInicial dataFinal
                                        let updatedTasks = tasks ++ [newTask]

                                        -- Salvar tarefas no arquivo
                                        saveTasks filePath updatedTasks
                                        putStrLn $ "Tarefa criada e salva: " ++ show newTask

                                        return updatedTasks  -- Retorna a lista de tarefas atualizada
                                _ -> do
                                    putStrLn "Erro: Formato de data inválido! Use o formato YYYY-MM-DD HH:MM:SS."
                                    return tasks  -- Retorna a lista original de tarefas
                        Nothing -> do
                            putStrLn "Erro: Status inválido!"
                            return tasks  -- Retorna a lista original de tarefas
                Nothing -> do
                    putStrLn "Erro: Categoria inválida!"
                    return tasks  -- Retorna a lista original de tarefas



-- Função auxiliar para buscar categoria por ID
findCategory :: Maybe Int -> [Category] -> Maybe Category
findCategory (Just cid) categories = lookup cid $ map (\c -> (categoryId c, c)) categories
findCategory Nothing _ = Nothing

-- Função auxiliar para validar e converter data e hora
parseDate :: String -> IO (Maybe UTCTime)
parseDate input =
  return $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" input


-- Função para filtrar tarefas por status
filterTasksByStatus :: Status -> [Task] -> [Task]
filterTasksByStatus st = filter (\task -> status task == st)

-- Função para ordenar tarefas por nome
sortTasksByName :: [Task] -> [Task]
sortTasksByName = sortBy (comparing nome)

filterTasksByCategory :: Int -> [Task] -> [Task]
filterTasksByCategory catId tasks =
    filter (\task -> categoryId (categoria task) == catId) tasks






