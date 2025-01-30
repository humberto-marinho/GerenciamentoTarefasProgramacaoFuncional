module Task (
    Task(..), 
    createTask, 
    saveTasks, 
    loadTasks, 
    Status(..), 
    filterTasksByStatus, 
    sortTasksByName,
    filterTasksByCategory,
    filtrarTasks,
    contarTarefasPorStatus,
    markTaskAsDone
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
  { taskId      :: Int        
  , nome        :: String     
  , descricao   :: String     
  , categoria   :: Category   
  , status      :: Status     
  , dataInicial :: UTCTime    
  , dataFinal   :: UTCTime    
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

-- Função para filtrar as tasks
filtrarTasks :: [Task] -> [Category] -> IO ()
filtrarTasks tasks categories = do
    -- Pergunta se deve filtrar por status
    putStrLn "Filtrar por status (s/n)?"
    statusResponse <- getLine
    let statusFilter = if statusResponse == "s" 
                        then Just <$> askStatus 
                        else return Nothing

    -- Pergunta se deve filtrar por categoria
    putStrLn "Filtrar por categoria (s/n)?"
    categoryResponse <- getLine
    let categoryFilter = if categoryResponse == "s" 
                         then askCategory categories
                         else return Nothing

    -- Pergunta se deve filtrar por data
    putStrLn "Filtrar por data (s/n)?"
    dataResponse <- getLine
    let dataFilter = if dataResponse == "s" 
                     then askData
                     else return Nothing

    -- Lê e valida todos os filtros
    status <- statusFilter
    category <- categoryFilter
    dateRange <- dataFilter

    -- Filtra as tarefas com base nos critérios fornecidos
    let validTasks = filterTasksByCriteria tasks status category dateRange

    -- Exibe as tarefas filtradas
    putStrLn "Tarefas filtradas:"
    mapM_ print validTasks

-- Função para perguntar e retornar o status
askStatus :: IO Status
askStatus = do
    putStrLn "Digite o status (Agendada | EmProgresso | Cancelada | Concluida):"
    statusInput <- getLine
    case readMaybe statusInput :: Maybe Status of
      Just status -> return status
      Nothing -> do
        putStrLn "Status inválido."
        askStatus

-- Função para perguntar e retornar a categoria
askCategory :: [Category] -> IO (Maybe Category)
askCategory categories = do
    putStrLn "Categorias disponíveis:"
    putStrLn $ showCategories categories
    putStrLn "Digite o ID da categoria:"
    categoryIdInput <- getLine
    let maybeCategoria = findCategory (readMaybe categoryIdInput) categories
    
    case maybeCategoria of
        Just categoria -> return (Just categoria)
        Nothing -> do
            putStrLn "Categoria não encontrada. Tente novamente."
            askCategory categories

-- Função para perguntar e retornar o intervalo de datas
askData :: IO (Maybe (UTCTime, UTCTime))
askData = do
    putStrLn "Digite a data inicial (YYYY-MM-DD HH:MM:SS):"
    startDateInput <- getLine
    startDate <- parseDate startDateInput
    case startDate of
      Nothing -> do
        putStrLn "Data inicial inválida."
        return Nothing
      Just start -> do
        putStrLn "Digite a data final (YYYY-MM-DD HH:MM:SS):"
        endDateInput <- getLine
        endDate <- parseDate endDateInput
        case endDate of
          Nothing -> do
            putStrLn "Data final inválida."
            return Nothing
          Just end -> return (Just (start, end))

-- Função para filtrar as tarefas com base nos critérios
filterTasksByCriteria :: [Task] -> Maybe Status -> Maybe Category -> Maybe (UTCTime, UTCTime) -> [Task]
filterTasksByCriteria tasks statusFilter categoryFilter dateFilter =
  let filteredByStatus = case statusFilter of
        Just inputStatus -> filter (\task -> inputStatus == status task) tasks
        Nothing -> tasks
      filteredByCategory = case categoryFilter of
        Just category -> filter (\task -> category == categoria task) filteredByStatus
        Nothing -> filteredByStatus
      filteredByDate = case dateFilter of
        Just (start, end) -> filter (\task -> dataFinal task >= start && dataFinal task <= end) filteredByCategory
        Nothing -> filteredByCategory
  in filteredByDate

--Contar tasks por status com função recursão de cauda
contarTarefasPorStatus :: [Task] -> IO ()
contarTarefasPorStatus tasks = do
    putStrLn "\nEscolha o status para contar:"
    putStrLn "1 - Agendada"
    putStrLn "2 - Em Progresso"
    putStrLn "3 - Concluído"
    putStrLn "4 - Cancelada"
    putStr "Digite o número do status: "
    hFlush stdout
    statusOpcao <- getLine
    let status = case statusOpcao of
            "1" -> Just Agendada
            "2" -> Just EmProgresso
            "3" -> Just Concluida
            "4" -> Just Cancelada
            _   -> Nothing
    case status of
        Just selectedStatus -> do
            let quantidade = countTasksByStatus tasks selectedStatus
            putStrLn $ "\nNúmero de tarefas com status " ++ show selectedStatus ++ ": " ++ show quantidade
        Nothing -> do
            putStrLn "Opção inválida. Por favor, escolha um número de 1 a 3."
            contarTarefasPorStatus tasks  -- Chama a função novamente para permitir uma nova tentativa


-- Função recursiva para contar tarefas por status.
countTasksByStatus :: [Task] -> Status -> Int
countTasksByStatus tasks targetStatus = go tasks 0
  where
    go [] acc = acc
    go (t:ts) acc
      | status t == targetStatus = go ts (acc + 1)
      | otherwise = go ts acc


-- Função para marcar a tarefa como concluída usando Functor na forma fmap
markTaskAsDone :: FilePath -> Int -> [Task] -> IO [Task]
markTaskAsDone filePath targetId tasks = do
    -- Mapeia sobre a lista de tarefas, e se encontrar a tarefa com o ID fornecido, atualiza o status para Concluida
    let updatedTasks = fmap (\task -> if taskId task == targetId 
                                      then task { status = Concluida }
                                      else task) tasks
    
    -- Verifica se a tarefa foi encontrada
    let taskFound = any (\task -> taskId task == targetId) tasks
    
    if taskFound
        then do
            saveTasks filePath updatedTasks  -- Persiste as tarefas atualizadas no arquivo
            putStrLn $ "Tarefa com ID " ++ show targetId ++ " foi marcada como concluída."
        else do
            putStrLn $ "Tarefa com ID " ++ show targetId ++ " não encontrada."
    return updatedTasks  -- Retorna a lista de tarefas atualizada
