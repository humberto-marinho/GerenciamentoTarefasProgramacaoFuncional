module Main where

import System.IO
import Category
import Task (Task(..), Status(..), createTask, saveTasks, loadTasks, filterTasksByStatus, sortTasksByName, filterTasksByCategory)

categoriesFilePath :: FilePath
categoriesFilePath = "dataBase/categories.txt"

activitiesFilePath :: FilePath
activitiesFilePath = "dataBase/activities.txt"

main :: IO ()
main = do
    categories <- loadCategories categoriesFilePath -- Carrega as categorias do arquivo
    tasks <- loadTasks activitiesFilePath -- Carrega as tarefas do arquivo
    putStrLn "Bem-vindo ao sistema de gerenciamento de categorias e tarefas!"
    putStrLn "\nCategorias carregadas do arquivo:"
    mapM_ print categories
    putStrLn "\nTarefas carregadas do arquivo:"
    mapM_ print tasks
    loop categories tasks

-- Função principal de loop
loop :: [Category] -> [Task] -> IO ()
loop categories tasks = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1 - Criar Categoria"
    putStrLn "2 - Criar Tarefa"
    putStrLn "3 - Listar Tarefas em Andamento (Ordenadas por Nome)"
    putStrLn "4 - Filtrar Tarefas"
    putStrLn "5 - Encerrar o Programa"
    putStr "Digite o número da opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> do
            updatedCategories <- createCategory categoriesFilePath categories
            loop updatedCategories tasks
        "2" -> do
            updatedTasks <- createTask activitiesFilePath categories tasks
            loop categories updatedTasks
        "3" -> do
            let inProgressTasks = sortTasksByName (filterTasksByStatus EmProgresso tasks)
            putStrLn "\nTarefas em andamento (ordenadas por nome):"
            mapM_ print inProgressTasks
            loop categories tasks
        "4" -> do
            menuFiltragem categories tasks
            loop categories tasks
        "5" -> do
            putStrLn "Encerrando o programa. Até logo!"
            putStrLn "\nCategorias criadas nesta sessão:"
            mapM_ print categories
            putStrLn "\nTarefas criadas nesta sessão:"
            mapM_ print tasks
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            loop categories tasks

-- Menu de filtragem (subopções)
menuFiltragem :: [Category] -> [Task] -> IO ()
menuFiltragem categories tasks = do
    putStrLn "\nEscolha o critério de filtragem:"
    putStrLn "1 - Filtrar por Status"
    putStrLn "2 - Filtrar por Categoria"
    putStrLn "3 - Voltar ao menu principal"
    putStr "Digite o número da opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> filtrarPorStatus tasks
        "2" -> filtrarPorCategoria categories tasks
        "3" -> return ()
        _   -> do
            putStrLn "Opção inválida. Retornando ao menu principal."

-- Função para filtrar por Status
filtrarPorStatus :: [Task] -> IO ()
filtrarPorStatus tasks = do
    putStrLn "\nEscolha o status para filtrar:"
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
    let tarefasFiltradas = filterTasksByStatus status tasks
    putStrLn "\nTarefas filtradas por status:"
    if null tarefasFiltradas
        then putStrLn "Nenhuma tarefa encontrada com esse status."
        else mapM_ print tarefasFiltradas

-- Função para filtrar por Categoria
filtrarPorCategoria :: [Category] -> [Task] -> IO ()
filtrarPorCategoria categories tasks = do
    putStrLn "\nCategorias disponíveis:"
    mapM_ (\(i, Category cid name) -> putStrLn (show i ++ " - " ++ name)) (zip [1..] categories)
    putStr "Digite o número da categoria: "
    hFlush stdout
    categoriaOpcao <- getLine
    let categoriaIndex = read categoriaOpcao - 1
    if categoriaIndex >= 0 && categoriaIndex < length categories
        then do
            let Category catId _ = categories !! categoriaIndex
            let tarefasFiltradas = filterTasksByCategory catId tasks
            putStrLn "\nTarefas filtradas por categoria:"
            if null tarefasFiltradas
                then putStrLn "Nenhuma tarefa encontrada para essa categoria."
                else mapM_ print tarefasFiltradas
        else putStrLn "Opção inválida. Retornando ao menu principal."
