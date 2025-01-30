module Main where

import System.IO
import Category
import Task (Task(..), Status(..), createTask, saveTasks, loadTasks, contarTarefasPorStatus, filterTasksByCategory,filtrarTasks,contarTarefasPorStatus,markTaskAsDone)

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
    putStrLn "2 - Deletar Categoria"
    putStrLn "3 - Criar Tarefa"
    putStrLn "4 - Filtrar Tarefas"
    putStrLn "5 - Contar tarefas por status"
    putStrLn "6 - Concluir Tarefa"
    putStrLn "7 - Para o Programa"
    putStr "Digite o número da opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> do
            updatedCategories <- createCategory categoriesFilePath categories
            loop updatedCategories tasks
        "2" -> do
            updatedCategories <- deleteCategory categoriesFilePath categories
            loop updatedCategories tasks    
        "3" -> do
            updatedTasks <- createTask activitiesFilePath categories tasks
            loop categories updatedTasks
        "4" -> do
            filtrarTasks tasks categories
            loop categories tasks
        "5" -> do
            contarTarefasPorStatus tasks
            loop categories tasks
        "6" -> do
            putStrLn "Digite o ID da tarefa que você deseja concluir:"
            hFlush stdout
            taskIdStr <- getLine
            let taskId = read taskIdStr :: Int
            updatedTasks <- markTaskAsDone activitiesFilePath taskId tasks
            loop categories updatedTasks
        "7" -> do
            putStrLn "Encerrando o programa. Até logo!"
            putStrLn "\nCategorias criadas nesta sessão:"
            mapM_ print categories
            putStrLn "\nTarefas criadas nesta sessão:"
            mapM_ print tasks
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            loop categories tasks

