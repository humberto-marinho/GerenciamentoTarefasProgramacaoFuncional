module Main where

import System.IO
import Category
import Task (createTask, Task, loadTasks, saveTasks)

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
    putStrLn "3 - Encerrar o Programa"
    putStr "Digite o número da opção: "
    hFlush stdout -- Garante que o prompt apareça antes da entrada
    opcao <- getLine
    case opcao of
        "1" -> do
            updatedCategories <- createCategory categoriesFilePath categories -- Cria e persiste
            loop updatedCategories tasks -- Atualiza a lista local
        "2" -> do
            putStrLn "Criando uma nova tarefa..."
            updatedTasks <- createTask activitiesFilePath categories tasks -- Corrigido aqui
            saveTasks activitiesFilePath updatedTasks -- Persiste as tarefas
            loop categories updatedTasks -- Atualiza a lista local
        "3" -> do
            putStrLn "Encerrando o programa. Até logo!"
            putStrLn "\nCategorias criadas nesta sessão:"
            mapM_ print categories
            putStrLn "\nTarefas criadas nesta sessão:"
            mapM_ print tasks
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            loop categories tasks
