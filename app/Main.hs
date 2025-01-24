module Main where

import System.IO
import Category

categoriesFilePath :: FilePath
categoriesFilePath = "dataBase/categories.txt"

main :: IO ()
main = do
    categorias <- loadCategories categoriesFilePath -- Carrega as categorias do arquivo
    putStrLn "Bem-vindo ao sistema de gerenciamento de categorias!"
    putStrLn "Categorias carregadas do arquivo:"
    mapM_ print categorias -- Exibe as categorias carregadas
    loop categorias

-- Função principal de loop
loop :: [Category] -> IO ()
loop categories = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1 - Criar Categoria"
    putStrLn "3 - Encerrar o programa"
    putStr "Digite o número da opção: "
    hFlush stdout -- Garante que o prompt apareça antes da entrada
    opcao <- getLine
    case opcao of
        "1" -> do
            updatedCategories <- createCategory categoriesFilePath categories -- Cria e persiste
            loop updatedCategories -- Atualiza a lista local
        "3" -> do
            putStrLn "Encerrando o programa. Até logo!"
            putStrLn "Categorias criadas nesta sessão:"
            mapM_ print categories -- Exibe as categorias criadas na sessão
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            loop categories
