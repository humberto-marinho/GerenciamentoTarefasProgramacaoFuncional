module Category (Category(..), createCategory, saveCategories, loadCategories, showCategories) where

import System.IO
import System.Directory (doesFileExist)

-- Definição do tipo Category
data Category = Category
    { categoryId :: Int
    , categoryName :: String
    } deriving (Show, Read)

-- Função para salvar as categorias no arquivo
saveCategories :: FilePath -> [Category] -> IO ()
saveCategories filePath categories = do
    let content = show categories -- Converte a lista para String usando `show`
    writeFile filePath content -- Salva no arquivo

-- Função para carregar as categorias do arquivo
loadCategories :: FilePath -> IO [Category]
loadCategories filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            content <- readFile filePath
            return $ read content -- Usa `read` para converter o conteúdo para `[Category]`
        else return [] -- Retorna uma lista vazia se o arquivo não existir

-- Função para criar uma nova categoria e persistir no arquivo
createCategory :: FilePath -> [Category] -> IO [Category]
createCategory filePath categories = do
    putStrLn "Digite o nome da nova categoria:"
    name <- getLine
    let newId = length categories + 1 -- O ID será o próximo índice
    let newCategory = Category newId name
    let updatedCategories = categories ++ [newCategory] -- Atualiza a lista local
    saveCategories filePath updatedCategories -- Persiste no arquivo
    putStrLn $ "Categoria criada e salva: " ++ show newCategory
    return updatedCategories

-- Função para exibir as categorias disponíveis
showCategories :: [Category] -> String
showCategories categories = 
    unlines $ map (\c -> show (categoryId c) ++ " - " ++ categoryName c) categories
