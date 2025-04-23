module Persistencia where

import Tipos 
import System.IO
import Control.Exception (catch, IOException)

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo caminho tarefas = writeFile caminho (show tarefas)

carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
    conteudo <- catch (readFile caminho) tratarErro
    return (read conteudo :: [Tarefa])
  where
    tratarErro :: IOException -> IO String
    tratarErro _ = return "[]"
    