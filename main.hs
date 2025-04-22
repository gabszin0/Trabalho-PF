{-}
module Main where

import Data.Time.Calendar
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef
import System.IO
import Tipos
import Funcoes

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  tarefasRef <- newIORef [] -- Lista vazia de início
  menu tarefasRef

menu :: IORef [Tarefa] -> IO ()
menu tarefasRef = do
  putStrLn "\n==== Menu ===="
  putStrLn "1. Adicionar tarefa"
  putStrLn "2. Remover tarefa"
  putStrLn "3. Marcar como concluída"
  putStrLn "4. Relatório de tarefas"
  putStrLn "5. Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> adicionarInterativo tarefasRef
    "2" -> removerInterativo tarefasRef
    "3" -> concluirInterativo tarefasRef
    "4" -> do
         lista <- readIORef tarefasRef
         putStrLn $ gerarRelatorio lista
         menu tarefasRef
    "5" -> putStrLn "Saindo..."
    _   -> putStrLn "Opção inválida." >> menu tarefasRef

adicionarInterativo :: IORef [Tarefa] -> IO ()
adicionarInterativo ref = do
  putStr "ID: "
  idStr <- getLine
  putStr "Descrição: "
  desc <- getLine

  putStrLn "Status (1 = Pendente, 2 = Concluída): "
  statusStr <- getLine
  let status' = case statusStr of
                  "2" -> Concluída
                  _   -> Pendente

  putStrLn "Prioridade (1 = Alta, 2 = Média, 3 = Baixa): "
  prioridadeStr <- getLine
  let prioridade' = case prioridadeStr of
                      "1" -> Alta
                      "3" -> Baixa
                      _   -> Media

  putStrLn "Categoria (1 = Trabalho, 2 = Estudo, 3 = Pessoal, 4 = Outro): "
  categoriaStr <- getLine
  let categoria' = case categoriaStr of
                     "1" -> Trabalho
                     "2" -> Estudo
                     "3" -> Pessoal
                     _   -> Outro

  putStr "Prazo (formato AAAA-MM-DD ou deixe vazio): "
  prazoStr <- getLine
  let prazo' = if null prazoStr then Nothing else Just (read prazoStr :: Day)

  putStr "Tags (separadas por vírgula): "
  tagsStr <- getLine
  let tags' = map trim (splitByComma tagsStr)

  let novaTarefa = Tarefa 
        { idTarefa = read idStr
        , descricao = desc
        , status = status'
        , prioridade = prioridade'
        , categoria = categoria'
        , prazo = prazo'
        , tags = tags' }

  lista <- readIORef ref
  case adicionarTarefa novaTarefa lista of
    Left erro -> putStrLn erro
    Right novaLista -> writeIORef ref novaLista >> putStrLn "Tarefa adicionada com sucesso!"
  menu ref

removerInterativo :: IORef [Tarefa] -> IO ()
removerInterativo ref = do
  putStr "ID da tarefa para remover: "
  idStr <- getLine
  lista <- readIORef ref
  case removerTarefa (read idStr) lista of
    Left erro -> putStrLn erro
    Right novaLista -> writeIORef ref novaLista >> putStrLn "Tarefa removida!"
  menu ref

concluirInterativo :: IORef [Tarefa] -> IO ()
concluirInterativo ref = do
  putStr "ID da tarefa para concluir: "
  idStr <- getLine
  lista <- readIORef ref
  case marcarConcluida (read idStr) lista of
    Left erro -> putStrLn erro
    Right novaLista -> writeIORef ref novaLista >> putStrLn "Tarefa marcada como concluída!"
  menu ref



-- Utilitários

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
  (a, ',':rest) -> a : splitByComma rest
  (a, "")       -> [a]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-}

module Main where

import Data.Time.Calendar
import Tipos
import Funcoes

-- Função principal de execução do sistema de tarefas
main :: IO ()
main = loop []

-- Loop principal que mantém o programa executando
loop :: [Tarefa] -> IO ()
loop tarefas = do
  putStrLn "======================="
  putStrLn "  SISTEMA DE TAREFAS"
  putStrLn "======================="
  putStrLn "1. Adicionar Tarefa"
  putStrLn "2. Listar Tarefas"
  putStrLn "3. Listar Tarefas por Prioridade"
  putStrLn "4. Marcar Tarefa como Concluída"
  putStrLn "5. Remover Tarefa"
  putStrLn "6. Relatório de tarefas"
  putStrLn "7. Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      novaTarefa <- criarTarefaInteractiva tarefas
      case adicionarTarefa novaTarefa tarefas of
        Left err -> putStrLn err >> loop tarefas
        Right novaLista -> putStrLn "Tarefa adicionada!" >> loop novaLista

    "2" -> do
      putStrLn "Tarefas cadastradas:"
      mapM_ print tarefas
      loop tarefas

    "3" -> do
      putStrLn "Informe a prioridade (Baixa | Media | Alta):"
      prioridadeStr <- getLine
      let prioridade = case prioridadeStr of
                          "Baixa" -> Baixa
                          "Media" -> Media
                          "Alta"  -> Alta
                          _       -> Media
      let listaFiltrada = listarPorPrioridade prioridade tarefas
      putStrLn "Tarefas com a prioridade informada:"
      mapM_ print listaFiltrada
      loop tarefas

    "4" -> do
      putStr "Informe o ID da tarefa que deseja marcar como concluída: "
      idStr <- getLine
      let idNum = read idStr :: Int
      case marcarConcluida idNum tarefas of
        Left err -> putStrLn err >> loop tarefas
        Right novaLista -> putStrLn "Tarefa marcada como concluída." >> loop novaLista

    "5" -> do
      putStr "Informe o ID da tarefa que deseja remover: "
      idStr <- getLine
      let idNum = read idStr :: Int
      case removerTarefa idNum tarefas of
        Left err -> putStrLn err >> loop tarefas
        Right novaLista -> putStrLn "Tarefa removida." >> loop novaLista

    "6" -> do
      putStrLn "Relatório de tarefas:"
      let relatorio = gerarRelatorio tarefas
      putStrLn relatorio
      loop tarefas

    "7" -> putStrLn "Saindo..."
    _   -> putStrLn "Opção inválida." >> loop tarefas

-- Função para criar uma tarefa interativamente via terminal
criarTarefaInteractiva :: [Tarefa] -> IO Tarefa
criarTarefaInteractiva tarefas = do
  putStrLn "== Criar Nova Tarefa =="
  putStrLn "Descrição:"
  desc <- getLine
  putStrLn "Prioridade (Baixa | Media | Alta):"
  prioStr <- getLine
  let prio = case prioStr of
                "Baixa" -> Baixa
                "Media" -> Media
                "Alta"  -> Alta
                _       -> Media
  putStrLn "Categoria (Trabalho | Estudo | Pessoal | Outro):"
  catStr <- getLine
  let cat = case catStr of
              "Trabalho" -> Trabalho
              "Estudo"   -> Estudo
              "Pessoal"  -> Pessoal
              _          -> Outro
  putStrLn "Prazo (formato: YYYY-MM-DD) ou ENTER para sem prazo:"
  prazoStr <- getLine
  let prazo = if null prazoStr then Nothing else Just (read prazoStr :: Day)
  putStrLn "Tags (separadas por espaço):"
  tagsStr <- getLine
  let tagsList = words tagsStr
  let novoId = if null tarefas then 1 else maximum (map idTarefa tarefas) + 1
  return (Tarefa novoId desc Pendente prio cat prazo tagsList)