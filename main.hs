module Main where


import Data.Time.Calendar
import Tipos
import Funcoes
import System.IO

-- Função principal de execução do sistema de tarefas
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- Desativa o buffering de saída
  loop []

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
  putStrLn "7. Verificar Atrasos"
  putStrLn "8. Buscar por palavra-chave"
  putStrLn "9. Sair"
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
      putStrLn "Informe o ID da tarefa que deseja marcar como concluída: "
      idStr <- getLine
      let idNum = read idStr :: Int
      case marcarConcluida idNum tarefas of
        Left err -> putStrLn err >> loop tarefas
        Right novaLista -> putStrLn "Tarefa marcada como concluída." >> loop novaLista

    "5" -> do
      putStrLn "Informe o ID da tarefa que deseja remover: "
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

    "7" -> do
      putStrLn "Data atual (yyyy-mm-dd): "; dataStr <- getLine
      let atrasadas = verificarAtrasos tarefas (read dataStr :: Day)
      putStrLn "Tarefas em atraso:"
      mapM_ print atrasadas
      loop tarefas

    "8" -> do
      putStrLn "Digite a palavra-chave: "; palavra <- getLine
      let encontradas = buscarPorPalavraChave palavra tarefas
      putStrLn "Tarefas encontradas:"
      mapM_ print encontradas
      loop tarefas

    "9" -> putStrLn "Saindo..."
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