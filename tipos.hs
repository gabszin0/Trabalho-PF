module Tipos where

import Data.Time.Calendar -- Para trabalhar com datas

-- Definição dos Tipos Algébricos

data Status = Pendente | Concluída 
   deriving (Show , Eq, Read)

data Prioridade = Baixa | Media | Alta 
   deriving (Show, Eq, Ord, Read)

data Categoria = Trabalho | Estudo | Pessoal | Outro 
   deriving (Show, Eq, Read)

data Tarefa = Tarefa
  { idTarefa    :: Int
  , descricao   :: String
  , status      :: Status
  , prioridade  :: Prioridade
  , categoria   :: Categoria
  , prazo       :: Maybe Day
  , tags        :: [String]
  } deriving (Eq, Read)

-- Instância personalizada de Show para imprimir as tarefas de forma mais bonita
instance Show Tarefa where
  show t = unlines
    [ "===================="
    , "ID: " ++ show (idTarefa t)
    , "Descrição: " ++ descricao t
    , "Status: " ++ show (status t)
    , "Prioridade: " ++ show (prioridade t)
    , "Categoria: " ++ show (categoria t)
    , "Prazo: " ++ maybe "Sem prazo" show (prazo t)
    , "Tags: " ++ unwords (tags t)
    , "===================="
    ]
