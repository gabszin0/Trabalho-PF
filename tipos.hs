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
  } deriving (Show, Eq, Read)
