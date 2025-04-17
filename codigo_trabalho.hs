-- Importando Bibliotecas Auxiliares

import Data.Time.Calendar -- Define e manipula datas (Day, fromGregorian, etc).


-- Instanciando o show para a melhor visibilidade 
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


-- Definição dos Tipos Algébricos

data Status = Pendente | Concluída 
   deriving (Show , Eq)

data Prioridade = Baixa | Media | Alta 
   deriving (Show, Eq, Ord)

data Categoria = Trabalho | Estudo | Pessoal | Outro 
   deriving (Show, Eq)

data Tarefa = Tarefa
  {idTarefa    :: Int
  , descricao  :: String
  , status     :: Status
  , prioridade :: Prioridade
  , categoria  :: Categoria
  , prazo      :: Maybe Day
  , tags       :: [String]
  } deriving (Eq)



-- Tarefas testes!

t1 = Tarefa 1 "estudar Haskell" Pendente Media Estudo (Just (fromGregorian 2025 03 21)) ["faculdade"]
t2 = Tarefa 2 "fazer compra" Pendente Alta Pessoal (Just (fromGregorian 2025 04 04)) ["mercado"]
t3 = Tarefa 3 "fazer exercicio" Concluída Baixa Pessoal (Just (fromGregorian 2025 04 15)) ["academia"]
t4 = Tarefa 4 "ler artigo de Ciência de Dados" Pendente Alta Estudo (Just (fromGregorian 2025 04 18)) ["leitura", "importante"]
t5 = Tarefa 5 "lavar roupa" Concluída Baixa Pessoal (Just (fromGregorian 2025 04 16)) ["casa"]
t6 = Tarefa 6 "fazer relatório do estágio" Pendente Alta Trabalho (Just (fromGregorian 2025 04 20)) ["estágio", "deadline"]
t7 = Tarefa 7 "assistir aula de Sistemas Operacionais" Concluída Media Estudo (Just (fromGregorian 2025 04 10)) ["faculdade"]
t8 = Tarefa 8 "ligar para o banco" Pendente Media Pessoal Nothing ["urgente", "financeiro"]
t9 = Tarefa 9 "refatorar código do projeto" Concluída Alta Trabalho (Just (fromGregorian 2025 04 22)) ["projeto", "código"]

lista1a5 = [t1, t2, t3, t4, t5]
lista6a9 = [t6, t7, t8, t9]



-- 1. Funções Básicas

{- 1.1
   No terminal é necessário criar uma 'lista0' = []
   e depois adicionar as tarefas com a função 'adicionarTarefa'
   Exemplo de uso:
   lista0 = []
   lista1 = adicionarTarefa t1 lista0
-} 
adicionarTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionarTarefa tarefa1 lista
  | existeId (idTarefa tarefa1) lista = Left "ID já existe"
  | otherwise = Right (tarefa1 : lista)
  where
    existeId id = any (\t -> idTarefa t == id)


{- 1.2
   No terminal é necessário já possuir uma lista com tarefas
   e depois remover a tarefa com a função 'removerTarefa'
   Exemplo de uso:
   lista1 = [t1, t2, t3]
   lista2 = removerTarefa 2 lista1
-}
removerTarefa :: Int -> [Tarefa] -> Either String [Tarefa]
removerTarefa idRemocao lista 
    | any (\t -> idTarefa t == idRemocao) lista = Right (filter (\t -> idTarefa t /= idRemocao) lista)
    | otherwise = Left "ID nao encontrado"


{- 1.3
   No terminal é necessário já possuir uma lista com tarefas
   e depois marcar a tarefa como concluída com a função 'marcarConcluida'
   Exemplo de uso:
   marcarConcluida 1 lista1
-}
marcarConcluida :: Int-> [Tarefa] -> Either String [Tarefa]
marcarConcluida idConcluida lista  
    | any (\t -> idTarefa t == idConcluida) lista = Right (map (\t -> if idTarefa t == idConcluida then t {status = Concluída} else t) lista)
    | otherwise = Left "ID nao encontrado"



-- 2. Funções Avançadas

{- 2.1
   Primeiro criamos um função de alta ordem para generalizar a utilização do código
   e depois utilizamos essa função para criar as funções de listar por categoria e prioridade
   A função listarPor recebe um predicado e uma lista, e retorna uma nova lista contendo apenas os elementos que satisfazem o predicado
-}
listarPor :: (a -> Bool) -> [a] -> [a]
listarPor pred lista = filter pred lista
   

{- 2.2
   Exemplo de uso:
   listarPorCategoria Trabalho lista1
-}
listarPorCategoria :: Categoria-> [Tarefa]-> [Tarefa]
listarPorCategoria cate lista = listarPor (\t -> categoria t == cate) lista


{- 2.3
   Exemplo de uso:
   listarPorPrioridade Alta lista1 
-}
listarPorPrioridade :: Prioridade-> [Tarefa]-> [Tarefa]
listarPorPrioridade prio lista = listarPor (\t -> prioridade t == prio) lista


{- 2.4
   A função 'ordenarPorPrioridade' utiliza a função 'listarPorPrioridade' para criar 
   uma lista ordenada por prioridade, concatenando a lista de tarefas de cada prioridade
   Exemplo de uso:
   ordenarPorPrioridade lista1a5 
-} 
ordenarPorPrioridade :: [Tarefa]-> [Tarefa]
ordenarPorPrioridade lista = listarPorPrioridade Alta lista ++ 
                             listarPorPrioridade Media lista ++ 
                             listarPorPrioridade Baixa lista  

{- 2.5
   Nessa função é utilizado a função de alta ordem que construímos para ser um novo filter,
   com base nisso ele busca a tarefa pelo status inserido, retornando todos os itens na lista que correspondem 
   Exemplo de uso:
   filtrarPorStatus Pendente lista1a5
-}
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus stat lista = listarPor (\t -> status t == stat) lista


{- 2.6
   A função 'buscarPorPalavraChave' utiliza a função 'listarPor' para filtrar as tarefas
   que contêm a palavra-chave na descrição, retornando uma lista com as tarefas correspondentes
   Exemplo de uso:
   buscarPorPalavraChave "estudar" lista1a5
-}
buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave desc lista = listarPor (\t -> desc `elem` words (descricao t)) lista


-- 3. Gestão de Prazos

{- 3.1
   Filtra tarefas atrasadas com base na data atual fornecida, onde recebe a lista
   e a data atual, fazendo uma comparação para assim retornar as tarefas que estão 
   com atrasos.
   Exemplo de uso:
   verificarAtrasos lista1a5 (fromGregorian 2025 04 17)
-}

verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos lista dataAtual = filter atrasos lista
  where
   atrasos tarefa = case prazo tarefa of 
                       Just d  -> d < dataAtual
                       Nothing -> False

{- 3.2
   A função 'calcularDiasRestantes' ela recebe a tarefa e a data atual,
   e verifica o seu prazo, com isso faz a diferença entre a data atual e o prazo
   retornando a quantidade de dias restantes, sendo o resultado positivo o quanto falta
   e negativo o quanto já se passou.
   Exemplo de uso:
   calcularPrazoTarefas lista1a5 (fromGregorian 2025 04 17)
-}
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes tarefa dataAtual
  | prazo tarefa == Nothing = Nothing
  | otherwise               = Just (fromEnum d - fromEnum dataAtual)
  where
    Just d = prazo tarefa


-- 4. Sistema de Tags

{- 4.1
   A função compara as tags (strings) e retorna a que for igual a pedida,
   recebendo a tag e a lista de tarefas, retornando as tarefas que possuem
   a tag desejada.
   Exemplo de uso:
   filtrarPorTag "faculdade" lista1a5
-}
filtrarPorTag :: String-> [Tarefa]-> [Tarefa]
filtrarPorTag tag lista = listarPor (\t -> tag `elem` tags t) lista


{- 4.2
   Exemplo de uso:
   
-}

removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados [] = []
removerDuplicados (x:xs)
  | x `elem` xs = removerDuplicados xs
  | otherwise   = x : removerDuplicados xs

nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas = map contarFrequencia tagsUnicas
  where
    todasAsTags  = concatMap tags tarefas
    tagsUnicas   = removerDuplicados todasAsTags
    contarFrequencia tag = (tag, length (filtrarPorTag tag tarefas))