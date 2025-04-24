# 🧠 Sistema de Gerenciamento de Tarefas em Haskell

Um gerenciador de tarefas simples, funcional e extensível, escrito inteiramente em **Haskell**, para organizar suas atividades com estilo funcional.

## 🚀 Funcionalidades

- ✅ Adicionar, listar e remover tarefas
- 🎯 Filtrar por prioridade, categoria, status, tags e palavras-chave
- 📅 Verificar tarefas em atraso
- 📊 Gerar relatórios com estatísticas de uso
- 💾 Salvar e carregar tarefas de arquivos
- 🤝 Interface interativa via terminal

## 🏗 Estrutura do Projeto

O projeto é dividido em módulos:

- **`Tipos.hs`**: Define os tipos de dados como `Tarefa`, `Status`, `Prioridade`, etc.
- **`Funcoes.hs`**: Funções para manipular, buscar, filtrar e gerar relatórios.
- **`Persistencia.hs`**: Lê e escreve tarefas em arquivos.
- **`Main.hs`**: Interface de texto com menu interativo e execução principal.


## 💾 Persistência

- `+` salva as tarefas no arquivo `tarefas.txt`
- `!` carrega as tarefas salvas anteriormente

## 📚 Requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- Biblioteca padrão (`base`)
- Execução via `ghci` ou `runhaskell`

## 📁 Como Rodar

```bash
ghc main.hs funcoes.hs tipos.hs persistencias.hs
    depois
ghc main.hs funcoes.hs tipos.hs persistencias.hs -o agenda
    em seguida
./agenda
```

## ✨ Relatório Inteligente

A função `gerarRelatorio` resume:

- Total de tarefas
- Tarefas pendentes vs. concluídas
- Quantidade e percentual por categoria

## 📌 Observações

- IDs são gerados automaticamente com base na lista existente
- Prazo é opcional (`YYYY-MM-DD`)
- Tags devem ser separadas por espaços
- E a busca por palavra-chave é referente a descrição da tarefa
