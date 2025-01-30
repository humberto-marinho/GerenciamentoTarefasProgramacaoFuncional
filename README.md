# Sistema de Gerenciamento de Tarefas pelo Terminal
Projeto para aprovação do estudo dirigido em programação funcional 2024.3

Este projeto é um sistema de gerenciamento de tarefas pelo terminal, desenvolvido com ênfase no paradigma funcional. Ele oferece as seguintes funcionalidades:

- **Criar e armazenar tarefas com categorias e status.**
- **Criar e deletar categorias de forma dinamica**
- **Listar tarefas com filtros (categoria,status e data).**
- **Marcar tarefas como concluídas.**
- **Persistir os dados de forma funcional**
- **Concluir tarefas com Functor**
- **Integrar conceitos como mônadas para IO e tratamento de erros.**

## Funcionalidades e Conceitos Implementados

### 1. Paradigma Funcional e Cálculo Lambda
**Objetivo:** Implementar funções simples para manipular listas de tarefas e categorias.

- **Implementação:**
  - Utilizar cálculo lambda e funções básicas para  criar, deletar e atualizar estruturas de dados(Categorias,Tarefas).

### 2. Funções, Recursão e Recursão de Cauda
**Objetivo:** Criar uma função para contar tarefas em determinado status.

- **Implementação:**
  - Implementar função com recursão de cauda para contar tarefas em determinados tipos.

### 3. Sistemas de Tipos e Álgebra de Tipos
**Objetivo:** Definir tipos algébricos para representar as tarefas.

- **Implementação:**
  - Criar tipos `Task`, `Category` usando `data`.

### 4. Funções de Alta Ordem
**Objetivo:** Implementar filtros e ordenações personalizadas.

- **Implementação:**
  - Criar funções de filtro em que o usuario escolha os criterios de filtragem como status, categoria e data.

### 5. Semigrupos, Monoides e Funtores
**Objetivo:** Utilizar semigrupos e monoides para combinar categorias e listas de tarefas.

- **Implementação:**
  - Utilizar `Functor` para mapear transformações sobre tarefas. Exemplo: Atualiza os status da tarefa para concluido, sem substituir elementos da lista diretamente.

### 6. Mônadas e Tratamento de Efeitos Colaterais
**Objetivo:** Gerenciar entrada e saída de dados no terminal e lidar com erros.

- **Implementação:**
  - Usar a mônada `Maybe` para tarefas opcionais. Exemplo: Saber se o usuário forneceu ou não um criterio obrigatorio, então tratar o caso.

### 7. Estruturas de Dados Funcionais
**Objetivo:** Implementar a persistência de dados com estruturas imutáveis.

- **Implementação:**
  - Criar funções para persistir tarefas e categorias.
