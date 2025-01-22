# Sistema de Gerenciamento de Tarefas pelo Terminal
Projeto para aprovação do estudo dirigido em programação funcional 2024.3

Este projeto é um sistema de gerenciamento de tarefas pelo terminal, desenvolvido com ênfase no paradigma funcional. Ele oferece as seguintes funcionalidades:

- **Criar e armazenar tarefas com categorias.**
- **Criar categorias de forma dinamica**
- **Listar tarefas com filtros (por categoria, status, etc.).**
- **Marcar tarefas como concluídas.**
- **Persistir os dados de forma funcional (usando estruturas imutáveis).**
- **Integrar conceitos como mônadas para IO e tratamento de erros.**

## Funcionalidades e Conceitos Implementados

### 1. Paradigma Funcional e Cálculo Lambda
**Objetivo:** Implementar funções simples para manipular listas de tarefas (adicionar, remover).

- **Possíveis Implementações:**
  - Implementar funções puras básicas.
  - Utilizar cálculo lambda para expressões como ordenação e filtros de tarefas.

### 2. Funções, Recursão e Recursão de Cauda
**Objetivo:** Criar uma função para listar e manipular tarefas de forma recursiva.

- **Possíveis Implementações:**
  - Implementar uma função recursiva para exibir tarefas.
  - Implementar função usando recursão de cauda. Exemplo: contar o número de tarefas em certo status ou categoria.

### 3. Sistemas de Tipos e Álgebra de Tipos
**Objetivo:** Definir tipos algébricos para representar as tarefas.

- **Possíveis Implementações:**
  - Criar tipos `Task`, `Category` e `TaskList` usando `data`.
  - Explorar polimorfismo paramétrico para criar listas genéricas. Exemplo: as tarefas podem compartilhar da mesma estrutura, mas armazenar tipos de atributos diferentes.

### 4. Funções de Alta Ordem
**Objetivo:** Implementar filtros e ordenações personalizadas.

- **Possíveis Implementações:**
  - Criar funções de alta ordem para filtrar tarefas por status ou categoria. Exemplo: uma função de filtragem que recebe outra função definindo os critérios de filtragem.

### 5. Semigrupos, Monoides e Funtores
**Objetivo:** Utilizar semigrupos e monoides para combinar categorias e listas de tarefas.

- **Possíveis Implementações:**
  - Implementar a combinação de listas de tarefas com a instância de `Semigroup`.
  - Utilizar `Functor` para mapear transformações sobre tarefas. Exemplo: uma função que percorre uma lista de tarefas e atualiza um atributo com base em um critério, sem substituir elementos da lista diretamente.

### 6. Mônadas e Tratamento de Efeitos Colaterais
**Objetivo:** Gerenciar entrada e saída de dados no terminal e lidar com erros.

- **Possíveis Implementações:**
  - Usar a mônada `Maybe` para tarefas opcionais. Exemplo: armazenar uma data de conclusão.
  - Implementar o tratamento de erros com a mônada `Either`. Exemplo: validar se uma data de conclusão não está no passado.

### 7. Estruturas de Dados Funcionais
**Objetivo:** Implementar a persistência de dados com estruturas imutáveis.

- **Possíveis Implementações:**
  - Criar uma função para armazenar tarefas em uma lista imutável.
  - Manipular estruturas como mapas para categorizar tarefas.
