# Aula 6 - Probabilidade minima para inferencia
# Metodos Quantitativos e Tecnicas em Ciencia Politica I / Metodos III

options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
})

set.seed(20260617)

# -------------------------------------------------------------------
# 1. Probabilidade como frequencia em repeticoes
# -------------------------------------------------------------------

# Imagine que, na populacao, 55% dos eleitores aprovam o governo.
# Vamos repetir muitas vezes o experimento: sortear 1 eleitor ao acaso.

p_verdadeiro <- 0.55
n_repeticoes <- 500

simulacao <- tibble(
  repeticao = seq_len(n_repeticoes),
  aprova_governo = rbinom(n_repeticoes, size = 1, prob = p_verdadeiro)
) |>
  mutate(frequencia_acumulada = cumsum(aprova_governo) / repeticao)

grafico_frequencia <- ggplot(simulacao, aes(x = repeticao, y = frequencia_acumulada)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = p_verdadeiro, color = "firebrick",
             linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    x = "Numero de repeticoes",
    y = "Frequencia acumulada de aprovacao",
    title = "Probabilidade como frequencia em repeticoes"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_frequencia)
}

# Tarefa 1:
# Mude p_verdadeiro para 0.30 e depois para 0.70.
# O que acontece com a linha azul?

# Tarefa 2:
# Mude n_repeticoes para 30, 100 e 2000.
# Com poucas repeticoes, a frequencia acumulada fica estavel?


# -------------------------------------------------------------------
# 2. Quatro distribuicoes de probabilidade
# -------------------------------------------------------------------

# Bernoulli: um evento binario.
# Exemplo: 1 = aprova, 0 = nao aprova.

p <- 0.55

bernoulli <- tibble(
  resultado = factor(c("0", "1"), levels = c("0", "1")),
  probabilidade = c(1 - p, p)
)

grafico_bernoulli <- ggplot(bernoulli, aes(x = resultado, y = probabilidade)) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.55) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    x = "Resultado",
    y = "Probabilidade",
    title = "Distribuicao Bernoulli"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_bernoulli)
}

# Binomial: numero de sucessos em n tentativas.
# Exemplo: quantos eleitores aprovam em uma amostra de 10?

n_tentativas <- 10

binomial <- tibble(
  sucessos = 0:n_tentativas,
  probabilidade = dbinom(sucessos, size = n_tentativas, prob = p)
)

grafico_binomial <- ggplot(binomial, aes(x = sucessos, y = probabilidade)) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.75) +
  scale_x_continuous(breaks = 0:n_tentativas) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Numero de aprovacoes",
    y = "Probabilidade",
    title = "Distribuicao Binomial"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_binomial)
}

# Uniforme: todos os valores no intervalo tem a mesma densidade.

uniforme <- tibble(
  x = seq(0, 1, length.out = 200),
  densidade = dunif(x, min = 0, max = 1)
)

grafico_uniforme <- ggplot(uniforme, aes(x = x, y = densidade)) +
  geom_area(fill = "#F58518", alpha = 0.75) +
  labs(
    x = "Valor sorteado",
    y = "Densidade",
    title = "Distribuicao Uniforme"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_uniforme)
}

# Normal: curva em sino. A Normal reaparece quando estudamos
# distribuicoes amostrais.

normal <- tibble(
  x = seq(-3.5, 3.5, length.out = 300),
  densidade = dnorm(x, mean = 0, sd = 1)
)

grafico_normal <- ggplot(normal, aes(x = x, y = densidade)) +
  geom_area(fill = "steelblue", alpha = 0.75) +
  labs(
    x = "Desvios-padrao da media",
    y = "Densidade",
    title = "Distribuicao Normal"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_normal)
}

# Tarefa 3:
# Mude p para 0.30 e 0.70 e rode de novo os graficos de Bernoulli e
# Binomial. O que muda?


# -------------------------------------------------------------------
# 3. Evento e probabilidade condicional
# -------------------------------------------------------------------

# Esta populacao e simulada. Ela nao descreve uma pesquisa real.
# O objetivo e mostrar a diferenca entre P(A) e P(A | grupo).

n_populacao <- 100000

escolaridade <- sample(
  c("Sem ensino superior", "Com ensino superior"),
  size = n_populacao,
  replace = TRUE,
  prob = c(0.72, 0.28)
)

prob_aprovacao <- ifelse(escolaridade == "Com ensino superior", 0.48, 0.58)

populacao <- tibble(
  id = seq_len(n_populacao),
  escolaridade = escolaridade,
  prob_aprovacao = prob_aprovacao,
  aprova_governo = rbinom(n_populacao, size = 1, prob = prob_aprovacao)
)

# Probabilidade geral: P(aprova)
mean(populacao$aprova_governo)

# Probabilidade condicional: P(aprova | escolaridade)
populacao |>
  group_by(escolaridade) |>
  summarise(
    n = n(),
    prob_aprovacao = mean(aprova_governo),
    .groups = "drop"
  )

# Tarefa 4:
# Explique por que P(aprova) e P(aprova | ensino superior) nao precisam ser iguais.
# Isso prova que escolaridade causa aprovacao? Por que nao?


# -------------------------------------------------------------------
# 4. Uma amostra e apenas uma realizacao possivel
# -------------------------------------------------------------------

n_amostra <- 100

amostra <- populacao |>
  slice_sample(n = n_amostra)

mean(amostra$aprova_governo)

# Tarefa 5:
# Rode o bloco acima varias vezes. A proporcao amostral e sempre igual?
# Por que isso acontece?


# -------------------------------------------------------------------
# 5. Repetindo muitas amostras
# -------------------------------------------------------------------

n_sim <- 1000

proporcoes_amostrais <- replicate(
  n_sim,
  mean(sample(populacao$aprova_governo, size = n_amostra))
)

resultados <- tibble(
  simulacao = seq_len(n_sim),
  prop_aprovacao = proporcoes_amostrais
)

parametro <- mean(populacao$aprova_governo)

grafico_amostras <- ggplot(resultados, aes(x = prop_aprovacao)) +
  geom_histogram(bins = 35, fill = "steelblue", color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = parametro, color = "firebrick",
             linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Proporcao de aprovacao em cada amostra",
    y = "Numero de amostras",
    title = "A mesma populacao pode gerar muitas amostras diferentes"
  ) +
  theme_minimal()

if (interactive()) {
  print(grafico_amostras)
}

# Tarefa 6:
# Mude n_amostra para 30, 100 e 1000.
# Os pontos ficam mais espalhados ou mais concentrados?
# O que isso antecipa sobre erro-padrao?
