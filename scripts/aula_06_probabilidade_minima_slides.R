options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
})

set.seed(20260617)

simular_frequencia_evento <- function(n = 500, p = 0.55, seed = 20260617) {
  set.seed(seed)

  tibble(
    repeticao = seq_len(n),
    aprova_governo = rbinom(n, size = 1, prob = p)
  ) |>
    mutate(frequencia_acumulada = cumsum(aprova_governo) / repeticao)
}

criar_populacao_eleitores <- function(n = 100000, seed = 20260617) {
  set.seed(seed)

  escolaridade <- sample(
    c("Sem ensino superior", "Com ensino superior"),
    size = n,
    replace = TRUE,
    prob = c(0.72, 0.28)
  )

  prob_aprovacao <- ifelse(escolaridade == "Com ensino superior", 0.48, 0.58)

  tibble(
    id = seq_len(n),
    escolaridade = factor(
      escolaridade,
      levels = c("Sem ensino superior", "Com ensino superior")
    ),
    prob_aprovacao = prob_aprovacao,
    aprova_governo = rbinom(n, size = 1, prob = prob_aprovacao)
  )
}

simular_medias_amostrais <- function(populacao, n_amostra = 100, n_sim = 1000,
                                     seed = 20260617) {
  set.seed(seed)

  medias <- replicate(
    n_sim,
    mean(sample(populacao$aprova_governo, size = n_amostra))
  )

  tibble(
    simulacao = seq_len(n_sim),
    n_amostra = n_amostra,
    prop_aprovacao = medias
  )
}

criar_distribuicoes_basicas <- function() {
  list(
    bernoulli = tibble(
      resultado = factor(c("0", "1"), levels = c("0", "1")),
      probabilidade = c(0.45, 0.55)
    ),
    binomial = tibble(
      sucessos = 0:10,
      probabilidade = dbinom(sucessos, size = 10, prob = 0.55)
    ),
    uniforme = tibble(
      x = seq(0, 1, length.out = 200),
      densidade = dunif(x, min = 0, max = 1)
    ),
    normal = tibble(
      x = seq(-3.5, 3.5, length.out = 300),
      densidade = dnorm(x, mean = 0, sd = 1)
    )
  )
}

df_frequencia <- simular_frequencia_evento(n = 500, p = 0.55)
populacao_eleitores <- criar_populacao_eleitores()
parametro_aprovacao <- mean(populacao_eleitores$aprova_governo)

set.seed(20260617)
amostra_exemplo <- populacao_eleitores |>
  slice_sample(n = 12) |>
  mutate(
    aprova_governo = ifelse(aprova_governo == 1, "Aprova", "Nao aprova")
  ) |>
  dplyr::select(id, escolaridade, aprova_governo)

probabilidades_condicionais <- populacao_eleitores |>
  group_by(escolaridade) |>
  summarise(
    n = n(),
    prob_aprovacao = mean(aprova_governo),
    .groups = "drop"
  )

df_amostras_100 <- simular_medias_amostrais(
  populacao = populacao_eleitores,
  n_amostra = 100,
  n_sim = 1000
  )

distribuicoes_basicas <- criar_distribuicoes_basicas()

grafico_bernoulli <- ggplot(
  distribuicoes_basicas$bernoulli,
  aes(x = resultado, y = probabilidade)
) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.55) +
  geom_text(
    aes(label = percent(probabilidade, accuracy = 1)),
    vjust = -0.45,
    size = 4
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.7)) +
  labs(
    x = "Resultado",
    y = "Probabilidade"
  ) +
  theme_minimal(base_size = 13)

grafico_binomial <- ggplot(
  distribuicoes_basicas$binomial,
  aes(x = sucessos, y = probabilidade)
) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.75) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Número de aprovações em 10 sorteios",
    y = "Probabilidade"
  ) +
  theme_minimal(base_size = 13)

grafico_uniforme <- ggplot(
  distribuicoes_basicas$uniforme,
  aes(x = x, y = densidade)
) +
  geom_area(fill = "#F58518", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 1.25)) +
  labs(
    x = "Valor sorteado",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 13)

grafico_normal <- ggplot(
  distribuicoes_basicas$normal,
  aes(x = x, y = densidade)
) +
  geom_area(fill = "steelblue", alpha = 0.75) +
  labs(
    x = "Desvios-padrão da média",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 13)

grafico_frequencia <- ggplot(
  df_frequencia,
  aes(x = repeticao, y = frequencia_acumulada)
) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0.55, color = "firebrick", linetype = "dashed",
             linewidth = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    x = "Repetições",
    y = "Freq. acumulada"
  ) +
  theme_minimal(base_size = 13)

grafico_condicional <- ggplot(
  probabilidades_condicionais,
  aes(x = escolaridade, y = prob_aprovacao, fill = escolaridade)
) +
  geom_col(width = 0.62, alpha = 0.85, show.legend = FALSE) +
  geom_text(
    aes(label = percent(prob_aprovacao, accuracy = 0.1)),
    vjust = -0.45,
    size = 4
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.7)) +
  scale_fill_manual(values = c("#4C78A8", "#F58518")) +
  labs(
    x = NULL,
    y = "Prob. de aprovação"
  ) +
  theme_minimal(base_size = 13)

grafico_amostras <- ggplot(df_amostras_100) +
  geom_point(
    aes(x = simulacao, y = prop_aprovacao),
    color = "steelblue",
    alpha = 0.55,
    size = 1.3
  ) +
  geom_hline(
    yintercept = parametro_aprovacao,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0.35, 0.75)) +
  labs(
    x = "Amostra simulada",
    y = "Aprovação na amostra"
  ) +
  theme_minimal(base_size = 13)

if (identical(environment(), globalenv()) && !interactive()) {
  message("Objetos criados para a Aula 6:")
  message("- df_frequencia")
  message("- populacao_eleitores")
  message("- amostra_exemplo")
  message("- probabilidades_condicionais")
  message("- df_amostras_100")
  message("- grafico_frequencia")
  message("- grafico_condicional")
  message("- grafico_amostras")
}
