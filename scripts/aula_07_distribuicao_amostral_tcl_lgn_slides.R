options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
})

set.seed(20260624)

estimar_moda_continua <- function(x) {
  densidade <- density(x, n = 1024)
  densidade$x[which.max(densidade$y)]
}

criar_populacao_renda <- function(n = 100000, seed = 20260624) {
  set.seed(seed)

  tibble(
    id = seq_len(n),
    renda = rgamma(n, shape = 2.2, scale = 1900)
  )
}

simular_medias_amostrais <- function(x, tamanhos = c(5, 30, 100, 500),
                                     n_sim = 2000, seed = 20260624) {
  set.seed(seed)

  bind_rows(lapply(tamanhos, function(n) {
    tibble(
      n_amostra = n,
      simulacao = seq_len(n_sim),
      media_amostral = replicate(n_sim, mean(sample(x, size = n)))
    )
  }))
}

populacao_renda <- criar_populacao_renda()
parametro_renda <- mean(populacao_renda$renda)
desvio_populacional <- sd(populacao_renda$renda)
moda_renda <- estimar_moda_continua(populacao_renda$renda)

resumo_renda <- tibble(
  Medida = c(
    "Média",
    "Mediana",
    "Moda aproximada",
    "Variância",
    "Desvio-padrão"
  ),
  Valor = c(
    mean(populacao_renda$renda),
    median(populacao_renda$renda),
    moda_renda,
    var(populacao_renda$renda),
    sd(populacao_renda$renda)
  )
)

resumo_renda_tabela <- resumo_renda |>
  mutate(
    Valor = case_when(
      Medida == "Variância" ~ number(
        Valor,
        accuracy = 1,
        big.mark = ".",
        decimal.mark = ","
      ),
      TRUE ~ paste0(
        "R$ ",
        number(Valor, accuracy = 1, big.mark = ".", decimal.mark = ",")
      )
    )
  )

grafico_tendencia_central <- ggplot(populacao_renda, aes(x = renda)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 55,
    fill = "gray82",
    color = "white"
  ) +
  geom_density(color = "gray30", linewidth = 0.8) +
  geom_vline(
    xintercept = mean(populacao_renda$renda),
    color = "#4C78A8",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = median(populacao_renda$renda),
    color = "#F58518",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = moda_renda,
    color = "#54A24B",
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = c(mean(populacao_renda$renda), median(populacao_renda$renda), moda_renda),
    y = c(0.000105, 0.000125, 0.000145),
    label = c("Média", "Mediana", "Moda"),
    angle = 90,
    vjust = -0.45,
    size = 3.2
  ) +
  scale_x_continuous(
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Renda mensal simulada",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 13)

df_variabilidade <- bind_rows(
  tibble(
    grupo = "Grupo A: baixa variabilidade",
    valor = rnorm(1200, mean = 50, sd = 5)
  ),
  tibble(
    grupo = "Grupo B: alta variabilidade",
    valor = rnorm(1200, mean = 50, sd = 17)
  )
)

resumo_variabilidade <- df_variabilidade |>
  group_by(grupo) |>
  summarise(
    Média = round(mean(valor), 1),
    Variância = round(var(valor), 1),
    `Desvio-padrão` = round(sd(valor), 1),
    .groups = "drop"
  )

grafico_variabilidade <- ggplot(
  df_variabilidade,
  aes(x = valor, fill = grupo, color = grupo)
) +
  geom_density(alpha = 0.22, linewidth = 0.9) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray30") +
  scale_fill_manual(values = c("#4C78A8", "#F58518")) +
  scale_color_manual(values = c("#4C78A8", "#F58518")) +
  labs(
    x = "Índice simulado",
    y = "Densidade",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

set.seed(20260624)
amostra_lgn <- sample(populacao_renda$renda, size = 5000, replace = TRUE)

df_lgn <- tibble(
  n = seq_along(amostra_lgn),
  media_acumulada = cumsum(amostra_lgn) / n
)

grafico_lgn <- ggplot(df_lgn, aes(x = n, y = media_acumulada)) +
  geom_line(color = "#4C78A8", linewidth = 0.8) +
  geom_hline(
    yintercept = parametro_renda,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_y_continuous(
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Tamanho acumulado da amostra",
    y = "Média acumulada"
  ) +
  theme_minimal(base_size = 13)

df_amostragens <- simular_medias_amostrais(
  populacao_renda$renda,
  tamanhos = c(5, 30, 100, 500),
  n_sim = 2000
)

resumo_amostragens <- df_amostragens |>
  group_by(n_amostra) |>
  summarise(
    `Média das médias` = round(mean(media_amostral), 1),
    `DP das médias` = round(sd(media_amostral), 1),
    `s / raiz(n)` = round(desvio_populacional / sqrt(first(n_amostra)), 1),
    .groups = "drop"
  )

df_amostragens_plot <- df_amostragens |>
  mutate(
    n_rotulo = factor(
      paste0("n = ", n_amostra),
      levels = paste0("n = ", c(5, 30, 100, 500))
    )
  )

grafico_tcl <- ggplot(df_amostragens_plot, aes(x = media_amostral)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "#4C78A8",
    color = "white",
    alpha = 0.82
  ) +
  geom_vline(
    xintercept = parametro_renda,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  facet_wrap(~n_rotulo, ncol = 2) +
  scale_x_continuous(
    labels = label_number(
      scale = 1 / 1000,
      decimal.mark = ",",
      accuracy = 0.1
    )
  ) +
  labs(
    x = "Média amostral (R$ mil)",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

grafico_erro_padrao <- ggplot(resumo_amostragens, aes(x = n_amostra)) +
  geom_line(aes(y = `DP das médias`), color = "#4C78A8", linewidth = 0.9) +
  geom_point(aes(y = `DP das médias`), color = "#4C78A8", size = 2.4) +
  geom_line(
    aes(y = `s / raiz(n)`),
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_x_continuous(breaks = c(5, 30, 100, 500)) +
  scale_y_continuous(
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Tamanho da amostra",
    y = "Erro-padrão da média"
  ) +
  theme_minimal(base_size = 13)

dados_pibpc <- readRDS("dados/pib_cid.RDS") |>
  filter(is.finite(pib_per_capita), pib_per_capita > 0) |>
  mutate(log_pib_per_capita = log(pib_per_capita))

resumo_pibpc <- tibble(
  Medida = c("Municípios", "Média", "Mediana", "Desvio-padrão"),
  Valor = c(
    nrow(dados_pibpc),
    mean(dados_pibpc$pib_per_capita),
    median(dados_pibpc$pib_per_capita),
    sd(dados_pibpc$pib_per_capita)
  )
) |>
  mutate(
    Valor = case_when(
      Medida == "Municípios" ~ number(Valor, accuracy = 1),
      TRUE ~ paste0(
        "R$ ",
        number(Valor, accuracy = 1, big.mark = ".", decimal.mark = ",")
      )
    )
  )

limite_pibpc <- quantile(dados_pibpc$pib_per_capita, 0.995)
media_log_pibpc <- mean(dados_pibpc$log_pib_per_capita)
dp_log_pibpc <- sd(dados_pibpc$log_pib_per_capita)

grafico_pibpc_original <- ggplot(dados_pibpc, aes(x = pib_per_capita)) +
  geom_histogram(
    bins = 70,
    fill = "#4C78A8",
    color = "white",
    alpha = 0.82
  ) +
  coord_cartesian(xlim = c(0, limite_pibpc)) +
  scale_x_continuous(
    labels = label_number(
      scale = 1 / 1000,
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    x = "PIB per capita municipal (R$ mil)",
    y = "Municípios"
  ) +
  theme_minimal(base_size = 13)

grafico_pibpc_log_normal <- ggplot(
  dados_pibpc,
  aes(x = log_pib_per_capita)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 55,
    fill = "#4C78A8",
    color = "white",
    alpha = 0.82
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = media_log_pibpc, sd = dp_log_pibpc),
    color = "firebrick",
    linewidth = 0.9
  ) +
  labs(
    x = "log(PIB per capita municipal)",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 13)

set.seed(20260625)
amostras_exemplo <- tibble(
  Amostra = paste0("Amostra ", seq_len(6)),
  `Média da renda` = replicate(
    6,
    mean(sample(populacao_renda$renda, size = 100))
  )
) |>
  mutate(
    `Média da renda` = paste0(
      "R$ ",
      number(`Média da renda`, accuracy = 1, big.mark = ".", decimal.mark = ",")
    )
  )

if (identical(environment(), globalenv()) && !interactive()) {
  message("Objetos criados para a Aula 7:")
  message("- resumo_renda_tabela")
  message("- resumo_variabilidade")
  message("- resumo_amostragens")
  message("- grafico_tendencia_central")
  message("- grafico_variabilidade")
  message("- grafico_lgn")
  message("- grafico_tcl")
  message("- grafico_erro_padrao")
  message("- grafico_pibpc_original")
  message("- grafico_pibpc_log_normal")
}
