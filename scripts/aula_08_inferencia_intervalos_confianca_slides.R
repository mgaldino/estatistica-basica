options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
})

set.seed(20260624)

nivel_padrao <- 0.95
z_95 <- qnorm(1 - (1 - nivel_padrao) / 2)
p_aprovacao_populacao <- 0.47
n_pesquisa <- 1000
sucessos_amostra <- rbinom(1, size = n_pesquisa, prob = p_aprovacao_populacao)

calcular_ic_proporcao <- function(sucessos, n, nivel = 0.95) {
  estimativa <- sucessos / n
  z_critico <- qnorm(1 - (1 - nivel) / 2)
  erro_padrao <- sqrt(estimativa * (1 - estimativa) / n)
  margem <- z_critico * erro_padrao

  tibble(
    estimativa = estimativa,
    erro_padrao = erro_padrao,
    margem = margem,
    inferior = estimativa - margem,
    superior = estimativa + margem
  )
}

simular_intervalos_proporcao <- function(p = 0.47, n = 1000, n_sim = 1500,
                                         nivel = 0.95, seed = 20260624) {
  set.seed(seed)
  z_critico <- qnorm(1 - (1 - nivel) / 2)
  sucessos <- rbinom(n_sim, size = n, prob = p)
  estimativa <- sucessos / n
  erro_padrao <- sqrt(estimativa * (1 - estimativa) / n)
  margem <- z_critico * erro_padrao

  tibble(
    simulacao = seq_len(n_sim),
    estimativa = estimativa,
    inferior = estimativa - margem,
    superior = estimativa + margem,
    cobre = inferior <= p & superior >= p
  )
}

ic_aprovacao <- calcular_ic_proporcao(sucessos_amostra, n_pesquisa)

df_normal_95 <- tibble(
  z = seq(-3.4, 3.4, length.out = 500),
  densidade = dnorm(z),
  area_95 = z >= -z_95 & z <= z_95
)

grafico_normal_95 <- ggplot(df_normal_95, aes(x = z, y = densidade)) +
  geom_area(
    data = filter(df_normal_95, area_95),
    fill = "#4C78A8",
    alpha = 0.28
  ) +
  geom_line(color = "gray25", linewidth = 0.9) +
  geom_vline(
    xintercept = c(-z_95, z_95),
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = c(-z_95, z_95),
    y = c(0.08, 0.08),
    label = c("-1,96", "+1,96"),
    color = "firebrick",
    size = 3.4
  ) +
  annotate(
    "text",
    x = 0,
    y = 0.2,
    label = "95%",
    color = "#4C78A8",
    fontface = "bold",
    size = 5
  ) +
  scale_x_continuous(breaks = -3:3) +
  labs(
    x = "Distância em erros-padrão",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 13)

tabela_pesquisa <- tibble(
  Estatística = c(
    "Entrevistados",
    "Aprovam na amostra",
    "Estimativa pontual",
    "Erro-padrão",
    "Margem de erro",
    "Limite inferior",
    "Limite superior"
  ),
  Valor = c(
    number(n_pesquisa, big.mark = ".", decimal.mark = ","),
    number(sucessos_amostra, big.mark = ".", decimal.mark = ","),
    percent(ic_aprovacao$estimativa, accuracy = 0.1, decimal.mark = ","),
    percent(ic_aprovacao$erro_padrao, accuracy = 0.1, decimal.mark = ","),
    paste0("\u00b1", percent(ic_aprovacao$margem, accuracy = 0.1, decimal.mark = ",")),
    percent(ic_aprovacao$inferior, accuracy = 0.1, decimal.mark = ","),
    percent(ic_aprovacao$superior, accuracy = 0.1, decimal.mark = ",")
  )
)

intervalos_95 <- simular_intervalos_proporcao(
  p = p_aprovacao_populacao,
  n = n_pesquisa,
  n_sim = 1500,
  nivel = 0.95
)

tabela_cobertura <- intervalos_95 |>
  summarise(
    Simulações = n(),
    `Cobrem o parâmetro` = sum(cobre),
    `Não cobrem` = sum(!cobre),
    `Cobertura observada` = mean(cobre),
    .groups = "drop"
  ) |>
  mutate(
    `Cobertura observada` = percent(
      `Cobertura observada`,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

df_intervalos_plot <- intervalos_95 |>
  filter(simulacao <= 120) |>
  mutate(
    status = if_else(cobre, "Cobre", "Não cobre"),
    status = factor(status, levels = c("Cobre", "Não cobre"))
  )

grafico_cobertura <- ggplot(df_intervalos_plot) +
  geom_hline(
    yintercept = p_aprovacao_populacao,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = simulacao,
      xend = simulacao,
      y = inferior,
      yend = superior,
      color = status
    ),
    linewidth = 0.55
  ) +
  geom_point(aes(x = simulacao, y = estimativa, color = status), size = 1.2) +
  annotate(
    "text",
    x = 65,
    y = p_aprovacao_populacao + 0.055,
    label = "linha vermelha: parâmetro verdadeiro",
    color = "firebrick",
    hjust = 0,
    size = 3.1
  ) +
  scale_color_manual(values = c("Cobre" = "#4C78A8", "Não cobre" = "#E45756")) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1, decimal.mark = ",")
  ) +
  labs(
    x = "Simulação",
    y = "Intervalo de confiança de 95%",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

df_margem_n <- tibble(n = seq(100, 3000, by = 25)) |>
  mutate(margem = z_95 * sqrt(0.25 / n))

df_margem_destaques <- tibble(
  n = c(400, 1000, 2401),
  margem = z_95 * sqrt(0.25 / c(400, 1000, 2401))
) |>
  mutate(
    rotulo = paste0(
      "n = ",
      number(n, big.mark = ".", decimal.mark = ","),
      "\n",
      percent(margem, accuracy = 0.1, decimal.mark = ",")
    )
  )

grafico_margem_n <- ggplot(df_margem_n, aes(x = n, y = margem)) +
  geom_line(color = "#4C78A8", linewidth = 0.9) +
  geom_point(
    data = df_margem_destaques,
    color = "firebrick",
    size = 2
  ) +
  geom_label(
    data = df_margem_destaques,
    aes(label = rotulo),
    linewidth = 0.15,
    fill = "white",
    color = "gray20",
    size = 3,
    vjust = c(-0.2, -0.5, -0.6)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1, decimal.mark = ",")
  ) +
  scale_x_continuous(
    breaks = c(500, 1000, 1500, 2000, 2500, 3000),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Tamanho da amostra",
    y = "Margem de erro aproximada"
  ) +
  theme_minimal(base_size = 13)

tabela_tamanho_amostra <- tibble(
  `Margem desejada` = c(0.05, 0.03, 0.02, 0.01),
  `n necessário` = ceiling(z_95^2 * 0.25 / `Margem desejada`^2)
) |>
  mutate(
    `Margem desejada` = percent(
      `Margem desejada`,
      accuracy = 1,
      decimal.mark = ","
    ),
    `n necessário` = number(`n necessário`, big.mark = ".", decimal.mark = ",")
  )

tabela_niveis_confianca <- tibble(
  `Nível de confiança` = c(0.80, 0.90, 0.95, 0.99),
  `Valor crítico` = qnorm(1 - (1 - `Nível de confiança`) / 2),
  `Margem com n = 1000` = `Valor crítico` * sqrt(0.25 / 1000)
) |>
  mutate(
    `Nível de confiança` = percent(
      `Nível de confiança`,
      accuracy = 1,
      decimal.mark = ","
    ),
    `Valor crítico` = number(`Valor crítico`, accuracy = 0.01, decimal.mark = ","),
    `Margem com n = 1000` = percent(
      `Margem com n = 1000`,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

set.seed(20260625)
populacao_satisfacao <- tibble(
  id = seq_len(100000),
  satisfacao = pmin(pmax(rnorm(100000, mean = 5.8, sd = 1.9), 0), 10)
)

amostra_satisfacao <- populacao_satisfacao |>
  slice_sample(n = 120)

media_satisfacao <- mean(amostra_satisfacao$satisfacao)
desvio_satisfacao <- sd(amostra_satisfacao$satisfacao)
erro_padrao_satisfacao <- desvio_satisfacao / sqrt(nrow(amostra_satisfacao))
t_critico_satisfacao <- qt(0.975, df = nrow(amostra_satisfacao) - 1)
margem_satisfacao <- t_critico_satisfacao * erro_padrao_satisfacao

tabela_media <- tibble(
  Estatística = c(
    "Média amostral",
    "Desvio-padrão da amostra",
    "Erro-padrão",
    "Valor crítico t",
    "Limite inferior",
    "Limite superior"
  ),
  Valor = c(
    number(media_satisfacao, accuracy = 0.01, decimal.mark = ","),
    number(desvio_satisfacao, accuracy = 0.01, decimal.mark = ","),
    number(erro_padrao_satisfacao, accuracy = 0.01, decimal.mark = ","),
    number(t_critico_satisfacao, accuracy = 0.01, decimal.mark = ","),
    number(media_satisfacao - margem_satisfacao, accuracy = 0.01, decimal.mark = ","),
    number(media_satisfacao + margem_satisfacao, accuracy = 0.01, decimal.mark = ",")
  )
)

df_ic_media <- tibble(
  estimativa = media_satisfacao,
  inferior = media_satisfacao - margem_satisfacao,
  superior = media_satisfacao + margem_satisfacao
)

grafico_ic_media <- ggplot(df_ic_media) +
  geom_segment(
    aes(x = inferior, xend = superior, y = 1, yend = 1),
    color = "#4C78A8",
    linewidth = 1.2
  ) +
  geom_point(aes(x = estimativa, y = 1), color = "#4C78A8", size = 3) +
  geom_point(aes(x = inferior, y = 1), color = "#4C78A8", size = 2) +
  geom_point(aes(x = superior, y = 1), color = "#4C78A8", size = 2) +
  annotate(
    "text",
    x = c(df_ic_media$inferior, df_ic_media$estimativa, df_ic_media$superior),
    y = c(1.14, 1.23, 1.14),
    label = c("limite inferior", "média", "limite superior"),
    color = "gray25",
    size = 3.6
  ) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(0.86, 1.27)) +
  scale_x_continuous(limits = c(5, 6.6), breaks = seq(5, 6.6, by = 0.4)) +
  labs(
    x = "Índice de satisfação com a democracia"
  ) +
  theme_minimal(base_size = 13)

if (identical(environment(), globalenv()) && !interactive()) {
  message("Objetos criados para a Aula 8:")
  message("- tabela_pesquisa")
  message("- tabela_cobertura")
  message("- tabela_tamanho_amostra")
  message("- tabela_niveis_confianca")
  message("- tabela_media")
  message("- grafico_normal_95")
  message("- grafico_cobertura")
  message("- grafico_margem_n")
  message("- grafico_ic_media")
}
