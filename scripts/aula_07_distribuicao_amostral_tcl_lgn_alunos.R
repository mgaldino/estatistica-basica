# Aula 7 - Laboratorio: distribuicao amostral, LGN e TCL
# Metodos Quantitativos e Tecnicas em Ciencia Politica I / Metodos III
#
# Objetivo:
# 1. revisar media, mediana, moda, variancia e desvio-padrao;
# 2. simular a Lei dos Grandes Numeros;
# 3. simular o Teorema Central do Limite;
# 4. comparar desvio-padrao e erro-padrao;
# 5. ver por que uma amostra grande, mas enviesada, nao resolve o problema.

options(scipen = 999)


library(dplyr)
library(ggplot2)
library(scales)
library(tibble)


set.seed(20260624)

# função pra calcular moda
moda_aproximada <- function(x) {
  densidade <- density(x, n = 1024)
  densidade$x[which.max(densidade$y)]
}

simular_medias <- function(vetor, n_amostra, n_sim = 1000) {
  replicate(
    n_sim,
    mean(sample(vetor, size = n_amostra))
  )
}


# -------------------------------------------------------------------
# 1. Populacao simulada
# -------------------------------------------------------------------

# Esta populacao nao descreve dados reais.
# Ela serve para estudar a logica da inferencia estatistica.
#
# A renda foi gerada com uma distribuicao assimétrica à direita:
# muitos valores baixos/intermediarios e poucos valores muito altos.

n_populacao <- 100000

populacao <- tibble(
  id = 1:n_populacao,
  renda = rlnorm(n_populacao, meanlog = log(3000), sdlog = 0.75)
)

# Validacao logica simples:
# renda nao pode ser negativa e nao deve ter valores ausentes.

sum(is.na(populacao$renda))
min(populacao$renda)

# histograma
ggplot(populacao, aes(x = renda)) +
  geom_histogram(bins = 60, fill = "gray80", color = "white") +
  scale_x_continuous(
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Renda mensal",
    y = "Número de pessoas",
    title = "Distribuição da renda na população simulada"
  ) +
  theme_minimal()

# difícil de ver a cauda longa. Vamos usar box-plot

ggplot(populacao, aes(x = renda)) +
  geom_boxplot() + coord_flip() +
  labs(
    x = "Renda mensal",
    y = "",
    title = "Box-plot da renda na população simulada"
  ) +
  theme_minimal()

# -------------------------------------------------------------------
# 2. Revisao: medidas descritivas
# -------------------------------------------------------------------

media_renda <- mean(populacao$renda)
mediana_renda <- median(populacao$renda)
moda_renda <- moda_aproximada(populacao$renda) # pouco útil!
variancia_renda <- var(populacao$renda)
desvio_renda <- sd(populacao$renda)

resumo_descritivo <- tibble(
  medida = c("media", "mediana", "moda aproximada", "variancia", "desvio-padrao"),
  valor = c(media_renda, mediana_renda, moda_renda, variancia_renda, desvio_renda)
)

resumo_descritivo

ggplot(populacao, aes(x = renda)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 60,
    fill = "gray82",
    color = "white"
  ) +
  geom_density(color = "gray30", linewidth = 0.8) +
  geom_vline(
    aes(xintercept = media_renda, color = "Média"),
    linewidth = 0.7
  ) +
  geom_vline(
    aes(xintercept = mediana_renda, color = "Mediana"),
    linewidth = 0.7
  ) +
  geom_vline(
    aes(xintercept = moda_renda, color = "Moda"),
    linewidth = 0.7
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Média" = "#4C78A8",
      "Mediana" = "#F58518",
      "Moda" = "#54A24B"
    )
  ) +
  scale_x_continuous(limits = c(0, 50000),
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Renda mensal",
    y = "Densidade",
    title = "Média, mediana e moda"
  ) +
  theme_minimal()
# coloquei limite até 50k, para não estourar o gráfico. Podem tirar e ver como fica.

# Tarefa1
# com mutate, do dplyr, crie uma nova variável,
# logrenda, que é o logaritmo natural da renda.
# plot o histograma dessa nova variável e compare com a renda
# o que mudou? Conhece a distribuicao de logrenda?
# Qual a conexao com o TCL?


# -------------------------------------------------------------------
# 3. Uma amostra e apenas uma realizacao possivel
# -------------------------------------------------------------------

n_amostra <- 100

amostra_1 <- sample(populacao$renda, size = n_amostra)
amostra_2 <- sample(populacao$renda, size = n_amostra)
amostra_3 <- sample(populacao$renda, size = n_amostra)

mean(amostra_1)
mean(amostra_2)
mean(amostra_3)

media_renda

# Tarefa 2:
# As três medias amostrais são iguais?
# Alguma delas é exatamente igual à média da populacao?
# Explique em uma frase o que é variação amostral.


# -------------------------------------------------------------------
# 4. Lei dos Grandes Numeros
# -------------------------------------------------------------------

set.seed(20260624)

amostra_lgn <- sample(populacao$renda, size = 5000, replace = TRUE)

lgn <- tibble(
  n = 1:length(amostra_lgn),
  media_acumulada = cumsum(amostra_lgn) / n
)

ggplot(lgn, aes(x = n, y = media_acumulada)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(
    yintercept = media_renda,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_y_continuous(
    labels = label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Tamanho acumulado da amostra",
    y = "Media acumulada",
    title = "Lei dos Grandes Numeros"
  ) +
  theme_minimal()



# Tarefa 3:
# Mude o tamanho de amostra_lgn para 100, 1000 e 10000.
# Em qual caso a m'eédia acumulada fica mais estável?
# A linha azul fica sempre exatamente sobre a linha vermelha?


# -------------------------------------------------------------------
# 5. Distribuição amostral da média
# -------------------------------------------------------------------

medias_n5 <- simular_medias(populacao$renda, n_amostra = 5)
medias_n30 <- simular_medias(populacao$renda, n_amostra = 30)
medias_n100 <- simular_medias(populacao$renda, n_amostra = 100)
medias_n500 <- simular_medias(populacao$renda, n_amostra = 500)

resultados_tcl <- bind_rows(
  tibble(n = "n = 5", media_amostral = medias_n5),
  tibble(n = "n = 30", media_amostral = medias_n30),
  tibble(n = "n = 100", media_amostral = medias_n100),
  tibble(n = "n = 500", media_amostral = medias_n500)
) |>
  mutate(
    n = factor(n, levels = c("n = 5", "n = 30", "n = 100", "n = 500"))
  )

ggplot(resultados_tcl, aes(x = media_amostral)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "steelblue",
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = media_renda,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  facet_wrap(~n, ncol = 2) +
  scale_x_continuous(
    labels = label_number(
      scale = 1 / 1000,
      suffix = " mil",
      decimal.mark = ",",
      accuracy = 0.1
    )
  ) +
  labs(
    x = "Media amostral",
    y = "Densidade",
    title = "Distribuicao amostral da media"
  ) +
  theme_minimal()



# Tarefa 4:
# Compare n = 5, n = 30, n = 100 e n = 500.
# O que acontece com a forma da distribuição amostral?
# O que acontece com a dispersão das médias amostrais?


# -------------------------------------------------------------------
# 6. Erro-padrão
# -------------------------------------------------------------------

resumo_erro_padrao <- tibble(
  n_amostra = c(5, 30, 100, 500),
  erro_padrao_simulado = c(
    sd(medias_n5),
    sd(medias_n30),
    sd(medias_n100),
    sd(medias_n500)
  ),
  erro_padrao_formula = desvio_renda / sqrt(n_amostra)
)

resumo_erro_padrao

ggplot(resumo_erro_padrao, aes(x = n_amostra)) +
  geom_line(aes(y = erro_padrao_simulado), color = "steelblue", linewidth = 0.9) +
  geom_point(aes(y = erro_padrao_simulado), color = "steelblue", size = 2.4) +
  geom_line(
    aes(y = erro_padrao_formula),
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
    y = "Erro-padrao da media",
    title = "Erro-padrao cai quando n aumenta"
  ) +
  theme_minimal()


# Tarefa 5:
# Compare erro_padrao_simulado e erro_padrao_formula.
# Eles sao exatamente iguais? Sao próximos?
# Por que o erro-padrão cai quando n aumenta?
