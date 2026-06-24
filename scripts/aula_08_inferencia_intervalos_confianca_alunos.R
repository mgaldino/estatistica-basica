# Aula 8 - Laboratório: inferência e intervalos de confiança
# Métodos Quantitativos e Técnicas em Ciência Política I / Métodos III
#
# Objetivo:
# 1. calcular uma estimativa pontual;
# 2. calcular erro-padrão e margem de erro;
# 3. construir intervalos de confiança para proporções;
# 4. simular a cobertura de intervalos de confiança;
# 5. comparar confiança, precisão e tamanho da amostra;
# 6. calcular um intervalo de confiança para uma média.

options(scipen = 999)


library(dplyr)
library(ggplot2)
library(scales)
library(tibble)


set.seed(20260624)


# -------------------------------------------------------------------
# 1. Uma população simulada
# -------------------------------------------------------------------

# Esta população não descreve uma pesquisa real.
# Ela serve para estudar a lógica dos intervalos de confiança.
#
# Imagine que queremos estimar a proporção de eleitores que aprova o governo.
# Como estamos simulando, sabemos o parâmetro verdadeiro.
# Em uma pesquisa real, esse parâmetro seria desconhecido.

n_populacao <- 100000
proporcao_verdadeira <- 0.47

populacao <- tibble(
  id = 1:n_populacao,
  aprova = rbinom(n_populacao, size = 1, prob = proporcao_verdadeira)
)

# Validação lógica simples:
# aprova deve ser sempre 0 ou 1 e não deve ter valores ausentes.

sum(is.na(populacao$aprova))
table(populacao$aprova)
mean(populacao$aprova)


# -------------------------------------------------------------------
# 2. Uma amostra e uma estimativa pontual
# -------------------------------------------------------------------

n_amostra <- 1000

amostra <- populacao |>
  slice_sample(n = n_amostra)

estimativa <- mean(amostra$aprova)
estimativa

# A estimativa pontual é o melhor chute com base na amostra.
# Mas ela não mostra sozinha quanta incerteza existe.


# -------------------------------------------------------------------
# 3. Erro-padrão, margem de erro e intervalo de confiança
# -------------------------------------------------------------------

# Para uma proporção, o erro-padrão aproximado é:
#
# EP = sqrt(p_chapeu * (1 - p_chapeu) / n)

erro_padrao <- sqrt(estimativa * (1 - estimativa) / n_amostra)
erro_padrao

# Para 95% de confiança, usamos aproximadamente 1,96.

z_95 <- qnorm(0.975)
z_95

margem_erro <- z_95 * erro_padrao
margem_erro

limite_inferior <- estimativa - margem_erro
limite_superior <- estimativa + margem_erro

intervalo_confianca <- tibble(
  estimativa = estimativa,
  erro_padrao = erro_padrao,
  margem_erro = margem_erro,
  limite_inferior = limite_inferior,
  limite_superior = limite_superior
)

intervalo_confianca

# Em percentual:

intervalo_confianca |>
  mutate(across(everything(), ~ percent(.x, accuracy = 0.1, decimal.mark = ",")))


# Tarefa 1:
# Troque n_amostra para 400, 1000 e 2400.
# O que acontece com a margem de erro?
# A estimativa muda exatamente do mesmo jeito que a margem de erro?


# -------------------------------------------------------------------
# 4. O que significa 95% de confiança?
# -------------------------------------------------------------------

# Vamos repetir a pesquisa muitas vezes.
# Em cada repetição, sorteamos uma amostra, calculamos a estimativa
# e construímos um intervalo de confiança.
#
# Como estamos em uma simulação, sabemos se cada intervalo contém
# a proporção verdadeira.

n_simulacoes <- 1000

simulacoes <- tibble(
  simulacao = 1:n_simulacoes,
  sucessos = rbinom(n_simulacoes, size = n_amostra, prob = proporcao_verdadeira)
) |>
  mutate(
    estimativa = sucessos / n_amostra,
    erro_padrao = sqrt(estimativa * (1 - estimativa) / n_amostra),
    margem_erro = z_95 * erro_padrao,
    limite_inferior = estimativa - margem_erro,
    limite_superior = estimativa + margem_erro,
    cobre_parametro = limite_inferior <= proporcao_verdadeira &
      limite_superior >= proporcao_verdadeira
  )

mean(simulacoes$cobre_parametro)

# O resultado deve ficar perto de 0,95, mas não precisa ser exatamente 0,95.
# A interpretação correta é sobre o procedimento repetido:
# se repetíssemos a pesquisa muitas vezes, cerca de 95% dos intervalos
# conteriam o parâmetro verdadeiro.


# -------------------------------------------------------------------
# 5. Visualizando a cobertura
# -------------------------------------------------------------------

simulacoes_grafico <- simulacoes |>
  filter(simulacao <= 120) |>
  mutate(
    status = if_else(cobre_parametro, "Cobre", "Não cobre"),
    status = factor(status, levels = c("Cobre", "Não cobre"))
  )

ggplot(simulacoes_grafico) +
  geom_hline(
    yintercept = proporcao_verdadeira,
    color = "firebrick",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = simulacao,
      xend = simulacao,
      y = limite_inferior,
      yend = limite_superior,
      color = status
    ),
    linewidth = 0.6
  ) +
  geom_point(aes(x = simulacao, y = estimativa, color = status), size = 1.3) +
  scale_color_manual(values = c("Cobre" = "steelblue", "Não cobre" = "firebrick")) +
  scale_y_continuous(labels = label_percent(accuracy = 1, decimal.mark = ",")) +
  labs(
    x = "Simulação",
    y = "Intervalo de confiança de 95%",
    color = "",
    title = "Nem todo intervalo de 95% contém o parâmetro"
  ) +
  theme_minimal()


# Tarefa 2:
# Rode a simulação com 100, 1000 e 5000 repetições.
# A cobertura observada se aproxima de 95% quando o número de simulações cresce?


# -------------------------------------------------------------------
# 6. Confiança e precisão
# -------------------------------------------------------------------

# Aumentar o nível de confiança aumenta a chance de cobertura.
# Mas isso tem custo: o intervalo fica mais largo.

niveis <- tibble(
  nivel = c(0.80, 0.90, 0.95, 0.99)
) |>
  mutate(
    z_critico = qnorm(1 - (1 - nivel) / 2),
    margem_erro = z_critico * erro_padrao
  )

niveis

niveis |>
  mutate(
    nivel = percent(nivel, accuracy = 1, decimal.mark = ","),
    margem_erro = percent(margem_erro, accuracy = 0.1, decimal.mark = ",")
  )


# Tarefa 3:
# Por que um intervalo de 99% é mais largo que um intervalo de 95%?
# Se você precisa de mais confiança, o que acontece com a precisão?


# -------------------------------------------------------------------
# 7. Tamanho de amostra para proporções
# -------------------------------------------------------------------

# Para 95% de confiança, uma aproximação comum para proporções é:
#
# n = 1,96^2 * p * (1 - p) / margem^2
#
# Quando não sabemos p, usamos p = 0,5.
# Esse é o caso mais conservador, porque maximiza p * (1 - p).

margens_desejadas <- tibble(
  margem = c(0.05, 0.03, 0.02, 0.01)
) |>
  mutate(
    n_necessario = ceiling(z_95^2 * 0.5 * 0.5 / margem^2)
  )

margens_desejadas

margens_desejadas |>
  mutate(
    margem = percent(margem, accuracy = 1, decimal.mark = ",")
  )


# Tarefa 4:
# Qual tamanho de amostra é necessário para uma margem de erro de 2 pontos?
# E para 1 ponto?
# O tamanho quadruplica, dobra ou aumenta pouco?


# -------------------------------------------------------------------
# 8. Intervalo de confiança para uma média
# -------------------------------------------------------------------

# Agora vamos usar uma variável contínua.
# Imagine um índice de satisfação com a democracia, de 0 a 10.

set.seed(20260625)

populacao_satisfacao <- tibble(
  id = 1:n_populacao,
  satisfacao = pmin(pmax(rnorm(n_populacao, mean = 5.8, sd = 1.9), 0), 10)
)

amostra_satisfacao <- populacao_satisfacao |>
  slice_sample(n = 120)

media_satisfacao <- mean(amostra_satisfacao$satisfacao)
desvio_satisfacao <- sd(amostra_satisfacao$satisfacao)
erro_padrao_media <- desvio_satisfacao / sqrt(nrow(amostra_satisfacao))

# Como não conhecemos o desvio-padrão da população, usamos a distribuição t.

t_critico <- qt(0.975, df = nrow(amostra_satisfacao) - 1)
t_critico

margem_media <- t_critico * erro_padrao_media

ic_media <- tibble(
  media = media_satisfacao,
  erro_padrao = erro_padrao_media,
  limite_inferior = media_satisfacao - margem_media,
  limite_superior = media_satisfacao + margem_media
)

ic_media

ggplot(ic_media, aes(x = 1, y = media)) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
    width = 0.08,
    color = "steelblue"
  ) +
  geom_point(color = "steelblue", size = 3) +
  coord_flip() +
  scale_x_continuous(NULL, breaks = NULL) +
  labs(
    y = "Índice de satisfação com a democracia",
    title = "Intervalo de confiança para uma média"
  ) +
  theme_minimal()


# Tarefa 5:
# Troque o tamanho da amostra de satisfação para 40, 120 e 500.
# O que acontece com o intervalo?
# A média amostral muda mais ou menos quando n aumenta?


# -------------------------------------------------------------------
# 9. O que o intervalo de confiança não resolve
# -------------------------------------------------------------------

# Intervalo de confiança mede incerteza amostral.
# Ele não corrige uma amostra enviesada.

set.seed(20260626)

populacao_vies <- tibble(
  id = 1:n_populacao,
  ensino_superior = rbinom(n_populacao, size = 1, prob = 0.32)
) |>
  mutate(
    aprova = rbinom(
      n_populacao,
      size = 1,
      prob = if_else(ensino_superior == 1, 0.38, 0.51)
    ),
    responde_online = rbinom(
      n_populacao,
      size = 1,
      prob = if_else(ensino_superior == 1, 0.80, 0.35)
    )
  )

parametro_total <- mean(populacao_vies$aprova)
parametro_online <- mean(populacao_vies$aprova[populacao_vies$responde_online == 1])

parametro_total
parametro_online

amostra_aleatoria <- populacao_vies |>
  slice_sample(n = 1000)

amostra_online <- populacao_vies |>
  filter(responde_online == 1) |>
  slice_sample(n = 1000)

mean(amostra_aleatoria$aprova)
mean(amostra_online$aprova)

# A amostra online pode ter intervalo estreito e mesmo assim estar errada
# para a população total, porque o problema é viés de seleção.


# Tarefa 6:
# Aumente o tamanho da amostra online para 5000.
# A estimativa online se aproxima do parâmetro total?
# O que isso ensina sobre amostras de conveniência?
