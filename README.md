# Estatística Básica — Materiais do Curso

Este diretório contém o projeto `bookdown` do curso e os materiais avulsos de aula, incluindo slides em PDF, atividades de laboratório e scripts em R para os alunos.

## Estrutura Principal

- `*.Rmd`: capítulos do livro, programas, slides e atividades.
- `docs/`: saída HTML do `bookdown`, usada para publicação.
- `dados/`: bases locais usadas nos exemplos do curso.
- `scripts/`: scripts auxiliares para gerar gráficos dos slides e scripts de laboratório para os alunos.
- `quality_reports/`: relatórios e planos de revisão de capítulos e materiais.

## Aula 06 — Probabilidade Mínima Para Inferência

Status: concluída.

Arquivos principais:

- `aula_06_probabilidade_minima.Rmd`
- `aula_06_probabilidade_minima.pdf`
- `scripts/aula_06_probabilidade_minima_slides.R`
- `scripts/aula_06_probabilidade_minima_alunos.R`

Conteúdo:

- probabilidade como linguagem da incerteza;
- experimento aleatório, resultado, evento e notação mínima;
- distribuições Bernoulli, Binomial, Uniforme e Normal;
- probabilidade condicional;
- simulação de amostras em R.

## Aula 07 — Distribuição Amostral, LGN e TCL

Status: concluída e validada em 2026-06-24.

Arquivos principais:

- `aula_07_distribuicao_amostral_tcl_lgn.Rmd`
- `aula_07_distribuicao_amostral_tcl_lgn.pdf`
- `scripts/aula_07_distribuicao_amostral_tcl_lgn_slides.R`
- `scripts/aula_07_distribuicao_amostral_tcl_lgn_alunos.R`
- `aula_07_laboratorio_distribuicao_amostral_tcl_lgn.Rmd`
- `aula_07_laboratorio_distribuicao_amostral_tcl_lgn.pdf`

Conteúdo dos slides:

- revisão de média, mediana, moda, variância e desvio-padrão;
- população, amostra, parâmetro, estatística, estimador e estimativa;
- Lei dos Grandes Números;
- distribuição amostral da média;
- Teorema Central do Limite com variáveis aleatórias IID;
- conexão entre amostragem aleatória simples e variáveis aproximadamente IID;
- diferença entre soma e média;
- origem da fórmula do erro-padrão da média;
- exemplo real com PIB per capita municipal como proxy de renda municipal;
- intuição da distribuição log-normal como resultado de fatores multiplicativos.

Leitura indicada:

- Kellstedt e Whitten, *Fundamentos da Pesquisa em Ciência Política*, Capítulo 6, "Probabilidade e inferência estatística".
- Para revisão descritiva: Capítulo 5, especialmente a seção sobre como conhecer os dados estatisticamente e descrever variáveis contínuas.

Atividade de laboratório:

- O script dos alunos reproduz o básico da aula em R.
- O handout tem tarefas para sala sobre medidas descritivas, variação amostral, LGN, TCL, erro-padrão e amostra enviesada.

## Reprodução Dos Materiais

Para renderizar os slides da aula 07:

```bash
env -u LC_ALL LC_CTYPE=pt_BR.UTF-8 Rscript -e "rmarkdown::render('aula_07_distribuicao_amostral_tcl_lgn.Rmd', output_format = 'beamer_presentation')"
```

Para renderizar a atividade de laboratório:

```bash
env -u LC_ALL LC_CTYPE=pt_BR.UTF-8 Rscript -e "rmarkdown::render('aula_07_laboratorio_distribuicao_amostral_tcl_lgn.Rmd', output_format = 'pdf_document')"
```

Para testar o script dos alunos:

```bash
env -u LC_ALL LC_CTYPE=pt_BR.UTF-8 Rscript -e "source('scripts/aula_07_distribuicao_amostral_tcl_lgn_alunos.R', encoding = 'UTF-8')"
```

## Validação Realizada

Na aula 07, foram realizados os seguintes checks:

- execução do script dos slides;
- execução do script dos alunos;
- renderização do PDF dos slides com `xelatex`;
- renderização do PDF da atividade de laboratório;
- extração de texto dos PDFs para conferir tópicos e leitura indicada;
- inspeção visual de páginas críticas dos PDFs renderizadas como PNG.

## Notas De Estilo

- Em materiais para alunos, escrever em português com acentuação completa.
- Em scripts didáticos, priorizar legibilidade para iniciantes quando isso não comprometer a execução.
- Manter computação dos slides em scripts separados, especialmente quando houver simulações ou gráficos.
- Preferir PDF como saída padrão para slides, handouts e atividades.
