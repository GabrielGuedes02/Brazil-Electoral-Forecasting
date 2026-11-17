# =============================================================================
# 01_estimacoes_com_conceitos_aula.R
# Estimacoes: Lewis-Beck, Abramovitz e R. Lins.
#
# Objetivo: preservar as formulas usadas no projeto e acrescentar comparacoes
# e diagnosticos da aula de dados empilhados e dados em painel.
# As paginas comentadas sao a numeracao impressa no PDF de Wooldridge enviado.
# =============================================================================

# 0. PACOTES -----------------------------------------------------------------
# plm estima modelos de painel; lmtest/sandwich/clubSandwich permitem inferencia
# robusta; tseries fornece Jarque-Bera; ggplot2 produz os graficos diagnosticos.
pacotes <- c("here", "readr", "dplyr", "tibble", "ggplot2", "plm",
             "lmtest", "sandwich", "clubSandwich", "tseries")
nao_instalados <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(nao_instalados) > 0) install.packages(nao_instalados)

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(plm)
  library(lmtest)
  library(sandwich)
  library(tseries)
})

# Identifica a raiz do projeto ao abrir este arquivo pelo RStudio.
here::i_am("R/01_estimacoes_com_conceitos_aula.R")

# 1. LEITURA E PREPARACAO ----------------------------------------------------
# voto_inc e uma proporcao (0 a 1) na base original. A multiplicacao por 100
# deixa o resultado em pontos percentuais, como nas tabelas do projeto.
dados <- read_csv(
  here("dados", "BaseDados1.csv"),
  na = c("", "NA"),
  show_col_types = FALSE
) %>%
  select(SG_UF, Ano, voto_inc, votos, Var, Inc_Aprov, IBF) %>%
  mutate(
    SG_UF = as.character(SG_UF),
    Ano = as.integer(Ano),
    across(c(voto_inc, votos, Var, Inc_Aprov, IBF), as.numeric),
    voto_inc_pct = 100 * voto_inc
  )

# A populacao do Censo 2022 e mantida conforme a logica dos scripts originais.
# Ela serve para ponderar agregacoes nacionais; nao muda a unidade da observacao.
pop_uf <- tribble(
  ~SG_UF, ~pop,
  "AC", 830026, "AL", 3351543, "AM", 4144597, "AP", 877613,
  "BA", 14873064, "CE", 9132078, "DF", 3055149, "ES", 4064052,
  "GO", 7018354, "MA", 7114598, "MG", 21168791, "MS", 2778986,
  "MT", 3526220, "PA", 8690745, "PB", 4039277, "PE", 9557071,
  "PI", 3273227, "PR", 11433957, "RJ", 17264943, "RN", 3560903,
  "RO", 1777225, "RR", 652713, "RS", 11422973, "SC", 7610361,
  "SE", 2318822, "SP", 45919049, "TO", 1590248
)

# Os modelos de painel usam apenas eleicoes observadas ate 2022. A linha de
# 2024 contem IBF, mas nao contem voto; VT e um territorio, nao uma UF da amostra.
# Assim, o painel de estimacao e formado por UF x ano eleitoral.
dados_painel <- dados %>%
  filter(Ano <= 2022, SG_UF != "VT") %>%
  mutate(third_term = as.integer(Ano %in% c(2002, 2010))) %>%
  left_join(pop_uf, by = "SG_UF") %>%
  mutate(weight_norm = pop / mean(pop, na.rm = TRUE)) %>%
  filter(complete.cases(voto_inc_pct, Var, Inc_Aprov, third_term, weight_norm))

# O painel e organizado explicitamente por estado e ano. Wooldridge discute a
# organizacao dos registros de painel no cap. 13, pp. 419-421.
painel <- pdata.frame(dados_painel, index = c("SG_UF", "Ano"))

# 2. MODELOS LEWIS-BECK E ABRAMOVITZ: PAINEL -------------------------------
# A estrutura e a mesma dos scripts originais: desempenho eleitoral estadual
# explicado pelo crescimento estadual (Var) e pela aprovacao (Inc_Aprov).
# Abramovitz adiciona a dummy third_term, igual a 1 em 2002 e 2010.
# Efeitos aleatorios (GLS) estao no cap. 14, pp. 441-444.
formula_lewis <- voto_inc_pct ~ Var + Inc_Aprov
formula_abramovitz <- voto_inc_pct ~ Var + Inc_Aprov + third_term

# Funcao que aplica a mesma formula em cinco especificacoes sem pesos, para
# comparar os estimadores e fazer um Hausman com FE e RE estimados na mesma
# escala. Depois estima RE ponderado por populacao para reproduzir o modelo
# principal do projeto. A diferenca entre variacao dentro e entre estados e
# discutida no cap. 14, pp. 433-445.
ajustar_painel <- function(formula, nome) {
  message("\n================ ", nome, " ================")

  message("Ajustando pooling...")
  modelo_pool <- plm(
    formula, data = painel, model = "pooling"
  )
  message("Ajustando efeitos fixos...")
  modelo_fe <- plm(
    formula, data = painel, model = "within", effect = "individual"
  )
  message("Ajustando efeitos aleatorios...")
  modelo_re <- plm(
    formula, data = painel, model = "random", effect = "individual"
  )
  # A primeira diferenca transforma os erros e os pesos da observacao original;
  # os pesos populacionais do modelo em nivel nao podem ser reaproveitados aqui.
  message("Ajustando primeiras diferencas...")
  modelo_fd <- plm(
    formula, data = painel, model = "fd", effect = "individual"
  )
  message("Ajustando between...")
  modelo_between <- plm(
    formula, data = painel, model = "between", effect = "individual"
  )
  modelo_re_ponderado <- plm(
    formula, data = painel, model = "random", effect = "individual",
    weights = dados_painel$weight_norm
  )

  # Comparar coeficientes ajuda a ver o quanto conclusoes dependem da fonte de
  # variacao usada. Pooling mistura diferencas entre UFs e mudancas no tempo.
  # FE usa mudancas dentro da UF; between usa diferencas entre medias estaduais.
  print(summary(modelo_pool))
  print(summary(modelo_fe))
  print(summary(modelo_re))
  print(summary(modelo_fd))
  print(summary(modelo_between))
  cat("\nEfeitos aleatorios ponderados como no modelo original\n")
  print(summary(modelo_re_ponderado))

  # Hausman: H0 e que o efeito individual nao observado nao se correlaciona com
  # os regressores, tornando RE consistente. Rejeitar H0 favorece FE.
  # Cap. 14, p. 445. O teste nao escolhe causalidade nem corrige especificacao.
  teste_hausman <- phtest(modelo_fe, modelo_re)
  print(teste_hausman)

  # Erros-padrao robustos agrupados por UF permitem heterocedasticidade e
  # correlacao serial dentro do estado. A logica do erro composto e tratada no
  # cap. 14, pp. 441-443. Ha 27 grupos, por isso tambem reportamos CR2.
  cat("\nMQO agrupado: erros-padrao Arellano agrupados por UF\n")
  print(coeftest(
    modelo_pool,
    vcov. = vcovHC(modelo_pool, method = "arellano", type = "HC1", cluster = "group")
  ))
  cat("\nEfeitos fixos: erros-padrao Arellano agrupados por UF\n")
  print(coeftest(
    modelo_fe,
    vcov. = vcovHC(modelo_fe, method = "arellano", type = "HC1", cluster = "group")
  ))
  cat("\nEfeitos aleatorios: CR2 com cluster por UF\n")
  print(clubSandwich::coef_test(modelo_re_ponderado, vcov = "CR2", cluster = "individual"))

  # Mantemos a logica de previsao historica dos scripts originais: os valores
  # ajustados por UF sao agregados por ano com peso populacional. Isso resume
  # ajuste preditivo; nao deve ser confundido com efeito causal.
  previsoes_anuais <- dados_painel %>%
    mutate(ajustado_re = as.numeric(fitted(modelo_re_ponderado))) %>%
    group_by(Ano) %>%
    summarise(
      previsto_ponderado = weighted.mean(ajustado_re, pop),
      observado_ponderado = weighted.mean(voto_inc_pct, pop),
      .groups = "drop"
    )
  cat("\nValores ajustados e observados, ponderados pela populacao\n")
  print(previsoes_anuais)

  # Diagnosticos de correlacao serial em painel: Breusch-Godfrey e Wooldridge.
  # O teste de Wooldridge usa a logica de primeiras diferencas (cap. 13,
  # pp. 414-418; cap. 14, pp. 438-440). Rejeicao pede inferencia robusta e
  # investigacao da especificacao; nao e motivo automatico para trocar coeficientes.
  cat("\nBreusch-Godfrey para painel, H0: sem correlacao serial\n")
  print(pbgtest(modelo_fe, order = 1))
  cat("\nWooldridge para painel, H0: sem correlacao serial\n")
  print(pwartest(modelo_fe))

  # Choques comuns podem correlacionar os residuos de estados diferentes.
  # Dependencia entre unidades: cap. 14, pp. 442-443 e 448.
  cat("\nDependencia entre UFs, H0: independencia transversal\n")
  print(pcdtest(modelo_fe, test = "cd"))

  # Breusch-Pagan testa variancia constante na regressao auxiliar com efeitos
  # especificos de UF. A discussao de heterocedasticidade nos exemplos de painel
  # aparece no cap. 13, pp. 427-429. Este teste nao substitui erros robustos.
  formula_bp <- update(formula, . ~ . + factor(SG_UF))
  cat("\nBreusch-Pagan, H0: variancia constante\n")
  print(bptest(formula_bp, data = dados_painel))

  # Normalidade e inspecao complementar dos residuos. O cap. 13-14 usado aqui
  # nao tem uma secao especifica de Shapiro/Jarque-Bera; nao atribuir pagina a
  # esse teste. Para inferencia, priorizamos erros-padrao robustos.
  resid_re <- as.numeric(residuals(modelo_re_ponderado))
  cat("\nShapiro-Wilk dos residuos (diagnostico complementar)\n")
  print(shapiro.test(resid_re))
  cat("\nJarque-Bera dos residuos (diagnostico complementar)\n")
  print(tseries::jarque.bera.test(resid_re))

  diagnostico <- tibble(
    ajustado = as.numeric(fitted(modelo_re_ponderado)),
    residuo = as.numeric(residuals(modelo_re_ponderado))
  )
  print(
    ggplot(diagnostico, aes(x = ajustado, y = residuo)) +
      geom_point(alpha = 0.55) +
      geom_hline(yintercept = 0, color = "red") +
      geom_smooth(se = FALSE, color = "steelblue") +
      labs(title = paste(nome, "- residuos versus ajustados"),
           x = "Valor ajustado", y = "Residuo") +
      theme_minimal()
  )
  print(
    ggplot(diagnostico, aes(sample = residuo)) +
      stat_qq(alpha = 0.55) + stat_qq_line(color = "red") +
      labs(title = paste(nome, "- QQ-plot dos residuos")) +
      theme_minimal()
  )

  # Sensibilidade: Driscoll-Kraay tambem admite dependencia entre unidades,
  # mas T e curto (anos eleitorais); por isso o resultado e apenas comparativo.
  cat("\nEfeitos fixos: Driscoll-Kraay como sensibilidade (T curto)\n")
  print(coeftest(modelo_fe, vcov. = vcovSCC(modelo_fe, type = "HC1")))

  list(
    pooling = modelo_pool, fixed = modelo_fe, random_unweighted = modelo_re,
    random = modelo_re_ponderado,
    first_difference = modelo_fd, between = modelo_between,
    hausman = teste_hausman, previsoes_anuais = previsoes_anuais
  )
}

# Estimar os dois modelos de painel com exatamente suas formulas originais.
# No RStudio interativo, os graficos aparecem no painel Plots. Em Rscript, um
# dispositivo nulo evita criar um Rplots.pdf incidental ao executar o arquivo.
dispositivo_nulo <- !interactive()
if (dispositivo_nulo) grDevices::pdf(NULL)
resultado_lewis <- ajustar_painel(formula_lewis, "Lewis-Beck")
resultado_abramovitz <- ajustar_painel(formula_abramovitz, "Abramovitz")

# Efeitos fixos de ano (two-way) nao sao adicionados a essas formulas. Inc_Aprov
# e third_term variam por ano, mas sao comuns a todas as UFs; dummies de ano
# absorveriam essas variaveis. Cap. 14, pp. 433-440 explica o within e a remocao
# de componentes invariantes; a colinearidade e uma restricao da nossa medida.
# A estimacao principal continua comparavel aos scripts originais one-way RE.

# 3. MODELO R. LINS: CORTES TRANSVERSAIS -----------------------------------
# R. Lins e estimado separadamente para cada eleicao, em 27 UFs. Portanto,
# primeira diferenca, FE/RE, Hausman e testes de serialidade de painel nao se
# aplicam a esta formula. A associacao cross-section nao e automaticamente causal.
# O cap. 13, pp. 403-412 discute cortes empilhados e analise de politica; nao
# transforma este desenho em diferencas-em-diferencas.
dados_lins <- dados %>%
  filter(Ano %in% c(2018, 2022), SG_UF != "VT") %>%
  left_join(pop_uf, by = "SG_UF") %>%
  mutate(IBF = as.numeric(IBF))

estimar_lins <- function(ano_eleitoral) {
  amostra <- dados_lins %>%
    filter(Ano == ano_eleitoral) %>%
    filter(complete.cases(voto_inc_pct, IBF, pop))

  modelo <- lm(voto_inc_pct ~ IBF, data = amostra)
  cat("\n================ R. Lins - ", ano_eleitoral, " ================\n", sep = "")
  print(summary(modelo))

  # Em corte transversal, HC1/HC3 protege contra heterocedasticidade entre UFs.
  # Nao ha repeticao temporal por UF dentro de cada estimacao para clusterizar.
  cat("\nErros-padrao HC1 robustos a heterocedasticidade\n")
  print(coeftest(modelo, vcov. = vcovHC(modelo, type = "HC1")))
  cat("\nErros-padrao HC3 (sensibilidade em amostra pequena)\n")
  print(coeftest(modelo, vcov. = vcovHC(modelo, type = "HC3")))

  cat("\nBreusch-Pagan, H0: variancia constante\n")
  print(bptest(modelo))
  cat("\nShapiro-Wilk dos residuos (diagnostico complementar)\n")
  print(shapiro.test(residuals(modelo)))
  cat("\nJarque-Bera dos residuos (diagnostico complementar)\n")
  print(tseries::jarque.bera.test(residuals(modelo)))

  # Inspecoes graficas: forma da associacao e padroes nos residuos.
  print(
    ggplot(amostra, aes(x = IBF, y = voto_inc_pct)) +
      geom_point() + geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste("R. Lins", ano_eleitoral),
           x = "IBF (% dos domicilios)", y = "Voto do incumbente (%)") +
      theme_minimal()
  )
  diagnostico <- tibble(
    ajustado = fitted(modelo), residuo = residuals(modelo)
  )
  print(
    ggplot(diagnostico, aes(x = ajustado, y = residuo)) +
      geom_point() + geom_hline(yintercept = 0, color = "red") +
      geom_smooth(se = FALSE) +
      labs(title = paste("R. Lins", ano_eleitoral, "- residuos versus ajustados"),
           x = "Voto ajustado (%)", y = "Residuo") + theme_minimal()
  )
  print(
    ggplot(diagnostico, aes(sample = residuo)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste("R. Lins", ano_eleitoral, "- QQ-plot")) +
      theme_minimal()
  )

  # Mantem a agregacao nacional ponderada por populacao usada no projeto.
  # Trata-se de uma media das previsoes estaduais, nao de nova estimacao.
  amostra <- amostra %>% mutate(previsto = predict(modelo, newdata = amostra))
  resumo_nacional <- amostra %>% summarise(
    Ano = ano_eleitoral,
    previsto_ponderado = weighted.mean(previsto, pop),
    observado_ponderado = weighted.mean(voto_inc_pct, pop)
  )
  print(resumo_nacional)

  list(model = modelo, sample = amostra, national_summary = resumo_nacional)
}

resultado_lins_2018 <- estimar_lins(2018)
resultado_lins_2022 <- estimar_lins(2022)
if (dispositivo_nulo) grDevices::dev.off()

# 4. DIFERENCAS-EM-DIFERENCAS: CONCEITO NAO APLICADO -----------------------
# O cap. 13, pp. 410-412 apresenta DiD como interacao entre grupo tratado e
# periodo posterior, sob a hipotese de tendencias paralelas. Nossos modelos nao
# definem grupo tratado/controle nem choque de politica; criar uma interacao aqui
# seria inventar um desenho de pesquisa. Um teste de medias pre nao provaria essa
# hipotese, e uma so onda pre nao permite observar tendencias paralelas.

# Objetos principais no ambiente ao fim da execucao:
# resultado_lewis, resultado_abramovitz, resultado_lins_2018 e resultado_lins_2022.
