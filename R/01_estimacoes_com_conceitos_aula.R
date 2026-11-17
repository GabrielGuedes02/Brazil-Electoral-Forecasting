# =============================================================================
# 01_estimacoes_com_conceitos_aula.R
# Estimacoes: Lewis-Beck, Abramovitz e R. Lins.
#
# O arquivo esta organizado por modelo. Cada etapa aparece no proprio bloco,
# sem funcao auxiliar que esconda os passos da estimacao.
# As referencias indicam a numeracao impressa nas paginas do PDF fornecido.
# =============================================================================

# 0. PACOTES -----------------------------------------------------------------
# Instale apenas o que estiver faltando. Os pacotes sao usados nas secoes abaixo.
pacotes <- c(
  "here", "readr", "dplyr", "tibble", "ggplot2", "plm",
  "lmtest", "sandwich", "clubSandwich", "tseries"
)

pacotes_faltantes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]
if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes)
}

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

# Identifica a pasta raiz do projeto quando o arquivo e aberto no RStudio.
here::i_am("R/01_estimacoes_com_conceitos_aula.R")

# 1. LEITURA DA BASE ---------------------------------------------------------
# voto_inc esta entre 0 e 1. Multiplicamos por 100 para expressar o resultado
# eleitoral em pontos percentuais, como nas estimacoes originais.
dados <- read_csv(
  here("dados", "BaseDados1.csv"),
  na = c("", "NA"),
  show_col_types = FALSE
)

dados <- dados %>%
  select(SG_UF, Ano, voto_inc, votos, Var, Inc_Aprov, IBF) %>%
  mutate(
    SG_UF = as.character(SG_UF),
    Ano = as.integer(Ano),
    across(c(voto_inc, votos, Var, Inc_Aprov, IBF), as.numeric),
    voto_inc_pct = 100 * voto_inc
  )

# Populacao do Censo 2022, mantida conforme a logica dos scripts originais.
# Ela e usada como peso nas medias nacionais e nas estimacoes RE principais.
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

# 2. PREPARACAO DO PAINEL ----------------------------------------------------
# A linha de 2024 contem IBF, mas nao contem voto ou crescimento. Ela nao entra
# no painel eleitoral. VT tambem e removido, pois nao e uma UF da amostra.
dados_painel <- dados %>%
  filter(Ano <= 2022, SG_UF != "VT") %>%
  mutate(third_term = as.integer(Ano %in% c(2002, 2010))) %>%
  left_join(pop_uf, by = "SG_UF") %>%
  mutate(weight_norm = pop / mean(pop, na.rm = TRUE)) %>%
  filter(complete.cases(voto_inc_pct, Var, Inc_Aprov, third_term, weight_norm))

# Cada linha identifica um estado em um ano eleitoral. A organizacao dos dados
# em painel e discutida no cap. 13, pp. 419-421.
painel <- pdata.frame(dados_painel, index = c("SG_UF", "Ano"))

# Em execucao pelo RStudio, os graficos aparecem no painel Plots. Ao executar
# com Rscript, o dispositivo nulo impede a criacao de um Rplots.pdf incidental.
dispositivo_nulo <- !interactive()
if (dispositivo_nulo) grDevices::pdf(NULL)

# 3. LEWIS-BECK: PAINEL ------------------------------------------------------
# Formula original: voto estadual depende de crescimento e aprovacao.
# Estimacao principal: efeitos aleatorios ponderados pela populacao.
# RE e o erro composto sao apresentados no cap. 14, pp. 441-444.

# 3.1 MQO agrupado: trata cada estado-ano como observacao e nao controla
# explicitamente a heterogeneidade permanente entre estados.
lewis_pool <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "pooling"
)
summary(lewis_pool)

# 3.2 Efeitos fixos (within): usa mudancas dentro de cada estado ao longo do
# tempo e remove caracteristicas estaduais constantes. Cap. 14, pp. 433-440.
lewis_fe <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "within",
  effect = "individual"
)
summary(lewis_fe)

# 3.3 Efeitos aleatorios sem pesos: esta versao e usada junto com o FE no
# teste de Hausman, para comparar estimacoes sem pesos na mesma escala.
lewis_re_sem_peso <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "random",
  effect = "individual"
)
summary(lewis_re_sem_peso)

# 3.4 Primeiras diferencas: compara a mudanca no voto com as mudancas nas
# variaveis explicativas. Cap. 13, pp. 414-423 e pp. 424-431.
# Este bloco fica sem pesos porque os pesos em nivel nao podem ser transferidos
# diretamente para a equacao diferenciada.
lewis_fd <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "fd",
  effect = "individual"
)
summary(lewis_fd)

# 3.5 Between: relaciona as medias das variaveis de cada UF; compara estados,
# nao mudancas dentro do mesmo estado.
lewis_between <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "between",
  effect = "individual"
)
summary(lewis_between)

# 3.6 Modelo original ponderado: normalizamos a populacao para que a media dos
# pesos seja 1. Essa e a estimacao principal do projeto.
lewis_re_ponderado <- plm(
  voto_inc_pct ~ Var + Inc_Aprov,
  data = painel,
  model = "random",
  effect = "individual",
  weights = dados_painel$weight_norm
)
summary(lewis_re_ponderado)

# 3.7 Hausman: H0 diz que o efeito estadual nao observado nao se correlaciona
# com os regressores. Rejeitar H0 favorece FE. Cap. 14, p. 445.
hausman_lewis <- phtest(lewis_fe, lewis_re_sem_peso)
hausman_lewis

# 3.8 Erros-padrao robustos para pooling e FE, agrupados por UF. Isso permite
# heterocedasticidade e correlacao serial dentro do estado.
coeftest(
  lewis_pool,
  vcov. = vcovHC(lewis_pool, method = "arellano", type = "HC1", cluster = "group")
)
coeftest(
  lewis_fe,
  vcov. = vcovHC(lewis_fe, method = "arellano", type = "HC1", cluster = "group")
)

# Para o RE ponderado, tambem mostramos CR2 agrupado por UF, conforme a logica
# do script original. Ha 27 UFs na amostra.
clubSandwich::coef_test(
  lewis_re_ponderado,
  vcov = "CR2",
  cluster = "individual"
)

# 3.9 Diagnosticos de painel. H0 dos testes abaixo e ausencia do problema citado.
# BG e Wooldridge verificam correlacao serial (cap. 13, pp. 414-418).
pbgtest(lewis_fe, order = 1)
pwartest(lewis_fe)

# CD verifica se choques comuns correlacionam os residuos entre estados.
# Cap. 14, pp. 442-443 e p. 448.
pcdtest(lewis_fe, test = "cd")

# Breusch-Pagan testa variancia constante usando dummies de UF. Heterocedasticidade
# em painel e discutida nos exemplos do cap. 13, pp. 427-429.
bptest(
  voto_inc_pct ~ Var + Inc_Aprov + factor(SG_UF),
  data = dados_painel
)

# Normalidade e inspecao complementar; os caps. 13-14 nao apresentam uma pagina
# especifica para Shapiro-Wilk ou Jarque-Bera. A inferencia usa erros robustos.
residuos_lewis <- as.numeric(residuals(lewis_re_ponderado))
shapiro.test(residuos_lewis)
tseries::jarque.bera.test(residuos_lewis)

# Inspecao grafica dos residuos versus ajustados e QQ-plot.
diagnostico_lewis <- tibble(
  ajustado = as.numeric(fitted(lewis_re_ponderado)),
  residuo = as.numeric(residuals(lewis_re_ponderado))
)

grafico_lewis_ajustados <- ggplot(diagnostico_lewis, aes(ajustado, residuo)) +
  geom_point(alpha = 0.55) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(se = FALSE, color = "steelblue") +
  labs(title = "Lewis-Beck: residuos versus ajustados",
       x = "Voto ajustado (%)", y = "Residuo") +
  theme_minimal()
print(grafico_lewis_ajustados)

grafico_lewis_qq <- ggplot(diagnostico_lewis, aes(sample = residuo)) +
  stat_qq(alpha = 0.55) +
  stat_qq_line(color = "red") +
  labs(title = "Lewis-Beck: QQ-plot dos residuos") +
  theme_minimal()
print(grafico_lewis_qq)

# Driscoll-Kraay e mostrado como sensibilidade para dependencia transversal.
# O painel tem poucos anos eleitorais; por isso nao tratamos essa correcao como
# solucao definitiva. Cap. 14, pp. 442-448.
coeftest(lewis_fe, vcov. = vcovSCC(lewis_fe, type = "HC1"))

# Valores ajustados por estado agregados por ano e ponderados pela populacao.
# Isso resume ajuste preditivo, nao e um novo efeito causal.
dados_painel$ajustado_lewis <- as.numeric(fitted(lewis_re_ponderado))
previsoes_lewis <- dados_painel %>%
  group_by(Ano) %>%
  summarise(
    previsto_ponderado = weighted.mean(ajustado_lewis, pop),
    observado_ponderado = weighted.mean(voto_inc_pct, pop),
    .groups = "drop"
  )
previsoes_lewis

# 4. ABRAMOVITZ: PAINEL ------------------------------------------------------
# Formula original: Lewis-Beck acrescida da dummy third_term, igual a 1 em
# 2002 e 2010. Repetimos os passos para que cada estimacao fique visivel.

abramovitz_pool <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "pooling"
)
summary(abramovitz_pool)

abramovitz_fe <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "within",
  effect = "individual"
)
summary(abramovitz_fe)

# RE sem pesos entra na comparacao com FE pelo teste de Hausman.
abramovitz_re_sem_peso <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "random",
  effect = "individual"
)
summary(abramovitz_re_sem_peso)

# Primeiras diferencas sem pesos, pelas mesmas razoes explicadas acima.
abramovitz_fd <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "fd",
  effect = "individual"
)
summary(abramovitz_fd)

# Between resume a relacao entre medias estaduais.
abramovitz_between <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "between",
  effect = "individual"
)
summary(abramovitz_between)

# RE ponderado reproduz a especificacao principal de Abramovitz no projeto.
abramovitz_re_ponderado <- plm(
  voto_inc_pct ~ Var + Inc_Aprov + third_term,
  data = painel,
  model = "random",
  effect = "individual",
  weights = dados_painel$weight_norm
)
summary(abramovitz_re_ponderado)

# Hausman sem pesos: H0 favorece RE; rejeitar H0 favorece FE. Cap. 14, p. 445.
hausman_abramovitz <- phtest(abramovitz_fe, abramovitz_re_sem_peso)
hausman_abramovitz

# Erros-padrao robustos agrupados por UF para pooling e FE.
coeftest(
  abramovitz_pool,
  vcov. = vcovHC(abramovitz_pool, method = "arellano", type = "HC1", cluster = "group")
)
coeftest(
  abramovitz_fe,
  vcov. = vcovHC(abramovitz_fe, method = "arellano", type = "HC1", cluster = "group")
)

# CR2 por UF para o modelo aleatorio ponderado.
clubSandwich::coef_test(
  abramovitz_re_ponderado,
  vcov = "CR2",
  cluster = "individual"
)

# Testes de autocorrelacao e dependencia entre as UFs.
pbgtest(abramovitz_fe, order = 1)
pwartest(abramovitz_fe)
pcdtest(abramovitz_fe, test = "cd")

# Heterocedasticidade: H0 do Breusch-Pagan e variancia constante.
bptest(
  voto_inc_pct ~ Var + Inc_Aprov + third_term + factor(SG_UF),
  data = dados_painel
)

# Testes de normalidade complementares e graficos de residuos.
residuos_abramovitz <- as.numeric(residuals(abramovitz_re_ponderado))
shapiro.test(residuos_abramovitz)
tseries::jarque.bera.test(residuos_abramovitz)

diagnostico_abramovitz <- tibble(
  ajustado = as.numeric(fitted(abramovitz_re_ponderado)),
  residuo = as.numeric(residuals(abramovitz_re_ponderado))
)

grafico_abramovitz_ajustados <- ggplot(
  diagnostico_abramovitz, aes(ajustado, residuo)
) +
  geom_point(alpha = 0.55) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(se = FALSE, color = "steelblue") +
  labs(title = "Abramovitz: residuos versus ajustados",
       x = "Voto ajustado (%)", y = "Residuo") +
  theme_minimal()
print(grafico_abramovitz_ajustados)

grafico_abramovitz_qq <- ggplot(
  diagnostico_abramovitz, aes(sample = residuo)
) +
  stat_qq(alpha = 0.55) +
  stat_qq_line(color = "red") +
  labs(title = "Abramovitz: QQ-plot dos residuos") +
  theme_minimal()
print(grafico_abramovitz_qq)

# Driscoll-Kraay como sensibilidade; T e curto neste painel eleitoral.
coeftest(abramovitz_fe, vcov. = vcovSCC(abramovitz_fe, type = "HC1"))

# Agrega ajustados e observados por ano com pesos populacionais.
dados_painel$ajustado_abramovitz <- as.numeric(fitted(abramovitz_re_ponderado))
previsoes_abramovitz <- dados_painel %>%
  group_by(Ano) %>%
  summarise(
    previsto_ponderado = weighted.mean(ajustado_abramovitz, pop),
    observado_ponderado = weighted.mean(voto_inc_pct, pop),
    .groups = "drop"
  )
previsoes_abramovitz

# 5. R. LINS: CORTE TRANSVERSAL DE 2018 ------------------------------------
# Cada ano e estimado separadamente por MQO. R. Lins nao e um painel; por isso
# FE/RE, Hausman e testes de correlacao serial nao se aplicam a este modelo.

lins_2018 <- dados %>%
  filter(Ano == 2018, SG_UF != "VT") %>%
  left_join(pop_uf, by = "SG_UF") %>%
  filter(complete.cases(voto_inc_pct, IBF, pop))

# Formula original: voto do incumbente explicado pelo IBF estadual.
lins_2018_mqo <- lm(voto_inc_pct ~ IBF, data = lins_2018)
summary(lins_2018_mqo)

# HC1 e HC3 robustos a heterocedasticidade; HC3 e uma verificacao util em N=27.
coeftest(lins_2018_mqo, vcov. = vcovHC(lins_2018_mqo, type = "HC1"))
coeftest(lins_2018_mqo, vcov. = vcovHC(lins_2018_mqo, type = "HC3"))

# BP testa H0 de variancia constante. Shapiro/Jarque-Bera sao complementares.
bptest(lins_2018_mqo)
shapiro.test(residuals(lins_2018_mqo))
tseries::jarque.bera.test(residuals(lins_2018_mqo))

# Grafico da relacao e graficos dos residuos.
grafico_lins_2018 <- ggplot(lins_2018, aes(IBF, voto_inc_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "R. Lins: estimacao de 2018",
       x = "IBF (% dos domicilios)", y = "Voto do incumbente (%)") +
  theme_minimal()
print(grafico_lins_2018)

diagnostico_lins_2018 <- tibble(
  ajustado = fitted(lins_2018_mqo),
  residuo = residuals(lins_2018_mqo)
)

grafico_lins_2018_residuos <- ggplot(
  diagnostico_lins_2018, aes(ajustado, residuo)
) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(se = FALSE) +
  labs(title = "R. Lins 2018: residuos versus ajustados",
       x = "Voto ajustado (%)", y = "Residuo") +
  theme_minimal()
print(grafico_lins_2018_residuos)

print(
  ggplot(diagnostico_lins_2018, aes(sample = residuo)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "R. Lins 2018: QQ-plot") +
    theme_minimal()
)

# Previsao por UF e media nacional ponderada pela populacao.
lins_2018$previsto <- predict(lins_2018_mqo, newdata = lins_2018)
resumo_lins_2018 <- lins_2018 %>%
  summarise(
    Ano = 2018,
    previsto_ponderado = weighted.mean(previsto, pop),
    observado_ponderado = weighted.mean(voto_inc_pct, pop)
  )
resumo_lins_2018

# 6. R. LINS: CORTE TRANSVERSAL DE 2022 ------------------------------------
# Repetimos as etapas para deixar visivel que esta e uma segunda regressao,
# com dados e coeficientes proprios, e nao uma continuacao da regressao de 2018.
lins_2022 <- dados %>%
  filter(Ano == 2022, SG_UF != "VT") %>%
  left_join(pop_uf, by = "SG_UF") %>%
  filter(complete.cases(voto_inc_pct, IBF, pop))

lins_2022_mqo <- lm(voto_inc_pct ~ IBF, data = lins_2022)
summary(lins_2022_mqo)

coeftest(lins_2022_mqo, vcov. = vcovHC(lins_2022_mqo, type = "HC1"))
coeftest(lins_2022_mqo, vcov. = vcovHC(lins_2022_mqo, type = "HC3"))

bptest(lins_2022_mqo)
shapiro.test(residuals(lins_2022_mqo))
tseries::jarque.bera.test(residuals(lins_2022_mqo))

grafico_lins_2022 <- ggplot(lins_2022, aes(IBF, voto_inc_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "R. Lins: estimacao de 2022",
       x = "IBF (% dos domicilios)", y = "Voto do incumbente (%)") +
  theme_minimal()
print(grafico_lins_2022)

diagnostico_lins_2022 <- tibble(
  ajustado = fitted(lins_2022_mqo),
  residuo = residuals(lins_2022_mqo)
)

print(
  ggplot(diagnostico_lins_2022, aes(ajustado, residuo)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    geom_smooth(se = FALSE) +
    labs(title = "R. Lins 2022: residuos versus ajustados",
         x = "Voto ajustado (%)", y = "Residuo") +
    theme_minimal()
)

print(
  ggplot(diagnostico_lins_2022, aes(sample = residuo)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "R. Lins 2022: QQ-plot") +
    theme_minimal()
)

lins_2022$previsto <- predict(lins_2022_mqo, newdata = lins_2022)
resumo_lins_2022 <- lins_2022 %>%
  summarise(
    Ano = 2022,
    previsto_ponderado = weighted.mean(previsto, pop),
    observado_ponderado = weighted.mean(voto_inc_pct, pop)
  )
resumo_lins_2022

# 7. CONCEITO DID: NAO APLICADO --------------------------------------------
# Wooldridge, cap. 13, pp. 410-412: DiD usa uma interacao tratamento x periodo
# posterior e depende da hipotese de tendencias paralelas. Estes modelos nao
# definem grupo tratado, grupo de controle e um evento com periodos pre/pos.
# Um teste de medias pre nao provaria tendencias paralelas. Por isso nao criamos
# uma estimacao de DiD sem um desenho de pesquisa que a sustente.

if (dispositivo_nulo) {
  grDevices::dev.off()
}
