library(electionsBR)
library(dplyr)
library(here)
library(rio)

# =========================
# 1998 — FHC
# =========================

df_1998 <- elections_tse(1998, "vote_mun_zone", br_archive = TRUE)

base_1998 <- df_1998 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "FERNANDO HENRIQUE CARDOSO") %>%
  transmute(estado = SG_UF, ano = 1998, voto_inc)

# =========================
# 2002 — Serra
# =========================

df_2002 <- elections_tse(2002, "vote_mun_zone", br_archive = TRUE)

base_2002 <- df_2002 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "JOSE SERRA") %>%
  transmute(estado = SG_UF, ano = 2002, voto_inc)

# =========================
# 2006 — Lula
# =========================

df_2006 <- elections_tse(2006, "vote_mun_zone", br_archive = TRUE)

base_2006 <- df_2006 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "LUIZ INACIO LULA DA SILVA") %>%
  transmute(estado = SG_UF, ano = 2006, voto_inc)

# =========================
# 2010 — Dilma
# =========================

df_2010 <- elections_tse(2010, "vote_mun_zone", br_archive = TRUE)

base_2010 <- df_2010 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "DILMA ROUSSEFF") %>%
  transmute(estado = SG_UF, ano = 2010, voto_inc)

# =========================
# 2014 — Dilma
# =========================

df_2014 <- elections_tse(2014, "vote_mun_zone", br_archive = TRUE)

base_2014 <- df_2014 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "DILMA ROUSSEFF") %>%
  transmute(estado = SG_UF, ano = 2014, voto_inc)

# =========================
# 2018 — Haddad
# =========================

df_2018 <- elections_tse(2018, "vote_mun_zone", br_archive = TRUE)

base_2018 <- df_2018 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "FERNANDO HADDAD") %>%
  transmute(estado = SG_UF, ano = 2018, voto_inc)

# =========================
# 2022 — Bolsonaro
# =========================

df_2022 <- elections_tse(2022, "vote_mun_zone", br_archive = TRUE)

base_2022 <- import(here("2022/votacao_candidato_munzona_2022_BRASIL.csv")) %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "JAIR MESSIAS BOLSONARO") %>%
  transmute(estado = SG_UF, ano = 2022, voto_inc,votos)

# Biblioteca ElectionsBR não tem 2026
# Fonte exata: conjunto “Resultados - 2026” do TSE · ZIP original. A geração indicada nas linhas do arquivo é 17/11/2026, às 08:32:47.
# 
# SHA-256 para conferir a integridade do CSV:

e2026 <- import("votacao_candidato_munzona_2026.csv")

base_2026 <- e2026 %>%
  filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ") %>%
  group_by(SG_UF, NM_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop") %>%
  group_by(SG_UF) %>%
  mutate(voto_inc = votos / sum(votos)) %>%
  filter(NM_CANDIDATO == "LUIZ IN\xc1CIO LULA DA SILVA") %>%
  transmute(estado = SG_UF, ano = 2026, voto_inc,votos)



# Variação do PIB ---------------------------------------------------------

# Instale uma vez, se necessário:
# install.packages("jsonlite")

library(jsonlite)

# API oficial do Ipeadata: série PIBVRAE
url_pagina <- "https://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PIBVRAE')"

# Lê todas as páginas retornadas pela API.
paginas <- list()

while (!is.null(url_pagina) && nzchar(url_pagina)) {
  resposta <- fromJSON(url_pagina, simplifyDataFrame = TRUE)
  paginas[[length(paginas) + 1]] <- resposta$value
  url_pagina <- resposta[["@odata.nextLink"]]
}

dados_api <- do.call(rbind, paginas)

# Converte os códigos territoriais do IBGE em siglas de UF.
mapa_uf <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO",
  "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN",
  "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
  "29" = "BA",
  "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "50" = "MS", "51" = "MT", "52" = "GO", "53" = "DF"
)

dados_api$codigo_uf <- sprintf("%02d", as.integer(dados_api$TERCODIGO))

# Mantém somente as observações estaduais, excluindo Brasil e regiões.
dados_estaduais <- dados_api[
  dados_api$NIVNOME == "Estados" &
    dados_api$codigo_uf %in% names(mapa_uf),
]

# Organiza a variável Var e as chaves usadas no join.
dados_var <- data.frame(
  SG_UF = unname(mapa_uf[dados_estaduais$codigo_uf]),
  Ano = as.integer(substr(dados_estaduais$VALDATA, 1, 4)),
  Var = as.numeric(dados_estaduais$VALVALOR)
)

dados_var <- dados_var[order(dados_var$SG_UF, dados_var$Ano), ]

# Salva a série anual completa até o último ano disponível na API.
write.csv(
  dados_var,
  "Var_PIB_Estadual_UF_Serie_Completa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Salva apenas os anos das eleições presidenciais/estaduais.
ano_mais_recente <- max(dados_var$Ano, na.rm = TRUE)
anos_eleitorais <- seq(1994, ano_mais_recente, by = 4)

var_anos_eleitorais <- dados_var[
  dados_var$Ano %in% anos_eleitorais,
]

write.csv(
  var_anos_eleitorais,
  "Var_PIB_Estadual_UF_Anos_Eleitorais.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Último ano disponível na API:", ano_mais_recente, "\n")

# Buscando os dados mais atuais. Pelo jeito, temos o o Var apenas até 2024. Vou aproveitar e buscar o Inc_Aprovement de uma vez
# FONTES DA BASE DE 2024
#
# Var — variação anual do PIB estadual:
# Fonte: IBGE, Sistema de Contas Regionais: Brasil 2024.
# Arquivo: Especiais_2002_2024_xls.zip, arquivo interno tab03.xls,
# aba "Tabela3", Tabela 3 — Série encadeada do volume do PIB
# por Brasil, Grandes Regiões e Unidades da Federação, 2002–2024.
# Download: https://ftp.ibge.gov.br/Contas_Regionais/2024/xls/Especiais_2002_2024_xls.zip
# Página da publicação: https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9054-contas-regionais-do-brasil
# Cálculo: (índice de volume do PIB em 2024 / índice em 2023 - 1) * 100.
#
# Inc_Aprov — aprovação nacional do governo incumbente:
# Fonte: Datafolha, avaliação do governo Lula.
# Trabalho de campo: 29 a 31 de julho de 2024; resultado nacional:
# 35% de avaliação "ótimo/bom". Valor armazenado como 0.35,
# repetido para todas as UFs.
# Resultado: https://datafolha.folha.uol.com.br/avaliacao-de-governo/2024/08/estavel-governo-lula-e-aprovado-por-35-e-33-reprovam.shtml
# Relatório completo: https://media.folha.uol.com.br/datafolha/2024/08/05/spw-s11oywz-160hjii5lj-linlgbi8dbmuhskohizkd5lbcc67ch-cq0lczrkkemrlhsaustqconorogwsxla.pdf
#
# Acesso às fontes: 17/11/2026.

dados2024 <- import("Base_Var_IncAprov_UF_2024.csv")




# =========================
# JUNTA FINAL
# =========================

# base_eleitoral <- bind_rows(
#   teste_base_eleitoral1,
#   base_2022,
#   fhc_votos_1994_por_estado
# ) %>% select(-estado)


dados <- read_csv(here("dados", "BaseDados1.csv"))

#Limpeza
# Base 2026
base_2026 <- base_2026 %>%
  ungroup() %>%
  transmute(
    SG_UF,
    Ano = ano,
    voto_inc,
    votos
  )

# Base 2024
dados2024 <- dados2024 %>%
  transmute(
    SG_UF,
    Ano,
    Var,
    Inc_Aprov
  )

#Join

# Atualizar informações de 2024
dados <- dados %>%
  rows_update(
    dados2024,
    by = c("SG_UF", "Ano")
  )

# Acrescentar informações de 2026
dados <- dados %>%
  bind_rows(base_2026) %>%
  arrange(Ano, SG_UF)


# Salvar
write.csv(dados, "BaseDados2.csv", row.names = FALSE)
