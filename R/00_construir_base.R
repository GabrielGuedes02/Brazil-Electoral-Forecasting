# =============================================================================
# 00_construir_base.R
# Reconstroi a base de trabalho usada nas estimacoes do repositorio.
#
# Fonte historica: BaseDados.csv (1994-2022), preparada antes deste script e
# distribuida em dados/. Fonte do IBF de 2024: planilha do IBGE em dados/.
# A planilha tem cabecalhos explicativos; por isso a leitura começa na linha 6.
#
# O arquivo BaseDados1_original.csv preserva a versao recebida. Este script
# gera BaseDados1.csv com as mesmas variaveis analiticas, sem colunas de indice
# introduzidas por exportacoes anteriores e sem uma linha final vazia.
# =============================================================================

# Pacotes usados somente para ler, organizar e salvar os arquivos.
pacotes <- c("dplyr", "readr", "readxl", "tibble")
nao_instalados <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(nao_instalados) > 0) {
  install.packages(nao_instalados)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(tibble)
})

# Use o diretorio raiz do projeto quando executado no RStudio ou via Rscript.
# Se iniciar o R em outra pasta, ajuste o diretorio de trabalho para esta pasta.
arquivo_historico <- file.path("dados", "BaseDados.csv")
arquivo_ibf <- file.path("dados", "IBF - 2024.xlsx")
arquivo_saida <- file.path("dados", "BaseDados1.csv")

# A base historica tem colunas de indice herdadas de CSVs anteriores. Selecionamos
# apenas as variaveis que entram na analise para evitar indices duplicados.
base_historica <- read_csv(
  arquivo_historico, name_repair = "minimal", show_col_types = FALSE
) %>%
  select(SG_UF, Ano, voto_inc, votos, Var, Inc_Aprov, IBF) %>%
  mutate(
    SG_UF = as.character(SG_UF),
    Ano = as.integer(Ano),
    across(c(voto_inc, votos, Var, Inc_Aprov, IBF), as.numeric)
  ) %>%
  filter(!is.na(SG_UF), SG_UF != "", !is.na(Ano))

# A planilha tem 27 observacoes estaduais nas linhas 6 a 32. Ler esse intervalo
# exclui a nota de fonte que aparece abaixo da tabela e nao e uma Unidade da Federacao.
ibf_lido <- read_excel(
  arquivo_ibf,
  range = "A6:B32",
  col_names = c("Estado", "IBF")
)

# Nesta versao da tabela do IBGE, as UFs aparecem nesta ordem. Atribuir as siglas
# pela posicao evita falhas de comparacao de caracteres acentuados no Windows.
siglas_na_ordem_da_planilha <- c(
  "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
  "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
  "PR", "SC", "RS", "MS", "MT", "GO", "DF"
)
stopifnot(nrow(ibf_lido) == length(siglas_na_ordem_da_planilha))

ibf_2024 <- ibf_lido %>%
  mutate(SG_UF = siglas_na_ordem_da_planilha) %>%
  transmute(
    SG_UF,
    Ano = 2024L,
    voto_inc = NA_real_,
    votos = NA_real_,
    Var = NA_real_,
    Inc_Aprov = NA_real_,
    IBF = as.numeric(IBF)
  )

# Verificacoes de estrutura antes do empilhamento: exatamente uma observacao por
# UF em 2024, com nenhuma sigla sem correspondencia no mapa.
stopifnot(nrow(ibf_2024) == 27L)
stopifnot(!anyNA(ibf_2024$SG_UF), !anyDuplicated(ibf_2024$SG_UF))

# bind_rows empilha a nova onda com as observacoes historicas. A base resultante
# e nao balanceada no tempo para 2024: nesse ano so existe IBF, sem voto ou PIB.
base_final <- bind_rows(base_historica, ibf_2024) %>%
  arrange(Ano, SG_UF)

# Salvamos um CSV UTF-8 sem numeros de linha, pronto para os demais scripts.
write_csv(base_final, arquivo_saida, na = "NA")

message("Base salva em: ", normalizePath(arquivo_saida, winslash = "/"))
message("Linhas historicas: ", nrow(base_historica),
        " | linhas de IBF 2024: ", nrow(ibf_2024),
        " | total: ", nrow(base_final))
