# ==============================================================================
# 02. LIGAÇÃO SISU/UFES -> microdados do ENEM (edição = ano de ingresso - 1)
# ------------------------------------------------------------------------------
# Reproduz o formato de ingressantes_<ano>_ufes_com_enem_todas_colunas.csv:
# colunas do cadastro SISU + matricula, nota_faltando, key + colunas do ENEM.
# Chave = cinco notas (CN_CH_LC_MT_RED, x10, inteiras). Chave ambígua no ENEM
# (mais de um participante com as mesmas cinco notas) => sem ligação (NA).
# Antes: python3 -I scripts/01_filtra_enem.py <ano>  (filtra o CSV de 2-3 GB)
# Uso:   Rscript scripts/02_liga_enem.R <ano>
# ==============================================================================
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })
source("scripts/00_util.R")
args <- commandArgs(trailingOnly = TRUE); ano <- as.integer(args[1])
dec1 <- function(x) { x <- str_replace(str_trim(x), ",", ".")
  ifelse(is.na(x) | x %in% c("", "[Null]", "NA"), NA_integer_, as.integer(round(suppressWarnings(as.numeric(x)) * 10))) }

sisu <- read_delim(file.path(DIR_DADOS, "UFES", "INGRESSANTES_SISU_2018_2026.csv"),
                   delim = ";", col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE) |>
  filter(`ANO INGRESSO` == as.character(ano)) |>
  mutate(TOTAL = as.numeric(str_replace(TOTAL, ",", ".")),
         matricula = str_trim(MATRÍCULA),
         k_cn = dec1(`CIÊNCIAS DA NATUREZA`), k_ch = dec1(`CIÊNCIAS HUMANAS`), k_lc = dec1(LINGUAGENS),
         k_mt = dec1(MATEMÁTICA), k_rd = dec1(REDAÇÃO),
         nota_faltando = is.na(k_cn) | is.na(k_ch) | is.na(k_lc) | is.na(k_mt) | is.na(k_rd),
         key = ifelse(nota_faltando, NA, paste(k_cn, k_ch, k_lc, k_mt, k_rd, sep = "_"))) |>
  select(-starts_with("k_"))

enem <- read_delim(file.path(DIR_GERADO, sprintf("enem_filtrado_%d.csv", ano)), delim = ";",
                   col_types = cols(.default = "c"), locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) |>
  mutate(key = paste(dec1(NU_NOTA_CN), dec1(NU_NOTA_CH), dec1(NU_NOTA_LC), dec1(NU_NOTA_MT),
                     dec1(NU_NOTA_REDACAO), sep = "_")) |>
  # Q026/Q027 só existem nas edições 2017 e 2018: fora, para todos os anos terem as mesmas colunas
  select(key, everything(), -any_of(c("Q026", "Q027")))
amb  <- enem |> count(key) |> filter(n > 1)
enem <- enem |> filter(!key %in% amb$key)

res <- sisu |> left_join(enem, by = "key")
stopifnot(nrow(res) == nrow(sisu))
cat(sprintf("Ingressantes %d: %d | sem nota: %d | ligados: %d | chave ambígua no ENEM: %d | sem par: %d\n",
            ano, nrow(res), sum(res$nota_faltando), sum(!is.na(res$NU_INSCRICAO)),
            sum(res$key %in% amb$key), sum(!res$nota_faltando & is.na(res$NU_INSCRICAO) & !res$key %in% amb$key)))
write_csv(res, file.path(DIR_GERADO, sprintf("ingressantes_%d_ufes_com_enem_todas_colunas.csv", ano)))
