# ==============================================================================
# 11. FORMAS DO ESTIMADOR SANDUÍCHE (HC0 a HC3) NO MODELO PRINCIPAL
# ------------------------------------------------------------------------------
# Long e Ervin (2000) recomendam a HC3 até 250 observações e relatam formas quase
# iguais a partir de 500. Com n = 1.312, confere-se quanto a escolha muda os
# erros-padrão e os testes globais da renda e da origem.
# Saída: saidas/art_formas_hc.csv
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(splines); library(sandwich); library(lmtest)
})
source("scripts/00_util.R")

d <- readRDS("dados/gerados/base_modelo_evasao_1ano.rds") |> prep_artigo() |>
  filter(coorte %in% ANOS_ANALISE) |> droplevels()
RENDA  <- "ns(renda_sm, df = 3)"
OUTRAS <- c("origem", "sexo", "cor_raca", "curso7", "coorte")
F_PR   <- reformulate(c(RENDA, "enem_100", OUTRAS), "evadido_1ano")
cc <- d |> filter(if_all(c(evadido_1ano, renda_sm, enem_100, mt_100, cn_100, all_of(OUTRAS)), ~ !is.na(.x)))
m  <- glm(F_PR, data = cc, family = poisson(link = "log"))
stopifnot(nobs(m) == 1312)

wald <- function(termo, tipo) {
  red <- update(m, as.formula(paste(". ~ . -", termo)), data = cc)
  waldtest(red, m, vcov = vcovHC(m, type = tipo), test = "Chisq")[2, "Pr(>Chisq)"]
}
ep3 <- sqrt(diag(vcovHC(m, type = "HC3")))[-1]
res <- bind_rows(lapply(c("HC0", "HC1", "HC2", "HC3"), \(tipo) {
  ep <- sqrt(diag(vcovHC(m, type = tipo)))[-1]
  data.frame(forma = tipo,
             razao_ep_min = min(ep / ep3), razao_ep_max = max(ep / ep3),
             p_renda = wald(RENDA, tipo), p_origem = wald("origem", tipo))
}))
dir.create("saidas", showWarnings = FALSE)
write_csv(res, "saidas/art_formas_hc.csv")
print(res, digits = 4)
