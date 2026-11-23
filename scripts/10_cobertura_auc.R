# ==============================================================================
# 10. COBERTURA DO INTERVALO DA AUC CORRIGIDA E DO ΔAUC
# ------------------------------------------------------------------------------
# População: as covariáveis das matrículas do modelo principal, com o risco do
# modelo completo ajustado como verdade. Em cada simulação: sorteiam-se n
# matrículas com reposição e o desfecho de Bernoulli(risco verdadeiro); ajustam-se
# a linha de base (curso + coorte) e o modelo completo; calculam-se a AUC corrigida
# (Harrell) e o intervalo adaptado de Noma et al. (riscos do modelo da amostra
# fixos, percentis deslocados pelo otimismo médio), como em 05_modelo.R, com B
# reamostras. Alvo (Noma et al.): a AUC do modelo ajustado na amostra avaliada na
# população, calculada exatamente sobre os pares de matrículas ponderados pelos
# riscos verdadeiros. Saída: saidas/art_cobertura_auc.csv (agregados).
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
F_BASE <- evadido_1ano ~ curso7 + coorte
cc <- d |> select(evadido_1ano, renda_sm, enem_100, mt_100, cn_100, all_of(OUTRAS)) |>
  filter(if_all(everything(), ~ !is.na(.x)))
n <- nrow(cc)

auc <- function(p, y) {                       # Mann–Whitney (empates = 1/2)
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# AUC populacional de um escore s quando Y_i ~ Bernoulli(p_i), i = 1..N, pares independentes
auc_pop <- function(s, p) {
  o <- order(s); s <- s[o]; p <- p[o]; q <- 1 - p
  g <- split(seq_along(s), s)                 # grupos de empate, em ordem crescente
  sp <- vapply(g, \(i) sum(p[i]), 0); sq <- vapply(g, \(i) sum(q[i]), 0)
  menores_q <- cumsum(sq) - sq                # massa de não eventos com escore menor
  sum(sp * (menores_q + 0.5 * sq)) / (sum(p) * sum(q))
}
fit <- function(f, dat) suppressWarnings(glm(f, data = dat, family = poisson(link = "log")))
m_true <- fit(F_PR, cc); p_true <- fitted(m_true)
stopifnot(max(p_true) < 1)

# Uma simulação: devolve AUC corrigida, IC, alvo, para o completo e para o ΔAUC
uma <- function(s, B) {
  idx <- sample(n, replace = TRUE); dat <- cc[idx, ]
  dat$evadido_1ano <- rbinom(n, 1, p_true[idx])
  mods <- list(base = F_BASE, comp = F_PR)
  fits <- lapply(mods, fit, dat = dat)
  if (any(vapply(fits, \(m) any(is.na(coef(m))), TRUE))) return(NULL)
  y <- dat$evadido_1ano; p0 <- lapply(fits, fitted)
  ap <- vapply(p0, auc, 0, y = y)
  alvo <- vapply(fits, \(m) auc_pop(predict(m, newdata = cc, type = "response"), p_true), 0)
  bo <- replicate(B, {
    i <- sample(n, replace = TRUE); bs <- dat[i, ]
    out <- tryCatch(vapply(names(mods), \(k) {
      fb <- fit(mods[[k]], bs)
      c(auc(fitted(fb), bs$evadido_1ano) - auc(predict(fb, newdata = dat, type = "response"), y),
        auc(p0[[k]][i], y[i]))
    }, c(0, 0)), error = function(e) matrix(NA, 2, 2))
    c(out)                                    # opt_base, samp_base, opt_comp, samp_comp
  })
  bo <- bo[, colSums(is.na(bo)) == 0, drop = FALSE]
  opt  <- c(base = mean(bo[1, ]), comp = mean(bo[3, ]))
  corr <- ap - opt
  ic_c <- quantile(bo[4, ], c(.025, .975)) - opt[["comp"]]
  ic_d <- quantile(bo[4, ] - bo[2, ], c(.025, .975)) - (opt[["comp"]] - opt[["base"]])
  data.frame(sim = s, B_efetivo = ncol(bo),
             corr_comp = corr[["comp"]], inf_comp = ic_c[[1]], sup_comp = ic_c[[2]], alvo_comp = alvo[["comp"]],
             corr_delta = corr[["comp"]] - corr[["base"]], inf_delta = ic_d[[1]], sup_delta = ic_d[[2]],
             alvo_delta = alvo[["comp"]] - alvo[["base"]])
}

S <- 200; B <- 200
RNGkind("L'Ecuyer-CMRG"); set.seed(20261122)
res <- bind_rows(parallel::mclapply(seq_len(S), uma, B = B, mc.set.seed = TRUE,
                                    mc.cores = max(1, parallel::detectCores() - 1)))
cob <- function(inf, sup, alvo) mean(inf <= alvo & alvo <= sup)
resumo <- data.frame(
  quantidade = c("AUC corrigida do modelo completo", "ΔAUC (completo − linha de base)"),
  simulacoes = nrow(res), B = B,
  cobertura = c(cob(res$inf_comp, res$sup_comp, res$alvo_comp), cob(res$inf_delta, res$sup_delta, res$alvo_delta)),
  abaixo = c(mean(res$sup_comp < res$alvo_comp), mean(res$sup_delta < res$alvo_delta)),
  acima  = c(mean(res$inf_comp > res$alvo_comp), mean(res$inf_delta > res$alvo_delta)),
  largura_media = c(mean(res$sup_comp - res$inf_comp), mean(res$sup_delta - res$inf_delta)),
  alvo_medio = c(mean(res$alvo_comp), mean(res$alvo_delta)),
  estimativa_media = c(mean(res$corr_comp), mean(res$corr_delta)))
resumo$ep_monte_carlo <- sqrt(resumo$cobertura * (1 - resumo$cobertura) / resumo$simulacoes)
out(resumo, "art_cobertura_auc.csv"); print(resumo)
