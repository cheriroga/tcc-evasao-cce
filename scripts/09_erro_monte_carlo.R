# ==============================================================================
# 09. ERRO DE MONTE CARLO DOS INTERVALOS DA AUC
# ------------------------------------------------------------------------------
# Refaz o bootstrap da AUC de 05_modelo.R (linha de base e modelo completo) com
# dez sementes para cada B e grava a variação dos limites dos intervalos entre
# sementes. Sustenta a escolha de B = 5.000 nos Métodos.
# Saída: saidas/art_auc_monte_carlo.csv            (~10 min em 12 núcleos)
# ==============================================================================
# ponytail: reaproveita o código de 05_modelo.R avaliando as linhas dele (dados,
# modelos, auc, auc_boot, resumo_auc), em vez de duplicá-lo; os stopifnot quebram
# se as âncoras mudarem. Se 05 ganhar mais dependentes, mover auc_boot para 00_util.R.
src <- readLines("scripts/05_modelo.R")
l0 <- grep("^suppressPackageStartupMessages", src)[1]
lB <- grep("^B   <- ", src); lF <- grep("^F_BASE <-", src)
stopifnot(length(lB) == 1, length(lF) == 1, grepl("^IDX <- ", src[lB + 1]))
eval(parse(text = src[l0:(lB - 1)]))          # dados, modelos e auc()
eval(parse(text = src[(lB + 2):(lF - 1)]))    # auc_boot() e resumo_auc()
F_BASE <- evadido_1ano ~ curso7 + coorte

roda <- function(seed, B) {
  set.seed(seed)
  IDX <<- replicate(B, sample(nrow(cc), replace = TRUE), simplify = FALSE)
  a_base <- auc_boot(F_BASE); a_full <- auc_boot(F_PR)
  rb <- resumo_auc(a_base, "base"); rf <- resumo_auc(a_full, "completo", a_base)
  data.frame(B = B, semente = seed,
             base_inf = rb$IC95_inf, base_sup = rb$IC95_sup,
             completo_inf = rf$IC95_inf, completo_sup = rf$IC95_sup,
             delta_inf = rf$delta_inf, delta_sup = rf$delta_sup)
}
res <- bind_rows(lapply(c(500, 5000), function(B) bind_rows(lapply(1:10, roda, B = B))))
tab <- res |> tidyr::pivot_longer(-c(B, semente), names_to = "limite") |>
  group_by(B, limite) |>
  summarise(sementes = n(), minimo = min(value), maximo = max(value), dp = sd(value), .groups = "drop")
out(tab, "art_auc_monte_carlo.csv"); print(as.data.frame(tab), digits = 3)
