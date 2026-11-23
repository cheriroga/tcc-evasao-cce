# ==============================================================================
# 05. MODELO
# ------------------------------------------------------------------------------
# Análise PRINCIPAL (especificação sem escolhas guiadas pelos dados):
#   Poisson modificado (RR, erro-padrão HC3): renda em spline natural (3 gl) +
#   nota total do SISU (por 100 pontos) + origem (modalidade × escola, 6 cat.;
#   ref. AC escola privada; a priori, AC escola pública) + sexo + cor/raça + curso (7 cat.) + coorte.
#   Testes de Wald globais; curva de RR da renda vs. mediana.
# Análise COMPLEMENTAR (geradora de hipóteses): notas por área (MT, CN) e renda
#   em faixas (≤ 1, 1–2, > 2 SM).
# Discriminação: AUC aparente, corrigida por otimismo, IC 95% e ΔAUC vs. linha
#   de base (bootstrap; mesmas reamostras para todos os modelos).
# Sensibilidades do modelo principal: sem 2020; sem 2024; sem ambas; janela de
#   2 anos; + escolaridade da mãe e idade; evasão ou trancamento no 1º ano;
#   renda per capita ≤ 1,5 SM; renda linear. Renda em coortes regulares vs. atípicas.
# Também: log-binomial, ajuste, imputação múltipla, formas da renda, matrículas
#   da mesma pessoa e verificações da nota total (apêndices do TCC).
# Saídas: saidas/art_*.csv
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(splines)
  library(sandwich); library(lmtest)
})
source("scripts/00_util.R")
set.seed(20261107)
VC <- function(m) vcovHC(m, type = "HC3")

d <- readRDS("dados/gerados/base_modelo_evasao_1ano.rds") |> prep_artigo() |> filter(coorte %in% ANOS_ANALISE) |> droplevels()
tranc <- read_delim(file.path(DIR_DADOS, "UFES", "alunos_x_situacao_curso.csv"), delim = "\t",
                    quote = "", show_col_types = FALSE, locale = locale(encoding = "latin1"),
                    col_types = cols(.default = "c")) |>
  filter(SITUACAO == "Trancamento de Curso") |>
  transmute(chave = paste(norm_matr(MATR_ALUNO), ANO)) |> distinct()
d <- d |> mutate(trancou_1ano = paste(matricula_norm, `ANO INGRESSO`) %in% tranc$chave,
                 evadido_ou_tranc = as.integer(evadido_1ano == 1 | trancou_1ano))

RENDA  <- "ns(renda_sm, df = 3)"
OUTRAS <- c("origem", "sexo", "cor_raca", "curso7", "coorte")
f_mod  <- function(enem = "enem_100", renda = RENDA, outras = OUTRAS)
  reformulate(c(renda, enem, outras), "evadido_1ano")
F_PR   <- f_mod()
cc     <- d |> select(evadido_1ano, evadido_ou_tranc, renda_sm, renda3m, renda_pc15, renda_pc, enem_100, mt_100, cn_100,
                      esc_mae, idade3, reserva, evadido_1sem, n_registros_1osem, all_of(OUTRAS)) |>
  filter(if_all(c(evadido_1ano, renda_sm, enem_100, mt_100, cn_100, all_of(OUTRAS)), ~ !is.na(.x)))

ajusta <- function(f, data) {
  m <- glm(f, data = data, family = poisson(link = "log"))
  list(model = m, ct = coeftest(m, vcov = VC(m)), data = data)
}
wald <- function(obj, termo) {                 # teste global de um termo (HC3)
  red <- update(obj$model, as.formula(paste(". ~ . -", termo)), data = obj$data)
  waldtest(red, obj$model, vcov = VC(obj$model), test = "Chisq")[2, "Pr(>Chisq)"]
}
tidy_rr <- function(obj, rotulo) {
  ct <- obj$ct; z <- qnorm(0.975)
  data.frame(modelo = rotulo, termo = rownames(ct), RR = exp(ct[, 1]),
             IC95_inf = exp(ct[, 1] - z * ct[, 2]), IC95_sup = exp(ct[, 1] + z * ct[, 2]),
             p = ct[, 4], n = nobs(obj$model), eventos = sum(obj$model$y), row.names = NULL) |>
    filter(termo != "(Intercept)")
}
rr_txt <- function(r, t) if (!t %in% r$termo) NA_character_ else
  with(r[r$termo == t, ], sprintf("%.2f (%.2f–%.2f)", RR, IC95_inf, IC95_sup))

# ---- (1) Modelo principal ----------------------------------------------------
mp <- ajusta(F_PR, cc)
stopifnot(max(fitted(mp$model)) < 1)
tab_mp <- tidy_rr(mp, "principal") |> filter(!grepl("renda_sm", termo))
tab_glob <- data.frame(
  termo = c("renda (spline, 3 gl)", "origem (6 categorias)", "curso (7 categorias)", "coorte",
            "cor/raça (4 categorias)"),
  gl    = c(3, 5, 6, nlevels(cc$coorte) - 1, nlevels(cc$cor_raca) - 1),
  p     = sapply(c(RENDA, "origem", "curso7", "coorte", "cor_raca"), \(t) wald(mp, t)))

# Contrastes diretos dos cotistas de escola federal com as demais categorias fora da referência
# (HC3), em vez de compará-los pelos contrastes com a AC privada (Gelman e Stern 2006)
b_mp <- coef(mp$model); V_mp <- VC(mp$model); FED <- "origemCotista, escola federal"
tab_fed <- do.call(rbind, lapply(setdiff(grep("^origem", names(b_mp), value = TRUE), FED), \(o) {
  L <- setNames(numeric(length(b_mp)), names(b_mp)); L[c(FED, o)] <- c(1, -1)
  e <- sum(L * b_mp); se <- sqrt(drop(L %*% V_mp %*% L)); z <- qnorm(0.975)
  data.frame(contraste = paste("Cotista, escola federal vs.", sub("^origem", "", o)), RR = exp(e),
             IC95_inf = exp(e - z * se), IC95_sup = exp(e + z * se), p = 2 * pnorm(-abs(e / se)))
}))

# GVIF de cada termo (Fox e Monette, 1992): det(R11) det(R22) / det(R), R = correlação
# das estimativas sem o intercepto; GVIF^(1/(2 gl)) é comparável entre termos com gl
# diferentes. Variância do modelo (definição usual); HC3 só para comparação.
gvif <- function(m, V) {
  R <- cov2cor(V[-1, -1]); asg <- attr(model.matrix(m), "assign")[-1]
  sapply(seq_along(attr(terms(m), "term.labels")), function(j) {
    i <- asg == j; det(R[i, i, drop = FALSE]) * det(R[!i, !i, drop = FALSE]) / det(R) })
}
tab_gvif <- data.frame(termo = attr(terms(mp$model), "term.labels"),
                       gl = as.vector(table(attr(model.matrix(mp$model), "assign")[-1])),
                       GVIF = gvif(mp$model, vcov(mp$model)), GVIF_HC3 = gvif(mp$model, VC(mp$model))) |>
  mutate(GVIF_adj = GVIF^(1 / (2 * gl)), GVIF_HC3_adj = GVIF_HC3^(1 / (2 * gl)))

# Curva de RR da renda vs. mediana (modelo principal), demais covariáveis fixas
ref  <- median(cc$renda_sm)
grid <- seq(0, 20, by = 0.25)
nd   <- cc[rep(1, length(grid) + 1), ]; nd$renda_sm <- c(ref, grid)
X    <- model.matrix(delete.response(terms(mp$model)), nd)
X    <- sweep(X[-1, , drop = FALSE], 2, X[1, ])
curva_rr <- function(m) {   # serve também ao log-binomial (mesma fórmula e mesmos dados)
  lp <- drop(X %*% coef(m)); se <- sqrt(rowSums((X %*% VC(m)) * X))
  data.frame(RR = exp(lp), IC95_inf = exp(lp - 1.96 * se), IC95_sup = exp(lp + 1.96 * se))
}
tab_curva <- data.frame(renda_sm = grid, ref_sm = ref, curva_rr(mp$model))

# ---- (1b) Análise complementar (geradora de hipóteses) ----------------------
# Renda em faixas no lugar do spline (nota total mantida); notas por área no
# lugar da nota total, uma de cada vez (MT e CN juntas não são interpretadas).
mc_r  <- ajusta(f_mod(renda = "renda3m"), cc)
mc_cn <- ajusta(f_mod(enem = "cn_100"), cc)
mc_mt <- ajusta(f_mod(enem = "mt_100"), cc)
tab_comp <- bind_rows(
  tidy_rr(mc_r, "renda em faixas") |> filter(grepl("renda3m", termo)) |>
    mutate(p_global = wald(mc_r, "renda3m")),
  tidy_rr(mc_cn, "CN no lugar da nota total") |> filter(termo == "cn_100"),
  tidy_rr(mc_mt, "MT no lugar da nota total") |> filter(termo == "mt_100"))
r_mtcn  <- cor(cc$mt_100, cc$cn_100)
vif_mtcn <- 1 / (1 - r_mtcn^2)

# Renda em coortes regulares (2021–2023) vs. atípicas (2020, 2024)
cc <- cc |> mutate(atipica = coorte %in% c("2020", "2024"))
f_int <- function(r) update(f_mod(renda = r), as.formula(paste(". ~ . +", r, ": atipica")))
p_int_faixas <- wald(ajusta(f_int("renda3m"), cc), "renda3m:atipica")
p_int_spline <- wald(ajusta(f_int(RENDA), cc), paste(RENDA, ": atipica"))
estr <- function(a, rot) {
  x <- filter(cc, atipica == a) |> droplevels(); o <- ajusta(f_mod(renda = "renda3m"), x)
  tx <- x |> group_by(renda3m) |> summarise(taxa = 100 * mean(evadido_1ano), .groups = "drop")
  r <- tidy_rr(o, rot) |> filter(termo == "renda3m≤1 SM")
  data.frame(coortes = rot, n = nobs(o$model), eventos = sum(o$model$y),
             taxa_ate1 = tx$taxa[tx$renda3m == "≤1 SM"], taxa_acima2 = tx$taxa[tx$renda3m == ">2 SM"],
             RR = r$RR, IC95_inf = r$IC95_inf, IC95_sup = r$IC95_sup)
}
tab_renda_coortes <- bind_rows(estr(FALSE, "regulares (2019, 2021–2023)"), estr(TRUE, "atípicas (2020 e 2024)")) |>
  mutate(p_int_faixas = p_int_faixas, p_int_spline = p_int_spline)

# ---- (2) Discriminação: mesmas reamostras para todos os modelos -------------
auc <- function(p, y) {                 # Mann–Whitney (empates = 1/2)
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# B = 5.000: o menor B testado (500 a 10.000) em que o desvio-padrão dos limites dos
# intervalos entre sementes ficou em até 0,001 (com 500 chegava a 0,0026).
B   <- 5000
IDX <- replicate(B, sample(nrow(cc), replace = TRUE), simplify = FALSE)
# Por reamostra i: (a) otimismo = AUC do modelo reajustado em i, avaliado em i,
# menos a do mesmo modelo avaliado na amostra original (Harrell); (b) variação
# amostral = AUC dos riscos preditos do modelo original recalculada em i.
# IC da AUC corrigida = percentis de (b) deslocados pelo otimismo médio; o ΔAUC
# usa as mesmas reamostras para os dois modelos (diferença pareada).
auc_boot <- function(f, dat = cc, idx = IDX) {
  fit <- glm(f, data = dat, family = poisson); p0 <- fitted(fit); y <- dat$evadido_1ano
  bo  <- do.call(rbind, parallel::mclapply(idx, mc.cores = max(1, parallel::detectCores() - 1), function(i) {
    bs <- dat[i, ]; fb <- suppressWarnings(glm(f, data = bs, family = poisson))
    c(opt  = auc(fitted(fb), bs$evadido_1ano) -
             auc(predict(fb, newdata = dat, type = "response"), y),
      samp = auc(p0[i], y[i]))
  }))
  list(ap = auc(p0, y), samp = bo[, "samp"], opt = mean(bo[, "opt"]))
}
resumo_auc <- function(a, rotulo, base = NULL) {
  ic  <- quantile(a$samp, c(.025, .975)) - a$opt
  out <- data.frame(modelo = rotulo, aparente = a$ap, otimismo = a$opt,
                    corrigido = a$ap - a$opt, IC95_inf = ic[[1]], IC95_sup = ic[[2]],
                    delta = NA_real_, delta_inf = NA_real_, delta_sup = NA_real_)
  if (!is.null(base)) {
    icd <- quantile(a$samp - base$samp, c(.025, .975)) - (a$opt - base$opt)
    out$delta <- (a$ap - a$opt) - (base$ap - base$opt)
    out$delta_inf <- icd[[1]]; out$delta_sup <- icd[[2]]
  }
  out
}
F_BASE <- evadido_1ano ~ curso7 + coorte
a_base <- auc_boot(F_BASE)
blocos <- list(
  "+ nota total do SISU"      = update(F_BASE, . ~ . + enem_100),
  "+ renda (spline)"          = update(F_BASE, . ~ . + ns(renda_sm, df = 3)),
  "+ origem"                  = update(F_BASE, . ~ . + origem),
  "+ sexo e cor/raça"         = update(F_BASE, . ~ . + sexo + cor_raca),
  "modelo principal completo" = F_PR)
a_blocos <- lapply(blocos, auc_boot)
tab_auc <- bind_rows(resumo_auc(a_base, "curso + coorte (linha de base)"),
                     bind_rows(Map(resumo_auc, a_blocos, names(blocos), list(a_base))))

# Especificações do ENEM no modelo principal (complementar; mesmas reamostras)
specs <- list("total SISU" = "enem_100", "MT + CN" = c("mt_100", "cn_100"),
              "só CN" = "cn_100", "só MT" = "mt_100", "sem ENEM" = character(0))
tab_enem <- bind_rows(lapply(names(specs), function(s) {
  f <- f_mod(enem = specs[[s]]); o <- ajusta(f, cc); r <- tidy_rr(o, s)
  a <- if (s == "total SISU") a_blocos[["modelo principal completo"]] else auc_boot(f)
  data.frame(especificacao = s,
             RR_total = rr_txt(r, "enem_100"), RR_MT = rr_txt(r, "mt_100"),
             RR_CN = rr_txt(r, "cn_100"), p_renda_global = wald(o, RENDA),
             AIC = AIC(o$model), AUC_corrigido = a$ap - a$opt)
}))

# ---- (3) Sensibilidades do modelo principal ---------------------------------
sens_row <- function(rotulo, obj, termo_renda = RENDA, nota = NA) {
  r <- tidy_rr(obj, rotulo)
  data.frame(analise = rotulo, n = nobs(obj$model), eventos = sum(obj$model$y),
             p_renda_global = wald(obj, termo_renda), RR_enem = rr_txt(r, "enem_100"),
             p_origem = wald(obj, "origem"), nota = nota)
}
sub_c <- function(excl) filter(cc, !coorte %in% excl) |> droplevels()

ev <- read_csv2(file.path(DIR_DADOS, "EVADIDOS", "alunos_evadidos_2015_2025.csv"),
                show_col_types = FALSE, locale = locale(encoding = "latin1")) |>
  mutate(matricula_norm = norm_matr(MATR_ALUNO),
         nao_consta = str_detect(str_to_lower(FORMA_EVASAO), "formado|adapta..o curricular"))
ev2 <- d |> select(matricula_norm, ano = `ANO INGRESSO`) |>
  left_join(ev |> select(matricula_norm, ANO_EVASAO, nao_consta),
            by = "matricula_norm", relationship = "many-to-many") |>
  group_by(matricula_norm) |>
  summarise(evadido_2anos = as.integer(any(!is.na(ANO_EVASAO) & !nao_consta &
                                           ANO_EVASAO >= ano & ANO_EVASAO <= ano + 1)),
            .groups = "drop")
# janela de 2 anos só com 2019–2023: mesmas matrículas de "sem coorte 2024", muda só o horizonte
c2a <- d |> left_join(ev2, by = "matricula_norm") |> filter(coorte != "2024") |> droplevels() |>
  mutate(evadido_1ano = evadido_2anos) |>      # reaproveita as fórmulas
  select(evadido_1ano, renda_sm, enem_100, all_of(OUTRAS)) |> na.omit()
cadj  <- cc |> filter(!is.na(esc_mae), !is.na(idade3))
m_adj <- ajusta(update(F_PR, . ~ . + esc_mae + idade3), cadj)
m_pc  <- ajusta(f_mod(renda = "renda_pc15"), cc)
m_ln  <- ajusta(f_mod(renda = "renda_sm"), cc)
# Origem (a priori) + indicador de reserva com critério de renda
cc    <- mutate(cc, reserva_renda = factor(reserva == "Reserva com critério de renda", c(FALSE, TRUE), c("Não", "Sim")))
m_crit <- ajusta(f_mod(outras = c(OUTRAS, "reserva_renda")), cc)

tab_sens <- bind_rows(
  sens_row("principal", mp),
  sens_row("sem coorte 2020", ajusta(F_PR, sub_c("2020"))),
  sens_row("sem coorte 2024", ajusta(F_PR, sub_c("2024"))),
  sens_row("sem coortes 2020 e 2024", ajusta(F_PR, sub_c(c("2020", "2024")))),
  sens_row("janela de 2 anos (coortes 2019–2023)", ajusta(F_PR, c2a)),
  sens_row("+ escolaridade da mãe e idade", m_adj),
  # sem a nota: a nota pode estar no caminho entre renda/origem e evasão
  sens_row("sem a nota total do SISU", ajusta(f_mod(enem = character(0)), cc)),
  sens_row("evasão ou trancamento no 1º ano", ajusta(update(F_PR, evadido_ou_tranc ~ .), cc)),
  # sem as saídas do 1º sem. sem nenhum registro no histórico, exceto em 2020 (Earte:
  # disciplinas canceladas sem nota); retiradas da base
  sens_row("sem as saídas do 1º sem. sem registro no histórico (exceto 2020)",
           ajusta(F_PR, filter(cc, !(evadido_1sem == 1 & n_registros_1osem == 0 & coorte != "2020")))),
  sens_row("renda per capita ≤ 1,5 SM (critério das cotas até 2023)", m_pc, "renda_pc15",
           nota = paste("RR =", rr_txt(tidy_rr(m_pc, ""), "renda_pc15≤ 1,5 SM"))),
  sens_row("renda per capita em spline", ajusta(f_mod(renda = "ns(renda_pc, df = 3)"), cc), "ns(renda_pc, df = 3)"),
  sens_row("renda linear", m_ln, "renda_sm",
           nota = paste("RR por SM =", rr_txt(tidy_rr(m_ln, ""), "renda_sm"))),
  sens_row("+ critério de renda da reserva", m_crit,
           nota = paste("RR reserva com critério de renda =", rr_txt(tidy_rr(m_crit, ""), "reserva_rendaSim"))))
# Variante com mãe e idade ao lado do principal nas mesmas matrículas: separa a mudança
# da amostra da do ajuste (apêndice do TCC)
m_adj0 <- ajusta(F_PR, cadj)
glob_adj <- function(obj, rotulo, termos) data.frame(
  modelo = rotulo, termo = paste("global:", termos), p = sapply(termos, \(t) wald(obj, t)), row.names = NULL)
madj <- bind_rows(
  tidy_rr(m_adj0, "principal nas mesmas matrículas"),
  glob_adj(m_adj0, "principal nas mesmas matrículas", c(RENDA, "origem")),
  tidy_rr(m_adj, "+ mãe e idade"),
  glob_adj(m_adj, "+ mãe e idade", c(RENDA, "origem", "esc_mae", "idade3"))) |>
  filter(!grepl("renda_sm", termo) | grepl("^global", termo))
# testes globais das duas covariáveis da sensibilidade (categorias lidas pelo teste global)
tab_glob <- bind_rows(tab_glob, data.frame(
  termo = c("escolaridade da mãe (sensibilidade + mãe e idade)", "faixa etária (sensibilidade + mãe e idade)"),
  gl    = c(nlevels(cadj$esc_mae), nlevels(cadj$idade3)) - 1,
  p     = sapply(c("esc_mae", "idade3"), \(t) wald(m_adj, t))))

out(tab_mp, "art_modelo_principal.csv"); out(tab_glob, "art_testes_globais.csv"); out(tab_gvif, "art_gvif.csv")
out(tab_fed, "art_origem_contrastes_federal.csv")
out(tab_curva, "art_renda_curva.csv"); out(data.frame(renda_sm = cc$renda_sm), "art_renda_obs.csv")
out(tab_comp, "art_complementar.csv"); out(tab_enem, "art_enem_especificacoes.csv")
w_conj <- waldtest(glm(F_BASE, data = cc, family = poisson), mp$model, vcov = VC(mp$model), test = "Chisq")
out(data.frame(teste = "modelo completo vs. curso + coorte (Wald conjunto, HC3)",
               gl = w_conj$Df[2], chisq = w_conj$Chisq[2], p = w_conj[2, "Pr(>Chisq)"]), "art_wald_conjunto.csv")
out(tab_auc, "art_auc.csv"); out(tab_sens, "art_sensibilidade.csv")
out(madj, "art_sens_covariaveis_extra.csv")
# contrastes da origem na janela de 2 anos (ref. AC escola privada)
out(tidy_rr(ajusta(F_PR, c2a), "janela de 2 anos (coortes 2019–2023)") |> filter(grepl("^origem", termo)),
    "art_sens_janela_origem.csv")
out(data.frame(r_mt_cn = r_mtcn, vif = vif_mtcn), "art_cor_mt_cn.csv")
out(tab_renda_coortes, "art_renda_coortes.csv")

# ---- (5) Matrículas da mesma pessoa e diagnóstico ----------------------------
# 20 estudantes têm duas matrículas (identificados pelo nome). Variância agrupada
# por pessoa e bootstrap por pessoa; retirada das 10 observações mais influentes.
set.seed(20261110)
pessoa <- d |> filter(if_all(c(evadido_1ano, renda_sm, enem_100, mt_100, cn_100, all_of(OUTRAS)), ~ !is.na(.x))) |>
  pull(NOME) |> iconv(to = "ASCII//TRANSLIT") |> toupper() |> str_squish()
stopifnot(length(pessoa) == nrow(cc))
wald_v <- function(m, termo, V, dat = cc) {
  red <- update(m, as.formula(paste(". ~ . -", termo)), data = dat)
  waldtest(red, m, vcov = V, test = "Chisq")[2, "Pr(>Chisq)"]
}
V_cl <- vcovCL(mp$model, cluster = pessoa, type = "HC1")
rows <- split(seq_len(nrow(cc)), pessoa); ids <- names(rows); y <- cc$evadido_1ano
fit0 <- glm(F_BASE, data = cc, family = poisson)
bo <- t(replicate(500, {            # verificação: fica com 500 reamostras
  i <- unlist(rows[sample(ids, replace = TRUE)], use.names = FALSE); bs <- cc[i, ]
  o <- sapply(list(F_BASE, F_PR), function(f) {
    fb <- suppressWarnings(glm(f, data = bs, family = poisson))
    auc(fitted(fb), bs$evadido_1ano) - auc(predict(fb, newdata = cc, type = "response"), y)
  })
  c(o0 = o[1], o1 = o[2], s0 = auc(fitted(fit0)[i], y[i]), s1 = auc(fitted(mp$model)[i], y[i]))
}))
d_pes <- (auc(fitted(mp$model), y) - mean(bo[, "o1"])) - (auc(fitted(fit0), y) - mean(bo[, "o0"]))
ic_pes <- quantile(bo[, "s1"] - bo[, "s0"], c(.025, .975)) - (mean(bo[, "o1"]) - mean(bo[, "o0"]))
cd <- cooks.distance(mp$model); top <- order(cd, decreasing = TRUE)[1:10]
m_inf <- glm(F_PR, data = cc[-top, ], family = poisson(link = "log"))
tab_pessoa <- data.frame(
  pessoas_com_2_matriculas = sum(lengths(rows) > 1), matriculas_repetidas = sum(lengths(rows)[lengths(rows) > 1]),
  p_renda_HC3 = wald(mp, RENDA), p_renda_pessoa = wald_v(mp$model, RENDA, V_cl),
  p_origem_HC3 = wald(mp, "origem"), p_origem_pessoa = wald_v(mp$model, "origem", V_cl),
  razao_EP_min = min(sqrt(diag(V_cl)) / sqrt(diag(VC(mp$model)))),
  razao_EP_max = max(sqrt(diag(V_cl)) / sqrt(diag(VC(mp$model)))),
  dAUC_pessoa = d_pes, dAUC_pessoa_inf = ic_pes[[1]], dAUC_pessoa_sup = ic_pes[[2]])
tab_diag <- data.frame(convergiu = mp$model$converged, iteracoes = mp$model$iter,
                       cook_max = max(cd), p_renda_sem_10_influentes = wald_v(m_inf, RENDA, VC(m_inf), cc[-top, ]))
out(tab_pessoa, "art_sens_pessoa.csv"); out(tab_diag, "art_diagnostico.csv")
cat("\n== Matrículas da mesma pessoa\n"); print(tab_pessoa); print(tab_diag)

options(width = 220, digits = 3)
cat("\n== Modelo principal (n =", nrow(cc), "; eventos =", sum(cc$evadido_1ano), ")\n")
print(tab_mp[, c("termo", "RR", "IC95_inf", "IC95_sup", "p")]); print(tab_glob)
cat("\n== Curva da renda (ref =", ref, "SM)\n")
print(tab_curva[tab_curva$renda_sm %in% c(0, 0.5, 1, 1.5, 2, 3, 5, 10, 20), ])
cat("\n== Complementar (cor MT-CN =", round(r_mtcn, 3), "; VIF =", round(vif_mtcn, 2), ")\n")
print(tab_comp[, c("modelo", "termo", "RR", "IC95_inf", "IC95_sup", "p", "p_global")])
cat("\n== Renda por tipo de coorte\n"); print(tab_renda_coortes)
cat("\n== Especificações do ENEM\n"); print(tab_enem)
cat("\n== AUC\n"); print(tab_auc)
cat("\n== Sensibilidades\n"); print(tab_sens)
print(madj[, c("termo", "RR", "IC95_inf", "IC95_sup")])

# ---- (6) Log-binomial, ajuste e imputação múltipla (crítica de 11/11/2026) ----
# Fica no fim para não alterar a sequência aleatória das seções anteriores.
# (a) Log-binomial: mesma especificação, estimação por máxima verossimilhança
#     (início nos coeficientes do Poisson); Wald global com HC3, como no principal.
lb_fit <- function(f, data, start) glm(f, data = data, family = binomial(link = "log"),
                                      start = start, control = glm.control(maxit = 200))
lb <- lb_fit(F_PR, cc, coef(mp$model))
stopifnot(lb$converged)
wald_lb <- function(termo) {
  fr <- update(F_PR, as.formula(paste(". ~ . -", termo)))
  red <- lb_fit(fr, cc, coef(glm(fr, data = cc, family = poisson(link = "log"))))
  waldtest(red, lb, vcov = VC(lb), test = "Chisq")[2, "Pr(>Chisq)"]
}
lb_obj <- list(model = lb, ct = coeftest(lb, vcov = VC(lb)), data = cc)
r_lb <- tidy_rr(lb_obj, "log-binomial")
tab_lb <- tidy_rr(mp, "Poisson modificado") |> filter(!grepl("renda_sm", termo)) |>
  select(termo, RR_poisson = RR, inf_poisson = IC95_inf, sup_poisson = IC95_sup) |>
  left_join(r_lb |> select(termo, RR_logbin = RR, inf_logbin = IC95_inf, sup_logbin = IC95_sup), by = "termo")
# Renda: RR em relação à mediana em três pontos da curva, nos dois modelos
pts <- grid %in% c(0, 1, 5); cp <- curva_rr(mp$model)[pts, ]; cl <- curva_rr(lb)[pts, ]
tab_lb <- bind_rows(
  data.frame(termo = paste0("renda_sm=", grid[pts]),
             RR_poisson = cp$RR, inf_poisson = cp$IC95_inf, sup_poisson = cp$IC95_sup,
             RR_logbin = cl$RR, inf_logbin = cl$IC95_inf, sup_logbin = cl$IC95_sup), tab_lb)
# (b) Ajuste dos dois modelos (mesma estrutura de média; difere a estimação)
ajuste <- function(m, nome) {
  p <- fitted(m); y <- m$y; k <- length(coef(m)); n <- length(y)
  g <- cut(p, quantile(p, 0:10 / 10), include.lowest = TRUE)
  O <- tapply(y, g, sum); E <- tapply(p, g, sum); N <- tapply(y, g, length)
  hl <- sum((O - E)^2 / (E * (1 - E / N)))
  b <- cut(p, quantile(p, 0:20 / 20), include.lowest = TRUE)
  rb <- tapply(y - p, b, mean); seb <- 2 * sqrt(tapply(p * (1 - p), b, mean) / tapply(y, b, length))
  u <- ifelse(y == 1, runif(n, 1 - p, 1), runif(n, 0, 1 - p))    # resíduo quantílico aleatorizado
  data.frame(modelo = nome, logLik_bernoulli = sum(dbinom(y, 1, p, log = TRUE)), brier = mean((y - p)^2),
             HL = hl, HL_p = pchisq(hl, 8, lower.tail = FALSE),
             cal_intercepto = coef(glm(y ~ offset(qlogis(p)), family = binomial))[[1]],
             cal_inclinacao = coef(glm(y ~ qlogis(p), family = binomial))[[2]],
             dispersao_pearson = sum((y - p)^2 / (p * (1 - p))) / (n - k),
             KS_quantilico_p = ks.test(qnorm(u), "pnorm")$p.value,
             binados_fora_2EP = sum(abs(rb) > seb), grupos_binados = nlevels(b),
             cook_max = max(cooks.distance(m)), risco_max = max(p))
}
set.seed(20261111)
tab_ajuste <- bind_rows(ajuste(mp$model, "Poisson modificado"), ajuste(lb, "log-binomial")) |>
  mutate(cor_riscos = cor(fitted(mp$model), fitted(lb)))
# (b2) Ligação logit com padronização marginal (verificação da ligação, 18/11): mesma
#     especificação; RR da renda em 0, 1 e 5 SM vs. mediana pelo risco médio com todas as
#     matrículas naquele valor dividido pelo risco médio com todas na mediana, e RR da nota
#     pelo risco médio com 100 pontos a mais dividido pelo observado (demais covariáveis como
#     observadas). IC por bootstrap percentil (1.000 reamostras, modelo e nós reajustados em
#     cada uma); testes globais com HC3. Semente própria; a seção (c) usa a semente do mice
#     e a (f) refaz a sua, de modo que as saídas seguintes não mudam.
lg <- glm(F_PR, data = cc, family = binomial); y_lg <- lg$y
rr_pad <- function(m, dd, v) mean(predict(m, mutate(dd, renda_sm = v), type = "response")) /
  mean(predict(m, mutate(dd, renda_sm = ref), type = "response"))
rr_nota <- function(m, dd) mean(predict(m, mutate(dd, enem_100 = enem_100 + 1), type = "response")) /
  mean(predict(m, dd, type = "response"))
est_lg <- function(m, dd) c(sapply(c(0, 1, 5), \(v) rr_pad(m, dd, v)), rr_nota(m, dd))
set.seed(20261119)
bt_lg <- replicate(1000, { i <- sample(nrow(cc), replace = TRUE)
  est_lg(glm(F_PR, data = cc[i, ], family = binomial), cc[i, ]) })
e_lg <- est_lg(lg, cc); ic_lg <- apply(bt_lg, 1, quantile, c(0.025, 0.975))
lg_obj <- list(model = lg, data = cc)
eta_lg <- predict(lg)   # teste da ligação: η̂² acrescentado ao modelo completo (Pregibon, 1980, p. 23)
lk_lg <- glm(update(F_PR, . ~ . + I(eta_lg^2)), data = mutate(cc, eta_lg = eta_lg), family = binomial)
tab_logit <- data.frame(termo = c(paste0("renda_sm=", c(0, 1, 5)), "enem_100"), ref_sm = ref,
  RR_logit = e_lg, IC95_inf = ic_lg[1, ], IC95_sup = ic_lg[2, ], RR_poisson = c(cp$RR, exp(coef(mp$model)[["enem_100"]])),
  p_renda_logit = wald(lg_obj, RENDA), p_link_logit = coeftest(lk_lg, vcov = VC(lk_lg))["I(eta_lg^2)", 4],
  logLik_logit = sum(dbinom(y_lg, 1, fitted(lg), log = TRUE)),
  logLik_poisson = sum(dbinom(y_lg, 1, fitted(mp$model), log = TRUE)), B = ncol(bt_lg))
# (c) Imputação múltipla das matrículas sem renda:
#     pmm para a renda (e, se houver ausentes, regressão logística/politômica para
#     sexo e cor/raça); o desfecho entra como preditor. Rubin com HC3; testes globais por D1 com gl de
#     Li, Raghunathan e Rubin (1991).
suppressPackageStartupMessages(library(mice))
M_IMP <- 20
di <- d |> filter(if_all(c(evadido_1ano, enem_100, origem, curso7, coorte), ~ !is.na(.x))) |>
  select(evadido_1ano, renda_sm, enem_100, all_of(OUTRAS))
imp <- mice(di, m = M_IMP, maxit = 20, printFlag = FALSE, seed = 20261111)
fits <- lapply(seq_len(M_IMP), \(k) glm(F_PR, data = complete(imp, k), family = poisson(link = "log")))
pool_d1 <- function(padrao) {                   # Wald conjunto (D1) para os termos que casam o padrão
  j <- grep(padrao, names(coef(fits[[1]])), fixed = TRUE)
  Q <- sapply(fits, \(m) coef(m)[j]); Q <- matrix(Q, nrow = length(j))
  U <- Reduce(`+`, lapply(fits, \(m) VC(m)[j, j, drop = FALSE])) / M_IMP
  qb <- rowMeans(Q); Bm <- if (length(j) == 1) matrix(var(Q[1, ])) else cov(t(Q)); k <- length(j)
  r <- (1 + 1 / M_IMP) * sum(diag(Bm %*% solve(U))) / k
  D1 <- drop(t(qb) %*% solve(U) %*% qb) / (k * (1 + r)); t_ <- k * (M_IMP - 1)
  v1 <- if (t_ > 4) 4 + (t_ - 4) * (1 + (1 - 2 / t_) / r)^2 else t_ * (1 + 1 / k) * (1 + r)^2 / 2
  list(p = pf(D1, k, v1, lower.tail = FALSE), est = qb, se = sqrt(diag(U + (1 + 1 / M_IMP) * Bm)))
}
pe <- pool_d1("enem_100")
tab_sens <- bind_rows(tab_sens,
  data.frame(analise = "log-binomial", n = nobs(lb), eventos = sum(lb$y),
             p_renda_global = wald_lb(RENDA), RR_enem = rr_txt(r_lb, "enem_100"),
             p_origem = wald_lb("origem"), nota = NA),
  data.frame(analise = "logística (RR por padronização marginal)", n = nobs(lg), eventos = sum(y_lg),
             p_renda_global = wald(lg_obj, RENDA), RR_enem = sprintf("%.2f (%.2f–%.2f)", e_lg[4], ic_lg[1, 4], ic_lg[2, 4]),
             p_origem = wald(lg_obj, "origem"), nota = NA),
  data.frame(analise = sprintf("imputação múltipla (m = %d)", M_IMP), n = nrow(di), eventos = sum(di$evadido_1ano),
             p_renda_global = pool_d1("ns(renda_sm")$p,
             RR_enem = sprintf("%.2f (%.2f–%.2f)", exp(pe$est), exp(pe$est - 1.96 * pe$se), exp(pe$est + 1.96 * pe$se)),
             p_origem = pool_d1("origem")$p, nota = NA))
out(tab_sens, "art_sensibilidade.csv"); out(tab_lb, "art_logbin.csv"); out(tab_ajuste, "art_ajuste.csv")
out(tab_logit, "art_logit_padronizado.csv"); print(tab_logit)
cat("\n== Log-binomial, ajuste e imputação\n"); print(tab_lb); print(tab_ajuste); print(tail(tab_sens, 2))
# (d) Formas da renda (TCC, apêndice): mesmo modelo, muda só o termo da renda.
#     Wald global HC3; log-verossimilhança de Bernoulli e AIC correspondente.
forma <- function(rot, termo, dat = cc) {
  o <- ajusta(f_mod(renda = termo), dat); p <- fitted(o$model); y <- o$model$y; k <- length(coef(o$model))
  ll <- sum(dbinom(y, 1, p, log = TRUE))
  data.frame(forma = rot, gl = k - (length(coef(mp$model)) - 3), p_global = wald(o, termo),
             logLik_bernoulli = ll, AIC_bernoulli = -2 * ll + 2 * k)
}
teto <- function(v) mutate(cc, renda_sm = ifelse(renda_sm == 20, v, renda_sm))
cc <- mutate(cc, renda_q006 = factor(renda_sm),
             renda_ord = match(renda_sm, sort(unique(renda_sm))) - 1)   # escore ordinal 0–16
tab_formas <- bind_rows(
  forma("spline, 3 gl (principal; até 1 SM = 1 SM, faixa aberta = 20 SM)", RENDA),
  forma("spline, 3 gl; faixa aberta = 25 SM", RENDA, teto(25)),
  forma("spline, 3 gl; faixa aberta = 30 SM", RENDA, teto(30)),
  forma("spline, 3 gl; faixa de até 1 SM = 0,5 SM (ponto médio)", RENDA,
        mutate(cc, renda_sm = ifelse(renda_sm == 1, 0.5, renda_sm))),
  forma("linear", "renda_sm"),
  forma("três faixas (até 1, 1 a 2, acima de 2 SM)", "renda3m"),
  forma("escore ordinal das faixas (0 a 16), linear", "renda_ord"),
  forma("escore ordinal das faixas (0 a 16), spline 3 gl", "ns(renda_ord, df = 3)"),
  forma("faixas da Q006 como categorias", "renda_q006"))
# (e) Matrículas sem renda (TCC, apêndice): casos completos, imputação e valores
#     extremos atribuídos às matrículas sem renda que têm as demais covariáveis.
ext <- function(v) {
  x <- d |> filter(if_all(c(evadido_1ano, enem_100, all_of(OUTRAS)), ~ !is.na(.x)))
  n_sem <- sum(is.na(x$renda_sm))
  o <- ajusta(F_PR, mutate(x, renda_sm = ifelse(is.na(renda_sm), v, renda_sm)))
  data.frame(tratamento = sprintf("as %d sem renda com %s SM", n_sem, format(v, decimal.mark = ",")),
             n = nobs(o$model), eventos = sum(o$model$y), p_renda_global = wald(o, RENDA))
}
tab_falt <- bind_rows(
  data.frame(tratamento = "casos completos (principal)", n = nobs(mp$model), eventos = sum(mp$model$y),
             p_renda_global = wald(mp, RENDA)),
  data.frame(tratamento = sprintf("imputação múltipla (m = %d)", M_IMP), n = nrow(di),
             eventos = sum(di$evadido_1ano), p_renda_global = pool_d1("ns(renda_sm")$p),
  ext(0), ext(1), ext(20))
# (f) Resíduos para a figura do apêndice (quantílicos aleatorizados e binados)
set.seed(20261112)
res <- bind_rows(lapply(list("Poisson modificado" = mp$model, "log-binomial" = lb), function(m) {
  p <- fitted(m); y <- m$y; n <- length(y)
  data.frame(p = p, y = y, rq = qnorm(ifelse(y == 1, runif(n, 1 - p, 1), runif(n, 0, 1 - p))))
}), .id = "modelo")
# Resíduos parciais (resíduo de trabalho + componente do termo, na escala do log do risco)
# da renda e da nota no Poisson modificado. Só agregados (nota e renda por matrícula,
# junto com o resíduo, quase identificariam a pessoa): média por grupo do x (renda: cada
# faixa da Q006; nota: 20 grupos de mesmo tamanho), com o componente do modelo no grupo.
rp <- residuals(mp$model, type = "partial"); tm <- predict(mp$model, type = "terms")
stopifnot(nrow(rp) == nrow(cc))
parcial <- function(x, termo, g, rot)
  data.frame(x = x, r = rp[, termo], f = tm[, termo], g = g) |> group_by(g) |>
    summarise(x = mean(x), r_med = mean(r), ep = sd(r) / sqrt(n()), f = mean(f), n = n(), .groups = "drop") |>
    mutate(termo = rot, fora_2EP = abs(r_med - f) > 2 * ep) |> select(-g)
res_parc <- bind_rows(
  parcial(cc$renda_sm, RENDA, cc$renda_sm, "renda"),
  parcial(cc$enem_100, "enem_100", cut(cc$enem_100, quantile(cc$enem_100, 0:20 / 20), include.lowest = TRUE), "nota"))
out(tab_formas, "art_renda_formas.csv"); out(tab_falt, "art_faltantes.csv"); out(res, "art_residuos.csv")
out(res_parc, "art_residuos_parciais.csv")
print(tab_formas); print(tab_falt)

# (g) Nota total: pesos das provas variam entre
#     os cursos; inclinação comum, linearidade e escala verificadas. Determinístico.
wt_extra <- function(f_grande) { g <- glm(f_grande, data = cc, family = poisson(link = "log"))
  waldtest(mp$model, g, vcov = VC(g), test = "Chisq")[2, c("Df", "Pr(>Chisq)")] }
w_int <- wt_extra(update(F_PR, . ~ . + enem_100:curso7))
w_ns  <- wt_extra(update(F_PR, . ~ . + ns(enem_100, df = 3)))
cz <- cc |> group_by(curso7, coorte) |> mutate(nota_z = (enem_100 - mean(enem_100)) / sd(enem_100)) |> ungroup()
mz <- glm(update(F_PR, . ~ . - enem_100 + nota_z), data = cz, family = poisson(link = "log"))
bz <- coeftest(mz, vcov = VC(mz))["nota_z", ]
tab_nota <- data.frame(
  verificacao = c("interação nota × curso (Wald)", "desvio da linearidade: spline 3 gl (Wald)",
                  "nota padronizada em curso × coorte (RR por DP)"),
  gl = c(w_int$Df[1], w_ns$Df[1], NA), p = c(w_int[[2]], w_ns[[2]], bz[[4]]),
  RR = c(NA, NA, exp(bz[[1]])), IC95_inf = c(NA, NA, exp(bz[[1]] - 1.96 * bz[[2]])),
  IC95_sup = c(NA, NA, exp(bz[[1]] + 1.96 * bz[[2]])))
out(tab_nota, "art_nota_especificacoes.csv"); print(tab_nota)

# ---- (8) Tamanho do estudo: RR detectável pelo desenho ----------------------
# Descrição do desenho, não leitura dos resultados (Hoenig e Heisey, 2001): não usa
# estimativas nem erros-padrão do modelo. Covariável binária com a prevalência da
# renda até 1 SM; risco de referência = risco observado nas demais matrículas.
# Var(log RR) ≈ (1−π1)/(n1·π1) + (1−π0)/(n0·π0); poder 80%, α = 0,05 bilateral.
n1 <- sum(cc$renda_sm <= 1); n0 <- nrow(cc) - n1; p0 <- mean(cc$evadido_1ano[cc$renda_sm > 1])
k_pod <- qnorm(0.975) + qnorm(0.80)
z_rr <- function(rr) { p1 <- rr * p0; abs(log(rr)) / sqrt((1 - p1) / (n1 * p1) + (1 - p0) / (n0 * p0)) - k_pod }
out(data.frame(n = nrow(cc), n_exposto = n1, prevalencia = n1 / nrow(cc), risco_ref = p0,
               poder = 0.80, alfa = 0.05,
               RR_min_acima = uniroot(z_rr, c(1.001, 3))$root, RR_max_abaixo = uniroot(z_rr, c(0.2, 0.999))$root),
    "art_poder.csv")

# ---- (9) Discriminação com escolaridade da mãe e faixa etária ----
# Nas 1.282 matrículas com as duas covariáveis: linha de base, principal e principal + mãe
# e idade, com as mesmas reamostras entre os três. Fica no fim para não alterar a
# sequência aleatória das seções anteriores.
set.seed(20261117)
IDX_ADJ <- replicate(B, sample(nrow(cadj), replace = TRUE), simplify = FALSE)
a0_adj <- auc_boot(F_BASE, cadj, IDX_ADJ)
tab_auc_adj <- bind_rows(
  resumo_auc(a0_adj, "curso + coorte (linha de base)"),
  resumo_auc(auc_boot(F_PR, cadj, IDX_ADJ), "modelo principal", a0_adj),
  resumo_auc(auc_boot(update(F_PR, . ~ . + esc_mae + idade3), cadj, IDX_ADJ), "+ mãe e idade", a0_adj)) |>
  mutate(n = nrow(cadj), eventos = sum(cadj$evadido_1ano))
out(tab_auc_adj, "art_auc_mae_idade.csv"); print(tab_auc_adj)

# ---- (10) Diagnósticos da média do modelo principal ----
# Teste da ligação: η̂² acrescentado ao modelo completo (termo de Tukey, 1949, apud Pregibon, 1980,
# p. 23), Wald com HC3.
# Soma de quadrados não ponderada (Hosmer et al., 1997), com p por bootstrap paramétrico
# (a aproximação normal deles é da logística): desfechos sorteados de Bernoulli(p̂), modelo
# reajustado, p bilateral pela distância à média das réplicas. Influência na renda e na nota:
# DFBETA (variação de um passo ao retirar a matrícula) do log RR de 1 SM vs. mediana e do
# coeficiente da nota, em erros-padrão HC3. Fica no fim para não alterar a sequência
# aleatória das seções anteriores.
eta <- predict(mp$model); y <- mp$model$y
lk <- glm(update(F_PR, . ~ . + I(eta^2)), data = mutate(cc, eta = eta), family = poisson(link = "log"))
uss <- function(yy, pp) sum((yy - pp)^2)
set.seed(20261118)
s_boot <- replicate(1000, {
  ys <- rbinom(length(y), 1, fitted(mp$model))
  uss(ys, fitted(glm(update(F_PR, ys ~ .), data = mutate(cc, ys = ys), family = poisson(link = "log"))))
})
s_obs <- uss(y, fitted(mp$model))
L1  <- X[grid == 1, ]; dfb <- dfbeta(mp$model)
d_r <- drop(dfb %*% L1) / sqrt(drop(L1 %*% VC(mp$model) %*% L1))
d_n <- dfb[, "enem_100"] / sqrt(VC(mp$model)["enem_100", "enem_100"])
out(data.frame(coef_eta2 = coef(lk)[["I(eta^2)"]], p_link_eta2 = coeftest(lk, vcov = VC(lk))["I(eta^2)", 4],
               uss = s_obs, uss_media_boot = mean(s_boot),
               p_uss_boot = mean(abs(s_boot - mean(s_boot)) >= abs(s_obs - mean(s_boot))), B_uss = length(s_boot),
               dfbeta_renda1sm_max = max(abs(d_r)), dfbeta_nota_max = max(abs(d_n)),
               limiar_2_raiz_n = 2 / sqrt(length(y)),
               n_renda1sm_acima = sum(abs(d_r) > 2 / sqrt(length(y))), n_nota_acima = sum(abs(d_n) > 2 / sqrt(length(y)))),
    "art_diagnostico_media.csv")
cat("\n== Diagnósticos da média\n"); print(read.csv("saidas/art_diagnostico_media.csv"))


# ---- (11) Curvas ROC aparentes ----
# Pares (1 − especificidade, sensibilidade) da linha de base e do modelo completo,
# ajustados às matrículas do modelo principal. Só os pares agregados, sem os limiares,
# que são riscos preditos de matrículas. Determinístico; no fim para não alterar a
# sequência aleatória das seções anteriores.
roc_pts <- function(p, y, rotulo) {
  cortes <- sort(unique(p), decreasing = TRUE)
  data.frame(modelo = rotulo,
             fpr = c(0, vapply(cortes, \(c) mean(p[y == 0] >= c), 0)),
             tpr = c(0, vapply(cortes, \(c) mean(p[y == 1] >= c), 0)))
}
y_cc <- cc$evadido_1ano
out(bind_rows(roc_pts(fitted(glm(F_BASE, data = cc, family = poisson)), y_cc, "curso + coorte (linha de base)"),
              roc_pts(fitted(glm(F_PR, data = cc, family = poisson)), y_cc, "modelo principal completo")),
    "art_roc.csv")

# ---- (12) Interações com o curso, exploratórias ----
# Feitas depois de ver os dados e relatadas como exploratórias. Wald com
# HC3 da interação, cada uma sobre o modelo sem ela. A renda em spline × curso dá risco predito
# acima de 1 e a origem × curso tem combinações sem evasão (Wald não estimável); por isso a renda
# entra em faixas ou como indicador de até 1 SM, e a origem, como cotista ou não. Determinístico.
cci <- cc |> mutate(ate1 = as.integer(renda_sm <= 1), cotista_i = as.integer(grepl("^Cotista", origem)))
fit_i <- function(f) glm(f, data = cci, family = poisson(link = "log"))
w_int_i <- function(rot, f0, termo) {
  g0 <- fit_i(f0); g <- fit_i(update(f0, as.formula(paste(". ~ . +", termo))))
  w <- waldtest(g0, g, vcov = VC(g), test = "Chisq")[2, ]
  data.frame(interacao = rot, gl = w$Df, chisq = w$Chisq, p = w[[4]], risco_max = max(fitted(g)),
             parametros = sum(!is.na(coef(g))) - 1, eventos = sum(cci$evadido_1ano))
}
g_sp <- fit_i(update(F_PR, . ~ . + ns(renda_sm, df = 3):curso7))
F_A1 <- f_mod(renda = "ate1")
out(rbind(w_int_i("renda em faixas × curso", f_mod(renda = "renda3m"), "renda3m:curso7"),
          w_int_i("renda até 1 SM × curso", F_A1, "ate1:curso7"),
          w_int_i("cotista × curso", F_PR, "cotista_i:curso7"),
          w_int_i("nota × curso", F_PR, "enem_100:curso7"),
          data.frame(interacao = "renda (spline) × curso", gl = 18, chisq = NA, p = NA,
                     risco_max = max(fitted(g_sp)), parametros = sum(!is.na(coef(g_sp))) - 1,
                     eventos = sum(cci$evadido_1ano))),
    "art_interacao_curso.csv")
g_a1 <- fit_i(update(F_A1, . ~ . - ate1 + ate1:curso7)); ct_a1 <- coeftest(g_a1, vcov = VC(g_a1))
k_a1 <- grep("ate1", rownames(ct_a1))
out(data.frame(curso7 = sub(":ate1$", "", sub("^curso7", "", rownames(ct_a1)[k_a1])),
               RR = exp(ct_a1[k_a1, 1]), IC95_inf = exp(ct_a1[k_a1, 1] - qnorm(0.975) * ct_a1[k_a1, 2]),
               IC95_sup = exp(ct_a1[k_a1, 1] + qnorm(0.975) * ct_a1[k_a1, 2])) |>
      left_join(cci |> group_by(curso7 = as.character(curso7)) |>
                  summarise(n_ate1 = sum(ate1), ev_ate1 = sum(evadido_1ano[ate1 == 1]),
                            n_acima = sum(1 - ate1), ev_acima = sum(evadido_1ano[ate1 == 0])), by = "curso7"),
    "art_renda_curso.csv")

# ---- (13) Kolmogorov–Smirnov dos resíduos quantílicos em 20 realizações ----
# Dunn e Smyth (1996, p. 243) recomendam olhar várias realizações dos resíduos aleatorizados;
# o valor p de uma só realização (tab_ajuste) depende do sorteio. Semente própria, no fim.
set.seed(20261123)
ks_real <- function(m, nome, R = 20) {
  p <- fitted(m); y <- m$y; n <- length(y)
  pv <- replicate(R, ks.test(qnorm(ifelse(y == 1, runif(n, 1 - p, 1), runif(n, 0, 1 - p))), "pnorm")$p.value)
  data.frame(modelo = nome, realizacoes = R, p_mediana = median(pv), p_min = min(pv), p_max = max(pv),
             n_abaixo_005 = sum(pv < 0.05))
}
out(bind_rows(ks_real(mp$model, "Poisson modificado"), ks_real(lb, "log-binomial")), "art_ks_realizacoes.csv")
