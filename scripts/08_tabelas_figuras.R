# ==============================================================================
# 08. TABELAS (LaTeX) E FIGURAS, a partir de saidas/ (scripts 04 e 05)
# ------------------------------------------------------------------------------
# Saídas: tabelas/tab_*.tex (só o tabular) e figuras/fig_*.{png,eps}
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
source("scripts/00_util.R")

le  <- function(f) read.csv(file.path(DIR_CSV, f), check.names = FALSE, stringsAsFactors = FALSE)
br  <- function(x) ifelse(is.na(x), "---", gsub("–", "--", gsub("\\.", ",", x)))
# grafia das siglas: só a inicial maiúscula
# nas de quatro ou mais letras lidas como palavra
SIGLAS <- c(UFES = "Ufes", INEP = "Inep", ENEM = "Enem", SISU = "Sisu",
            REUNI = "Reuni", UDESC = "Udesc", PROGRAD = "Prograd")
siglas <- function(x) { for (k in names(SIGLAS)) x <- gsub(paste0("\\b", k, "\\b"), SIGLAS[[k]], x, perl = TRUE); x }
tex <- function(linhas, cab, alin, arq) {
  writeLines(siglas(c(sprintf("\\begin{tabular}{%s}", alin), "\\toprule", cab, "\\midrule",
               linhas, "\\bottomrule", "\\end{tabular}")), file.path(DIR_TEX, arq))
}
linha <- function(...) paste(paste(..., sep = " & "), "\\\\")
cap   <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
ic3   <- function(e, lo, hi) paste0(neg(fmt(e, 3)), " (", neg(fmt(lo, 3)), " a ", neg(fmt(hi, 3)), ")")
grupo <- function(rot, x, k = 4) c(paste0("\\multicolumn{", k, "}{l}{\\textit{", rot, "}} \\\\"), x)
mc    <- function(x, a = "r") paste0("\\makecell[", a, "]{", x, "}")   # cabeçalho em duas linhas
neg   <- function(x) sub("^-", "$-$", x)
mil   <- function(x) format(x, big.mark = ".", trim = TRUE)   # 1.312
# Rótulo de bloco em duas partes (separadas por "\n"): a segunda vai para a linha seguinte do bloco,
# que está em branco, para que todas as linhas do corpo tenham a mesma altura
bloco2 <- function(x) {
  for (i in which(grepl("\n", x, fixed = TRUE))) {
    p <- strsplit(x[i], "\n", fixed = TRUE)[[1]]
    stopifnot(i < length(x), x[i + 1] == "")
    x[i] <- p[1]; x[i + 1] <- p[2]
  }
  x
}

# ---- Tab. 1: características e evasão ----------------------------------------
# Rótulos de linha iguais aos das tabelas dos modelos ("Física, bacharelado")
rot_niv <- function(x) sub("Amarela/Indígena/Não declarado", "Amarela/indígena/não declarada",
                           sub(" - (.)", ", \\L\\1", x, perl = TRUE), fixed = TRUE)
rot_var <- c(curso7 = "Curso", coorte = "Coorte", renda3 = "Renda familiar",
             origem = "Origem (modalidade de ingresso e escola)", sexo = "Sexo", cor_raca = "Cor/raça",
             nota4 = "Nota total do SISU (quartis)", idade3 = "Faixa etária")
ordem_niv <- c("≤1 SM", "1–2 SM", ">2 SM", "AC, escola pública", "AC, escola privada",
               "AC, sem informação de escola", "Cotista, escola estadual/municipal",
               "Cotista, escola federal", "Cotista, sem informação de escola",
               "Branca", "Parda", "Preta", "Amarela/Indígena/Não declarado",
               "Até 18 anos", "19 e 20 anos", "21 anos ou mais")
cat_t <- le("desc_artigo_categorias.csv") |>
  mutate(nivel = ifelse(is.na(nivel) | nivel == "", "Sem informação", nivel),
         nivel = dplyr::recode(nivel, "até 18" = "Até 18 anos", "19-20" = "19 e 20 anos", "21 ou mais" = "21 anos ou mais",
                               "AC, egresso sem informação" = "AC, sem informação de escola",
                               "Cotista, egresso" = "Cotista, sem informação de escola"),
         o1 = match(variavel, names(rot_var)),
         o2 = ifelse(nivel %in% ordem_niv, match(nivel, ordem_niv),
                     ifelse(nivel == "Sem informação", 99, 50))) |>
  arrange(o1, o2, nivel)
l1 <- character(0)
# o bloco Curso fica só na tabela por curso
for (v in setdiff(names(rot_var), "curso7")) {
  x <- filter(cat_t, variavel == v)
  if (length(l1)) l1 <- c(l1, "\\addlinespace")
  l1 <- c(l1, grupo(rot_var[[v]], linha(paste0("\\quad ", esc(rot_niv(x$nivel))),
                paste0(x$n, " (", fmt(x$pct_amostra, 1), ")"),
                fmt(x$taxa_1ano, 1), fmt(x$taxa_1sem, 1))))
}
tex(l1, linha("Característica", "$n$ (\\%)", mc("Evasão no\\\\1º ano (\\%)"), mc("Saída no\\\\1º sem.\\ (\\%)")),
    "lrrr", "tab_art_descritiva.tex")

# ---- Tabela por curso ---------------------------------------------------------
cu <- le("desc_artigo_curso.csv")
tex(linha(esc(rot_niv(cu$curso7)), paste0(cu$n, "/", cu$evad), fmt(cu$taxa_1ano, 1), fmt(cu$saidas_1sem_pct, 0),
          fmt(cu$renda_ate1sm_pct, 1), fmt(cu$cotista_pct, 1), fmt(cu$nota_mediana, 0)),
    linha("Curso", "$n$/eventos", mc("Evasão no\\\\1º ano (\\%)"), mc("Saídas no\\\\1º sem.\\ (\\%)"),
          mc("Renda até\\\\1 SM (\\%)"), mc("Cotistas\\\\(\\%)"), mc("Nota total\\\\mediana")),
    "lrrrrrr", "tab_tcc_curso.tex")

# ---- Apêndice: RR da renda de até 1 SM em cada curso (interação exploratória)
rcu <- le("art_renda_curso.csv")
tex(linha(esc(rot_niv(rcu$curso7)), paste0(rcu$n_ate1, "/", rcu$ev_ate1), paste0(rcu$n_acima, "/", rcu$ev_acima),
          rr_ci(rcu$RR, rcu$IC95_inf, rcu$IC95_sup)),
    linha("Curso", mc("Até 1 SM\\\\$n$/eventos"), mc("Acima de 1 SM\\\\$n$/eventos"), mc("RR até 1 SM\\\\(IC 95\\%)")),
    "lrrr", "tab_tcc_renda_curso.tex")

# Figura: RR da renda de até 1 SM em cada curso (interação exploratória)
# um painel só, ordenado pelo RR; sem o grupo "Licenciaturas e Estatística", lido nos dados
# (Estatística é bacharelado)
rcu_f <- rcu |> mutate(rot = rot_niv(curso7), ev = paste0(ev_ate1, "/", n_ate1)) |>
  arrange(desc(RR))
rcu_f$rot <- factor(rcu_f$rot, rev(rcu_f$rot))
gRC <- ggplot(rcu_f, aes(RR, rot)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_errorbar(aes(xmin = IC95_inf, xmax = IC95_sup), width = 0, orientation = "y", linewidth = 0.4, color = "grey45") +
  geom_point(size = 1.8) +
  geom_text(aes(x = 24, label = ev), hjust = 1, size = 2.9, color = "grey35", family = "Latin Modern Roman") +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4), labels = \(x) fmt(x), limits = c(0.2, 24)) +
  labs(x = "RR (IC 95%), escala log", y = NULL) +
  tema_artigo(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        axis.text.y = element_text(size = 10)) + theme(text = element_text(family = "Latin Modern Roman"))
salvar_figura(gRC, file.path(DIR_FIG, "fig_tcc_renda_curso.png"), largura = 5.35, altura = 3.1)

# ---- Apêndice: cobertura dos intervalos da AUC (simulação) -----------------------
ca <- le("art_cobertura_auc.csv")
tex(linha(esc(ifelse(grepl("^AUC", ca$quantidade), "AUC corrigida do modelo completo", "$\\Delta$AUC (completo $-$ linha de base)")),
          paste0(fmt(100 * ca$cobertura, 0), " (", fmt(100 * ca$ep_monte_carlo, 1), ")"),
          fmt(100 * ca$abaixo, 0), fmt(100 * ca$acima, 0), fmt(ca$largura_media, 3),
          fmt(ca$alvo_medio, 3), fmt(ca$estimativa_media, 3)),
    linha("Quantidade", mc("Cobertura\\\\(\\%; EP)"), mc("Alvo acima do\\\\intervalo (\\%)"), mc("Alvo abaixo do\\\\intervalo (\\%)"),
          mc("Largura\\\\média"), mc("Alvo\\\\médio"), mc("Estimativa\\\\média")),
    "lrrrrrr", "tab_tcc_cobertura_auc.tex")

# ---- Apêndice: GVIF e trancamento ---------------------------------------------
gv <- le("art_gvif.csv")
rot_gv <- c("ns(renda_sm, df = 3)" = "Renda (\\textit{spline})", enem_100 = "Nota total do Sisu", origem = "Origem",
            sexo = "Sexo", cor_raca = "Cor/raça", curso7 = "Curso", coorte = "Coorte")
tex(linha(rot_gv[gv$termo], gv$gl, fmt(gv$GVIF, 2), fmt(gv$GVIF_adj, 3), fmt(gv$GVIF_HC3, 2), fmt(gv$GVIF_HC3_adj, 3)),
    linha("Termo", "gl", "GVIF", "$\\text{GVIF}^{1/(2\\,\\text{gl})}$", mc("GVIF\\\\(HC3)"), mc("$\\text{GVIF}^{1/(2\\,\\text{gl})}$\\\\(HC3)")),
    "lrrrrr", "tab_tcc_gvif.tex")
trd <- le("desc_artigo_trancamento_depois.csv") |> arrange(desc(trancou_1ano))
tex(linha(ifelse(trd$trancou_1ano, "Trancou no 1º ano", "Não trancou"), trd$n, trd$evadiu_depois, fmt(trd$evadiu_depois_pct, 1)),
    linha("Situação no 1º ano", "$n$", mc("Saíram\\\\depois"), mc("Saíram\\\\depois (\\%)")),
    "lrrr", "tab_tcc_trancamento.tex")

# ---- Tab. 2: por coorte -------------------------------------------------------
co <- le("desc_artigo_coorte.csv")
tex(linha(co$coorte, co$n, fmt(co$evad_1ano_pct, 1), fmt(co$evad_1sem_pct, 1),
          fmt(co$evad_2sem_pct, 1), fmt(co$reprov_freq_pct, 1), fmt(co$reprov_nota_pct, 1),
          fmt(co$cotista_pct, 1), fmt(co$renda_ate1sm_pct, 1)),
    linha("Coorte", "$n$", mc("Evasão no\\\\1º ano (\\%)"), mc("Saída no\\\\1º sem.\\ (\\%)"),
          mc("Saída no\\\\2º sem.\\ (\\%)"), mc("Reprov.\\\\freq.\\ (\\%)"), mc("Reprov.\\\\nota (\\%)"),
          mc("Cotistas\\\\(\\%)"), mc("Renda\\\\$\\leq$1 SM (\\%)")),
    "lrrrrrrrr", "tab_art_coorte.tex")

# ---- Tab.: renda familiar segundo a origem e a reserva ----------------------
ro <- le("desc_artigo_renda_origem.csv") |>
  mutate(grupo = dplyr::recode(grupo, "AC, egresso sem informação" = "AC, sem informação de escola",
                               "Cotista, egresso" = "Cotista, sem informação de escola", "AC" = "Ampla concorrência"),
         o = match(grupo, c(ordem_niv, "Ampla concorrência", "Reserva com critério de renda",
                            "Reserva sem critério de renda", "Total"))) |> arrange(match(variavel, c("origem", "reserva", "total")), o)
lro <- function(x) linha(paste0("\\quad ", esc(x$grupo)), mil(x$n), fmt(x$ate1_pct, 1), fmt(x$de1a2_pct, 1),
                         fmt(x$acima2_pct, 1), fmt(x$mediana_sm, 2), fmt(x$pc15_pct, 1))
tex(c(grupo("Origem", lro(filter(ro, variavel == "origem")), 7), "\\addlinespace",
      grupo("Modalidade de ingresso", lro(filter(ro, variavel == "reserva")), 7), "\\addlinespace",
      sub("^\\\\quad ", "", lro(filter(ro, variavel == "total")))),
    linha("Grupo", "$n$", mc("$\\leq$1 SM\\\\(\\%)"), mc("1 a 2 SM\\\\(\\%)"), mc("$>$2 SM\\\\(\\%)"),
          mc("Mediana\\\\(SM)"), mc("$\\leq$1,5 SM per\\\\capita (\\%)")),
    "lrrrrrr", "tab_art_renda_origem.tex")

# ---- Tab. S: reprovação no 1º semestre por momento da saída (descritivo) -----
rp <- le("desc_artigo_reprovacao.csv")
tex(linha(bloco2(sub(" (", "\n(", blank_repeats(esc(cap(rp$coortes))), fixed = TRUE)), cap(rp$momento), rp$n, fmt(rp$reprov_freq_pct, 1),
          fmt(rp$reprov_nota_pct, 1), fmt(rp$alguma_reprov_pct, 1), fmt(rp$sem_registro_pct, 1)),
    linha("Coortes", mc("Situação ao fim\\\\do 1º ano", "l"), "$n$", mc("Reprov.\\\\freq.\\ (\\%)"),
          mc("Reprov.\\\\nota (\\%)"), mc("Alguma\\\\reprov.\\ (\\%)"), mc("Sem\\\\registro (\\%)")),
    "llrrrrr", "tab_art_reprovacao.tex")

# ---- Tab. 3: composição ingresso x permanência -------------------------------
rot_comp <- c(renda3 = "Renda familiar", cotista = "Cotista",
              ppi = "Preto, pardo\nou indígena", cor_raca = "Cor/raça")
cp <- le("desc_artigo_composicao.csv") |>
  mutate(o = match(grupo, c("≤1 SM", "1–2 SM", ">2 SM", "Sim", "Não", "Branca",
                            "Parda", "Preta", "Amarela/Indígena/Não declarado"))) |>
  arrange(match(variavel, names(rot_comp)), o)
tex(linha(bloco2(blank_repeats(rot_comp[cp$variavel])), esc(rot_niv(cp$grupo)), cp$ingresso,
          fmt(cp$pct_ingresso, 1), fmt(cp$pct_permanece, 1), fmt(cp$retencao_pct, 1)),
    linha("Variável", "Grupo", "$n$", mc("\\% no\\\\ingresso"),
          mc("\\% após o\\\\1º ano"), mc("Retenção\\\\(\\%)")),
    "llrrrr", "tab_art_composicao.tex")

# ---- Tab. 4: modelo principal -----------------------------------------------
mp <- le("art_modelo_principal.csv"); gl <- le("art_testes_globais.csv")
rr_l <- function(t, rot, ind = "\\quad ") with(mp[mp$termo == t, ],
  linha(paste0(ind, rot), rr_ci(RR, IC95_inf, IC95_sup), fmtp(p)))
pg <- function(t) fmtp(gl$p[gl$termo == t])
cv4 <- le("art_renda_curva.csv")   # RR em três pontos da curva
rr_cv <- function(v) with(cv4[cv4$renda_sm == v, ],
  linha(sprintf("\\quad %s SM", fmt(v, 0)), rr_ci(RR, IC95_inf, IC95_sup), ""))
l4 <- c(
  linha(sprintf("\\textit{Renda familiar} (\\textit{spline}, 3 gl; ref.: mediana, %s SM; Figura~\\ref{fig:renda})",
                fmt(cv4$ref_sm[1], 2)), "", paste("global:", pg("renda (spline, 3 gl)"))),
  rr_cv(0), rr_cv(1), rr_cv(5),
  "\\addlinespace",
  rr_l("enem_100", "Nota total do SISU (+100 pontos)", ""),
  "\\addlinespace",
  linha("\\textit{Origem: modalidade e escola} (ref.: AC, escola privada)", "", paste("global:", pg("origem (6 categorias)"))),
  rr_l("origemAC, escola pública", "AC, escola pública"),
  rr_l("origemAC, egresso sem informação", "AC, sem informação de escola"),
  rr_l("origemCotista, escola estadual/municipal", "Cotista, escola estadual/municipal"),
  rr_l("origemCotista, escola federal", "Cotista, escola federal"),
  rr_l("origemCotista, egresso", "Cotista, sem informação de escola"),
  "\\addlinespace", rr_l("sexoMasculino", "Masculino (ref.: feminino)", ""),
  "\\addlinespace", linha("\\textit{Cor/raça} (ref.: branca)", "", paste("global:", pg("cor/raça (4 categorias)"))),
  rr_l("cor_racaParda", "Parda"), rr_l("cor_racaPreta", "Preta"),
  rr_l("cor_racaAmarela/Indígena/Não declarado", "Amarela/indígena/não declarada"),
  "\\midrule",
  linha("Curso (7 categorias), teste global", "", pg("curso (7 categorias)")),
  linha("Coorte, teste global", "", pg("coorte")))
tex(l4, linha("Termo", "RR (IC 95\\%)", "$p$"), "lrr", "tab_art_modelo.tex")

# ---- Apêndice: sensibilidade com escolaridade da mãe e faixa etária -----------
ca <- le("art_sens_covariaveis_extra.csv")
mods <- c("principal nas mesmas matrículas", "+ mãe e idade")
ca_rr <- function(t) unlist(lapply(mods, \(m) with(ca[ca$modelo == m & ca$termo == t, ],
  if (length(RR)) c(rr_ci(RR, IC95_inf, IC95_sup), fmtp(p)) else c("---", "---"))))
ca_gl <- function(t) unlist(lapply(mods, \(m) { p <- ca$p[ca$modelo == m & ca$termo == paste("global:", t)]
  c("", if (length(p)) fmtp(p) else "---") }))
ca_l <- function(t, rot, ind = "\\quad ") do.call(linha, as.list(c(paste0(ind, rot), ca_rr(t))))
ca_g <- function(t, rot) do.call(linha, as.list(c(rot, ca_gl(t))))
la <- c(
  ca_g("ns(renda_sm, df = 3)", "Renda familiar (\\textit{spline}, 3 gl)"),
  ca_l("enem_100", "Nota total do SISU (+100 pontos)", ""),
  "\\addlinespace",
  ca_g("origem", "\\textit{Origem} (ref.: AC, escola privada)"),
  ca_l("origemAC, escola pública", "AC, escola pública"),
  ca_l("origemAC, egresso sem informação", "AC, sem informação de escola"),
  ca_l("origemCotista, escola estadual/municipal", "Cotista, escola estadual/municipal"),
  ca_l("origemCotista, escola federal", "Cotista, escola federal"),
  ca_l("origemCotista, egresso", "Cotista, sem informação de escola"),
  "\\addlinespace",
  ca_g("esc_mae", "\\textit{Escolaridade da mãe} (ref.: médio)"),
  ca_l("esc_maeAté médio incompleto", "Até médio incompleto"),
  ca_l("esc_maeSuperior", "Superior"),
  "\\addlinespace",
  ca_g("idade3", "\\textit{Faixa etária} (ref.: até 18)"),
  ca_l("idade319-20", "19 e 20 anos"),
  ca_l("idade321 ou mais", "21 anos ou mais"))
tex(la, c("& \\multicolumn{2}{c}{Principal} & \\multicolumn{2}{c}{Com mãe e idade} \\\\",
          "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
          linha("Termo", "RR (IC 95\\%)", "$p$", "RR (IC 95\\%)", "$p$")),
    "lrrrr", "tab_tcc_mae_idade.tex")

# ---- Tab. 5: análises complementares ----------------------------------------
cm <- le("art_complementar.csv"); en <- le("art_enem_especificacoes.csv")
auc_spec <- function(s) fmt(en$AUC_corrigido[en$especificacao == s], 3)
cml <- function(t, rot, auc = "---") with(cm[cm$termo == t, ],
  linha(paste0("\\quad ", rot), rr_ci(RR, IC95_inf, IC95_sup), fmtp(p), auc))
l5 <- c(
  paste0("\\multicolumn{4}{l}{\\textit{Renda em faixas} (ref.: $>$ 2 SM; global: ",
         fmtp(cm$p_global[cm$termo == "renda3m≤1 SM"]), ")} \\\\"),
  cml("renda3m≤1 SM", "até 1 SM"), cml("renda3m1–2 SM", "1 a 2 SM"),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{Nota de uma área no lugar da nota total} (+100 pontos)} \\\\",
  cml("cn_100", "Ciências da Natureza", auc_spec("só CN")),
  cml("mt_100", "Matemática", auc_spec("só MT")),
  "\\addlinespace",
  linha("\\quad Nota total do SISU (modelo principal)", "---", "---", auc_spec("total SISU")),
  linha("\\quad Sem nota do ENEM", "---", "---", auc_spec("sem ENEM")))
tex(l5, linha("Especificação", "RR (IC 95\\%)", "$p$", "AUC corrigida"), "lrrr",
    "tab_art_complementar.tex")

# ---- Tab. 6: discriminação ---------------------------------------------------
au <- le("art_auc.csv")
tex(linha(esc(sub("Modelo principal completo", "Modelo completo", cap(au$modelo))), fmt(au$aparente, 3), fmt(au$otimismo, 3),
          ic3(au$corrigido, au$IC95_inf, au$IC95_sup),
          ifelse(is.na(au$delta), "---", ic3(au$delta, au$delta_inf, au$delta_sup))),
    linha("Modelo", "Aparente", "Otimismo", mc("AUC corrigida\\\\(IC 95\\%)", "l"), mc("$\\Delta$AUC\\\\(IC 95\\%)", "l")),
    "lrrll", "tab_art_auc.tex")

# ---- Tab. 7: sensibilidades ---------------------------------------------------
se <- le("art_sensibilidade.csv")
rot_se <- esc(cap(se$analise))
# rótulos longos encurtados, com a explicação na legenda (linhas de altura igual)
rot_se <- ifelse(grepl("histórico", rot_se), "Sem saídas do 1º sem. sem histórico$^{a}$",
                 ifelse(grepl("critério das cotas", rot_se), "Renda per capita $\\leq$ 1,5 SM$^{b}$", rot_se))
tex(linha(rot_se, paste0(mil(se$n), "/", se$eventos), fmtp(se$p_renda_global),
          br(se$RR_enem), fmtp(se$p_origem)),
    linha("Análise", "$n$/eventos", "$p$ renda", "RR nota total", "$p$ origem"),
    "lrrlr", "tab_art_sensibilidade.tex")

# Figura: valores p da renda e da origem nas análises de sensibilidade
rot_curto <- c("principal" = "Modelo principal", "sem coorte 2020" = "Sem a coorte de 2020",
  "sem coorte 2024" = "Sem a coorte de 2024", "sem coortes 2020 e 2024" = "Sem as coortes de 2020 e 2024",
  "janela de 2 anos (coortes 2019–2023)" = "Evasão em dois anos (2019–2023)",
  "evasão ou trancamento no 1º ano" = "Evasão ou trancamento",
  "sem as saídas do 1º sem. sem registro no histórico (exceto 2020)" = "Sem saídas sem registro no histórico",
  "+ escolaridade da mãe e idade" = "Com escolaridade da mãe e idade", "sem a nota total do SISU" = "Sem a nota total",
  "renda per capita ≤ 1,5 SM (critério das cotas até 2023)" = "Renda per capita até 1,5 SM",
  "renda per capita em spline" = "Renda per capita em spline", "renda linear" = "Renda linear",
  "+ critério de renda da reserva" = "Com o critério de renda da reserva",
  "log-binomial" = "Log-binomial", "logística (RR por padronização marginal)" = "Regressão logística",
  "imputação múltipla (m = 20)" = "Imputação múltipla")
grp_se <- c(rep("Principal", 1), rep("Amostra", 3), rep("Desfecho", 3), rep("Covariáveis", 6), rep("Estimador", 3))
ord_se <- names(rot_curto)
stopifnot(setequal(ord_se, se$analise))
sf <- se |> mutate(rot = rot_curto[analise], grp = grp_se[match(analise, ord_se)]) |>
  select(rot, grp, renda = p_renda_global, origem = p_origem) |>
  tidyr::pivot_longer(c(renda, origem), names_to = "var", values_to = "p") |>
  mutate(var = factor(ifelse(var == "renda", "Renda", "Origem"), c("Renda", "Origem")),
         rot = factor(rot, rev(rot_curto)), grp = factor(grp, c("Principal", "Amostra", "Desfecho", "Covariáveis", "Estimador")))
faixas <- data.frame(xmin = c(0.01, 0.05), xmax = c(0.05, 0.10), cor = c("grey80", "grey90"))
gS <- ggplot(sf, aes(p, rot)) +
  geom_rect(data = faixas, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = cor), inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_point(aes(shape = grp == "Principal"), size = 1.8) +
  scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), guide = "none") +
  facet_grid(grp ~ var, scales = "free_y", space = "free_y") +
  scale_x_log10(breaks = c(0.01, 0.05, 0.1, 0.5), labels = \(x) format(x, decimal.mark = ",", trim = TRUE, drop0trailing = TRUE), limits = c(0.01, 0.6)) +
  labs(x = "Valor p do teste global (escala log)", y = NULL) +
  tema_artigo(base_size = 10) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 8), panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.5, "lines")) + theme(text = element_text(family = "Latin Modern Roman"))
salvar_figura(gS, file.path(DIR_FIG, "fig_tcc_sensibilidade.png"), largura = 5.6, altura = 4.4)

# ---- Tab. 8: renda em coortes regulares e atípicas --------------------------
rc <- le("art_renda_coortes.csv")
rc$coortes <- sub("^regulares", "calendário regular", sub("^atípicas", "calendário alterado", rc$coortes))
tex(linha(esc(cap(rc$coortes)), paste0(rc$n, "/", rc$eventos), fmt(rc$taxa_ate1, 1),
          fmt(rc$taxa_acima2, 1), rr_ci(rc$RR, rc$IC95_inf, rc$IC95_sup)),
    linha("Coortes", "$n$/eventos", mc("Evasão até\\\\1 SM (\\%)"), mc("Evasão\\\\$>$ 2 SM (\\%)"),
          mc("RR até 1 SM\\\\(IC 95\\%)")),
    "lrrrr", "tab_art_renda_coortes.tex")

# ---- Figura 2: curva de RR da renda (modelo principal), com os valores observados
cv  <- le("art_renda_curva.csv")
set.seed(1)
# deslocamento só a partir de 1 SM: na escala de raiz, o de 0 espalharia a faixa de nenhuma renda
obs <- le("art_renda_obs.csv") |> mutate(renda_sm = ifelse(renda_sm >= 1, renda_sm + runif(n(), -0.15, 0.15), renda_sm))
g <- ggplot(cv, aes(renda_sm, RR)) +
  geom_ribbon(aes(ymin = IC95_inf, ymax = IC95_sup), fill = "grey80") +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  # mediana: ponto de referência, RR = 1 por construção (não se marca
  # "início do platô", que seria limiar lido na forma da curva)
  geom_vline(xintercept = cv$ref_sm[1], linetype = "dotted", linewidth = 0.4) +
  geom_rug(data = obs, aes(x = renda_sm), inherit.aes = FALSE, sides = "b",
           alpha = 0.25, linewidth = 0.3) +
  scale_y_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3), labels = \(x) fmt(x)) +
  # eixo em raiz quadrada: abre o trecho de 0 a 3 SM sem cortar dados
  scale_x_sqrt(breaks = c(0, 0.5, 1, 2, 3, 5, 10, 15, 20), labels = \(x) format(x, decimal.mark = ",", trim = TRUE, drop0trailing = TRUE)) +
  labs(x = "Renda familiar mensal (salários mínimos), escala de raiz quadrada",
       y = sprintf("RR em relação à mediana (%s SM), escala log", fmt(cv$ref_sm[1]))) +
  tema_artigo(base_size = 11) + theme(text = element_text(family = "Latin Modern Roman"))
# tamanho final no .tex (0,85 \\textwidth = 13,6 cm): a fonte sai em tamanho real
salvar_figura(g, file.path(DIR_FIG, "fig_art_renda_curva.png"), largura = 5.35, altura = 3.5)

# ---- Figuras dos Resultados ---------------------------------------------------
# Escala de cinza (impressão), fonte e largura da Figura 2; dois tons com contraste
# de luminosidade e, nas linhas, também o tipo de traço, para não depender da cor.
fonte_lm <- theme(text = element_text(family = "Latin Modern Roman"))
pct <- function(x, dg = 0) paste0(fmt(x, dg), "%")

# Figura A: evasão por coorte, segundo o semestre a que a saída foi atribuída
co <- le("desc_artigo_coorte.csv")
coA <- bind_rows(data.frame(coorte = co$coorte, sem = "1º semestre", v = co$evad_1sem_pct),
                 data.frame(coorte = co$coorte, sem = "2º semestre", v = co$evad_2sem_pct)) |>
  mutate(sem = factor(sem, c("2º semestre", "1º semestre")), coorte = factor(coorte))
gA <- ggplot(coA, aes(coorte, v, fill = sem)) +
  geom_col(width = 0.62, color = "white", linewidth = 0.4) +
  geom_text(data = co, aes(factor(coorte), evad_1ano_pct, label = pct(evad_1ano_pct, 1)),
            inherit.aes = FALSE, vjust = -0.5, size = 3.2, family = "Latin Modern Roman") +
  scale_fill_manual(values = c("1º semestre" = "grey30", "2º semestre" = "grey72"),
                    breaks = c("1º semestre", "2º semestre"), name = "Saída atribuída ao") +
  scale_y_continuous(labels = \(x) pct(x), limits = c(0, 34), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Coorte de ingresso", y = "Evasão no primeiro ano") +
  tema_artigo(base_size = 11) + theme(legend.position = "top", legend.box.spacing = unit(0, "pt")) + fonte_lm
salvar_figura(gA, file.path(DIR_FIG, "fig_tcc_evasao_coorte.png"), largura = 5.35, altura = 3)

# Figura B: evasão bruta por categoria, com IC 95% de Wilson e a taxa geral
wilson <- function(k, n, z = qnorm(0.975)) {
  p <- k / n; m <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(inf = 100 * (m - h), sup = 100 * (m + h))
}
vB <- c(curso7 = "Curso", renda3 = "Renda", origem = "Origem", cor_raca = "Cor/raça", sexo = "Sexo",
        nota4 = "Nota")
geral <- with(filter(cat_t, variavel == "curso7"), 100 * sum(evad) / sum(n))
cb <- cat_t |> filter(variavel %in% names(vB), nivel != "Sem informação") |>
  mutate(grupo = factor(vB[variavel], vB), rot = rot_niv(nivel),
         rot = sub("^≤1 SM$", "Até 1 SM", sub("^1–2 SM$", "1 a 2 SM", sub("^>2 SM$", "Acima de 2 SM", rot)))) |>
  arrange(grupo, o2, nivel)
cb <- cbind(cb, wilson(cb$evad, cb$n)) |> mutate(rot = factor(rot, rev(unique(rot))))
gB <- ggplot(cb, aes(taxa_1ano, rot)) +
  geom_vline(xintercept = geral, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_errorbar(aes(xmin = inf, xmax = sup), width = 0, orientation = "y", linewidth = 0.4, color = "grey45") +
  geom_point(size = 1.8) +
  geom_text(aes(x = 62, label = paste0("n = ", mil(n))), hjust = 1, size = 3, color = "grey35",
            family = "Latin Modern Roman") +
  facet_grid(grupo ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = \(x) pct(x), breaks = seq(0, 50, 10), limits = c(0, 62)) +
  labs(x = "Evasão no primeiro ano (IC 95%)", y = NULL) +
  tema_artigo(base_size = 11) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "bold"),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        panel.spacing.y = unit(0.6, "lines")) + fonte_lm
salvar_figura(gB, file.path(DIR_FIG, "fig_tcc_evasao_categorias.png"), largura = 5.35, altura = 6.6)

# Figura C: floresta dos RR do modelo principal (sem a renda, que tem a Figura 2)
mpC <- le("art_modelo_principal.csv") |> filter(modelo == "principal") |>
  mutate(grupo = case_when(termo == "enem_100" ~ "Nota", grepl("^origem", termo) ~ "Origem",
                           grepl("^sexo", termo) ~ "Sexo", grepl("^cor_raca", termo) ~ "Cor/raça",
                           grepl("^curso7", termo) ~ "Curso", grepl("^coorte", termo) ~ "Coorte"),
         rot = case_when(termo == "enem_100" ~ "+100 pontos",
                         grepl("^origem", termo) ~ sub("egresso sem informação", "sem informação de escola",
                                                   sub("Cotista, egresso$", "Cotista, sem informação de escola", sub("^origem", "", termo))),
                         grepl("^sexo", termo) ~ "Masculino",
                         grepl("^cor_raca", termo) ~ rot_niv(sub("^cor_raca", "", termo)),
                         grepl("^curso7", termo) ~ rot_niv(sub("^curso7", "", termo)),
                         TRUE ~ sub("^coorte", "", termo))) |>
  filter(!is.na(grupo))
refs <- data.frame(grupo = c("Origem", "Sexo", "Cor/raça", "Curso", "Coorte"),
                   rot = c("AC, escola privada", "Feminino", "Branca", "Estatística", "2019"),
                   RR = 1, IC95_inf = NA, IC95_sup = NA, ref = TRUE)
mpC <- bind_rows(refs, mutate(mpC, ref = FALSE)) |>
  mutate(grupo = factor(grupo, c("Nota", "Origem", "Sexo", "Cor/raça", "Curso", "Coorte"),
                        c(" ", "Origem", "Sexo", "Cor/raça", "Curso", "Coorte")),
         rot = ifelse(rot == "+100 pontos", "Nota total do Sisu, +100 pontos",
                      ifelse(ref, paste(rot, "(referência)"), rot))) |>
  mutate(o = match(rot, c("Nota total do Sisu, +100 pontos", "AC, escola pública", "AC, sem informação de escola",
                           "Cotista, escola estadual/municipal", "Cotista, escola federal",
                           "Cotista, sem informação de escola", "Parda", "Preta", "Amarela/indígena/não declarada"))) |>
  arrange(grupo, !ref, o)
mpC$rot <- factor(mpC$rot, rev(unique(mpC$rot)))
gC <- ggplot(mpC, aes(RR, rot)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_errorbar(aes(xmin = IC95_inf, xmax = IC95_sup), width = 0, orientation = "y", linewidth = 0.4, color = "grey45") +
  geom_point(aes(shape = ref), size = 1.8, show.legend = FALSE) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1)) +
  facet_grid(grupo ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 1.5), labels = \(x) fmt(x)) +
  labs(x = "RR (IC 95%), escala log", y = NULL) +
  tema_artigo(base_size = 11) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "bold"),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        panel.spacing.y = unit(0.6, "lines")) + fonte_lm
salvar_figura(gC, file.path(DIR_FIG, "fig_tcc_rr_floresta.png"), largura = 5.35, altura = 6)

# Figura D: (a) curvas ROC aparentes; (b) AUC corrigida com IC por modelo
au <- le("art_auc.csv")
rotD <- c("curso + coorte (linha de base)" = "Linha de base",
          "modelo principal completo" = "Modelo completo")
roc <- le("art_roc.csv") |>
  mutate(rot = paste0(rotD[modelo], " (", fmt(au$aparente[match(modelo, au$modelo)], 3), ")"),
         rot = factor(rot, unique(rot)))
gDa <- ggplot(roc, aes(fpr, tpr, linetype = rot, color = rot)) +
  geom_abline(linewidth = 0.3, linetype = "dotted", color = "grey40") +
  geom_step(linewidth = 0.5, direction = "vh") +
  scale_color_manual(values = c("grey55", "black"), name = "AUC aparente") +
  scale_linetype_manual(values = c("dashed", "solid"), name = "AUC aparente") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = \(x) fmt(x, 1), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = \(x) fmt(x, 1), expand = c(0.01, 0)) +
  coord_equal() + guides(color = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
  labs(x = "1 − especificidade", y = "Sensibilidade") +
  tema_artigo(base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "vertical", legend.title.position = "top",
        legend.key.width = unit(1.4, "lines"), legend.text = element_text(size = 8),
        legend.title = element_text(size = 8), legend.key.height = unit(0.8, "lines"),
        legend.background = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(16, 4, 2, 4)) + fonte_lm
rotM <- c("curso + coorte (linha de base)" = "Linha de base", "+ nota total do SISU" = "+ nota total do Sisu",
          "+ renda (spline)" = "+ renda", "+ origem" = "+ origem", "+ sexo e cor/raça" = "+ sexo e cor/raça",
          "modelo principal completo" = "Modelo completo")
auD <- au |> mutate(rot = factor(rotM[modelo], rev(rotM)))
gDb <- ggplot(auD, aes(corrigido, rot)) +
  geom_vline(xintercept = 0.5, linetype = "dotted", linewidth = 0.3, color = "grey40") +
  geom_errorbar(aes(xmin = IC95_inf, xmax = IC95_sup), width = 0, orientation = "y", linewidth = 0.4, color = "grey45") +
  geom_point(size = 1.8) +
  scale_x_continuous(labels = \(x) fmt(x), limits = c(0.45, 0.6)) +
  labs(x = "AUC corrigida (IC 95%)", y = NULL) +
  tema_artigo(base_size = 10) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(16, 4, 2, 4)) +
  fonte_lm
gD <- cowplot::plot_grid(gDa, gDb, labels = c("(a)", "(b)"), label_fontfamily = "Latin Modern Roman",
                         label_fontface = "plain", label_size = 11, rel_widths = c(1, 1.05), align = "h", axis = "b",
                         label_x = 0, label_y = 1, hjust = 0, vjust = 1)
salvar_figura(gD, file.path(DIR_FIG, "fig_tcc_discriminacao.png"), largura = 5.35, altura = 3.8)

# ---- Apêndice do TCC: comparação de modelos (crítica de 11/11/2026) ----------
rot_termo <- c(enem_100 = "Nota total do SISU (+100 pontos)",
  "origemAC, escola pública" = "Origem: AC, escola pública", "origemAC, egresso sem informação" = "Origem: AC, sem informação de escola",
  "origemCotista, escola estadual/municipal" = "Origem: cotista, escola estadual/municipal",
  "origemCotista, escola federal" = "Origem: cotista, escola federal", "origemCotista, egresso" = "Origem: cotista, sem informação de escola",
  sexoMasculino = "Masculino", cor_racaParda = "Parda", cor_racaPreta = "Preta",
  "cor_racaAmarela/Indígena/Não declarado" = "Amarela/indígena/não declarada")
lb <- le("art_logbin.csv")
lb$rot <- ifelse(grepl("^renda_sm=", lb$termo), sprintf("Renda familiar: %s SM", fmt(as.numeric(sub("^renda_sm=", "", lb$termo)), 0)),
          ifelse(lb$termo %in% names(rot_termo), rot_termo[lb$termo],
          ifelse(grepl("^curso7", lb$termo), paste("Curso:", sub(" - B", ", b", sub(" - L", ", l", sub("^curso7", "", lb$termo)))),
                 paste("Coorte", sub("^coorte", "", lb$termo)))))
tex(linha(esc(lb$rot), rr_ci(lb$RR_poisson, lb$inf_poisson, lb$sup_poisson),
          rr_ci(lb$RR_logbin, lb$inf_logbin, lb$sup_logbin)),
    linha("Termo", mc("Poisson modificado\\\\RR (IC 95\\%)"), mc("Log-binomial\\\\RR (IC 95\\%)")),
    "lrr", "tab_tcc_logbin.tex")

aj <- le("art_ajuste.csv")
med <- list(
  c("Log-verossimilhança (Bernoulli)", "logLik_bernoulli", 2), c("Escore de Brier", "brier", 4),
  c("Hosmer--Lemeshow (decis), $p$", "HL_p", NA), c("Calibração: intercepto", "cal_intercepto", 3),
  c("Calibração: inclinação", "cal_inclinacao", 3), c("Dispersão de Pearson", "dispersao_pearson", 3),
  c("Grupos de resíduos binados fora de $\\pm$2 EP", "binados_fora_2EP", 0),
  c("Distância de Cook máxima", "cook_max", 3), c("Risco predito máximo", "risco_max", 3))
la <- sapply(med, function(m) {
  v <- aj[[m[2]]]; v[abs(v) < 5e-4] <- 0
  vv <- if (is.na(m[3])) ifelse(v > 0.99, "$>$0,99", fmtp(v)) else if (m[3] == "0") paste0(v, " de ", aj$grupos_binados) else neg(fmt(v, as.numeric(m[3])))
  linha(m[1], vv[1], vv[2])
})
# KS dos resíduos quantílicos: mediana (mín.–máx.) de várias realizações
ksr <- le("art_ks_realizacoes.csv")
fpk <- function(v) ifelse(v > 0.99, "$>$0,99", fmt(v, 2))
la <- append(la, linha(sprintf("Resíduos quantílicos (KS), $p$ em %d realizações", ksr$realizacoes[1]),
                       paste0(fpk(ksr$p_mediana), " (", fpk(ksr$p_min), "--", fpk(ksr$p_max), ")")[1],
                       paste0(fpk(ksr$p_mediana), " (", fpk(ksr$p_min), "--", fpk(ksr$p_max), ")")[2]), after = 6)
tex(c(la, "\\midrule", linha("Correlação entre os riscos preditos", paste0("\\multicolumn{2}{c}{", fmt(aj$cor_riscos[1], 3), "}"))),
    linha("Medida", "Poisson modificado", "Log-binomial"), "lrr", "tab_tcc_ajuste.tex")

fo <- le("art_renda_formas.csv") |> mutate(forma = ifelse(forma == "linear", "linear em SM", forma))
tex(linha(esc(cap(fo$forma)), fo$gl, fmtp(fo$p_global), fmt(fo$AIC_bernoulli, 1)),
    linha("Forma da renda", "gl", "$p$ global", "AIC"), "lrrr", "tab_tcc_renda_formas.tex")

fa <- le("art_faltantes.csv")
tex(linha(esc(cap(fa$tratamento)), paste0(mil(fa$n), "/", fa$eventos), fmtp(fa$p_renda_global)),
    linha("Tratamento das matrículas sem renda", "$n$/eventos", "$p$ renda"), "lrr", "tab_tcc_faltantes.tex")

# Figuras: resíduos binados (20 grupos pelo risco predito) e QQ dos resíduos
# quantílicos aleatorizados, um painel por modelo (uma série por painel, sem cor).
rs <- le("art_residuos.csv") |> mutate(modelo = factor(modelo, c("Poisson modificado", "log-binomial"),
                                                       c("Poisson modificado", "Log-binomial")))
bin <- rs |> group_by(modelo) |> mutate(g = cut(p, quantile(p, 0:20 / 20), include.lowest = TRUE)) |>
  group_by(modelo, g) |> summarise(p_med = mean(p), r = mean(y - p), lim = 2 * sqrt(mean(p * (1 - p)) / n()),
                                   .groups = "drop")
fonte <- theme(text = element_text(family = "Latin Modern Roman"))
g1 <- ggplot(bin, aes(p_med, r)) +
  geom_line(aes(y = lim), linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_line(aes(y = -lim), linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_hline(yintercept = 0, linewidth = 0.3) + geom_point(size = 1.8) +
  facet_wrap(~ modelo) + scale_x_continuous(labels = \(x) fmt(x)) + scale_y_continuous(labels = \(x) fmt(x)) +
  labs(x = "Risco predito médio do grupo", y = "Resíduo médio (observado menos predito)") +
  tema_artigo(base_size = 11) + fonte
salvar_figura(g1, file.path(DIR_FIG, "fig_tcc_residuos_binados.png"), largura = 5.35, altura = 3)
# Banda simultânea de 95% no QQ, sensível às caudas (Aldor-Noiman et al., 2013): bandas
# Beta das estatísticas de ordem, com o nível pontual ajustado por simulação para que
# uma amostra normal fique inteira dentro da banda em 95% das vezes.
banda_qq <- function(n, M = 2000, nivel = 0.95) {
  i <- seq_len(n)
  cmin <- replicate(M, { p <- pbeta(sort(runif(n)), i, n + 1 - i); 2 * min(p, 1 - p) })
  a <- quantile(cmin, 1 - nivel, names = FALSE)
  data.frame(teo = qnorm(ppoints(n)), inf = qnorm(qbeta(a / 2, i, n + 1 - i)),
             sup = qnorm(qbeta(1 - a / 2, i, n + 1 - i)))
}
n_rs <- unique(table(rs$modelo)); stopifnot(length(n_rs) == 1)
set.seed(20261118)
bq <- banda_qq(n_rs)
qq <- rs |> group_by(modelo) |> arrange(rq, .by_group = TRUE) |> mutate(bq) |> ungroup()
cat("Pontos fora da banda do QQ:\n"); print(qq |> group_by(modelo) |> summarise(fora = sum(rq < inf | rq > sup)))
g2 <- ggplot(qq, aes(teo, rq)) + geom_ribbon(aes(ymin = inf, ymax = sup), fill = "grey85") +
  geom_abline(linewidth = 0.3, linetype = "dashed", color = "grey40") +
  geom_point(size = 0.5, alpha = 0.5) + facet_wrap(~ modelo) +
  scale_x_continuous(labels = \(x) fmt(x, 0)) + scale_y_continuous(labels = \(x) fmt(x, 1)) +
  labs(x = "Quantil da normal padrão", y = "Resíduo quantílico aleatorizado") +
  tema_artigo(base_size = 11) + fonte
salvar_figura(g2, file.path(DIR_FIG, "fig_tcc_residuos_qq.png"), largura = 5.35, altura = 3)

# Resíduos parciais da renda e da nota (Poisson modificado): média por grupo ± 2 EP e
# componente do modelo (linha), na escala do log do risco.
pr <- le("art_residuos_parciais.csv") |>
  mutate(x = ifelse(termo == "nota", 100 * x, x),
         termo = factor(termo, c("renda", "nota"), c("Renda familiar (SM)", "Nota total do Sisu (pontos)")))
g3 <- ggplot(pr, aes(x)) + geom_line(aes(y = f), linewidth = 0.4) +
  geom_errorbar(aes(ymin = r_med - 2 * ep, ymax = r_med + 2 * ep), width = 0, linewidth = 0.3, color = "grey40") +
  geom_point(aes(y = r_med), size = 1.5) +
  facet_wrap(~ termo, scales = "free_x", strip.position = "bottom") +
  scale_x_continuous(labels = \(x) fmt(x, 0)) + scale_y_continuous(labels = \(x) fmt(x, 1)) +
  labs(x = NULL, y = "Resíduo parcial médio") +
  tema_artigo(base_size = 11) + theme(strip.placement = "outside") + fonte
salvar_figura(g3, file.path(DIR_FIG, "fig_tcc_residuos_parciais.png"), largura = 5.35, altura = 3)
cat("Tabelas e figuras do artigo e do apêndice do TCC geradas.\n")
