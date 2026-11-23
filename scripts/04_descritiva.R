# ==============================================================================
# 04. DESCRITIVA DAS VARIÁVEIS
# ------------------------------------------------------------------------------
# Variáveis do modelo: curso (7 cat.), coorte, renda (3
# faixas), origem (modalidade × escola), sexo, cor/raça, notas MT/CN/total;
# reprovação por frequência no 1º sem. e trancamentos no 1º ano (descritivo).
# Recodificações em prep_artigo() (00_util.R).
# Saídas: saidas/desc_artigo_*.csv
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("scripts/00_util.R")

d <- readRDS("dados/gerados/base_modelo_evasao_1ano.rds") |> prep_artigo() |>
  filter(coorte %in% ANOS_ANALISE) |> droplevels()
cat("Base:", nrow(d), "matrículas;", sum(d$evadido_1ano, na.rm = TRUE), "evasões\n")

cats <- c("curso7", "coorte", "renda3", "origem", "sexo", "cor_raca")

# --- 1. Categóricas: n, % da amostra, taxa de evasão (1º ano, 1º e 2º sem.) ---
# Tabela 1 também com nota total em quartis e faixa etária
q_nota <- round(quantile(d$total_enem, c(.25, .5, .75)))
d <- d |> mutate(nota4 = cut(total_enem, c(-Inf, quantile(total_enem, c(.25, .5, .75)), Inf),
  labels = c(sprintf("1º quartil (até %d)", q_nota[1]), sprintf("2º quartil (%d a %d)", q_nota[1], q_nota[2]),
             sprintf("3º quartil (%d a %d)", q_nota[2], q_nota[3]), sprintf("4º quartil (acima de %d)", q_nota[3]))))
tab_cat <- bind_rows(lapply(c(cats, "nota4", "idade3"), function(v) {
  d |> group_by(nivel = as.character(.data[[v]])) |>
    summarise(n = n(), evad = sum(evadido_1ano),
              taxa_1ano = 100 * mean(evadido_1ano),
              taxa_1sem = 100 * mean(evadido_1sem), .groups = "drop") |>
    mutate(variavel = v, pct_amostra = 100 * n / sum(n), .before = 1)
}))

# --- 2. Contínuas: notas do ENEM por desfecho (média, dp) --------------------
tab_enem <- d |> group_by(evadido_1ano) |>
  summarise(n = n(),
            across(c(NU_NOTA_MT, NU_NOTA_CN, total_enem),
                   list(media = ~ mean(.x, na.rm = TRUE), dp = ~ sd(.x, na.rm = TRUE))),
            .groups = "drop")

# --- 3. Por coorte: histórico do 1º semestre (justifica excluir 2020) --------
tab_coorte <- d |> group_by(coorte) |>
  summarise(
    n               = n(),
    evad_1ano_pct   = 100 * mean(evadido_1ano),
    evad_1sem_pct   = 100 * mean(evadido_1sem),
    evad_2sem_pct   = 100 * mean(evadido_2sem),
    sem_registro_pct = 100 * mean(n_registros_1osem == 0),
    reprov_freq_pct = 100 * mean(reprov_freq_1osem),
    reprov_nota_pct = 100 * mean(reprov_nota_1osem),
    media_reprov_freq = mean(n_reprov_freq_1osem),
    media_aproveit  = mean(n_aproveitamento),
    cotista_pct     = 100 * mean(cotista == "Sim"),
    concluiu_antes_pct = 100 * mean(concluiu_antes == "Sim", na.rm = TRUE),
    renda_ate1sm_pct = 100 * mean(renda_ate1sm == "Sim", na.rm = TRUE),
    enem_media      = mean(total_enem),
    .groups = "drop")

# --- 4. Fluxo de seleção (STROBE) --------------------------------------------
cce_cursos <- c("Estatística", "Estatística e Ciência de Dados",
                "Matemática",                      # 2018: ingresso por área básica (11/11/2026)
                "Matemática - Bacharelado", "Matemática - Licenciatura",
                "Física - Bacharelado", "Física - Licenciatura",
                "Química - Bacharelado", "Química - Licenciatura")
u_all <- bind_rows(lapply(ANOS_BASE, \(a) readr::read_csv(
    file.path(DIR_GERADO, sprintf("ingressantes_%d_ufes_com_enem_todas_colunas.csv", a)),
    show_col_types = FALSE, col_types = readr::cols(.default = "c")))) |>
  mutate(mn = norm_matr(matricula))
u_cce <- filter(u_all, CURSO %in% cce_cursos)
tab_fluxo0 <- tibble(
  etapa = c(sprintf("Ingressantes UFES %d-%d (registros)", min(ANOS_BASE), max(ANOS_BASE)), "Matrículas distintas (deduplicação)",
            "Cursos do CCE (Goiabeiras), todos os semestres", "Cursos do CCE, 1º semestre"),
  n = c(nrow(u_all), n_distinct(u_all$mn), n_distinct(u_cce$mn),
        n_distinct(u_cce$mn[u_cce$SEMESTRE == "1º Semestre"])))
all0 <- read.csv(file.path(DIR_GERADO, "base_cce_evasao_1ano.csv"))
all  <- all0[all0$ANO.INGRESSO %in% ANOS_ANALISE, ]
tab_fluxo <- tibble(
  etapa = c(sprintf("Ingressantes CCE 1º sem. %d-%d (vínculos)", min(ANOS_BASE), max(ANOS_BASE)),
            "Excluídos: coorte de 2018 (registro de saídas não comparável)",
            sprintf("Ingressantes CCE 1º sem. %d-%d", min(ANOS_ANALISE), max(ANOS_ANALISE)),
            "Excluídos: sem nota por área do ENEM",
            "Excluídos: ajuste de currículo no ano de ingresso (desfecho indefinido)",
            "Base analítica",
            "Excluídos: renda (Q006) ausente",
            "Modelo principal (complete-case renda)"),
  n = c(nrow(all0), sum(!all0$ANO.INGRESSO %in% ANOS_ANALISE), nrow(all), sum(all$nota_faltando), sum(!all$nota_faltando & is.na(all$evadido_1ano)), nrow(d), sum(is.na(d$Q006)),
        sum(!is.na(d$Q006))),
  eventos = c(sum(all0$evadido_1ano, na.rm = TRUE), sum(all0$evadido_1ano[!all0$ANO.INGRESSO %in% ANOS_ANALISE], na.rm = TRUE),
              sum(all$evadido_1ano, na.rm = TRUE), sum(all$evadido_1ano[all$nota_faltando], na.rm = TRUE), NA,
              sum(d$evadido_1ano), sum(d$evadido_1ano[is.na(d$Q006)]),
              sum(d$evadido_1ano[!is.na(d$Q006)])))

# --- 5. Composição do corpo discente: ingresso vs. permanece após o 1º ano ---
comp <- function(v) d |> filter(!is.na(.data[[v]])) |>
  group_by(grupo = as.character(.data[[v]])) |>
  summarise(ingresso = n(), permanece = sum(evadido_1ano == 0), .groups = "drop") |>
  mutate(variavel = v, pct_ingresso = 100 * ingresso / sum(ingresso),
         pct_permanece = 100 * permanece / sum(permanece),
         retencao_pct = 100 * permanece / ingresso, .before = 1)
tab_comp <- bind_rows(comp("renda3"), comp("cotista"), comp("ppi"), comp("cor_raca"))

# --- 6. Trancamentos no 1º ano (histórico), por desfecho ---------------------
hist <- readr::read_delim(file.path(DIR_DADOS, "UFES", "alunos_x_situacao_curso.csv"),
    delim = "\t", quote = "", show_col_types = FALSE,
    locale = readr::locale(encoding = "latin1"), col_types = readr::cols(.default = "c")) |>
  filter(SITUACAO == "Trancamento de Curso") |>
  transmute(matricula_norm = norm_matr(MATR_ALUNO), ANO = as.numeric(ANO)) |> distinct()
tranc <- d |> select(matricula_norm, ano = `ANO INGRESSO`, evadido_1ano) |>
  mutate(trancou_1ano = matricula_norm %in% hist$matricula_norm[
    paste(hist$matricula_norm, hist$ANO) %in% paste(matricula_norm, ano)])
tab_tranc <- tranc |> group_by(evadido_1ano) |>
  summarise(n = n(), trancou_1ano = sum(trancou_1ano), .groups = "drop")
# Entre os não evadidos no 1º ano: saída registrada depois (até 2025, último ano
# do registro de desligamentos), por trancamento no 1º ano. O seguimento é mais
# curto nas coortes recentes; descritivo (10/11/2026).
ev_dep <- readr::read_csv2(file.path(DIR_DADOS, "EVADIDOS", "alunos_evadidos_2015_2025.csv"),
    show_col_types = FALSE, locale = readr::locale(encoding = "latin1")) |>
  mutate(matricula_norm = norm_matr(MATR_ALUNO),
         nao_consta = grepl("formado|adapta..o curricular", tolower(FORMA_EVASAO))) |>
  filter(!nao_consta) |> select(matricula_norm, ANO_EVASAO)
tab_tranc_dep <- tranc |> filter(evadido_1ano == 0) |>
  mutate(evadiu_depois = matricula_norm %in% ev_dep$matricula_norm[
    ev_dep$ANO_EVASAO > ano[match(ev_dep$matricula_norm, matricula_norm)]]) |>
  group_by(trancou_1ano) |>
  summarise(n = n(), evadiu_depois_pct = 100 * mean(evadiu_depois),
            evadiu_depois = sum(evadiu_depois), .groups = "drop")
out(tab_tranc_dep, "desc_artigo_trancamento_depois.csv")
print(as.data.frame(tab_tranc_dep))

# CCE vs. demais cursos da UFES (1º sem., com questionário) — perfil de ingresso
tab_ufes <- u_all |> distinct(mn, .keep_all = TRUE) |>
  filter(SEMESTRE == "1º Semestre", !is.na(Q006)) |>
  group_by(grupo = if_else(CURSO %in% cce_cursos, "CCE", "Demais UFES")) |>
  summarise(n = n(), pct_ate1sm = 100 * mean(Q006 %in% c("A", "B")),
            pct_ate2sm = 100 * mean(Q006 %in% LETTERS[1:4]),
            pct_cotista = 100 * mean(!MODALIDADE %in% c("NOPT", "AC")),
            pct_ppi = 100 * mean(TP_COR_RACA %in% c("2", "3", "5")), .groups = "drop")

# --- 7. Números citados na seção de Dados (11/11/2026) ------------------------
# Vagas surgidas (sem nota por área): pontuação de classificação e aproveitamentos;
# desistências do 1º sem. lançadas entre julho e outubro; reopções e transferências.
vs <- all |> filter(ANO.INGRESSO %in% c(2019, 2020)) |>
  group_by(ano = ANO.INGRESSO, vagas_surgidas = nota_faltando) |>
  summarise(n = n(), pontuacao_min = min(TOTAL, na.rm = TRUE), pontuacao_max = max(TOTAL, na.rm = TRUE),
            media_aproveitamentos = mean(n_aproveitamento, na.rm = TRUE), .groups = "drop")
ev_all <- readr::read_csv2(file.path(DIR_DADOS, "EVADIDOS", "alunos_evadidos_2015_2025.csv"),
    show_col_types = FALSE, locale = readr::locale(encoding = "latin1")) |>
  mutate(matricula_norm = norm_matr(MATR_ALUNO), ANO_INGRESSO = as.integer(ANO_INGRESSO),
         ANO_EVASAO = as.integer(ANO_EVASAO), k = ANO_EVASAO - ANO_INGRESSO,
         mes = as.integer(substr(DT_SAIDA, 4, 5)))
des <- ev_all |> filter(matricula_norm %in% d$matricula_norm[d$coorte %in% as.character(2019:2023) & d$evadido_1sem == 1],
                        FORMA_EVASAO == "Desistência", grepl("^1", PERIODO_EVASAO), k == 0)
troca <- ev_all |> filter(ANO_INGRESSO %in% ANOS_ANALISE, grepl("Reop|Transf", FORMA_EVASAO))
# Ano de lançamento das saídas do 1º ano, por coorte (comparação com o INEP)
out(ev_all |> filter(matricula_norm %in% d$matricula_norm[d$evadido_1ano == 1], k == 0,
                     !grepl("Formado|Adapta", FORMA_EVASAO)) |>
      distinct(matricula_norm, .keep_all = TRUE) |>
      count(coorte = ANO_INGRESSO, ano_lancamento = as.integer(substr(DT_SAIDA, 7, 10))),
    "desc_artigo_ano_lancamento.csv")
troca_cce <- filter(troca, matricula_norm %in% d$matricula_norm)
# Forma das saídas do 1º ano e data do último lançamento no registro
forma <- ev_all |> filter(matricula_norm %in% d$matricula_norm[d$evadido_1ano == 1], k == 0,
                          !grepl("Formado|Adapta", FORMA_EVASAO)) |>
  distinct(matricula_norm, .keep_all = TRUE) |> count(FORMA_EVASAO)
ult <- max(as.Date(ev_all$DT_SAIDA, "%d/%m/%Y"), na.rm = TRUE)
# enem_filtrado_<ano>.csv traz todos os participantes da edição com as cinco notas de algum ingressante da UFES
n_chaves_repetidas <- sum(sapply(ANOS_ANALISE, function(a) {
  e <- read.csv2(file.path(DIR_GERADO, sprintf("enem_filtrado_%d.csv", a)), fileEncoding = "latin1", colClasses = "character")
  sum(duplicated(e[c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]))
}))
tab_dados <- bind_rows(
  vs |> tidyr::pivot_longer(c(n, pontuacao_min, pontuacao_max, media_aproveitamentos), names_to = "medida") |>
    transmute(indicador = paste0("coorte ", ano, if_else(vagas_surgidas, ", vagas surgidas", ", SISU"), ": ", medida), valor = value),
  tibble(indicador = c("desistências 1º sem. 2019-2023", "  lançadas entre julho e outubro",
                       "UFES 2019-2024, reopção/transferência no ano de ingresso", "  no ano seguinte",
                       "CCE analisados, reopção/transferência no ano de ingresso", "  no ano seguinte", "  dois anos depois"),
         valor = c(nrow(des), sum(des$mes %in% 7:10), sum(troca$k == 0, na.rm = TRUE), sum(troca$k == 1, na.rm = TRUE),
                   sum(troca_cce$k == 0, na.rm = TRUE), sum(troca_cce$k == 1, na.rm = TRUE), sum(troca_cce$k == 2, na.rm = TRUE))),
  # Faixa "nenhuma renda" da Q006, à qual se atribui 0 SM
  tibble(indicador = c("nenhuma renda (0 SM): matrículas", "  evasões"),
         valor = c(sum(d$renda_sm == 0, na.rm = TRUE), sum(d$evadido_1ano[d$renda_sm %in% 0]))),
  # Ligação SISU–ENEM: taxa na base e unicidade da chave na edição
  tibble(indicador = c("ligação SISU–ENEM: matrículas da base ligadas", "  de um total de",
                       "  combinações de cinco notas repetidas na edição (2018 a 2023)"),
         valor = c(sum(!is.na(d$NU_INSCRICAO)), nrow(d), n_chaves_repetidas)),
  tibble(indicador = paste0("evasões do 1º ano por forma: ", forma$FORMA_EVASAO), valor = forma$n),
  tibble(indicador = "último lançamento no registro de desligamentos (AAAAMMDD)",
         valor = as.numeric(format(ult, "%Y%m%d"))))
out(tab_dados, "desc_artigo_dados.csv"); print(as.data.frame(tab_dados))

out(tab_comp, "desc_artigo_composicao.csv")
out(tab_ufes, "desc_artigo_cce_vs_ufes.csv")
out(tab_cat, "desc_artigo_categorias.csv")
out(tab_enem, "desc_artigo_enem.csv")
out(tab_coorte, "desc_artigo_coorte.csv")
# Origem por coorte (regra de admissão da Lei 14.723/2023 a partir de 2024) e
# faixa etária por origem (categorias "egresso" = sem dependência da escola no ENEM,
# sobretudo quem concluiu o EM antes do ano do exame; ver desc_artigo_origem_conclusao.csv).
out(d |> count(coorte, origem) |> group_by(coorte) |> mutate(pct = 100 * n / sum(n)) |> ungroup(),
    "desc_artigo_origem_coorte.csv")
out(d |> filter(!is.na(idade3)) |> count(origem, idade3) |> group_by(origem) |>
      mutate(pct = 100 * n / sum(n)) |> ungroup(), "desc_artigo_idade_origem.csv")
# Origem × situação de conclusão do EM declarada no ENEM (TP_ST_CONCLUSAO: 1 já concluiu,
# 2 conclui no ano do exame, 3 conclui depois, 4 não concluiu), na base e no modelo
oc <- function(x, a) x |> count(origem, conclusao = as.character(TP_ST_CONCLUSAO)) |>
  group_by(origem) |> mutate(pct = 100 * n / sum(n)) |> ungroup() |> mutate(amostra = a, .before = 1)
out(bind_rows(oc(d, "base"), oc(filter(d, !is.na(renda_sm)), "modelo")), "desc_artigo_origem_conclusao.csv")
out(bind_rows(tab_fluxo0, tab_fluxo), "desc_artigo_fluxo.csv")
out(tab_tranc, "desc_artigo_trancamento.csv")
# Abrangência do critério de renda das cotas (≤ 1,5 SM per capita) na base com renda
out(d |> filter(!is.na(renda_pc15)) |> count(renda_pc15) |> mutate(pct = 100 * n / sum(n)),
    "desc_artigo_renda_pc15.csv")
# Renda familiar (3 faixas, mediana, ≤ 1,5 SM per capita) por origem e por reserva com e
# sem critério de renda, entre as matrículas com renda
ro <- function(x, v) x |> group_by(grupo = as.character(.data[[v]])) |>
  summarise(n = n(), ate1_pct = 100 * mean(renda3 == "≤1 SM"), de1a2_pct = 100 * mean(renda3 == "1–2 SM"),
            acima2_pct = 100 * mean(renda3 == ">2 SM"), mediana_sm = median(renda_sm),
            pc15_pct = 100 * mean(renda_pc15 == "≤ 1,5 SM"), .groups = "drop") |> mutate(variavel = v, .before = 1)
dr <- filter(d, !is.na(renda_sm)) |> mutate(total = "Total")
out(bind_rows(ro(dr, "origem"), ro(dr, "reserva"), ro(dr, "total")), "desc_artigo_renda_origem.csv")

options(width = 200, digits = 3)
print(as.data.frame(tab_fluxo0)); print(as.data.frame(tab_fluxo)); print(as.data.frame(tab_coorte))
print(as.data.frame(tab_tranc))
print(as.data.frame(tab_cat)); print(as.data.frame(tab_enem))
print(as.data.frame(tab_comp)); print(as.data.frame(tab_ufes))

# --- Reprovação no 1º semestre por momento da saída (descritivo; 10/11/2026) ---
# Não entra no modelo: a maior parte das saídas é atribuída ao 1º semestre, e a
# reprovação desses estudantes registra o abandono. Todas as coortes e só as
# regulares (sem 2020, Earte, e 2024, greve).
reprov_saida <- function(x, rot) x |>
  mutate(momento = case_when(evadido_1sem == 1 ~ "saiu no 1º sem.",
                             evadido_2sem == 1 ~ "saiu no 2º sem.",
                             TRUE ~ "permaneceu")) |>
  group_by(momento) |>
  summarise(n = n(),
            sem_registro_pct = 100 * mean(n_registros_1osem == 0),
            reprov_freq_pct  = 100 * mean(reprov_freq_1osem),
            reprov_nota_pct  = 100 * mean(reprov_nota_1osem),
            alguma_reprov_pct = 100 * mean(reprov_freq_1osem == 1 | reprov_nota_1osem == 1),
            .groups = "drop") |>
  mutate(coortes = rot, .before = 1)
tab_reprov <- bind_rows(reprov_saida(d, "todas (2019–2024)"),
                        reprov_saida(filter(d, !coorte %in% c(2020, 2024)), "regulares (2019, 2021–2023)"))
out(tab_reprov, "desc_artigo_reprovacao.csv")
print(as.data.frame(tab_reprov))

# Saídas do 1º semestre sem nenhum registro no histórico: por
# coorte; por mês de lançamento (regulares); por origem e na coorte de 2019 sem 2020,
# cujo "sem registro" vem das disciplinas canceladas sem nota no Earte.
sr <- d |> mutate(sr = n_registros_1osem == 0, sr1 = evadido_1sem == 1 & sr)
mes1 <- ev_all |> filter(k == 0, grepl("^1", PERIODO_EVASAO), !grepl("Formado|Adapta", FORMA_EVASAO)) |>
  distinct(matricula_norm, .keep_all = TRUE) |> select(matricula_norm, mes)
s1 <- sr |> filter(evadido_1sem == 1) |> left_join(mes1, by = "matricula_norm")
blk <- function(x, bloco, base, cat, num) x |> group_by(categoria = {{ cat }}) |>
  summarise(n = n(), n_sem_registro = sum({{ num }}), .groups = "drop") |>
  mutate(bloco = bloco, base = base, categoria = as.character(categoria),
         pct = 100 * n_sem_registro / n, .before = 1)
s19 <- filter(sr, coorte == "2019")
tab_sr <- bind_rows(
  blk(s1, "coorte", "saídas do 1º sem.", coorte, sr),
  blk(filter(s1, !coorte %in% c(2020, 2024)), "lançamento (2019, 2021–2023)", "saídas do 1º sem.",
      if_else(mes <= 6, "fev–jun", "jul–dez"), sr),
  blk(filter(s1, coorte == "2019"), "lançamento (2019)", "saídas do 1º sem.",
      if_else(mes <= 6, "fev–jun", "jul–dez"), sr),
  blk(filter(sr, coorte != "2020"), "origem (sem 2020)", "ingressantes", origem, sr1),
  tibble(bloco = "coorte 2019, taxa de evasão", base = "ingressantes (n_sem_registro = evasões)",
         categoria = c("todas as saídas", "sem as do 1º sem. sem registro"),
         n = c(nrow(s19), sum(!s19$sr1)), n_sem_registro = c(sum(s19$evadido_1ano), sum(s19$evadido_1ano & !s19$sr1)),
         pct = 100 * n_sem_registro / n))
out(tab_sr, "desc_artigo_sem_registro.csv"); print(as.data.frame(tab_sr))

# --- Disciplinas do 1º semestre no histórico -----------------------------------
# O histórico registra reprovações e situações não convencionais, não aprovações:
# a lista mostra as disciplinas com mais ingressantes registrados no 1º semestre.
disc <- readr::read_delim(file.path(DIR_DADOS, "UFES", "alunos_x_situacao_curso.csv"),
    delim = "\t", quote = "", show_col_types = FALSE,
    locale = readr::locale(encoding = "latin1"), col_types = readr::cols(.default = "c")) |>
  mutate(matricula_norm = norm_matr(MATR_ALUNO)) |>
  inner_join(d |> transmute(matricula_norm, ano = as.character(`ANO INGRESSO`), curso7), by = "matricula_norm") |>
  filter(ANO == ano, PERIODO == "1º Semestre", !grepl("Tranc", SITUACAO)) |>
  group_by(curso7, disciplina = NOME_DISCIPLINA) |>
  summarise(ingressantes = n_distinct(matricula_norm), .groups = "drop") |>
  group_by(curso7) |> slice_max(ingressantes, n = 6, with_ties = FALSE) |> ungroup()
out(disc, "desc_artigo_disciplinas_1sem.csv"); print(as.data.frame(disc))

# --- Por curso -----------------------------------------------------------------
# Descrição marginal: evasão, momento da saída e perfil de ingresso de cada curso.
out(d |> group_by(curso7) |>
      summarise(n = n(), evad = sum(evadido_1ano), taxa_1ano = 100 * mean(evadido_1ano),
                saidas_1sem_pct = 100 * sum(evadido_1sem) / sum(evadido_1ano),
                renda_ate1sm_pct = 100 * mean(renda_ate1sm == "Sim", na.rm = TRUE),
                cotista_pct = 100 * mean(cotista == "Sim"),
                nota_mediana = median(total_enem), .groups = "drop"),
    "desc_artigo_curso.csv")
