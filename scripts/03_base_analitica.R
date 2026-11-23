# ==============================================================================
# 03. BASE ANALÍTICA: ingressantes do CCE no 1º semestre, desfecho e histórico
# ------------------------------------------------------------------------------
#   - Cursos: os 8 cursos de Exatas do CCE-Goiabeiras ("Matemática" = ABI, 2018)
#   - Evasão no 1º ano = saída lançada no ano de ingresso (só há ingresso no 1º
#     sem.), qualquer FORMA_EVASAO exceto "Formado" e "Adaptação Curricular"
#     (essas duas deixam o desfecho indefinido: NA)
#   - Matrículas com sufixo "X" (mudança de PPC) são a mesma matrícula base:
#     1 linha por matrícula base
#   - Histórico: reprovações e situações não convencionais (não há aprovações
#     regulares), só no 1º semestre do ano de ingresso, sem vazamento pós-desfecho
# Saídas (dados/gerados/): base_modelo_evasao_1ano.rds (base com nota e desfecho)
#   e base_cce_evasao_1ano.csv (todas as matrículas, para o fluxo de seleção)
# ==============================================================================
suppressPackageStartupMessages(library(tidyverse))
source("scripts/00_util.R")

CCE_CURSOS <- c(
  "Estatística", "Estatística e Ciência de Dados", "Matemática",
  "Matemática - Bacharelado", "Matemática - Licenciatura",
  "Física - Bacharelado", "Física - Licenciatura",
  "Química - Bacharelado", "Química - Licenciatura")
norm_txt <- function(x) str_squish(str_to_lower(x))

# ---- 1. Ingressantes do CCE no 1º semestre ----------------------------------
ing <- map_dfr(ANOS_BASE, ~ read_csv(
    file.path(DIR_GERADO, paste0("ingressantes_", .x, "_ufes_com_enem_todas_colunas.csv")),
    show_col_types = FALSE, col_types = cols(matricula = col_character()))) |>
  filter(CURSO %in% CCE_CURSOS, SEMESTRE == "1º Semestre") |>
  mutate(matricula_norm = norm_matr(matricula),
         CURSO_NORMALIZADO = str_extract(CURSO, "Estatística|Matemática|Física|Química")) |>
  arrange(matricula_norm, grepl("[Xx]$", matricula)) |>    # prefere a matrícula base
  distinct(matricula_norm, .keep_all = TRUE)

# ---- 2. Desfecho: evasão no 1º ano, no 1º e no 2º semestre ------------------
ev <- read_csv2(file.path(DIR_DADOS, "EVADIDOS", "alunos_evadidos_2015_2025.csv"),
                show_col_types = FALSE, locale = locale(encoding = "latin1")) |>
  mutate(matricula_norm = norm_matr(MATR_ALUNO))

ev_status <- ing |>
  select(matricula, matricula_norm, `ANO INGRESSO`) |>
  left_join(ev |> select(matricula_norm, ANO_EVASAO, PERIODO_EVASAO, FORMA_EVASAO),
            by = "matricula_norm", relationship = "many-to-many") |>
  mutate(na_janela      = !is.na(ANO_EVASAO) & ANO_EVASAO == `ANO INGRESSO`,
         sem_evasao     = case_when(str_detect(norm_txt(PERIODO_EVASAO), "^1") ~ "1sem",
                                    str_detect(norm_txt(PERIODO_EVASAO), "^2") ~ "2sem"),
         flg_nao_consta = str_detect(norm_txt(FORMA_EVASAO), "formado|adapta..o curricular")) |>
  group_by(matricula) |>
  summarise(
    evadido_1ano = case_when(any(na_janela & !flg_nao_consta) ~ 1L,
                             any(na_janela &  flg_nao_consta) ~ NA_integer_, TRUE ~ 0L),
    evadido_1sem = case_when(any(na_janela & !flg_nao_consta & sem_evasao == "1sem") ~ 1L,
                             any(na_janela &  flg_nao_consta & sem_evasao == "1sem") ~ NA_integer_, TRUE ~ 0L),
    evadido_2sem = case_when(any(na_janela & !flg_nao_consta & sem_evasao == "2sem") ~ 1L,
                             any(na_janela &  flg_nao_consta & sem_evasao == "2sem") ~ NA_integer_, TRUE ~ 0L),
    .groups = "drop")
ing <- left_join(ing, ev_status, by = "matricula")

# ---- 3. Histórico do 1º semestre --------------------------------------------
hist <- read_delim(file.path(DIR_DADOS, "UFES", "alunos_x_situacao_curso.csv"),
                   delim = "\t", quote = "", show_col_types = FALSE,
                   locale = locale(encoding = "latin1"), col_types = cols(.default = col_character())) |>
  mutate(ANO = suppressWarnings(as.numeric(ANO)), matricula_norm = norm_matr(MATR_ALUNO),
         periodo = str_squish(str_to_lower(PERIODO))) |>
  filter(SITUACAO %in% c("Reprovado por Nota", "Reprovado por Frequência",
                         "Reprovado sem Nota", "Aproveitamento de Estudos",
                         "Aprovado sem Nota", "Dispensa sem nota", "Dispensa com nota",
                         "Aprovado com nota - não usado", "Matrícula",
                         "Trancamento de Curso", "Amparo Legal", "Incompleto",
                         "Realizado", "Mobilidade Acadêmica"),
         !is.na(ANO), str_detect(matricula_norm, "^[0-9]{6,}")) |>
  # a matrícula base e a "X" podem repetir o mesmo evento: 1 registro por evento
  distinct(matricula_norm, ANO, periodo, SITUACAO, COD_DISCIPLINA, .keep_all = TRUE) |>
  inner_join(ing |> select(matricula_norm, `ANO INGRESSO`), by = "matricula_norm")
sem1 <- filter(hist, ANO == `ANO INGRESSO`, periodo == "1º semestre")
conta <- function(x, nome) count(x, matricula_norm, name = nome)

ing <- ing |>
  left_join(conta(filter(sem1, SITUACAO == "Reprovado por Frequência"), "n_reprov_freq_1osem"), by = "matricula_norm") |>
  left_join(conta(filter(sem1, SITUACAO == "Reprovado por Nota"), "n_reprov_nota_1osem"), by = "matricula_norm") |>
  left_join(conta(filter(hist, SITUACAO == "Aproveitamento de Estudos"), "n_aproveitamento"), by = "matricula_norm") |>
  left_join(conta(sem1, "n_registros_1osem"), by = "matricula_norm") |>
  mutate(across(c(n_reprov_freq_1osem, n_reprov_nota_1osem, n_aproveitamento, n_registros_1osem),
                ~ coalesce(.x, 0L)),
         reprov_freq_1osem = as.integer(n_reprov_freq_1osem > 0),
         reprov_nota_1osem = as.integer(n_reprov_nota_1osem > 0))

# ---- 4. Base analítica: com nota de seleção e desfecho definido -------------
escol <- c("Nunca estudou", "Fund. incompleto (até 4ª/5º)", "Fund. incompleto (até 8ª/9º)",
           "Médio incompleto", "Médio completo", "Superior incompleto/completo",
           "Pós-graduação", "Não sabe")
df <- ing |>
  filter(nota_faltando == FALSE, !is.na(TOTAL), !is.na(evadido_1ano)) |>
  mutate(
    Q002 = factor(Q002, levels = LETTERS[1:8], labels = escol),
    Q005 = as.numeric(Q005),
    Q006 = factor(Q006, levels = LETTERS[1:17], labels = names(SM_MEDIO)),
    TP_SEXO_m     = fct_relevel(factor(TP_SEXO, c("F", "M"), c("Feminino", "Masculino")), "Feminino"),
    TP_COR_RACA   = factor(TP_COR_RACA, levels = 0:5,
                           labels = c("Não declarado", "Branca", "Preta", "Parda", "Amarela", "Indígena")),
    TP_COR_RACA_m = TP_COR_RACA |>
      fct_collapse("Amarela/Indígena/Não declarado" = c("Amarela", "Indígena", "Não declarado")) |>
      fct_relevel("Branca"),
    TP_FAIXA_ETARIA   = factor(TP_FAIXA_ETARIA),
    CURSO_NORMALIZADO = fct_relevel(factor(CURSO_NORMALIZADO), "Estatística"),
    total_enem        = TOTAL)
saveRDS(df, file.path(DIR_GERADO, "base_modelo_evasao_1ano.rds"))

out(ing |> select(matricula, `ANO INGRESSO`, evadido_1ano, nota_faltando, TOTAL, n_aproveitamento),
    "base_cce_evasao_1ano.csv", dir = DIR_GERADO)
cat("Ingressantes CCE 1º sem.:", nrow(ing), "| base analítica:", nrow(df),
    "| evasões no 1º ano:", sum(df$evadido_1ano), "\n")
