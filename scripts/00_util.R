# ==============================================================================
# 00. UTIL: caminhos, coortes, recodificações do modelo, E/S, LaTeX e figuras
# Uso em qualquer script (a partir da raiz): source("scripts/00_util.R")
# ==============================================================================
suppressPackageStartupMessages({ library(stringr); library(ggplot2) })

# --- Caminhos (relativos à raiz do projeto) -----------------------------------
DIR_DADOS  <- "dados/brutos"
DIR_CSV    <- "saidas"
DIR_FIG    <- "figuras"
DIR_TEX    <- "tabelas"
DIR_GERADO <- "dados/gerados"
# Coortes: ANOS_BASE = cadastro ligado ao ENEM (01_filtra_enem.py + 02_liga_enem.R);
# ANOS_ANALISE = desfecho de 1 ano. 2018 fica fora da análise principal: a
# convenção de registro das saídas difere (período invertido, saídas deslocadas
# para 2019) e Matemática entrou por ABI; fica só no fluxograma (excluída de todos os modelos).
ANOS_BASE    <- 2018:2024
ANOS_ANALISE <- 2019:2024
for (d in c(DIR_CSV, DIR_FIG, DIR_TEX, DIR_GERADO))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- Normalização de matrícula (sufixo "X" = mudança de PPC/adaptação) --------
norm_matr <- function(x) sub("X$", "", toupper(gsub("[^0-9A-Za-z]", "", x)))

# --- Escrita de CSV -----------------------------------------------------------
out <- function(x, f, dir = DIR_CSV) write.csv(x, file.path(dir, f), row.names = FALSE)

# --- Formatação LaTeX (padrão brasileiro: vírgula decimal) --------------------
fmt <- function(x, dg = 2) ifelse(is.na(x), "---",
  formatC(round(as.numeric(x), dg), format = "f", digits = dg, decimal.mark = ","))
fmtp <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), "---",
         ifelse(p < 0.001, "<0,001",
                # 3 casas abaixo de 0,10 (perto do limiar), 2 acima: o padrão do texto
                ifelse(p < 0.10, formatC(round(p, 3), format = "f", digits = 3, decimal.mark = ","),
                       formatC(round(p, 2), format = "f", digits = 2, decimal.mark = ","))))
}
esc <- function(x) {
  x <- as.character(x)
  for (a in list(c("%", "\\%"), c("&", "\\&"), c("_", "\\_"), c("#", "\\#"),
                 c("≤", "$\\leq$"), c("≥", "$\\geq$"),
                 c("±", "$\\pm$"), c("–", "--"), c("—", "---"),
                 c("·", "\\textperiodcentered{}")))
    x <- str_replace_all(x, fixed(a[1]), a[2])
  x
}
blank_repeats <- function(x) {
  x <- as.character(x)
  if (length(x) > 1) {
    last <- x[1]
    for (i in 2:length(x)) if (x[i] == last) x[i] <- "" else last <- x[i]
  }
  x
}
rr_ci <- function(rr, lo, hi) paste0(fmt(rr), " (", fmt(lo), "--", fmt(hi), ")")

# --- Renda em SM: ponto médio das 17 faixas da Q006 ---------------------------
SM_MEDIO <- c(
  "Nenhuma renda" = 0,    "Até 1 SM"      = 1,    "1 a 1,5 SM"    = 1.25,
  "1,5 a 2 SM"    = 1.75, "2 a 2,5 SM"    = 2.25, "2,5 a 3 SM"    = 2.75,
  "3 a 4 SM"      = 3.5,  "4 a 5 SM"      = 4.5,  "5 a 6 SM"      = 5.5,
  "6 a 7 SM"      = 6.5,  "7 a 8 SM"      = 7.5,  "8 a 9 SM"      = 8.5,
  "9 a 10 SM"     = 9.5,  "10 a 12 SM"    = 11,   "12 a 15 SM"    = 13.5,
  "15 a 20 SM"    = 17.5, "Mais de 20 SM" = 20)   # faixa aberta: 20 SM

# --- Recodificações ------------------------------------------------------------
#   cotista        = modalidade fora de ampla concorrência (NOPT/AC)
#   concluiu_antes = TP_ST_CONCLUSAO == 1 (o "Não respondeu" de TP_ESCOLA é isso)
prep_artigo <- function(d) {
  d |> dplyr::mutate(
    coorte         = factor(`ANO INGRESSO`),
    curso          = relevel(factor(as.character(CURSO_NORMALIZADO)), "Estatística"),
    cotista        = factor(!MODALIDADE %in% c("NOPT", "AC"), c(FALSE, TRUE), c("Não", "Sim")),
    # Reserva com e sem critério de renda (BR*: baixa renda até 2023; LB*: idem em 2024;
    # RN*/LI*: renda não considerada); só descritivo e sensibilidade
    reserva        = factor(dplyr::case_when(MODALIDADE %in% c("NOPT", "AC") ~ "AC",
                                             grepl("^(BR|LB)", MODALIDADE) ~ "Reserva com critério de renda",
                                             TRUE ~ "Reserva sem critério de renda")),
    concluiu_antes = factor(as.character(TP_ST_CONCLUSAO) == "1", c(FALSE, TRUE), c("Não", "Sim")),
    renda_sm       = unname(SM_MEDIO[as.character(Q006)]),
    renda_ate1sm   = factor(renda_sm <= 1, c(FALSE, TRUE), c("Não", "Sim")),
    renda3         = factor(dplyr::case_when(renda_sm <= 1 ~ "≤1 SM", renda_sm <= 2 ~ "1–2 SM",
                                             !is.na(renda_sm) ~ ">2 SM"), c("≤1 SM", "1–2 SM", ">2 SM")),
    ppi            = factor(ifelse(is.na(TP_COR_RACA), NA, TP_COR_RACA %in% c("Preta", "Parda", "Indígena")),
                            c(FALSE, TRUE), c("Não", "Sim")),
    sexo           = TP_SEXO_m,
    cor_raca       = TP_COR_RACA_m,
    enem_100       = total_enem / 100,
    mt_100         = NU_NOTA_MT / 100,
    cn_100         = NU_NOTA_CN / 100,
    # Renda em 3 faixas (referência > 2 SM; faixas vindas da fase exploratória)
    renda3m        = relevel(renda3, ">2 SM"),
    # Renda familiar per capita e critério da Lei 12.711 (≤ 1,5 SM per capita)
    renda_pc       = renda_sm / pmax(as.numeric(Q005), 1),
    renda_pc15     = factor(renda_pc <= 1.5, c(FALSE, TRUE), c("> 1,5 SM", "≤ 1,5 SM")),
    # Origem = modalidade × escola (cotista e escola são colineares; uma variável só)
    origem         = relevel(factor(dplyr::case_when(
      cotista == "Não" & TP_DEPENDENCIA_ADM_ESC == 4     ~ "AC, escola privada",
      cotista == "Não" & TP_DEPENDENCIA_ADM_ESC %in% 1:3 ~ "AC, escola pública",
      cotista == "Não"                                   ~ "AC, egresso sem informação",
      TP_DEPENDENCIA_ADM_ESC %in% 2:3                    ~ "Cotista, escola estadual/municipal",
      TP_DEPENDENCIA_ADM_ESC == 1                        ~ "Cotista, escola federal",
      TRUE                                               ~ "Cotista, egresso")),
      "AC, escola privada"),  # referência; a priori era AC escola pública
    # Curso em 7 categorias: "Estatística e Ciência de Dados" é o mesmo curso após
    # mudança de PPC — rótulo atribuído a quem permaneceu (0 evasões por construção)
    curso7         = relevel(factor(dplyr::case_when(
      grepl("^Estatística", CURSO) ~ "Estatística",
      # 2018: ingresso por ABI; bacharelado/licenciatura só atribuídos a quem ficou
      coorte == "2018" & grepl("Matemática", CURSO) ~ "Matemática (ABI)",
      TRUE ~ as.character(CURSO))), "Estatística"),
    # Sensibilidade: escolaridade da mãe (3 níveis) e idade no ENEM (3 faixas)
    esc_mae        = factor(dplyr::case_when(
      Q002 %in% c("Nunca estudou", "Fund. incompleto (até 4ª/5º)", "Fund. incompleto (até 8ª/9º)",
                  "Médio incompleto") ~ "Até médio incompleto",
      Q002 == "Médio completo" ~ "Médio completo",
      Q002 %in% c("Superior incompleto/completo", "Pós-graduação") ~ "Superior"),
      c("Médio completo", "Até médio incompleto", "Superior")),
    idade3         = factor(dplyr::case_when(
      as.numeric(as.character(TP_FAIXA_ETARIA)) <= 3 ~ "até 18",
      as.numeric(as.character(TP_FAIXA_ETARIA)) <= 5 ~ "19-20",
      !is.na(TP_FAIXA_ETARIA) ~ "21 ou mais"), c("até 18", "19-20", "21 ou mais"))
  )
}

# --- Figuras: tema e exportação (PNG + EPS) -----------------------------------
tema_artigo <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title    = element_text(face = "bold", size = base_size, hjust = 0, color = "black"),
      plot.subtitle = element_text(size = base_size - 1, color = "grey35"),
      axis.title    = element_text(size = base_size - 1, color = "black"),
      axis.text     = element_text(size = base_size - 2, color = "black"),
      legend.title  = element_text(size = base_size - 1),
      legend.text   = element_text(size = base_size - 2),
      strip.text    = element_text(size = base_size - 1, face = "bold"),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      legend.key        = element_blank())
}
salvar_figura <- function(g, arquivo, largura, altura, dpi = 300) {
  ggsave(arquivo, g, width = largura, height = altura, dpi = dpi)
  ggsave(sub("\\.png$", ".eps", arquivo), g, width = largura, height = altura, device = grDevices::cairo_ps)
  invisible(g)
}
