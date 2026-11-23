# ==============================================================================
# 06. CONTEXTO NACIONAL — desistência acumulada por ano desde o ingresso
# ------------------------------------------------------------------------------
# Fonte: INEP, Indicadores de Trajetória da Educação Superior, coorte 2019-2024
#   https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/indicadores_trajetoria_es_2019_2024.zip
#   (o servidor do INEP não envia o certificado intermediário; baixar com
#    curl --cacert <bundle do sistema + RNP ICPEdu GR46 OV TLS CA 2025>)
# Desistência = desvinculado do curso ou transferido para outro curso da mesma
# IES; trancados contam como permanência (INEP, 2017). "Ano 1" = ano civil de
# ingresso, situação na data de referência do Censo.
# Saída: saidas/art_inep_trajetoria.csv
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(readxl) })
source("scripts/00_util.R")

zip <- file.path(DIR_DADOS, "INEP_TRAJETORIA", "indicadores_trajetoria_es_2019_2024.zip")
f   <- unzip(zip, files = "indicadores_trajetoria_educacao_superior_2019_2024.xlsx", exdir = tempdir())
cab <- read_excel(f, n_max = 15, col_names = FALSE, .name_repair = "minimal")
x   <- as.data.table(read_excel(f, skip = which(apply(cab, 1, \(z) "CO_IES" %in% z)) - 1))

# universidades federais, presenciais
x <- x[TP_MODALIDADE_ENSINO == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1 & TP_ORGANIZACAO_ACADEMICA == 1]
x[, ano   := NU_ANO_REFERENCIA - NU_ANO_INGRESSO + 1]
x[, grupo := fifelse(grepl("^(Matemática|Física|Química|Estatística)", NO_CINE_ROTULO),
                     "Matemática, Física, Química e Estatística", "Demais cursos")]

res <- x[, .(ingressantes = sum(QT_INGRESSANTE[ano == 1]),
             desist = as.numeric(tapply(QT_DESISTENCIA, factor(ano, 1:5), sum))), by = grupo]
res[, `:=`(ano = 1:5, tda = round(100 * cumsum(desist) / ingressantes, 1)), by = grupo]
print(dcast(res, grupo + ingressantes ~ ano, value.var = "tda"))
out(res, "art_inep_trajetoria.csv")
# Mesma série para a UFES (campus de Vitória), para comparar fonte com fonte
uf <- x[NO_IES == "UNIVERSIDADE FEDERAL DO ESPÍRITO SANTO" & CO_MUNICIPIO == 3205309 & grupo != "Demais cursos"]
uf <- uf[, .(ingressantes = sum(uf[ano == 1]$QT_INGRESSANTE), desist = sum(QT_DESISTENCIA)), by = ano][order(ano)]
uf[, tda := round(100 * cumsum(desist) / ingressantes, 1)]
out(uf, "art_inep_trajetoria_ufes.csv")

# UFES, campus de Vitória, mesmos rótulos (referência para a Discussão)
u <- x[NO_IES == "UNIVERSIDADE FEDERAL DO ESPÍRITO SANTO" & CO_MUNICIPIO == 3205309 &
       grupo != "Demais cursos" & ano == 1]
cat(sprintf("UFES Vitória, Exatas, ano de ingresso 2019: %d cursos, %d ingressantes, %.1f%% de desistência\n",
            nrow(u), sum(u$QT_INGRESSANTE), 100 * sum(u$QT_DESISTENCIA) / sum(u$QT_INGRESSANTE)))
