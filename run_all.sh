#!/usr/bin/env bash
# Reproduz o TCC do zero: dados/brutos -> dados/gerados -> saidas, tabelas, figuras -> main.pdf.
# Cada script roda em sessão limpa, a partir da raiz. Uso: bash run_all.sh
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p dados/gerados saidas tabelas figuras

# Sem os microdados completos do Enem (14 GB, edições 2017 a 2023), o passo 01 usa os
# recortes já filtrados em dados/gerados/enem_filtrado_<ano>.csv, e o passo 07 mantém
# saidas/desc_q006_nacional.csv.
for ano in 2018 2019 2020 2021 2022 2023 2024; do   # = ANOS_BASE em scripts/00_util.R
  if [ -f "dados/brutos/MICRODADOS_ENEM_$((ano - 1)).csv" ]; then
    python3 -I scripts/01_filtra_enem.py "$ano"
  else
    echo "sem MICRODADOS_ENEM_$((ano - 1)).csv: usa dados/gerados/enem_filtrado_$ano.csv"
    test -f "dados/gerados/enem_filtrado_$ano.csv"
  fi
  Rscript scripts/02_liga_enem.R "$ano"
done
Rscript scripts/03_base_analitica.R
Rscript scripts/04_descritiva.R
Rscript scripts/05_modelo.R
Rscript scripts/09_erro_monte_carlo.R   # depois do 05: reaproveita o código dele
Rscript scripts/10_cobertura_auc.R      # cobertura dos intervalos da AUC (simulação; ~20 min)
Rscript scripts/11_formas_hc.R          # formas HC0 a HC3 no modelo principal
Rscript scripts/06_inep_trajetoria.R
if ls dados/brutos/MICRODADOS_ENEM_*.csv >/dev/null 2>&1; then
  bash scripts/07_q006_nacional.sh
else
  echo "sem os microdados do Enem: mantém saidas/desc_q006_nacional.csv"
fi
Rscript scripts/08_tabelas_figuras.R

Rscript -e 'suppressPackageStartupMessages({library(sandwich); library(lmtest); library(mice); library(splines)}); writeLines(capture.output(sessionInfo()), "saidas/sessionInfo.txt")'   # com os pacotes citados no texto carregados
latexmk -silent main.tex
