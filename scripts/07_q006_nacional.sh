#!/usr/bin/env bash
# ==============================================================================
# 07. Q006 (renda familiar) no conjunto dos inscritos do ENEM, por edição.
# Mostra que o deslocamento de "1 a 1,5 SM" (C) para "até 1 SM" (B) a partir da
# edição de 2020 é nacional, e não erro de codificação da base. Saída: saidas/desc_q006_nacional.csv (edição, letra, n, pct entre
# os que responderam; respostas em branco, NA, sem pct).
# Uso: bash scripts/07_q006_nacional.sh   (a partir da raiz do projeto; ~2 min)
# ==============================================================================
set -euo pipefail
OUT=saidas/desc_q006_nacional.csv
echo "edicao,q006,n,pct" > "$OUT"
for y in 2017 2018 2019 2020 2021 2022 2023; do
  f=dados/brutos/MICRODADOS_ENEM_$y.csv
  i=$(head -1 "$f" | tr ';' '\n' | tr -d '"\r' | grep -nx 'Q006' | cut -d: -f1)
  cut -d';' -f"$i" "$f" | tail -n +2 | tr -d '"\r' | sort | uniq -c |
    awk -v y="$y" '{n[$2]=$1; if ($2 != "") t+=$1} END {for (k in n) printf "%s,%s,%d,%s\n", y, (k==""?"NA":k), n[k], (k==""?"":sprintf("%.2f", 100*n[k]/t))}' |
    sort -t, -k2,2 >> "$OUT"
done
# "Até 1 SM" com a mesma definição da Tabela 2 (nenhuma renda + até 1 SM = A+B)
awk -F, 'NR > 1 && ($2 == "A" || $2 == "B") {n[$1] += $3; p[$1] += $4} END {for (y in n) printf "%s,A+B,%d,%.2f\n", y, n[y], p[y]}' "$OUT" | sort >> "$OUT"
