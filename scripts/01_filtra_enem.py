#!/usr/bin/env python3
"""Filtra os microdados do ENEM (edição = ano de ingresso - 1) às linhas cujas
cinco notas coincidem com as de algum ingressante SISU/UFES do ano.
Uso: python3 -I scripts/01_filtra_enem.py <ano_ingresso>
Saída: dados/gerados/enem_filtrado_<ano>.csv (mesmas colunas do ENEM)."""
import csv, sys, io
ano = int(sys.argv[1]); base = "dados/brutos"

def dec1(s):  # "561,1"/"534.8" -> 5611 ; "600" -> 6000 ; "" -> None
    s = s.strip().replace(",", ".")
    if not s or s in ("NA", "[Null]"): return None
    if "." in s:
        a, b = s.split("."); b = (b + "0")[:1]
        return int(a) * 10 + int(b)
    return int(s) * 10

keys = set()
with open(f"{base}/UFES/INGRESSANTES_SISU_2018_2026.csv", encoding="utf-8-sig") as f:
    for row in csv.DictReader(f, delimiter=";"):
        if row["ANO INGRESSO"] != str(ano): continue
        v = [dec1(row[c]) for c in ("CIÊNCIAS DA NATUREZA", "CIÊNCIAS HUMANAS", "LINGUAGENS", "MATEMÁTICA", "REDAÇÃO")]
        if None not in v: keys.add("_".join(map(str, v)))
print("chaves UFES:", len(keys))

src = f"{base}/MICRODADOS_ENEM_{ano-1}.csv"; dst = f"dados/gerados/enem_filtrado_{ano}.csv"
n = kept = 0
with open(src, "rb") as fin, open(dst, "wb") as fout:
    hdr = fin.readline(); fout.write(hdr)
    cols = hdr.decode("latin-1").rstrip("\r\n").split(";")
    idx = [cols.index(c) for c in ("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]
    for line in fin:
        n += 1
        p = line.split(b";")
        try: v = [dec1(p[i].decode("ascii")) for i in idx]
        except (ValueError, IndexError): continue
        if None in v: continue
        if "_".join(map(str, v)) in keys:
            fout.write(line); kept += 1
print(f"ENEM {ano-1}: {n} linhas lidas, {kept} mantidas -> {dst}")
