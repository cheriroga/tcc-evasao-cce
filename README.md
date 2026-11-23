# Evasão no 1º ano nos cursos de Ciências Exatas do CCE/UFES

Código, tabelas, figuras e fonte LaTeX do Trabalho de Conclusão de Curso (Estatística, UFES).

## Estrutura

| Pasta / arquivo | Conteúdo |
|---|---|
| `main.tex`, `referencias.bib`, `apalike-br.bst`, `latexmkrc` | Fonte do documento |
| `template_tex/` | Template e logos do Departamento de Estatística |
| `declaracao_de_uso_de_ai.pdf` | Formulário do colegiado, incluído no documento |
| `scripts/` | Análise, na ordem de execução (00 a 11) |
| `saidas/` | Resultados em CSV; todo número do texto vem daqui ou de `tabelas/` |
| `tabelas/`, `figuras/` | Gerados por `scripts/08_tabelas_figuras.R` |
| `run_all.sh` | Reproduz tudo, dos dados ao `main.pdf` |

## Dados

`dados/` não está no repositório: as bases cedidas pela Prograd têm nome, matrícula e CPF. Os arquivos são enviados à parte e ficam assim:

```
dados/
├── brutos/
│   ├── UFES/INGRESSANTES_SISU_2018_2026.csv
│   ├── UFES/alunos_x_situacao_curso.csv
│   ├── EVADIDOS/alunos_evadidos_2015_2025.csv
│   ├── INEP_TRAJETORIA/indicadores_trajetoria_es_2019_2024.zip
│   └── MICRODADOS_ENEM_2017.csv ... MICRODADOS_ENEM_2023.csv   (opcional)
└── gerados/
    └── enem_filtrado_2018.csv ... enem_filtrado_2024.csv
```

Os microdados completos do Enem (edições 2017 a 2023, cerca de 14 GB) são públicos, em <https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem>. Sem eles, `run_all.sh` parte dos recortes em `dados/gerados/enem_filtrado_<ano>.csv` (linhas do Enem cujas cinco notas coincidem com as de algum ingressante Sisu/UFES) e mantém `saidas/desc_q006_nacional.csv`, a única saída que usa o Enem inteiro.

## Como rodar

A partir da raiz do repositório:

```
bash run_all.sh
```

Requisitos: R (4.6.1; pacotes `tidyverse`, `data.table`, `readxl`, `sandwich`, `lmtest`, `mice`, `splines`), Python 3, LaTeX com `latexmk` e `pdftocairo` (poppler). A versão de cada pacote está em `saidas/sessionInfo.txt`. A simulação de `scripts/10_cobertura_auc.R` leva cerca de 20 minutos.
