# Hand-supplied inputs

Lines starting with `#` are comments. A file with no data rows is skipped, and so are the measures that depend on it.

| File | Status | Columns |
|---|---|---|
| `koren_peto.csv` | **Included.** Koren and Peto (2020), from github.com/ceumicrodata/social-distancing | `naics3, affected_share, teamwork_share, customer_share, presence_share` |
| `dingel_neiman.csv` | **Included.** Dingel and Neiman (2020), from github.com/jdingel/DingelNeiman-workathome | `naics3, teleworkable` |
| `manual_naics6.csv` | **Included.** Contains the paper's two examples; you can extend it | `naics6, affected` (1 or 0) |
| `hm_constraints.csv` | **Filled automatically** by `code/02_pull_public.R` | `gvkey, year, delaycon` |
| `io_bea.csv`, `io_fhp.csv` | Empty (data not available) | `supplier_naics3, customer_naics3, value` |
| `moodys_ff49.csv` | Empty (Moody's subscriber report) | `ff49` |
| `credit_lines.csv` | Optional alternative to the Capital IQ pull | `gvkey, undrawn_credit_usd_m` |

`paper_values.csv` holds the coefficients and statistics reported in the published paper, transcribed from its tables. It is used only by `replication_report.Rmd` to compare the paper with the replication.
