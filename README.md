# Replication of Fahlenbrach, Rageth and Stulz (2021)

**"How Valuable Is Financial Flexibility when Revenue Stops? Evidence from the COVID-19 Crisis," *Review of Financial Studies* 34(11), 5474–5521.**

Anthony Lamini, Advanced Financial Theorem

## 1. Introduction

Fahlenbrach, Rageth and Stulz (FRS) show that U.S. firms with more financial flexibility at the end of 2019 lost less value when the COVID-19 pandemic collapsed revenues in February–March 2020. Financial flexibility here means more cash, less short-term debt and less long-term debt.

This package replicates the whole paper in R, starting from raw data:

- every table (1–10)
- Appendix A and Tables C1–C2
- Figures 1 and 2
- the robustness checks described in the text

The results are estimated on two samples:

| Sample | Definition | Firms |
|---|---|---|
| **Original-vintage** | Data the authors could see in spring 2020: fiscal-2019 year ends by Dec 31, 2019 and a 10-K filed by March 12, 2020 | about 1,840 (paper: 1,857) |
| **Extended** | Every firm with fiscal-2019 data in Compustat today | about 2,730 |

The report, `replication_report.pdf` (or the Word version, `replication_report.docx`), compares both samples with the paper and discusses how the results change in the extended sample.

### Results at a glance

Collapse-period regressions (Feb 3 – Mar 23, 2020), paper's Table 3 column 1 and Table 5 column 1, with t-statistics in parentheses:

| | Paper | Original-vintage sample | Extended sample |
|---|---|---|---|
| Firms | 1,857 | 1,843 | 2,729 |
| Cash / assets | 0.134\*\*\* (2.81) | 0.070 (1.64) | 0.038 (1.08) |
| Short-term debt / assets | −0.232\*\* (−2.03) | −0.226\*\* (−2.00) | −0.105 (−1.61) |
| Long-term debt / assets | −0.163\*\*\* (−5.28) | −0.174\*\*\* (−5.85) | −0.149\*\*\* (−5.36) |
| Cash × highly affected | 0.315\*\* (2.02) | 0.301\*\* (2.53) | 0.203\*\* (2.05) |

On the original-vintage sample, the debt results, the cash × COVID-exposure interaction, the operating-performance results and the conglomerate test all replicate. The stand-alone cash effect is weaker than in the paper. In the extended sample, the long-term debt and cash × exposure results hold, while the average cash and short-term debt effects fade.

**Read the results:** [`replication_report.pdf`](replication_report.pdf) (written report) · [`replication_results_original.pdf`](replication_results_original.pdf) and [`replication_results_extended.pdf`](replication_results_extended.pdf) (every table and figure in the paper's format) · `output/<sample>/tables/all_tables.xlsx` (all tables in Excel). The report includes the full results. The tables, figures and results PDFs in this repository were produced without the Hoberg–Maksimovic file, so they lack the two HM columns of Table 10; running `run_all.R` with internet access adds them.

> **Data note.** Compustat data are licensed through WRDS and are **not** included in this repository. `data/raw/` and `data/derived/` are empty, and `run_all.R` downloads the data with your own WRDS account. The public inputs in `data/external/` are included.

## 2. What is in this folder

| File / folder | Content |
|---|---|
| `replication_report.pdf` / `.docx` | Written report: the replication results compared with the paper and the extended-sample analysis. |
| `replication_results_original.pdf` | Every table and figure in the paper's order and format, for the original-vintage sample. |
| `replication_results_extended.pdf` | The same tables and figures for the extended sample. |
| `output/<sample>/tables/` | All tables as `.csv` and `.md`, plus `all_tables.xlsx`, one sheet per table. |
| `output/<sample>/figures/` | Figures 1 and 2 as PNG and PDF, with the data behind each. |
| `data/derived/` | Analysis datasets: one row per firm with every variable used in the regressions, for each sample (`<sample>/firm_covid.csv` and `<sample>/firm_gfc.csv`), plus the event-window returns (`returns_covid.csv`, `returns_gfc.csv`). The large daily-return file for Figure 1 is rebuilt by `run_all.R`. |
| `data/external/` | Hand-collected inputs: the Koren–Peto and Dingel–Neiman exposure data, Hoberg–Maksimovic constraints, and the paper's reported values used in the report. |
| `data/raw/` | Raw downloads from WRDS (Compustat) and public sources. Not in this repository; filled by `run_all.R`. |
| `code/` | The seven pipeline steps (see Section 4). |
| `run_all.R` | Runs the whole replication. |
| `config.R` | Every date, window, sample rule and modelling choice, in one place. |

## 3. How to replicate

**Requirements:** R 4.1 or later and RStudio. A WRDS account is needed only if `data/raw/` is empty.

1. Unzip the package. If you received `Lamini_FRS_raw_data.zip`, unzip it into the same parent folder; this fills `ffs_replication_R/data/raw/`.
2. Double-click **`ffs_replication_R.Rproj`**. RStudio opens in this folder.
3. Open **`run_all.R`** and click **Source**.

`run_all.R` then does the following:

- It installs any missing R packages through `00_setup.R`, including TinyTeX for the PDFs.
- If `data/raw/` is empty, it asks for a WRDS username and password in pop-up boxes and downloads Compustat. Approve the Duo request; the download takes 10–30 minutes. If the raw data is already there, this step is skipped and no login is needed.
- It builds both samples, estimates all tables and figures, and writes the three PDFs and the Word report.

With the raw data in place, the whole run takes about 3 minutes.

## 4. Replication procedure

| Step | Script | What it does |
|---|---|---|
| 1 | `code/01_pull_wrds.R` | Downloads Compustat data from WRDS: Fundamentals Annual (2003–2019) and Quarterly (2019–2020), Security Daily, S&P ratings, Segments and 10-K filing dates. |
| 2 | `code/02_pull_public.R` | Downloads the Fama-French daily factors and FF49 industries, the FRED 1-month T-bill rate, and the Hoberg–Maksimovic constraints data. |
| 3 | `code/03_returns.R` | Computes daily excess returns ln(1 + r − rf) and compounds them over each event window. Also computes beta, momentum, market value, the price filter and four-factor abnormal returns. |
| 4 | `code/04_build_sample.R` | Applies the Appendix A screens and the sample's data-vintage rule. Builds the Appendix B variables and the KZ, WW, SA and Huang–Ritter indexes. |
| 5 | `code/05_exposure.R` | Builds the COVID-19 exposure measures, the COVID-19 conglomerates, the financial-constraint indicators and the Figure 1 portfolios. |
| 6 | `code/06_tables.R` | Estimates every table: OLS with Fama-French 49 industry fixed effects, with t-statistics. |
| 7 | `code/07_figures.R` | Draws Figure 1 (portfolio returns) and Figure 2 (Merton calibration). |
| — | `replication_results.Rmd` | Formats each sample's tables in the paper's style, producing `replication_results_<sample>.pdf`. |
| — | `replication_report.Rmd` | Builds the written report; every number in it is read from the outputs above. |

Steps 1–3 are shared by both samples. Steps 4–7 run once for each sample.

## 5. Main implementation choices

All of these are set in `config.R`.

1. **Returns are compounded.** Each window return is exp(Σ ln(1 + r − rf)) − 1, a buy-and-hold excess return. This reproduces the distribution in the paper's Table 1, including the minimum of −0.905, which a sum of log returns cannot. Figure 1 plots cumulative log returns, as in the paper.
2. **Original-vintage sample.** The 10-K filing cutoff of March 12, 2020 is calibrated so that the sample size, mean return and firm size match the paper's Appendix A and Table 1.
3. **Winsorizing.** Cash, payouts, capex, momentum, ln(MVE) and profitability are top-coded at the 99th percentile, as the paper's Table 1 implies. Debt ratios, SG&A/sales and B/M are winsorized at the 1st and 99th percentiles, and so are ROA and lagged ROA.
4. **Ratings** are S&P long-term issuer ratings as of February 2017, as in the paper's Appendix B.
5. **Momentum** is the 2019 stock return. The paper's reported momentum values are inconsistent with its own definition.
6. **Not available.** Three Table 5 exposure measures need data that isn't public: the Frésard–Hoberg–Phillips and BEA input–output adjustments, and Moody's industry list.

## 6. Data sources

- WRDS: Compustat North America (subscription required).
- Kenneth French data library; FRED series DGS1MO.
- Koren and Peto (2020): `github.com/ceumicrodata/social-distancing`.
- Dingel and Neiman (2020): `github.com/jdingel/DingelNeiman-workathome`.
- Hoberg and Maksimovic (2015): `faculty.marshall.usc.edu/Gerard-Hoberg/MaxDataSite`.

Compustat data are licensed through WRDS and may only be shared with people covered by a WRDS subscription.
