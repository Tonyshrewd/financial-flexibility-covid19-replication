# ======================================================================================
# Central configuration for the replication of
# Fahlenbrach, Rageth and Stulz (2021), "How Valuable Is Financial Flexibility when
# Revenue Stops? Evidence from the COVID-19 Crisis", Review of Financial Studies.
#
# Every date, window and judgement call used anywhere in the code lives here.  Items
# marked [ASSUMPTION] are not pinned down by the paper; the comment says what we chose.
# All scripts are run from the package root (run_all.R does this for you).
# ======================================================================================

CFG <- list()

# ---------------------------------------------------------------------------- sample
# Two samples are analysed (run_all.R runs both):
#   "original"  - mimics the data the authors had in April 2020: fiscal-2019 year ends on or
#                 before Dec 31, 2019 and a 10-K filed by CFG$vintage_cutoff. The cutoff
#                 (March 12, 2020) is calibrated so that the sample matches the paper's
#                 Appendix A (1,843 firms vs. 1,857) and Table 1 (mean collapse return
#                 -0.373 vs. -0.378; median assets $1.2bn vs. $1.4bn).
#   "extended"  - every firm with fiscal-2019 data in Compustat today, including late filers
#                 and firms with fiscal years ending January-May 2020.
CFG$sample <- Sys.getenv("FFS_SAMPLE", "original")
if (!CFG$sample %in% c("original", "extended")) stop("FFS_SAMPLE must be 'original' or 'extended'")
CFG$sample_label <- c(original = "Original-vintage sample", extended = "Extended sample")[[CFG$sample]]

# ---------------------------------------------------------------------------- paths
# FFS_WORKDIR redirects data and output (used by tests/ to run on simulated data).
CFG$work    <- Sys.getenv("FFS_WORKDIR", ".")
CFG$raw     <- file.path(CFG$work, "data", "raw")       # WRDS + public downloads
CFG$ext     <- Sys.getenv("FFS_EXTDIR", file.path(".", "data", "external"))  # hand-supplied
CFG$derived_common <- file.path(CFG$work, "data", "derived")             # returns (both samples)
CFG$derived <- file.path(CFG$work, "data", "derived", CFG$sample)        # firm-level analysis data
CFG$tables  <- file.path(CFG$work, "output", CFG$sample, "tables")
CFG$figures <- file.path(CFG$work, "output", CFG$sample, "figures")
for (p in c(CFG$raw, CFG$derived_common, CFG$derived, CFG$tables, CFG$figures))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------- WRDS
CFG$wrds_username <- Sys.getenv("WRDS_USERNAME", "")   # or type it here

# ---------------------------------------------------------------------------- fiscal years
CFG$fy_covid <- 2019L        # balance-sheet year, COVID-19 analysis
CFG$fy_gfc   <- 2006L        # "All firm characteristics are observed in 2006" (Sec. 3.2)

# Data vintage (original sample only; see "sample" above). NA = no vintage restriction.
CFG$vintage_cutoff <- if (CFG$sample == "original") as.Date("2020-03-12") else as.Date(NA)
CFG$fye_max        <- if (CFG$sample == "original") as.Date("2019-12-31") else as.Date(NA)

# ---------------------------------------------------------------------------- returns
# "compound": window return = exp(sum of daily log excess returns) - 1 (buy-and-hold excess
# return). This reproduces the paper's Table 1 (collapse-period mean -0.378, min -0.905,
# P1 -0.826; GFC minimum -0.996), which a sum of log returns cannot (unbounded below -1).
# "log": sum of daily log excess returns.
CFG$ret_type <- "compound"

# ---------------------------------------------------------------------------- windows
# Feb 2, 2020 (Table 1 / Sec. 2) was a Sunday; Feb 3 is the first trading day.
CFG$collapse    <- as.Date(c("2020-02-03", "2020-03-23"))
CFG$stimulus    <- as.Date(c("2020-03-24", "2020-03-24"))
CFG$rebound_end <- as.Date("2020-08-31")                  # "by the end of August" (Sec. 3.3)
CFG$fig1        <- as.Date(c("2020-02-03", "2020-09-11"))
CFG$gfc_a       <- as.Date(c("2008-09-15", "2008-12-31"))
CFG$gfc_b       <- as.Date(c("2008-09-15", "2009-03-09"))  # CRSP EW index low

CFG$beta_covid   <- as.Date(c("2019-01-02", "2019-12-31"))
CFG$beta_gfc     <- as.Date(c("2006-01-01", "2006-12-31"))
CFG$factor_est   <- as.Date(c("2017-01-01", "2019-12-31"))  # footnote 9

# [ASSUMPTION] "An average share price < $1": averaging window not stated -> calendar year.
CFG$price_covid <- as.Date(c("2019-01-01", "2019-12-31"))
CFG$price_gfc   <- as.Date(c("2006-01-01", "2006-12-31"))

# Security Daily download ranges
CFG$secd_ranges <- list(c("2016-12-01", "2020-09-30"), c("2005-12-01", "2009-03-31"))

# ---------------------------------------------------------------------------- risk-free
# "1-month Treasury-bill rate from the St. Louis Fed": FRED DGS1MO (% p.a.).
# [ASSUMPTION] daily rate = annual / 100 / 360.
CFG$rf_series  <- "DGS1MO"
CFG$rf_daycount <- 360

# ---------------------------------------------------------------------------- ratings
# Appendix B: S&P long-term issuer rating "at the end of February 2017" (Compustat's
# S&P ratings feed ends in early 2017).
CFG$rating_covid <- as.Date("2017-02-28")
CFG$rating_gfc   <- as.Date("2006-12-31")

# ---------------------------------------------------------------------------- momentum
# Appendix B: exp(sum of 2019 daily log excess returns). Table 1 reports mean -0.832 and
# max -0.241, inconsistent with that formula. Captions say "stock return in calendar year
# 2019" -> default exp(sum) - 1.  Options: "return" or "gross" (literal Appendix B).
CFG$momentum <- "return"

# ---------------------------------------------------------------------------- winsorizing
# [ASSUMPTION] Not described in the paper. In Table 1, max = P99 for exactly these
# variables, implying top-coding at P99. Set to character(0) to use raw values
# (the robustness table also reports that variant).
CFG$winsor_top  <- c("cash", "payout", "capex", "mom", "lnmve", "prof")
CFG$winsor_both <- c("hm", "std", "nstd", "bookdebt", "ltd", "sga", "bm")   # two-sided 1%/99%: extreme debt, SG&A and B/M ratios

# Table 4: minimum firms for an industry to be ranked. [ASSUMPTION] not stated.
CFG$min_firms_t4 <- 1L

# ---------------------------------------------------------------------------- SA index
# Size = book assets in 2004 dollars capped at $4.5bn; age capped at 37 years.
CFG$cpi <- c("2004" = 188.9, "2006" = 201.6, "2019" = 255.7)   # CPI-U annual averages
CFG$sa_size_cap <- 4500
CFG$sa_age_cap  <- 37

CFG$top_upstream <- 3L   # "top-three upstream industries"

# ---------------------------------------------------------------------------- variables
CFG$ap   <- c("beta", "bm", "lnmve", "mom", "prof")              # asset-pricing chars
CFG$cost <- c("ig", "capex", "sga", "cogs", "rd", "payout")      # Table 3 order
CFG$labels <- c(
  cash = "Cash / assets", std = "St debt / assets", nstd = "nSt debt / assets",
  ltd = "Lt debt / assets", bookdebt = "Book debt / assets", ig = "Ind: IG debt rating",
  hy = "Ind: HY debt rating", capex = "Capex / lagged assets", sga = "SGA / sales",
  cogs = "COGS / sales", rd = "RD / assets", payout = "Payout / assets",
  beta = "Equity beta", bm = "BM at EoY", lnmve = "ln(MVE) at EoY", mom = "Momentum",
  prof = "Profitability", assets = "Assets ($m)",
  payout3_tq = "Ind: 3-year cum. payout / assets top quartile",
  conglo = "Ind: COVID-19 conglomerate",
  ret_collapse = "Cum. daily exc. returns (Feb 3 - Mar 23, 2020)",
  ret_stimulus = "Daily exc. returns (March 24, 2020)",
  roa_w = "ROA (cumulative Q1 + Q2 2020)", kz = "KZ index", fc_kz = "FC (KZ index)",
  ww = "WW index", fc_ww = "FC (WW index)", sa = "SA index", fc_sa = "FC (SA index)",
  hr_cash = "HR cash ex ante", fc_hr = "FC (HR cash ex ante)", hm = "HM measure",
  fc_hm = "FC (HM measure)", roa_lag = "Lagged ROA: Q1+Q2 2019 / assets EoFY18",
  age = "Firm age", ppe = "PPE / assets", credit = "Unused credit lines / assets",
  payout3_avg = "3-yr avg payout / assets", payout_ni = "Payout / (NI - XI)")

# The 11 COVID-19 exposure measures, in Table 5 / Appendix C order
CFG$exposures <- c(
  x_affected = "Affected share (TQ)", x_teamwork = "Teamwork share (TQ)",
  x_customer = "Customer share (TQ)", x_presence = "Presence share (TQ)",
  x_fhp = "Affected share corrected for Fresard, Hoberg, and Phillips (TQ)",
  x_bea = "Affected share corrected for BEA USE table (TQ)",
  x_sales = "Q2 2020 year-on-year change in sales (BQ)",
  x_home = "Share of jobs that can be performed from home (BQ)",
  x_labor = "Labor intensity based on number of employees (TQ)",
  x_manual = "Manually classified as highly affected",
  x_moodys = "Ind: COVID-19 industries (Moody's)")
