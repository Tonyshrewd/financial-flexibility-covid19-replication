# ======================================================================================
# Step 3 - Daily excess returns and all return-based firm characteristics.
#
# Daily excess return (Appendix B): ln(1 + r - rf), r = daily simple total return from
# PRCCD adjusted by AJEXDI and TRFD; rf = 1-month T-bill rate per day.
# Window returns are compounded, exp(sum of daily log excess returns) - 1, when
# CFG$ret_type = "compound" (default; reproduces the distribution in the paper's Table 1),
# or left as the sum of log returns when CFG$ret_type = "log".
#
# Writes data/derived/ (shared by both samples)
#   returns_covid.csv  collapse / stimulus / rebound returns, 2019 beta, momentum, market
#                      cap, average price, security type, 4-factor CAR
#   returns_gfc.csv    the same for the 2008-09 windows with 2006 characteristics
#   daily_fig1.csv.gz  firm-day excess returns Feb 3 - Sep 11, 2020 (Figure 1)
# ======================================================================================
source("config.R"); source("code/lib.R")

s <- read_raw("secd")
s[, `:=`(gvkey = gvkey(gvkey), date = as.Date(datadate))]
setorder(s, gvkey, date)
s[, trfd := nafill(as.numeric(trfd), "locf"), by = gvkey]
s[is.na(trfd), trfd := 1]
s[, tri := prccd / ajexdi * trfd]
s[, `:=`(ret = tri / shift(tri) - 1, gap = as.numeric(date - shift(date))), by = gvkey]
s[is.na(gap) | gap > 10, ret := NA]                      # no returns across download gaps
rf <- read_raw("rf_daily")[, .(date = as.Date(date), rf)]
ff <- read_raw("ff_daily")[, date := as.Date(date)]
s <- merge(s, rf, by = "date", all.x = TRUE)
s[is.na(rf), rf := 0]
s[, exret := NA_real_]
s[!is.na(ret) & ret - rf > -1, exret := log1p(ret - rf)]   # bad prices (return <= -100%) left missing
s <- merge(s, ff, by = "date", all.x = TRUE)
s[, mkt := log1p(`Mkt-RF`)]
setkey(s, gvkey, date)
cat(sprintf("daily panel: %s firm-days, %s firms\n", format(nrow(s), big.mark = ","),
            format(uniqueN(s$gvkey), big.mark = ",")))

win_sum <- function(lo, hi, col = "exret") {
  s[date >= lo & date <= hi & !is.na(get(col)), .(v = sum(get(col)), n = .N), by = gvkey]
}

beta_est <- function(lo, hi) {
  s[date >= lo & date <= hi & !is.na(exret) & !is.na(mkt),
    .(beta = if (.N >= 60) cov(exret, mkt) / var(mkt) else NA_real_), by = gvkey]
}

# Footnote 9: loadings on Mkt, SMB, HML, UMD from 2017-2019; AR = r - b'f (no alpha).
factor_car <- function(est, win) {
  fac <- c("Mkt-RF", "SMB", "HML", "UMD")
  e <- s[date >= est[1] & date <= est[2] & complete.cases(s[, c("exret", fac), with = FALSE])]
  bet <- e[, {
    if (.N < 120) NULL else {
      X <- cbind(1, as.matrix(.SD[, fac, with = FALSE]))
      b <- qr.coef(qr(X), exret)[-1]
      as.list(setNames(b, c("b1", "b2", "b3", "b4")))
    }}, by = gvkey, .SDcols = c(fac, "exret")]
  w <- merge(s[date >= win[1] & date <= win[2] & !is.na(exret) & !is.na(UMD)], bet, by = "gvkey")
  w[, .(car4 = sum(exret - (b1 * `Mkt-RF` + b2 * SMB + b3 * HML + b4 * UMD))), by = gvkey]
}

firm_level <- function(year, windows, beta_win, price_win, est = NULL) {
  out <- data.table(gvkey = sort(unique(s$gvkey)))
  for (nm in names(windows)) {
    w <- win_sum(windows[[nm]][1], windows[[nm]][2])
    if (CFG$ret_type == "compound") w[, v := exp(v) - 1]
    setnames(w, c("gvkey", nm, paste0("n_", nm)))
    out <- merge(out, w, by = "gvkey", all.x = TRUE)
  }
  out <- merge(out, beta_est(beta_win[1], beta_win[2]), by = "gvkey", all.x = TRUE)
  m <- win_sum(as.Date(sprintf("%d-01-01", year)), as.Date(sprintf("%d-12-31", year)))
  m[, mom := if (CFG$momentum == "return") exp(v) - 1 else exp(v)]
  out <- merge(out, m[, .(gvkey, mom)], by = "gvkey", all.x = TRUE)
  yr <- s[year(date) == year]
  last <- yr[, .SD[.N], by = gvkey]
  out <- merge(out, last[, .(gvkey, mve = prccd * cshoc / 1e6)], by = "gvkey", all.x = TRUE)   # $ millions
  out <- merge(out, s[date >= price_win[1] & date <= price_win[2], .(avg_price = mean(prccd, na.rm = TRUE)), by = gvkey],
               by = "gvkey", all.x = TRUE)
  out <- merge(out, yr[, .(tpci = names(sort(table(as.character(tpci)), decreasing = TRUE))[1]), by = gvkey],
               by = "gvkey", all.x = TRUE)
  if (!is.null(est)) out <- merge(out, factor_car(est, windows$ret_collapse), by = "gvkey", all.x = TRUE)
  out
}

cov <- firm_level(CFG$fy_covid,
  list(ret_collapse = CFG$collapse, ret_stimulus = CFG$stimulus,
       ret_to_aug = c(CFG$collapse[1], CFG$rebound_end), ret_rebound = c(CFG$stimulus[1], CFG$rebound_end)),
  CFG$beta_covid, CFG$price_covid, est = CFG$factor_est)
fwrite(cov, file.path(CFG$derived_common, "returns_covid.csv"))

gfc <- firm_level(CFG$fy_gfc, list(ret_gfc_a = CFG$gfc_a, ret_gfc_b = CFG$gfc_b),
                  CFG$beta_gfc, CFG$price_gfc)
fwrite(gfc, file.path(CFG$derived_common, "returns_gfc.csv"))

fwrite(s[date >= CFG$fig1[1] & date <= CFG$fig1[2], .(gvkey, date, exret)],
       file.path(CFG$derived_common, "daily_fig1.csv.gz"))
cat(sprintf("returns_covid: %d firms with collapse returns; returns_gfc: %d\n",
            sum(!is.na(cov$ret_collapse)), sum(!is.na(gfc$ret_gfc_b))))
