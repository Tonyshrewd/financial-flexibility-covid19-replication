# ======================================================================================
# Step 4 - Sample selection (Appendix A) and firm characteristics (Appendix B), plus the
# financial-constraint indexes of Section 2 (KZ, WW, SA, Huang-Ritter, Hoberg-Maksimovic).
#
# Writes data/derived/<sample>/firm_covid_base.csv (fiscal 2019) and firm_gfc.csv (fiscal 2006),
# and output/tables/appendix_a* (the sample-selection funnel).
# ======================================================================================
source("config.R"); source("code/lib.R")

funda <- read_raw("funda")
funda[, `:=`(gvkey = gvkey(gvkey), datadate = as.Date(datadate))]
comp <- unique(read_raw("company")[, gvkey := gvkey(gvkey)], by = "gvkey")
first <- read_raw("firstyear")[, gvkey := gvkey(gvkey)]

by_year <- function(fy) {          # one row per gvkey for fiscal year fy (latest datadate)
  d <- funda[fyear == fy][order(gvkey, datadate)]
  d[, .SD[.N], by = gvkey]
}

ratings_at <- function(date) {
  r <- read_raw("ratings")[, `:=`(gvkey = gvkey(gvkey), datadate = as.Date(datadate))]
  r <- r[datadate <= date & datadate > seq(date, by = "-12 months", length.out = 2)[2] &
         !is.na(splticrm) & splticrm != ""][order(gvkey, datadate)][, .SD[.N], by = gvkey]
  f <- rating_flags(r$splticrm)
  data.table(gvkey = r$gvkey, ig = f$ig, hy = f$hy)
}

nz <- function(x) { x <- as.numeric(x); x[is.na(x) | x < 0] <- 0; x }   # missing/negative -> 0
z0 <- function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x }

build <- function(fy, ret_file, rating_date) {
  rets <- fread(file.path(CFG$derived_common, ret_file))[, gvkey := gvkey(gvkey)]
  funnel <- list()
  add <- function(step, n) funnel[[length(funnel) + 1]] <<- data.table(Step = step, Firms = n)

  cur <- funda[fyear == fy]
  add(sprintf("Number of firms with fiscal year %d data", fy), uniqueN(cur$gvkey))

  # data-vintage filter (original sample, COVID year only): keep fiscal years ending by
  # CFG$fye_max whose 10-K was filed by CFG$vintage_cutoff (firms without a recorded filing
  # date are treated as not yet available).
  if (fy == CFG$fy_covid && any(file.exists(file.path(CFG$raw, c("filedate.csv.gz", "filedate.csv"))))) {
    fd <- read_raw("filedate")[, `:=`(gvkey = gvkey(gvkey), datadate = as.Date(datadate), filedate = as.Date(filedate))]
    fd <- fd[, .(filedate = min(filedate)), by = .(gvkey, datadate)]
    cur <- merge(cur, fd, by = c("gvkey", "datadate"), all.x = TRUE)   # kept for information in both samples
  }
  if (fy == CFG$fy_covid && !is.na(CFG$vintage_cutoff)) {
    if (!"filedate" %in% names(cur)) stop("The original-vintage sample needs 10-K filing dates (data/raw/filedate.csv.gz).")
    avail <- !is.na(cur$filedate) & cur$filedate <= CFG$vintage_cutoff &
             (is.na(CFG$fye_max) | cur$datadate <= CFG$fye_max)
    add(sprintf("Not available by %s (later 10-K or fiscal year ending after %s)",
                format(CFG$vintage_cutoff, "%b %d, %Y"), format(CFG$fye_max, "%b %Y")), -sum(!avail))
    cur <- cur[avail]
    add("Number of firms with fiscal year 2019 data available in March 2020", uniqueN(cur$gvkey))
  }
  setorder(cur, gvkey, datadate)
  dup <- duplicated(cur$gvkey, fromLast = TRUE)
  add("Duplicate data", -sum(dup))
  d <- cur[!dup]
  d <- merge(d, comp[, .(gvkey, conm, sic, naics, loc)], by = "gvkey", all.x = TRUE)
  d[, sic_use := fifelse(is.na(sich), as.numeric(sic), as.numeric(sich))]
  d[, naics_use := fifelse(is.na(naicsh) | naicsh == "", as.character(naics), as.character(naicsh))]

  drop <- function(mask, label) { mask[is.na(mask)] <- FALSE; add(label, -sum(mask)); d <<- d[!mask] }
  drop(d$sic_use >= 6000 & d$sic_use <= 6999, "SIC codes 6000-6999 (financial firms)")
  drop(d$sic_use >= 4900 & d$sic_use <= 4949, "SIC codes 4900-4949 (utility firms)")
  drop(d$sic_use >= 8000, "SIC codes 8000s and 9000s (not-for-profit and governmental firms)")
  drop(is.na(d$loc) | d$loc != "USA", "Non-U.S. headquarters")
  drop(is.na(d$che) | is.na(d$at) | d$at <= 0, "Missing cash and short-term investments")
  d <- merge(d, rets, by = "gvkey", all.x = TRUE)
  key <- if (fy == CFG$fy_covid) "ret_collapse" else "ret_gfc_b"
  drop(is.na(d[[key]]), "Missing stock price data in Security Daily")
  drop(!is.na(d$avg_price) & d$avg_price < 1, "An average share price < $1")
  drop(is.na(d$tpci) | sub("\\.0$", "", d$tpci) != "0", 'Security type not equal to "common, ordinary"')
  add("Number of firms after all screens", nrow(d))

  # ------------------------------------------------ Appendix B variables
  d[, `:=`(assets = at, cash = che / at, std = dlc / at, ltd = dltt / at, bookdebt = (dlc + dltt) / at)]
  d[, nstd := std - cash]
  d[, payout_usd := nz(dvc) + nz(dvp) + nz(prstkc)]
  d[, payout := payout_usd / at]
  d[, rd := z0(xrd) / at]
  d[, sga := xsga / fifelse(sale > 0, sale, NA_real_)]
  d[, cogs := pmin(cogs / fifelse(sale > 0, sale, NA_real_), 1)]
  d[, `:=`(prof = gp / at, ppe = ppent / at)]
  d[, lnmve := log(fifelse(mve > 0, mve, NA_real_))]
  d[, bm := ceq / mve]
  d <- merge(d, first, by = "gvkey", all.x = TRUE)
  d[, age := fy - firstyear]

  lag <- by_year(fy - 1)[, .(gvkey, at_l = at, ppent_l = ppent, che_l = che, sale_l = sale,
                             pay_l = nz(dvc) + nz(dvp) + nz(prstkc))]
  lag2 <- by_year(fy - 2)[, .(gvkey, at_l2 = at, pay_l2 = nz(dvc) + nz(dvp) + nz(prstkc))]
  d <- merge(d, lag, by = "gvkey", all.x = TRUE)
  d <- merge(d, lag2, by = "gvkey", all.x = TRUE)
  d[, capex := capx / at_l]
  have3 <- !is.na(d$at_l) & !is.na(d$at_l2)
  d[, payout3 := fifelse(have3, (payout_usd + pay_l + pay_l2) / at, NA_real_)]
  d[, payout3_avg := fifelse(have3, (payout + pay_l / at_l + pay_l2 / at_l2) / 3, NA_real_)]
  d[, payout_ni := fifelse(ib > 0, payout_usd / ib, NA_real_)]      # robustness: payout / (NI - XI)

  rt <- ratings_at(rating_date)
  d <- merge(d, rt, by = "gvkey", all.x = TRUE)
  d[is.na(ig), ig := 0]; d[is.na(hy), hy := 0]

  # ------------------------------------------------ financial-constraint indexes
  # KZ index, Lamont, Polk and Saa-Requejo (2001) construction
  d[, K := fifelse(ppent_l > 0, ppent_l, NA_real_)]
  d[, q_kz := (at + csho * prcc_f - ceq - z0(txdb)) / at]
  d[, cap := z0(dltt) + z0(dlc) + seq]
  d[, kz := -1.001909 * (ib + z0(dp)) / K + 0.2826389 * q_kz +
            3.139193 * (z0(dltt) + z0(dlc)) / fifelse(cap > 0, cap, NA_real_) -
            39.3678 * (nz(dvc) + nz(dvp)) / K - 1.314759 * che / K]
  # WW index, Whited and Wu (2006); industry sales growth by 3-digit SIC over all Compustat firms
  sg_all <- merge(by_year(fy)[, .(gvkey, s1 = sale, sich)], by_year(fy - 1)[, .(gvkey, s0 = sale)], by = "gvkey")
  sg_all <- merge(sg_all, comp[, .(gvkey, sic_c = as.numeric(sic))], by = "gvkey", all.x = TRUE)
  sg_all[, sic3 := floor(fifelse(is.na(sich), sic_c, as.numeric(sich)) / 10)]
  isg <- sg_all[s0 > 0, .(isg = mean(s1 / s0 - 1)), by = sic3]
  d[, sic3 := floor(sic_use / 10)]
  d <- merge(d, isg, by = "sic3", all.x = TRUE)
  d[, sg := sale / fifelse(sale_l > 0, sale_l, NA_real_) - 1]   # no growth rate when prior sales are zero
  d[, ww := -0.091 * (ib + z0(dp)) / at - 0.062 * as.numeric((nz(dvc) + nz(dvp)) > 0) +
            0.021 * z0(dltt) / at - 0.044 * log(at) + 0.102 * isg - 0.035 * sg]
  # SA index, Hadlock and Pierce (2010)
  cpi_fy <- if (as.character(fy) %in% names(CFG$cpi)) CFG$cpi[[as.character(fy)]] else CFG$cpi[["2019"]]
  d[, size_sa := log(pmin(at * CFG$cpi[["2004"]] / cpi_fy, CFG$sa_size_cap))]
  d[, sa := -0.737 * size_sa + 0.043 * size_sa^2 - 0.040 * pmin(age, CFG$sa_age_cap)]
  # Huang and Ritter (2020) cash ex ante (Appendix B formula)
  d[, hr_cash := 2 * che - che_l - (z0(dltis) - z0(dltr) + z0(dlcch)) - (z0(sstk) - z0(prstkc))]
  # Hoberg and Maksimovic (2015) delaycon
  hm <- read_ext("hm_constraints.csv")
  if (!is.null(hm)) {
    yr <- if (fy == CFG$fy_covid) 2015 else fy
    hm <- unique(hm[as.integer(year) == yr, .(gvkey = gvkey(gvkey), hm = as.numeric(delaycon))], by = "gvkey")
    d <- merge(d, hm, by = "gvkey", all.x = TRUE)
  } else d[, hm := NA_real_]

  # ------------------------------------------------ quarterly items (COVID only)
  if (fy == CFG$fy_covid) {
    fq <- read_raw("fundq")[, gvkey := gvkey(gvkey)]
    fq <- unique(fq[order(gvkey, datadate)], by = c("gvkey", "datacqtr"), fromLast = TRUE)
    w <- dcast(fq, gvkey ~ datacqtr, value.var = c("oibdpq", "saleq"))
    need <- c(outer(c("oibdpq", "saleq"), c("2019Q1", "2019Q2", "2020Q1", "2020Q2"), paste, sep = "_"))
    for (n in setdiff(need, names(w))) w[[n]] <- NA_real_
    d <- merge(d, w[, c("gvkey", need), with = FALSE], by = "gvkey", all.x = TRUE)
    d[, roa := (oibdpq_2020Q1 + oibdpq_2020Q2) / at]
    d[, roa_lag := (oibdpq_2019Q1 + oibdpq_2019Q2) / at_l]
    d[, dsales_q2 := saleq_2020Q2 / fifelse(saleq_2019Q2 > 0, saleq_2019Q2, NA_real_) - 1]
    d[, labor := emp / fifelse(sale > 0, sale, NA_real_)]
    cl <- read_ext("credit_lines.csv")
    if (!is.null(cl)) {
      cl <- unique(cl[, .(gvkey = gvkey(gvkey), cr = as.numeric(undrawn_credit_usd_m))], by = "gvkey")
      d <- merge(d, cl, by = "gvkey", all.x = TRUE)
      d[, credit := cr / at][, cr := NULL]
    } else d[, credit := NA_real_]
  }

  # ------------------------------------------------ winsorizing (see config.R)
  for (v in CFG$winsor_top) { set(d, j = paste0(v, "_raw"), value = d[[v]]); set(d, j = v, value = cap_top(d[[v]])) }
  for (v in CFG$winsor_both) if (any(!is.na(d[[v]]))) set(d, j = v, value = winsor(d[[v]]))

  d[, ff49 := ff49_from_sic(sic_use)]
  d[, `:=`(naics3 = naics_str(naics_use, 3), naics6 = naics_str(naics_use, 6))]
  list(d = d, funnel = rbindlist(funnel))
}

cv <- build(CFG$fy_covid, "returns_covid.csv", CFG$rating_covid)
fwrite(cv$d, file.path(CFG$derived, "firm_covid_base.csv"))
save_table(cv$funnel, "appendix_a", sprintf("Appendix A. Sample selection procedure (%s)", CFG$sample_label),
           "The paper reports 3,859 firms with fiscal year 2019 data available in April 2020 and 1,857 after all screens.")
gf <- build(CFG$fy_gfc, "returns_gfc.csv", CFG$rating_gfc)
fwrite(gf$d, file.path(CFG$derived, "firm_gfc.csv"))
save_table(gf$funnel, "appendix_a_gfc", "Appendix A (analogue). Sample selection, GFC sample (fiscal 2006)")
cat(sprintf("COVID sample: %d firms; GFC sample: %d firms\n", nrow(cv$d), nrow(gf$d)))
