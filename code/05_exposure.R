# ======================================================================================
# Step 5 - COVID-19 exposure measures (Section 4.1, Appendix C), COVID-19 conglomerates
# (Section 6.2), financial-constraint indicators (Section 2) and Figure 1 groups.
#
# Reads data/derived/firm_covid_base.csv and the hand-supplied files in data/external/.
# A measure whose input file is missing is left empty and skipped downstream.
# Writes data/derived/firm_covid.csv
# ======================================================================================
source("config.R"); source("code/lib.R")

d <- fread(file.path(CFG$derived, "firm_covid_base.csv"),
           colClasses = list(character = c("gvkey", "naics3", "naics6")))
d[, `:=`(gvkey = gvkey(gvkey), datadate = as.Date(datadate))]

# Top-k supplier industries (by input value) into the set of affected industries,
# excluding suppliers that are themselves affected.
upstream <- function(io, affected, k) {
  io <- copy(io)[, `:=`(s = substr(supplier_naics3, 1, 3), c = substr(customer_naics3, 1, 3),
                        v = as.numeric(value))]
  a <- io[c %in% affected & !(s %in% affected), .(v = sum(v, na.rm = TRUE)), by = s][order(-v)]
  head(a$s, k)
}

# ---- Koren & Peto (2020)
kp <- read_ext("koren_peto.csv")
aff_naics <- character(0)
if (!is.null(kp)) {
  kp <- unique(kp[, naics3 := substr(naics3, 1, 3)], by = "naics3")
  for (v in c("affected_share", "teamwork_share", "customer_share", "presence_share"))
    set(d, j = v, value = as.numeric(kp[[v]])[match(d$naics3, kp$naics3)])
  d[, `:=`(x_affected = top_q(affected_share), x_teamwork = top_q(teamwork_share),
           x_customer = top_q(customer_share), x_presence = top_q(presence_share))]
  thr <- quantile(d$affected_share, .75, na.rm = TRUE)
  aff_naics <- kp$naics3[as.numeric(kp$affected_share) > thr]
  for (pr in list(c("x_fhp", "io_fhp.csv"), c("x_bea", "io_bea.csv"))) {
    io <- read_ext(pr[2])
    if (!is.null(io)) {
      ups <- upstream(io, aff_naics, CFG$top_upstream)
      set(d, j = pr[1], value = ifelse(is.na(d$x_affected), NA_real_, as.numeric(d$x_affected == 1 | d$naics3 %in% ups)))
      cat(sprintf("  %s: upstream industries added %s\n", pr[1], paste(ups, collapse = ", ")))
    }
  }
  d[, x_manual := x_affected]
  man <- read_ext("manual_naics6.csv")
  if (!is.null(man)) {
    ov <- setNames(as.numeric(man$affected), substr(man$naics6, 1, 6))
    hit <- !is.na(d$naics6) & d$naics6 %in% names(ov)
    d[hit, x_manual := ov[naics6]]
  }
} else {
  cat("  !! koren_peto.csv has no data: Koren-Peto-based measures, Table 4, 6 and 9 will be skipped\n")
}

# ---- firm-level and other industry measures
d[, `:=`(x_sales = bottom_q(dsales_q2), x_labor = top_q(labor))]
dn <- read_ext("dingel_neiman.csv")
if (!is.null(dn)) {
  dn <- unique(dn[, naics3 := substr(naics3, 1, 3)], by = "naics3")
  d[, home_share := as.numeric(dn$teleworkable)[match(naics3, dn$naics3)]]
  d[, x_home := bottom_q(home_share)]
}
mo <- read_ext("moodys_ff49.csv")
if (!is.null(mo)) d[, x_moodys := as.numeric(ff49 %in% as.integer(mo$ff49))]

# ---- COVID-19 conglomerates (Section 6.2, Appendix B)
segp <- file.path(CFG$raw, c("segments.csv.gz", "segments.csv"))
if (any(file.exists(segp))) {
  seg <- read_raw("segments")[, `:=`(gvkey = gvkey(gvkey), datadate = as.Date(datadate), srcdate = as.Date(srcdate))]
  seg <- merge(seg, d[, .(gvkey, datadate)], by = c("gvkey", "datadate"))
  seg <- seg[, .SD[srcdate == max(srcdate)], by = gvkey]
  seg[, n3 := naics_str(naicss1, 3)]
  seg <- unique(seg[!is.na(n3)], by = c("gvkey", "stype", "sid"))
  g <- seg[, .(sales = sum(sales, na.rm = TRUE)), by = .(gvkey, n3)]
  g[, aff := n3 %in% aff_naics]
  agg <- g[, .(n_ind = uniqueN(n3), seg_sales = sum(sales), any_aff = any(aff), any_non = any(!aff)), by = gvkey]
  agg <- merge(agg, d[, .(gvkey, sale)], by = "gvkey")
  agg[, ok := n_ind >= 2 & abs(seg_sales - sale) <= 0.01 * sale & any_aff & any_non]
  d[, conglo := as.numeric(gvkey %in% agg[ok == TRUE, gvkey])]
  cat(sprintf("  COVID-19 conglomerates: %d (%.1f%%)\n", sum(d$conglo), 100 * mean(d$conglo)))
} else {
  cat("  !! segments missing: Table 9 will be skipped\n")
  d[, conglo := NA_real_]
}
if ("x_affected" %in% names(d)) d[, ha_noconglo := x_affected * (1 - conglo)]

# ---- financial-constraint indicators (top quartile; HR = projected cash deficit)
for (v in c("kz", "ww", "sa", "hm")) set(d, j = paste0("fc_", v), value = top_q(d[[v]]))
d[, fc_hr := fifelse(is.na(hr_cash), NA_real_, as.numeric(hr_cash < 0))]
d[, payout3_tq := top_q(payout3)]

# ---- Figure 1 / rebound groups
cq <- quantile(d$cash, c(.25, .75), na.rm = TRUE); lq <- quantile(d$ltd, c(.25, .75), na.rm = TRUE)
d[, high_ff := as.numeric(!is.na(cash) & !is.na(ltd) & cash >= cq[2] & ltd <= lq[1])]
d[, low_ff := as.numeric(!is.na(cash) & !is.na(ltd) & cash <= cq[1] & ltd >= lq[2])]

fwrite(d, file.path(CFG$derived, "firm_covid.csv"))
have <- sum(vapply(names(CFG$exposures), function(v) has(d, v), logical(1)))
cat(sprintf("firm_covid.csv written; exposure measures available: %d/11\n", have))
