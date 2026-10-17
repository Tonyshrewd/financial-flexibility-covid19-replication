# ======================================================================================
# Step 6 - Every table in the paper (1-10, Appendix A, C1, C2), the unreported robustness
# checks, and the numbers quoted in the text.
# Output: output/tables/*.csv, *.md, all_tables.xlsx, text_numbers.csv
# ======================================================================================
source("config.R"); source("code/lib.R")

cov <- fread(file.path(CFG$derived, "firm_covid.csv"), colClasses = list(character = c("gvkey", "naics3", "naics6")))
gfc <- fread(file.path(CFG$derived, "firm_gfc.csv"), colClasses = list(character = c("gvkey", "naics3", "naics6")))
cov[, roa_w := if (any(!is.na(roa))) winsor(roa) else NA_real_]
cov[, roa_lag := if (any(!is.na(roa_lag))) winsor(roa_lag) else NA_real_]   # lagged ROA winsorized like ROA
cat(sprintf("COVID sample %d, GFC sample %d\n", nrow(cov), nrow(gfc)))

# Appendix A tables were written by step 4; put them first in the workbook.
for (nm in c("appendix_a", "appendix_a_gfc")) {
  p <- file.path(CFG$tables, paste0(nm, ".csv"))
  if (file.exists(p)) .ALL$tabs[[nm]] <- list(title = paste("Appendix A sample selection:", nm), note = "", df = fread(p))
}

AP <- CFG$ap
X1 <- c("cash", "std", "ltd", CFG$cost, AP)
X3 <- c("nstd", "ltd", "ig", "hy", setdiff(CFG$cost, "ig"), AP)
X5 <- c("cash", "bookdebt", CFG$cost, AP)
ROWS3 <- c("cash", "std", "nstd", "ltd", "bookdebt", "ig", "hy", "capex", "sga", "cogs", "rd", "payout", AP)
LAB <- CFG$labels

TEXT <- list()
note <- function(item, value, paper, desc)
  TEXT[[length(TEXT) + 1]] <<- data.table(item = item, description = desc, replicated = round(value, 4), paper = paper)
iqr <- function(x) diff(quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE))
lower_tri <- function(m) { m[upper.tri(m)] <- NA; m }

# ============================================================== Table 1 & 2
cols1 <- c("ret_collapse", "ret_stimulus", "roa_w", "assets", "cash", "std", "nstd", "ltd", "bookdebt",
           "payout", "payout3_tq", "conglo", "ig", "hy", "rd", "sga", "capex", "cogs", "kz", "fc_kz", "ww",
           "fc_ww", "sa", "fc_sa", "hr_cash", "fc_hr", "hm", "fc_hm", "beta", "bm", "lnmve", "mom", "prof")
cols1 <- Filter(function(v) has(cov, v), cols1)
t1 <- summary_stats(cov, cols1)
g1 <- summary_stats(gfc, c("ret_gfc_a", "ret_gfc_b"),
                    c(ret_gfc_a = "GFC: Cum. daily exc. returns (Sep 15 - Dec 31, 2008)",
                      ret_gfc_b = "GFC: Cum. daily exc. returns (Sep 15, 2008 - Mar 9, 2009)"))
t1 <- rbind(t1[1:min(3, nrow(t1)), ], g1, t1[-(1:min(3, nrow(t1))), ])
num <- vapply(t1, is.numeric, logical(1)); t1[num] <- lapply(t1[num], round, 3)
save_table(t1, "table1", "Table 1. Summary statistics")
note("mean_collapse", mean(cov$ret_collapse), -0.378, "Mean cumulative excess return, collapse period")
note("median_collapse", median(cov$ret_collapse), -0.387, "Median cumulative excess return, collapse period")
note("mean_stimulus", mean(cov$ret_stimulus, na.rm = TRUE), 0.092, "Mean excess return, stimulus day")
note("n_firms", nrow(cov), 1857, "Firms after all screens")
note("share_fc_hr", mean(cov$fc_hr, na.rm = TRUE), 0.201, "Share of firms with projected cash deficit (HR)")

cols2 <- c("ret_collapse", "ret_stimulus", "roa_w", "assets", "cash", "std", "nstd", "ltd", "bookdebt", "payout",
           "payout3_tq", "conglo", "ig", "hy", "rd", "sga", "capex", "cogs", "fc_kz", "fc_ww", "fc_sa", "fc_hr",
           "fc_hm", "beta", "bm", "lnmve", "mom", "prof")
cols2 <- Filter(function(v) has(cov, v), cols2)
cm <- round(lower_tri(cor(as.data.frame(cov)[, cols2], use = "pairwise.complete.obs")), 2)
t2 <- data.frame(Variable = sprintf("(%d) %s", seq_along(cols2), ifelse(is.na(LAB[cols2]), cols2, LAB[cols2])),
                 cm, check.names = FALSE, row.names = NULL)
names(t2)[-1] <- sprintf("(%d)", seq_along(cols2))
save_table(t2, "table2", "Table 2. Correlation matrix")

# ============================================================== Table 3
r3 <- list(reg(cov, "ret_collapse", X1, "ff49"), reg(cov, "ret_stimulus", X1, "ff49"),
           reg(cov, "ret_collapse", X3, "ff49"), reg(cov, "ret_stimulus", X3, "ff49"),
           reg(cov, "ret_collapse", X5, "ff49"), reg(cov, "ret_stimulus", X5, "ff49"),
           reg(gfc, "ret_gfc_a", X1, "ff49"), reg(gfc, "ret_gfc_b", X1, "ff49"))
save_table(reg_table(r3, ROWS3, col_names = c("Collapse", "Stimulus", "Collapse", "Stimulus", "Collapse",
                                              "Stimulus", "GFC to Dec 31, 2008", "GFC to Mar 9, 2009")),
           "table3", "Table 3. Stock returns, financial flexibility measures, and stock and firm characteristics",
           "OLS with Fama-French 49 industry fixed effects; t-statistics in parentheses.")

m <- abs(mean(cov$ret_collapse))
d1 <- B(r3[[1]], "cash") * iqr(cov$cash) - B(r3[[1]], "std") * iqr(cov$std) - B(r3[[1]], "ltd") * iqr(cov$ltd)
d3 <- -B(r3[[3]], "nstd") * iqr(cov$nstd) - B(r3[[3]], "ltd") * iqr(cov$ltd)
d5 <- B(r3[[5]], "cash") * iqr(cov$cash) - B(r3[[5]], "bookdebt") * iqr(cov$bookdebt)
note("ff_value_col1_pp", 100 * d1, 9.7, "High- vs low-flexibility firm, Table 3 col 1 (pp)")
note("ff_value_col1_pct", 100 * d1 / m, 26, "...as % of mean drop")
note("ff_value_col3_pp", 100 * d3, 8.0, "Same, Table 3 col 3 (pp)")
note("ff_value_col5_pp", 100 * d5, 9.5, "Same, Table 3 col 5 (pp)")
sdv <- function(d, v) sd(d[[v]], na.rm = TRUE)
note("sd_cash_pp", 100 * B(r3[[1]], "cash") * sdv(cov, "cash"), 3.5, "1-SD lower cash -> lower collapse return (pp)")
note("sd_std_pp", -100 * B(r3[[1]], "std") * sdv(cov, "std"), 1.3, "1-SD higher short-term debt (pp)")
note("sd_ltd_pp", -100 * B(r3[[1]], "ltd") * sdv(cov, "ltd"), 3.7, "1-SD higher long-term debt (pp)")
note("gfc_sd_cash_pp", 100 * B(r3[[8]], "cash") * sdv(gfc, "cash"), 3.6, "GFC: 1-SD lower cash (pp)")
note("gfc_sd_ltd_pp", -100 * B(r3[[8]], "ltd") * sdv(gfc, "ltd"), 2.5, "GFC: 1-SD higher long-term debt (pp)")
for (k in list(list(1, "cash", .134), list(1, "std", -.232), list(1, "ltd", -.163), list(2, "ltd", .025),
               list(3, "hy", -.057), list(4, "hy", .016), list(8, "cash", .152), list(8, "ltd", -.131)))
  note(sprintf("b_%s_c%d", k[[2]], k[[1]]), B(r3[[k[[1]]]], k[[2]]), k[[3]],
       sprintf("Table 3 coefficient: %s, column %d", LAB[[k[[2]]]], k[[1]]))

# Section 3.3: rebound (all Table 3 controls except cash and LT debt, which define the group)
rb <- reg(cov, "ret_to_aug", c("high_ff", setdiff(X1, c("cash", "ltd"))), "ff49")
note("rebound_highff_pp", 100 * B(rb, "high_ff"), 9.9, "High-flexibility vs all other firms, Feb 3 - Aug 31, 2020 (pp)")
note("n_high_ff", sum(cov$high_ff), 257, "Firms in high-flexibility portfolio (Figure 1)")
note("n_low_ff", sum(cov$low_ff), 184, "Firms in low-flexibility portfolio (Figure 1)")
# Section 5.1: firms with near-zero net leverage
netlev <- fifelse(is.na(cov$bookdebt), 0, cov$bookdebt) - cov$cash_raw
unlev <- cov$ret_collapse[netlev < 0.01]
if (CFG$ret_type == "log") unlev <- exp(unlev) - 1
note("unlevered_drop_pct", -100 * mean(unlev, na.rm = TRUE), 30.0,
     "Equity drop of firms with net leverage < 1% (%)")

# ============================================================== robustness (Section 3.1, untabulated)
rob <- list(); rn <- c()
addr <- function(d, y, xs, name, ...) {
  if (has(d, y) && all(vapply(setdiff(xs, AP), function(v) has(d, v), logical(1)))) {
    rob[[length(rob) + 1]] <<- reg(d, y, xs, "ff49", ...); rn <<- c(rn, name)
  }
}
sw <- function(from, to) replace(X1, X1 == from, to)
addr(cov, "ret_collapse", X1, "Clustered by FF49", cluster = "ff49")
addr(copy(cov)[, yw := winsor(ret_collapse)], "yw", X1, "Dep. var. winsorized 1/99")
qq <- quantile(cov$ret_collapse, c(.01, .99))
addr(cov[ret_collapse >= qq[1] & ret_collapse <= qq[2]], "ret_collapse", X1, "Dep. var. truncated 1/99")
if (has(cov, "car4")) addr(copy(cov)[, car4w := winsor(car4)], "car4w", X1, "4-factor CAR (winsorized)")
addr(cov, "ret_collapse", sw("payout", "payout3_avg"), "3-yr avg payout")
addr(cov, "ret_collapse", sw("payout", "payout_ni"), "Payout / (NI - XI)")
addr(cov, "ret_collapse", sw("capex", "ppe"), "PPE instead of capex")
addr(cov, "ret_stimulus", sw("capex", "ppe"), "PPE, stimulus day")
addr(cov, "ret_collapse", c(X1, "credit"), "+ unused credit lines")
addr(cov, "ret_stimulus", c(X1, "credit"), "+ unused credit lines, stim.")
raw <- copy(cov)
for (v in CFG$winsor_top) if (paste0(v, "_raw") %in% names(raw)) raw[[v]] <- raw[[paste0(v, "_raw")]]
addr(raw, "ret_collapse", X1, "No regressor winsorizing")
if (length(rob))
  save_table(reg_table(rob, c("cash", "std", "ltd", "payout", "payout3_avg", "payout_ni", "ppe", "credit"), col_names = rn),
             "table3_robustness", "Robustness checks for Table 3, column 1 (described but not tabulated in Section 3.1)",
             "Only flexibility and swapped-in variables shown; all other Table 3 controls and FF49 fixed effects included.")

# ============================================================== Table 4
if (has(cov, "affected_share")) {
  kp <- unique(cov[!is.na(naics3), .(naics3, affected_share)], by = "naics3")
  bottom20 <- function(d, y) {
    g <- d[!is.na(naics3), .(n = .N, ltd = mean(ltd, na.rm = TRUE), ppe = mean(ppe, na.rm = TRUE),
                             cr = mean(get(y), na.rm = TRUE)), by = naics3][n >= CFG$min_firms_t4]
    g[, aff := kp$affected_share[match(naics3, kp$naics3)]]
    head(g[order(cr)], 20)
  }
  bg <- bottom20(gfc, "ret_gfc_b"); bc <- bottom20(cov, "ret_collapse")
  both <- intersect(bg$naics3, bc$naics3)
  side <- function(g) {
    v <- as.matrix(g[, .(aff, ltd, ppe, cr)])
    pad <- 20 - nrow(g)
    data.frame(a = c(paste0(ifelse(g$naics3 %in% both, "* ", ""), g$naics3), rep("", pad), "Mean", "Median"),
               rbind(v, matrix(NA_real_, pad, 4), colMeans(v, na.rm = TRUE), apply(v, 2, median, na.rm = TRUE)),
               row.names = NULL)
  }
  t4 <- cbind(setNames(side(bg), paste("GFC", c("NAICS3", "Affected share", "Lt debt/assets", "PPE/assets", "CR"))),
              setNames(side(bc), paste("COVID", c("NAICS3", "Affected share", "Lt debt/assets", "PPE/assets", "CR"))))
  num <- vapply(t4, is.numeric, logical(1)); t4[num] <- lapply(t4[num], round, 3)
  save_table(t4, "table4", "Table 4. Bottom-20 NAICS three-digit industries: GFC versus COVID-19",
             paste("* = industry in the bottom 20 in both crises (bold in the paper). CR = equal-weighted mean cumulative",
                   "excess return (GFC: Sep 15, 2008 - Mar 9, 2009; COVID: Feb 3 - Mar 23, 2020)."))
  note("t4_overlap", length(both), 9, "Industries in bottom 20 in both crises")
  note("t4_mean_aff_covid", mean(bc$aff, na.rm = TRUE), 33.9, "Mean affected share, bottom-20 COVID")
  note("t4_mean_aff_gfc", mean(bg$aff, na.rm = TRUE), 16.4, "Mean affected share, bottom-20 GFC")
} else cat("  table4 skipped (no Koren-Peto data)\n")

# ============================================================== Table 5
T5_CTRL <- c("capex", "sga", "rd", "payout", "ig", AP)   # controls listed in the Table 5 caption
ILAB <- c(LAB, cash_x = "Cash / assets x highly affected", std_x = "St debt / assets x highly affected",
          ltd_x = "Lt debt / assets x highly affected", ha = "Ind: Highly affected")
r5 <- list(); n5 <- c()
for (v in names(CFG$exposures)) {
  if (!has(cov, v)) next
  dd <- copy(cov)[, ha := get(v)][, `:=`(cash_x = cash * ha, std_x = std * ha, ltd_x = ltd * ha)]
  r5[[length(r5) + 1]] <- reg(dd, "ret_collapse", c("cash_x", "std_x", "ltd_x", "cash", "std", "ltd", "ha", T5_CTRL), "ff49")
  n5 <- c(n5, CFG$exposures[[v]])
  if (v %in% c("x_affected", "x_manual"))
    note(paste0("t5_cash_iqr_", v), 100 * B(r5[[length(r5)]], "cash_x") * iqr(cov$cash), if (v == "x_affected") 9.0 else 7.3,
         sprintf("Cash interaction, 25th->75th pct of cash, %s (pp)", CFG$exposures[[v]]))
}
if (length(r5))
  save_table(reg_table(r5, c("cash_x", "std_x", "ltd_x", "cash", "std", "ltd", "ha"), ILAB,
                       extra = list(Controls = rep("Yes", length(r5))), col_names = n5), "table5",
             "Table 5. Stock returns, financial flexibility interacted with COVID-19 exposure measures",
             paste("Collapse-period returns; FF49 fixed effects. A blank 'Ind: Highly affected' coefficient means the",
                   "measure does not vary within FF49 industries (absorbed by the fixed effects)."))

# ============================================================== Table 6
if (has(cov, "x_affected") && has(cov, "roa_w")) {
  dd <- copy(cov)[, ha := x_affected][, `:=`(cash_x = cash * ha, std_x = std * ha, ltd_x = ltd * ha)]
  base <- c("cash_x", "std_x", "ltd_x", "cash", "std", "ltd", "ha")
  c2 <- c("payout", "ig", "capex", "sga", "cogs", "rd", "age", "roa_lag", "beta", "bm", "lnmve", "mom")
  r6 <- list(reg(dd, "roa_w", c(base, "roa_lag", "bm", "lnmve"), "ff49"), reg(dd, "roa_w", c(base, c2), "ff49"))
  save_table(reg_table(r6, c(base, c2), ILAB, col_names = c("ROA", "ROA")), "table6",
             "Table 6. Accounting performance (cumulative Q1+Q2 2020 ROA), financial flexibility x affectedness",
             "ROA winsorized at 1/99; FF49 fixed effects.")
} else cat("  table6 skipped\n")

# ============================================================== Table 7
blk <- function(p, keep, label) {
  d <- data.table(cash = cov$cash_raw, ltd = cov$ltd, p = p)[keep]
  d[, `:=`(cf_cash = (cash + p) / (1 + p), cf_ltd = ltd - p, q = rank_quartile(p))]
  groups <- list("All firms" = d, "Top quartile payout ratio" = d[q == 4],
                 "Bottom quartile payout ratio" = d[q == 1], "Middle quartiles payout ratios" = d[q %in% 2:3])
  vars <- c(cash = "Cash / assets", cf_cash = "(Cash + payout) / (assets + payout)", ltd = "Lt debt / assets",
            cf_ltd = "(Lt debt - payout) / assets", p = "Payout / assets")
  rows <- rbindlist(lapply(names(groups), function(g) rbindlist(lapply(names(vars), function(v) {
    s <- na.omit(groups[[g]][[v]]); data.table(Group = g, Variable = vars[[v]], N = length(s),
                                               Median = median(s), Mean = mean(s)) }))))
  list(rows = rows, groups = groups)
}
b19 <- blk(cov$payout_raw, rep(TRUE, nrow(cov)), "2019")
b3 <- blk(cov$payout3, !is.na(cov$payout3), "3yr")
t7 <- cbind(b19$rows, b3$rows[, .(`N (3-yr)` = N, `Median (3-yr)` = Median, `Mean (3-yr)` = Mean)])
setnames(t7, c("N", "Median", "Mean"), c("N (2019)", "Median (2019)", "Mean (2019)"))
num <- vapply(t7, is.numeric, logical(1)); for (j in names(t7)[num]) set(t7, j = j, value = round(t7[[j]], 3))
save_table(t7, "table7", "Table 7. Payout ratios by quartile and financial flexibility measures",
           paste("2019 payouts vs. cumulative 2017-2019 payouts over 2019 assets. Quartiles assigned by rank.",
                 "Return gains in text_numbers use the paper's Table 3 coefficients (0.134, -0.163)."))
a <- b19$groups[["All firms"]]; tp <- b3$groups[["Top quartile payout ratio"]]
note("t7_cash_gain_all_pp", 13.4 * (mean(a$cf_cash) - mean(a$cash)), 0.36, "Gain if 2019 payouts kept as cash, avg firm (pp)")
note("t7_ltd_gain_all_pp", 16.3 * (mean(a$ltd, na.rm = TRUE) - mean(a$cf_ltd, na.rm = TRUE)), 0.57,
     "Gain if 2019 payouts repaid LT debt, avg firm (pp)")
note("t7_3y_top_cf_ltd", mean(tp$cf_ltd, na.rm = TRUE), -0.019, "Top-quartile 3-yr payers: pro forma LT debt / assets")
note("t7_3y_top_gain_pp", 16.3 * mean(tp$ltd, na.rm = TRUE), 5.1, "Top-quartile 3-yr payers: return gain (pp)")

# ============================================================== Table 8
full8 <- c("payout3_tq", "cash", "std", "ltd", "ig", "capex", "sga", "cogs", "rd", AP)
r8 <- list(reg(cov, "ret_collapse", c("payout3_tq", AP), "ff49"), reg(cov, "ret_stimulus", c("payout3_tq", AP), "ff49"),
           reg(cov, "ret_collapse", full8, "ff49"), reg(cov, "ret_stimulus", full8, "ff49"))
save_table(reg_table(r8, full8, col_names = c("Collapse", "Stimulus", "Collapse", "Stimulus")), "table8",
           "Table 8. Stock returns, financial flexibility, and a top-quartile payout indicator", "FF49 fixed effects.")

# ============================================================== Table 9
if (has(cov, "conglo") && has(cov, "ha_noconglo")) {
  dd <- copy(cov)[, `:=`(cash_xc = cash * conglo, std_xc = std * conglo, ltd_xc = ltd * conglo,
                         cash_xh = cash * ha_noconglo, std_xh = std * ha_noconglo, ltd_xh = ltd * ha_noconglo)]
  base <- c("cash", "std", "ltd", "conglo", "ha_noconglo", "ig", "capex", "sga", "cogs", "rd", "payout", AP)
  inter <- c("cash_xc", "std_xc", "ltd_xc", "cash_xh", "std_xh", "ltd_xh")
  r9 <- list(reg(dd, "ret_collapse", base), reg(dd, "ret_stimulus", base),
             reg(dd, "ret_collapse", c(inter, base)), reg(dd, "ret_stimulus", c(inter, base)))
  pv <- vapply(r9, one_sided_diff, numeric(1), a = "conglo", b = "ha_noconglo")
  L9 <- c(LAB, cash_xc = "Cash / assets x Ind: COVID-19 conglomerate", std_xc = "St debt / assets x Ind: COVID-19 conglomerate",
          ltd_xc = "Lt debt / assets x Ind: COVID-19 conglomerate",
          cash_xh = "Cash / assets x Ind: Highly affected (0 for conglomerates)",
          std_xh = "St debt / assets x Ind: Highly affected (0 for conglomerates)",
          ltd_xh = "Lt debt / assets x Ind: Highly affected (0 for conglomerates)",
          ha_noconglo = "Ind: Highly affected (set to zero for COVID-19 conglomerate)")
  save_table(reg_table(r9, c(inter, base), L9,
                       extra = list("One-sided p: conglomerate > highly affected" = sprintf("%.3f", pv),
                                    "Industry fixed effects" = rep("No", 4)),
                       col_names = c("Collapse", "Stimulus", "Collapse", "Stimulus")), "table9",
             "Table 9. Stock returns, financial flexibility measures, and conglomerate indicators")
  note("n_conglo", sum(cov$conglo), 107, "Number of COVID-19 conglomerates")
  note("t9_p_col1", pv[1], 0.125, "One-sided p-value, Table 9 col 1")
} else cat("  table9 skipped\n")

# ============================================================== Table 10
r10 <- list(); n10 <- c()
for (v in c("fc_ww", "fc_kz", "fc_sa", "fc_hm", "fc_hr")) {
  if (!has(cov, v)) next
  r10[[length(r10) + 1]] <- reg(cov, "ret_collapse", c(v, AP), "ff49")
  r10[[length(r10) + 1]] <- reg(cov, "ret_stimulus", c(v, AP), "ff49")
  n10 <- c(n10, "Collapse", "Stimulus")
  p <- c(fc_hm = -0.045, fc_hr = -0.035, fc_ww = 0.045)[v]
  if (!is.na(p)) note(paste0("t10_", v), B(r10[[length(r10) - 1]], v), p, paste("Table 10 collapse coefficient,", LAB[[v]]))
}
save_table(reg_table(r10, c("fc_ww", "fc_kz", "fc_sa", "fc_hm", "fc_hr", AP), col_names = n10), "table10",
           "Table 10. Stock returns and measures of financial constraints", "FF49 fixed effects.")

# ============================================================== Appendix C
rawc <- c(affected_share = "Affected share", teamwork_share = "Teamwork share", customer_share = "Customer share",
          presence_share = "Presence share", dsales_q2 = "Q2 2020 year-on-year change in sales",
          home_share = "Share of jobs that can be performed from home",
          labor = "Labor intensity as number of employees / sales")
labc <- c(rawc, CFG$exposures)
cc <- Filter(function(v) has(cov, v), names(labc))
if (length(cc)) {
  tc1 <- summary_stats(cov, cc, labc)[, c("Variable", "N", "P25", "Median", "Mean", "P75", "SD")]
  num <- vapply(tc1, is.numeric, logical(1)); tc1[num] <- lapply(tc1[num], round, 3)
  save_table(tc1, "table_c1", "Table C1. Summary statistics, COVID-19 exposure measures")
  ex <- Filter(function(v) has(cov, v), names(CFG$exposures))
  cm <- round(lower_tri(cor(as.data.frame(cov)[, ex], use = "pairwise.complete.obs")), 2)
  tc2 <- data.frame(Measure = sprintf("(%d) %s", seq_along(ex), CFG$exposures[ex]), cm, check.names = FALSE, row.names = NULL)
  names(tc2)[-1] <- sprintf("(%d)", seq_along(ex))
  save_table(tc2, "table_c2", "Table C2. Correlation matrix, exposure measures")
}

save_table(rbindlist(TEXT), "text_numbers", "Numbers quoted in the text: replicated vs. paper")
write_workbook()
