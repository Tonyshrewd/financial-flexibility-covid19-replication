# ======================================================================================
# Step 7 - Figures (base R graphics, no extra packages).
#
# Figure 1: cumulative daily log excess returns, Feb 3 - Sep 11, 2020, for equal-weighted
#           portfolios of all firms, high-flexibility firms (top-quartile cash and
#           bottom-quartile LT debt) and low-flexibility firms (the reverse). Needs data.
# Figure 2: Merton (1974) calibration (Section 5.1). Needs no data:
#           V0 = 100, V_shock = 70, sigma = 30%, r = 3%, q = 1%, T = 5.
#
#   Rscript code/07_figures.R          # both (Figure 1 skipped if data missing)
#   Rscript code/07_figures.R --fig2   # Figure 2 only
# ======================================================================================
source("config.R")
suppressPackageStartupMessages(library(data.table))

# Equity as a European call on firm assets with continuous payout q (Merton 1974)
merton_equity <- function(V, F, sigma, r = 0.03, q = 0.01, T = 5) {
  F <- pmax(F, 1e-12)
  d1 <- (log(V / F) + (r - q + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  V * exp(-q * T) * pnorm(d1) - F * exp(-r * T) * pnorm(d2)
}

figure2 <- function() {
  V0 <- 100; V1 <- 70; s0 <- 0.30; ar <- log(V1 / V0)
  lev <- seq(0, 80, 2)
  dA <- log(merton_equity(V1, lev, s0) / merton_equity(V0, lev, s0)) - ar
  dA[1] <- 0
  levB <- seq(0, 78, 3); vol <- seq(0.30, 1.34, 0.04)
  dB <- outer(levB, vol, function(L, S) log(merton_equity(V1, L, S) / merton_equity(V0, L, s0)) - ar)

  draw <- function() {
    par(mfrow = c(2, 1), mar = c(4.5, 4.5, 3, 1))
    plot(lev, 100 * dA, pch = 19, col = "#3366cc", xlim = c(0, 90), ylim = c(-50, 40),
         xlab = "Leverage (in %)", ylab = "Equity return - asset return (%)",
         main = "(A) Merton model calibration with constant asset volatility")
    abline(h = 0, col = "grey50"); grid()
    z <- 100 * dB
    zc <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
    pal <- colorRampPalette(c("#d7301f", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))(100)
    cols <- pal[cut(zc, 100)]
    persp(levB, 100 * vol, z, theta = -35, phi = 22, col = cols, border = NA, shade = 0.3, ticktype = "detailed",
          xlab = "Leverage (%)", ylab = "Asset volatility (%)", zlab = "Equity - asset return (%)",
          main = "(B) Merton model calibration with varying asset volatility", expand = 0.7, cex.axis = 0.7)
  }
  png(file.path(CFG$figures, "figure2_merton.png"), width = 1600, height = 2200, res = 200); draw(); invisible(dev.off())
  pdf(file.path(CFG$figures, "figure2_merton.pdf"), width = 8, height = 11); draw(); invisible(dev.off())
  fwrite(data.table(leverage_pct = lev, equity_minus_asset_return = dA), file.path(CFG$figures, "figure2_panelA_data.csv"))
  pb <- data.table(leverage_pct = levB, dB); setnames(pb, c("leverage_pct", sprintf("vol_%.2f", vol)))
  fwrite(pb, file.path(CFG$figures, "figure2_panelB_data.csv"))

  grid_v <- seq(0.30, 1.5, 0.001)
  be <- grid_v[which.min(abs(log(merton_equity(V1, 30, grid_v) / merton_equity(V0, 30, s0)) - ar))]
  cat(sprintf("  Figure 2: diff at 80%% leverage = %.1f%% (paper panel A ~ -44%%); break-even asset vol at 30%% leverage = %.0f%% (+%.0f pp)\n",
              100 * tail(dA, 1), 100 * be, 100 * (be - s0)))
}

figure1 <- function() {
  p <- file.path(CFG$derived_common, "daily_fig1.csv.gz")
  if (!file.exists(p)) { cat("  Figure 1 skipped (run steps 1-5 first)\n"); return(invisible()) }
  d <- fread(p, colClasses = list(character = "gvkey"))[, date := as.Date(date)]
  f <- fread(file.path(CFG$derived, "firm_covid.csv"), colClasses = list(character = "gvkey"))[, .(gvkey, high_ff, low_ff)]
  d[, gvkey := sprintf("%06d", as.integer(gvkey))]; f[, gvkey := sprintf("%06d", as.integer(gvkey))]
  d <- merge(d, f, by = "gvkey")
  ser <- list("All sample firms - EW" = d, "High financial flexibility - EW" = d[high_ff == 1],
              "Low financial flexibility - EW" = d[low_ff == 1])
  cum <- lapply(ser, function(x) x[!is.na(exret), .(r = mean(exret)), by = date][order(date)][, .(date, cum = cumsum(r))])
  ns <- vapply(ser, function(x) uniqueN(x$gvkey), 0)
  draw <- function() {
    par(mar = c(4.5, 4.5, 4, 1))
    yl <- range(unlist(lapply(cum, `[[`, "cum")))
    plot(cum[[1]]$date, cum[[1]]$cum, type = "l", col = "red", lty = 2, lwd = 2, ylim = yl,
         xlab = "Date", ylab = "Cumulative daily log excess returns",
         main = "Evolution of stock returns\nFebruary 3 to September 11, 2020")
    lines(cum[[2]]$date, cum[[2]]$cum, col = "#4a6cd4", lty = 3, lwd = 2)
    lines(cum[[3]]$date, cum[[3]]$cum, col = "darkgreen", lty = 4, lwd = 2)
    abline(v = CFG$collapse[2], lty = 2); text(CFG$collapse[2], yl[2], " March 23", adj = c(0, 1))
    grid()
    legend("bottomright", sprintf("%s (n=%d)", names(ser), ns), col = c("red", "#4a6cd4", "darkgreen"),
           lty = c(2, 3, 4), lwd = 2, cex = 0.8, bg = "white")
  }
  png(file.path(CFG$figures, "figure1_returns.png"), width = 1800, height = 1100, res = 200); draw(); invisible(dev.off())
  pdf(file.path(CFG$figures, "figure1_returns.pdf"), width = 9, height = 5.5); draw(); invisible(dev.off())
  out <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE),
                Map(function(x, n) setnames(copy(x), "cum", n), cum, names(cum)))
  fwrite(out, file.path(CFG$figures, "figure1_data.csv"))
  gap <- tail(cum[[2]]$cum, 1) - tail(cum[[3]]$cum, 1)
  cat(sprintf("  Figure 1: high-low gap on %s = %.1f pp (paper: > 20 pp by end of August)\n", max(d$date), 100 * gap))
}

figure2()
if (!("--fig2" %in% commandArgs(trailingOnly = TRUE))) figure1()
