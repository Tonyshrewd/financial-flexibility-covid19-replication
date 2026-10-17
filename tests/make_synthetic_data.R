# ======================================================================================
# Generate SIMULATED inputs with the same file names and columns as the WRDS / public
# pulls, so the whole pipeline can be tested end to end without data access.
# The numbers are random (effects planted so regressions have the paper's signs).
# They are NOT real data and the resulting tables mean nothing.
#
#   Rscript tests/make_synthetic_data.R /path/to/workdir
#   FFS_WORKDIR=/path/to/workdir FFS_EXTDIR=/path/to/workdir/external Rscript run_all.R --skip-download
# ======================================================================================
suppressPackageStartupMessages(library(data.table))
set.seed(7)
W <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(W)) W <- "synthetic"
RAW <- file.path(W, "data", "raw"); EXT <- file.path(W, "external")
dir.create(RAW, recursive = TRUE, showWarnings = FALSE); dir.create(EXT, recursive = TRUE, showWarnings = FALSE)

N <- 1800
NAICS3 <- as.character(c(211, 213, 236, 311, 324, 325, 334, 336, 423, 441, 442, 448, 451, 454, 481, 483,
                         486, 511, 512, 517, 518, 532, 541, 561, 713, 721, 722, 314, 313, 327))
gv <- sprintf("%06d", 100000 + 0:(N - 1))
n3 <- sample(NAICS3, N, TRUE)
naics6 <- paste0(n3, sample(100:999, N, TRUE)); naics6[1] <- "454110"
sic <- sample(100:7999, N, TRUE)
sic[1:120] <- sample(6000:6999, 120, TRUE); sic[121:160] <- sample(4900:4949, 40, TRUE)
sic[161:200] <- sample(8000:9999, 40, TRUE)
loc <- ifelse(runif(N) < .08, "CAN", "USA")
fwrite(data.table(gvkey = gv, conm = paste("FIRM", 1:N), sic = sic, naics = naics6, loc = loc, fic = loc,
                  priusa = "01", ipodate = NA), file.path(RAW, "company.csv"))
fwrite(data.table(gvkey = gv, firstyear = sample(1960:2014, N, TRUE)), file.path(RAW, "firstyear.csv"))

cash0 <- rbeta(N, .8, 2.5); ltd0 <- pmin(pmax(rbeta(N, 1.3, 3.2) * (1 - .4 * cash0), 0), 1.1)
size0 <- rnorm(N, 7.3, 2)
aff_share <- setNames(sample(c(8, 9, 10, 12, 16, 31, 38, 44, 48, 53, 64, 82), length(NAICS3), TRUE), NAICS3)
aff_hi <- as.numeric(aff_share[n3] > 16)
funda <- rbindlist(lapply(2003:2019, function(fy) {
  at <- exp(size0 + rnorm(N, 0, .1) + .03 * (fy - 2019)); sale <- at * runif(N, .3, 1.5)
  pr <- ifelse(runif(N) < .35, 0, rgamma(N, 1, scale = .04))
  data.table(gvkey = gv, datadate = sprintf("%d-12-31", fy), fyear = fy, fyr = 12, at = at,
             che = at * pmin(pmax(cash0 + rnorm(N, 0, .02), .001), .96), dlc = at * rgamma(N, .6, scale = .05),
             dltt = at * pmin(pmax(ltd0 + rnorm(N, 0, .02), 0), 1.1), dvc = at * pr * .5, dvp = 0, prstkc = at * pr * .5,
             xrd = ifelse(runif(N) < .5, NA, at * rgamma(N, 1, scale = .06)), xsga = sale * rgamma(N, 2, scale = .15),
             sale = sale, capx = at * rgamma(N, 2, scale = .02), cogs = sale * runif(N, .1, 1),
             gp = at * rnorm(N, .26, .2), ceq = at * runif(N, .1, .7), seq = at * runif(N, .1, .7), txdb = at * .01,
             ib = at * rnorm(N, .03, .08), dp = at * .04, ppent = at * runif(N, .05, .7), oibdp = at * rnorm(N, .1, .08),
             sstk = at * .005, dltis = at * .03, dltr = at * .03, dlcch = 0, csho = 100, prcc_f = 30,
             sich = sic, naicsh = naics6, emp = sale * rgamma(N, 2, scale = .0015))
}))
fwrite(funda, file.path(RAW, "funda.csv.gz"))
fwrite(data.table(gvkey = gv, datadate = "2019-12-31", filedate = ifelse(runif(N) < .05, "2020-06-15", "2020-02-28"),
                  srctype = "10K"), file.path(RAW, "filedate.csv"))

bd <- function(a, b) { x <- seq(as.Date(a), as.Date(b), by = "day"); x[!format(x, "%u") %in% c("6", "7")] }
dates <- c(bd("2005-12-01", "2009-03-31"), bd("2016-12-01", "2020-09-30")); TT <- length(dates)
mkt <- rnorm(TT, .0004, .01)
crash <- dates >= as.Date("2020-02-24") & dates <= as.Date("2020-03-23")
gfcw <- dates >= as.Date("2008-09-15") & dates <= as.Date("2009-03-09")
reb <- dates > as.Date("2020-03-23") & dates <= as.Date("2020-09-11")
mkt[crash] <- rnorm(sum(crash), -.022, .04); mkt[gfcw] <- rnorm(sum(gfcw), -.004, .03); mkt[reb] <- rnorm(sum(reb), .003, .015)
fwrite(data.table(date = dates, `Mkt-RF` = mkt, SMB = rnorm(TT, 0, .005), HML = rnorm(TT, 0, .005), RF = .00005,
                  UMD = rnorm(TT, 0, .006)), file.path(RAW, "ff_daily.csv"))
fwrite(data.table(date = dates, rf_annual_pct = 1.5, rf = .015 / 360), file.path(RAW, "rf_daily.csv"))
e <- round(seq(0, 10000, length.out = 50))
fwrite(data.table(ff49 = 1:49, ff49_name = paste0("Ind", 1:49), sic_lo = e[-50], sic_hi = e[-1] - 1), file.path(RAW, "ff49_map.csv"))

f19 <- funda[fyear == 2019]; cash19 <- f19$che / f19$at; ltd19 <- f19$dltt / f19$at
beta <- rnorm(N, 1.2, .5)
R <- outer(beta, mkt) + matrix(rnorm(N * TT, 0, .02), N) + .00005 +
  outer((.13 * cash19 - .16 * ltd19 - .10 * aff_hi + .30 * cash19 * aff_hi) / sum(crash), crash) +
  outer((.15 * cash19 - .13 * ltd19) / sum(gfcw), gfcw) + outer((.12 * cash19 - .10 * ltd19) / sum(reb), reb)
P <- exp(log(runif(N, .5, 80)) + t(apply(log1p(pmin(pmax(R, -.9), 3)), 1, cumsum)))
fwrite(data.table(gvkey = rep(gv, each = TT), iid = "01", datadate = rep(format(dates), N),
                  prccd = round(as.vector(t(P)), 4), ajexdi = 1, trfd = 1,
                  cshoc = rep(exp(size0) * 1e6 / 30, each = TT),
                  tpci = rep(ifelse(runif(N) < .01, "F", "0"), each = TT)), file.path(RAW, "secd.csv.gz"))

rated <- runif(N) < .35
fwrite(rbindlist(lapply(c("2006-12-31", "2017-02-28"), function(d) data.table(gvkey = gv[rated], datadate = d,
  splticrm = sample(c("AA", "A", "BBB", "BBB-", "BB+", "BB", "B", "CCC+"), sum(rated), TRUE)))), file.path(RAW, "ratings.csv"))
s19 <- f19$sale / 4
fwrite(rbindlist(lapply(c("2019Q1", "2019Q2", "2020Q1", "2020Q2"), function(cq) data.table(
  gvkey = gv, datadate = "2020-06-30", fyearq = as.integer(substr(cq, 1, 4)), fqtr = as.integer(substr(cq, 6, 6)),
  datacqtr = cq, oibdpq = s19 * rnorm(N, .08 - if (substr(cq, 1, 4) == "2020") .05 * aff_hi else 0, .05),
  saleq = s19 * (1 - if (cq == "2020Q2") .6 * aff_hi else 0) * runif(N, .8, 1.2), atq = NA))), file.path(RAW, "fundq.csv"))
ix <- which(runif(N) < .12)
fwrite(rbindlist(lapply(ix, function(i) data.table(gvkey = gv[i], datadate = "2019-12-31", srcdate = "2020-02-28",
  stype = "BUSSEG", sid = 1:2, sales = f19$sale[i] * c(.6, .4), naicss1 = c(naics6[i], paste0(sample(NAICS3, 1), "111")),
  sics1 = sic[i]))), file.path(RAW, "segments.csv"))

fwrite(data.table(naics3 = NAICS3, affected_share = aff_share, teamwork_share = sample(c(17, 25, 30, 35), 30, TRUE),
                  customer_share = sample(c(7, 8, 18, 30, 45), 30, TRUE), presence_share = sample(c(3, 9, 11, 20, 30), 30, TRUE)),
       file.path(EXT, "koren_peto.csv"))
fwrite(data.table(naics3 = NAICS3, teleworkable = runif(30, .1, .9)), file.path(EXT, "dingel_neiman.csv"))
hm <- data.table(gvkey = gv, year = 2015, delaycon = rnorm(N, -.012, .086))
fwrite(rbind(hm, copy(hm)[, year := 2006]), file.path(EXT, "hm_constraints.csv"))
io <- CJ(supplier_naics3 = NAICS3, customer_naics3 = NAICS3)[, value := rgamma(.N, 1, scale = 100)]
fwrite(io, file.path(EXT, "io_bea.csv")); fwrite(copy(io)[, value := rgamma(.N, 1)], file.path(EXT, "io_fhp.csv"))
fwrite(data.table(naics6 = c("454110", "324110"), affected = c(0, 1)), file.path(EXT, "manual_naics6.csv"))
fwrite(data.table(ff49 = c(9, 33, 40, 41)), file.path(EXT, "moodys_ff49.csv"))
fwrite(data.table(gvkey = gv, undrawn_credit_usd_m = exp(size0) * runif(N, 0, .15)), file.path(EXT, "credit_lines.csv"))
cat(sprintf("synthetic inputs written to %s: %s firm-days\n", W, format(N * TT, big.mark = ",")))
