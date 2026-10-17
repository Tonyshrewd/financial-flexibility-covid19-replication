# ======================================================================================
# Step 1 - Pull every WRDS input the paper uses and write it to data/raw/.
#
# Requires a WRDS account with Compustat North America (Fundamentals Annual & Quarterly,
# Security Daily, Ratings, Segments). Capital IQ is optional (credit-line robustness only).
# Connects with RPostgres. When run from RStudio (source("run_all.R")) you are asked
# for your WRDS username and password; the password goes into a hidden pop-up and is
# never saved. Approve the Duo push on your phone when it arrives.
# Alternatives: set WRDS_USERNAME / WRDS_PASSWORD with Sys.setenv(), or use ~/.pgpass.
#
# Outputs (csv.gz): funda, firstyear, company, filedate, secd, ratings, fundq, segments,
#                   ciq_credit (optional)
# ======================================================================================
source("config.R"); source("code/lib.R")
suppressPackageStartupMessages({ library(DBI); library(RPostgres) })

# Pop-up boxes in RStudio; plain prompts elsewhere. Nothing is saved to disk.
in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
ask_text <- function(prompt) {
  if (in_rstudio) return(trimws(rstudioapi::showPrompt("WRDS login", prompt, default = "")))
  trimws(readline(paste0(prompt, ": ")))
}
ask_secret <- function(prompt) {
  if (in_rstudio) return(rstudioapi::askForPassword(prompt))
  readline(paste0(prompt, ": "))
}
user <- Sys.getenv("WRDS_USERNAME", CFG$wrds_username)
if (!nzchar(user) && interactive()) user <- ask_text("WRDS username")
if (is.null(user) || !nzchar(user)) stop("No WRDS username entered - the download was cancelled.")
pw <- Sys.getenv("WRDS_PASSWORD", "")
if (!nzchar(pw) && interactive() && !file.exists(file.path(Sys.getenv("HOME"), ".pgpass")))
  pw <- ask_secret(sprintf("WRDS password for %s", user))
if (is.null(pw)) stop("No password entered - the download was cancelled.")

cat("Connecting to WRDS as", user, "- approve the Duo request (push or phone call) now ...\n")
args <- list(RPostgres::Postgres(), host = "wrds-pgdata.wharton.upenn.edu", port = 9737,
             dbname = "wrds", sslmode = "require", gssencmode = "disable",
             connect_timeout = 60, user = user)
if (nzchar(pw)) args$password <- pw
db <- do.call(dbConnect, args)
rm(pw)
cat("Connected.\n")

qry <- function(sql) as.data.table(dbGetQuery(db, sql))
save <- function(d, name) {
  fwrite(d, file.path(CFG$raw, paste0(name, ".csv.gz")))
  cat(sprintf("  wrote %s.csv.gz: %s rows\n", name, format(nrow(d), big.mark = ",")))
}

cat("Fundamentals Annual ...\n")
funda <- qry("
  select gvkey, datadate, fyear, fyr, at, che, dlc, dltt, dvc, dvp, prstkc, xrd, xsga, sale,
         capx, cogs, gp, ceq, seq, txdb, ib, dp, ppent, oibdp, sstk, dltis, dltr, dlcch,
         csho, prcc_f, sich, naicsh, emp
  from comp.funda
  where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
    and fyear between 2003 and 2019")
save(funda, "funda")

cat("First Compustat year (firm age) ...\n")
save(qry("
  select gvkey, min(fyear) as firstyear from comp.funda
  where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
  group by gvkey"), "firstyear")

cat("Company header ...\n")
company <- qry("select gvkey, conm, sic, naics, loc, fic, priusa, ipodate from comp.company")
save(company, "company")

cat("Filing dates ...\n")
fd <- tryCatch(qry("
  select gvkey, datadate, filedate, srctype from comp.co_filedate
  where datadate between '2006-01-01' and '2020-12-31'
    and srctype in ('10K', '10K405', '10KSB')"), error = function(e) NULL)
if (is.null(fd)) cat("  co_filedate not available; vintage filter will be skipped\n") else save(fd, "filedate")

keys <- sort(unique(funda[fyear %in% c(CFG$fy_covid, CFG$fy_gfc), gvkey]))
cat(sprintf("Security Daily for %s gvkeys ...\n", format(length(keys), big.mark = ",")))
chunks <- list()
for (rg in CFG$secd_ranges) {
  for (i in seq(1, length(keys), by = 1000)) {
    ks <- paste0("'", keys[i:min(i + 999, length(keys))], "'", collapse = ",")
    chunks[[length(chunks) + 1]] <- qry(sprintf("
      select gvkey, iid, datadate, prccd, ajexdi, trfd, cshoc, tpci
      from comp.secd
      where gvkey in (%s) and datadate between '%s' and '%s'", ks, rg[1], rg[2]))
    cat(sprintf("   %s..%s batch %d\n", rg[1], rg[2], (i - 1) %/% 1000 + 1))
  }
}
secd <- rbindlist(chunks)
secd <- merge(secd, company[!is.na(priusa), .(gvkey, iid = priusa)], by = c("gvkey", "iid"))  # primary US issue
save(secd, "secd")

cat("Ratings ...\n")
save(qry("select gvkey, datadate, splticrm from comp.adsprate
        where datadate between '2005-01-01' and '2017-12-31'"), "ratings")

cat("Fundamentals Quarterly ...\n")
save(qry("
  select gvkey, datadate, fyearq, fqtr, datacqtr, oibdpq, saleq, atq from comp.fundq
  where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
    and datadate between '2018-10-01' and '2020-12-31'"), "fundq")

cat("Segments ...\n")
seg <- NULL
for (lib in c("compsegd", "comp_segments_hist_daily", "compseg")) {
  seg <- tryCatch(qry(sprintf("
    select gvkey, datadate, srcdate, stype, sid, sales, naicss1, sics1
    from %s.wrds_segmerged
    where datadate between '2019-01-01' and '2020-06-30' and stype in ('BUSSEG', 'OPSEG')", lib)),
    error = function(e) NULL)
  if (!is.null(seg)) { cat("  using", lib, "\n"); break }
}
if (is.null(seg)) cat("  !! no segments library found; Table 9 will be skipped\n") else save(seg, "segments")

cat("Capital IQ undrawn credit lines (optional) ...\n")
ciq <- tryCatch(qry("
  select g.gvkey, d.periodenddate, d.capitalstructuresubtypeid, d.dataitemvalue
  from ciq_capstrct.wrds_debt d
  join ciq_common.wrds_gvkey g on d.companyid = g.companyid
  where d.periodenddate between '2019-06-01' and '2020-06-30'"), error = function(e) NULL)
if (is.null(ciq)) {
  cat("  skipped (no Capital IQ access or different schema).\n",
      " -> you can instead place data/external/credit_lines.csv (gvkey, undrawn_credit_usd_m)\n")
} else {
  save(ciq, "ciq_credit")
  cat("  NOTE: aggregate undrawn revolver amounts per gvkey into data/external/credit_lines.csv\n")
}
dbDisconnect(db)
cat("Done.\n")
