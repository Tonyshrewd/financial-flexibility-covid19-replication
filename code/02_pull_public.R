# ======================================================================================
# Step 2 - Download the free inputs.
#   Ken French data library: daily Fama-French 3 factors, daily momentum, FF49 SIC map
#   FRED: 1-month Treasury rate (DGS1MO)
# Outputs in data/raw/: ff_daily.csv, ff49_map.csv, rf_daily.csv
# If a download is blocked, save the files by hand into data/raw/manual/ under the same
# names as in the URLs; the script uses local copies when present.
# ======================================================================================
source("config.R"); source("code/lib.R")

FF <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
urls <- c(ff3 = paste0(FF, "F-F_Research_Data_Factors_daily_CSV.zip"),
          mom = paste0(FF, "F-F_Momentum_Factor_daily_CSV.zip"),
          ff49 = paste0(FF, "Siccodes49.zip"),
          rf = sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", CFG$rf_series))
man <- file.path(CFG$raw, "manual"); dir.create(man, showWarnings = FALSE)

fetch <- function(url) {
  local <- file.path(man, sub("\\?.*$", "", basename(url)))
  if (basename(local) == "fredgraph.csv") local <- file.path(man, paste0(CFG$rf_series, ".csv"))
  if (!file.exists(local)) download.file(url, local, mode = "wb", quiet = TRUE)
  local
}
unzip_lines <- function(zipfile) {
  f <- unzip(zipfile, list = TRUE)$Name[1]
  readLines(unz(zipfile, f), warn = FALSE, encoding = "latin1")
}
parse_ff_daily <- function(lines) {
  hdr <- grep("^\\s*,", lines)[1]
  cols <- trimws(strsplit(lines[hdr], ",")[[1]])[-1]
  dat <- lines[grepl("^\\s*[0-9]{8}\\s*,", lines)]
  m <- do.call(rbind, strsplit(dat, ","))
  out <- data.table(date = as.Date(trimws(m[, 1]), "%Y%m%d"))
  for (j in seq_along(cols)) out[[cols[j]]] <- as.numeric(m[, j + 1]) / 100
  out
}
parse_ff49 <- function(lines) {
  rows <- list(); ind <- NA; nm <- NA
  for (l in lines) {
    r <- regmatches(l, regexec("^\\s*([0-9]{4})-([0-9]{4})", l))[[1]]
    h <- regmatches(l, regexec("^\\s*([0-9]{1,2})\\s+(\\S+)\\s+", l))[[1]]
    if (length(r) && !is.na(ind)) rows[[length(rows) + 1]] <- list(ind, nm, as.integer(r[2]), as.integer(r[3]))
    else if (length(h)) { ind <- as.integer(h[2]); nm <- h[3] }
  }
  rbindlist(lapply(rows, function(x) data.table(ff49 = x[[1]], ff49_name = x[[2]], sic_lo = x[[3]], sic_hi = x[[4]])))
}

public_done <- all(file.exists(file.path(CFG$raw, c("ff_daily.csv", "ff49_map.csv", "rf_daily.csv"))))
if (public_done && !(exists("force_download") && isTRUE(force_download))) {
  cat("Fama-French, FF49 and FRED files already in data/raw/ - not downloaded again\n")
} else {
  ff3 <- parse_ff_daily(unzip_lines(fetch(urls["ff3"])))
  mom <- parse_ff_daily(unzip_lines(fetch(urls["mom"])))
  setnames(mom, 2, "UMD")
  ff <- merge(ff3, mom[, .(date, UMD)], by = "date", all.x = TRUE)
  fwrite(ff, file.path(CFG$raw, "ff_daily.csv"))
  cat(sprintf("ff_daily.csv: %d days, %s..%s\n", nrow(ff), min(ff$date), max(ff$date)))

  ff49 <- parse_ff49(unzip_lines(fetch(urls["ff49"])))
  fwrite(ff49, file.path(CFG$raw, "ff49_map.csv"))
  cat(sprintf("ff49_map.csv: %d industries, %d SIC ranges\n", uniqueN(ff49$ff49), nrow(ff49)))

  rf <- fread(fetch(urls["rf"]))
  setnames(rf, c("date", "rf_annual_pct"))
  rf[, `:=`(date = as.Date(date), rf_annual_pct = suppressWarnings(as.numeric(rf_annual_pct)))]
  setorder(rf, date)
  rf[, rf_annual_pct := nafill(rf_annual_pct, "locf")]
  rf[, rf := rf_annual_pct / 100 / CFG$rf_daycount]
  fwrite(rf, file.path(CFG$raw, "rf_daily.csv"))
  cat(sprintf("rf_daily.csv: %d days\n", nrow(rf)))
}

# --------------------------------------------------------------------------------------
# Hoberg and Maksimovic (2015) text-based financial constraints (Table 10, columns 7-8).
# Downloaded from the authors' data site and converted to data/external/hm_constraints.csv
# (gvkey, year, delaycon). Skipped if that file already has data. A failure here only
# means the two HM columns of Table 10 are left out; nothing else depends on it.
# --------------------------------------------------------------------------------------
hm_out <- file.path(CFG$ext, "hm_constraints.csv")
if (is.null(read_ext("hm_constraints.csv"))) {
  hm_url <- "https://faculty.marshall.usc.edu/Gerard-Hoberg/MaxDataSite/idata/ConstraintsDatabase_ext2015.zip"
  res <- tryCatch({
    z <- fetch(hm_url)
    files <- unzip(z, list = TRUE)
    files <- files[grepl("\\.(txt|csv|tsv)$", files$Name, ignore.case = TRUE), ]
    if (!nrow(files)) stop("no text file inside the zip")
    f <- files$Name[which.max(files$Length)]
    tmp <- file.path(man, "hm_unzipped"); dir.create(tmp, showWarnings = FALSE)
    unzip(z, files = f, exdir = tmp)
    hm <- fread(file.path(tmp, f), showProgress = FALSE)
    setnames(hm, tolower(names(hm)))
    yr <- intersect(c("year", "fyear", "yr"), names(hm))[1]
    if (!all(c("gvkey", "delaycon") %in% names(hm)) || is.na(yr))
      stop(sprintf("unexpected columns: %s", paste(names(hm), collapse = ", ")))
    hm <- hm[, .(gvkey = sprintf("%06d", as.integer(gvkey)), year = as.integer(get(yr)), delaycon = as.numeric(delaycon))]
    writeLines(c("# Hoberg and Maksimovic (2015), Redefining financial constraints: A text-based analysis, RFS 28(5).",
                 "# Source: faculty.marshall.usc.edu/Gerard-Hoberg/MaxDataSite (ConstraintsDatabase_ext2015.zip)"), hm_out)
    fwrite(hm[!is.na(delaycon)], hm_out, append = TRUE, col.names = TRUE)
    sprintf("hm_constraints.csv: %s firm-years, %d-%d", format(nrow(hm), big.mark = ","), min(hm$year, na.rm = TRUE), max(hm$year, na.rm = TRUE))
  }, error = function(e) paste("Hoberg-Maksimovic data not added:", conditionMessage(e),
                               "- Table 10 will omit the HM columns. See data/external/README.md."))
  cat(res, "\n")
} else cat("hm_constraints.csv already present\n")
