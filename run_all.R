# ======================================================================================
# Replication of Fahlenbrach, Rageth and Stulz (2021, RFS)
# Anthony Lamini - Advanced Financial Theorem
#
# HOW TO RUN
#   1. Double-click ffs_replication_R.Rproj (opens RStudio in this folder).
#   2. Open this file and click "Source" (top-right of the editor).
#   3. If the WRDS data is not yet downloaded, enter your WRDS username and password in
#      the pop-up boxes and approve the Duo request on your phone.
# Output: replication_report.pdf/.docx, replication_results_original.pdf and
#         replication_results_extended.pdf in this folder (plus output/<sample>/).
#
# The WRDS download runs only if data/raw/ is empty; later runs reuse the saved data.
# To force a fresh download:  force_download <- TRUE; source("run_all.R")
# ======================================================================================

# ---- work in the folder that contains this file, however it was started
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here)) {                                       # Rscript run_all.R
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(f)) here <- dirname(normalizePath(f))
  }
  if (!is.null(here) && file.exists(file.path(here, "config.R"))) setwd(here)
})
if (!file.exists("config.R")) stop("Open ffs_replication_R.Rproj first (or setwd() to the replication folder).")

source("00_setup.R")   # installs any missing packages and loads them all

# ---- download only when needed
needed <- c("funda", "company", "secd", "ratings", "fundq", "firstyear", "ff_daily", "ff49_map", "rf_daily")
have_data <- all(vapply(needed, function(n) any(file.exists(file.path("data", "raw", paste0(n, c(".csv.gz", ".csv"))))), logical(1)))
force <- exists("force_download") && isTRUE(force_download)
skip <- (have_data && !force) || (exists("skip_download") && isTRUE(skip_download)) ||
        "--skip-download" %in% commandArgs(trailingOnly = TRUE)
steps <- c("01_pull_wrds.R", "02_pull_public.R", "03_returns.R", "04_build_sample.R",
           "05_exposure.R", "06_tables.R", "07_figures.R")
if (skip) {
  # The WRDS download is skipped; step 2 still runs because it only reuses its cached
  # public files and fills in the Hoberg-Maksimovic data if that file is still empty.
  cat("WRDS data found in data/raw/ - skipping the WRDS download.\n")
  steps <- setdiff(steps, "01_pull_wrds.R")
}

run_step <- function(s) {
  t0 <- Sys.time()
  cat(sprintf("\n=== %s [%s] ===\n", s, Sys.getenv("FFS_SAMPLE", "original")))
  source(file.path("code", s), local = new.env())
  cat(sprintf("--- %s done in %.0f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

# Steps 1-3 (downloads and returns) are common to both samples
for (s in intersect(steps, c("01_pull_wrds.R", "02_pull_public.R", "03_returns.R"))) run_step(s)

# Steps 4-7 and the paper-style results PDF, once for each sample
for (smp in c("original", "extended")) {
  Sys.setenv(FFS_SAMPLE = smp)
  for (s in c("04_build_sample.R", "05_exposure.R", "06_tables.R", "07_figures.R")) run_step(s)
  cat(sprintf("\n=== Rendering replication_results_%s.pdf ===\n", smp))
  rmarkdown::render("replication_results.Rmd", output_file = sprintf("replication_results_%s.pdf", smp),
                    envir = new.env(), quiet = TRUE)
}
Sys.unsetenv("FFS_SAMPLE")

# Written report comparing the paper, the original-vintage sample and the extended sample
cat("\n=== Rendering replication_report (PDF and Word) ===\n")
rmarkdown::render("replication_report.Rmd", output_format = "pdf_document", envir = new.env(), quiet = TRUE)
rmarkdown::render("replication_report.Rmd", output_format = "word_document", envir = new.env(), quiet = TRUE)

cat("\nDone.\n",
    "  replication_report.pdf / .docx        - written report (comparison with the paper)\n",
    "  replication_results_original.pdf      - all tables and figures, original-vintage sample\n",
    "  replication_results_extended.pdf      - all tables and figures, extended sample\n",
    "  output/<sample>/tables/all_tables.xlsx - every table in Excel\n")
