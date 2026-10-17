# ======================================================================================
# make_share_zip.R - builds the files to send, after run_all.R has finished.
#
# Open ffs_replication_R.Rproj, open this file and click "Source". It creates, next to this
# folder:
#   Lamini_FRS_replication.zip   code, README, report (PDF + Word), results PDFs, all tables
#                                and figures, the hand-collected data and the analysis
#                                datasets (data/derived) - small enough to e-mail.
#   Lamini_FRS_raw_data.zip      the raw WRDS/Compustat and public downloads (data/raw).
#                                Large; share it through OneDrive/Google Drive if needed.
# Compustat data may only be shared with people covered by a WRDS subscription.
# ======================================================================================
if (!file.exists("config.R")) stop("Open ffs_replication_R.Rproj first (or setwd() to the replication folder).")
need <- c("replication_report.pdf", "replication_results_original.pdf", "replication_results_extended.pdf")
if (!all(file.exists(need))) stop("Run run_all.R first - the report and results PDFs are missing.")

all_files <- list.files(".", recursive = TRUE, all.files = FALSE)
drop <- grepl("(^|/)\\.DS_Store$|\\.log$|\\.tex$|_files/|^\\.Rproj\\.user/|\\.Rhistory$", all_files) |
        grepl("daily_fig1\\.csv\\.gz$|firm_covid_base\\.csv$", all_files)   # intermediates rebuilt by run_all.R
code_files <- all_files[!drop & !grepl("^data/raw/", all_files)]
raw_files  <- all_files[!drop & grepl("^data/raw/", all_files)]

here <- basename(getwd())
out1 <- file.path("..", "Lamini_FRS_replication.zip")
out2 <- file.path("..", "Lamini_FRS_raw_data.zip")
owd <- setwd("..")
unlink(basename(c(out1, out2)))
zip(basename(out1), file.path(here, code_files), flags = "-r9Xq")
if (length(raw_files)) zip(basename(out2), file.path(here, raw_files), flags = "-r9Xq")
setwd(owd)
sz <- function(f) if (file.exists(f)) sprintf("%.1f MB", file.size(f) / 1e6) else "not created"
cat("Created in", normalizePath(".."), ":\n",
    " Lamini_FRS_replication.zip -", sz(out1), "\n",
    " Lamini_FRS_raw_data.zip    -", sz(out2), "\n")
