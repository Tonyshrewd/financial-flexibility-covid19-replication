# ======================================================================================
# 00_setup.R - installs (if missing) and loads every package used in the replication
# Replication of Fahlenbrach, Rageth and Stulz (2021) - Anthony Lamini
# Called automatically by run_all.R; it can also be run on its own.
# ======================================================================================
pkgs <- c(
  "data.table",  # data handling
  "R.utils",     # reading .csv.gz files
  "openxlsx",    # Excel workbook of all tables
  "DBI",         # database interface
  "RPostgres",   # WRDS connection
  "rstudioapi",  # hidden password pop-up in RStudio
  "rmarkdown",   # builds the PDF
  "knitr",       # tables in the PDF
  "kableExtra",  # paper-style tables
  "tinytex"      # LaTeX for the PDF
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

# LaTeX is needed only for the PDF; install TinyTeX once if no LaTeX is present
if (!tinytex::is_tinytex() && !nzchar(Sys.which("pdflatex"))) {
  cat("No LaTeX found - installing TinyTeX (one time, a few minutes) ...\n")
  tinytex::install_tinytex()
}

cat("All packages installed and loaded.\n")
