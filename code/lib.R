# Shared helpers: io, industry mapping, ratings, regressions, table formatting.
suppressPackageStartupMessages(library(data.table))

# ------------------------------------------------------------------ io
read_raw <- function(name) {
  for (ext in c(".csv.gz", ".csv")) {
    p <- file.path(CFG$raw, paste0(name, ext))
    if (file.exists(p)) {
      d <- fread(p, showProgress = FALSE)
      for (cc in intersect(c("gvkey", "iid", "tpci", "naics", "naicsh", "naicss1", "priusa",
                             "datacqtr", "splticrm", "loc"), names(d)))
        set(d, j = cc, value = as.character(d[[cc]]))
      return(d)
    }
  }
  stop(sprintf("data/raw/%s.csv(.gz) missing - run 01_pull_wrds.R / 02_pull_public.R", name))
}

# Hand-supplied files: lines starting with '#' are comments; NULL if no data rows.
read_ext <- function(name) {
  p <- file.path(CFG$ext, name)
  if (!file.exists(p)) return(NULL)
  l <- readLines(p, warn = FALSE)
  l <- l[!grepl("^\\s*#", l) & nzchar(trimws(l))]
  if (length(l) < 2) return(NULL)
  fread(text = paste(l, collapse = "\n"), colClasses = "character")
}

gvkey <- function(x) sprintf("%06d", as.integer(as.character(x)))

# ------------------------------------------------------------------ industries
ff49_from_sic <- function(sic) {
  m <- read_raw("ff49_map")
  sic <- suppressWarnings(as.numeric(sic))
  out <- rep(NA_real_, length(sic))
  for (i in seq_len(nrow(m))) out[!is.na(sic) & sic >= m$sic_lo[i] & sic <= m$sic_hi[i]] <- m$ff49[i]
  out[is.na(out) & !is.na(sic)] <- 49          # FF convention: unmapped -> "Other"
  out
}

naics_str <- function(x, n) {
  s <- sub("\\.0$", "", as.character(x))
  ifelse(!is.na(s) & grepl(sprintf("^[0-9]{%d,}$", n), s), substr(s, 1, n), NA_character_)
}

# ------------------------------------------------------------------ ratings
.SCALE <- c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-",
            "BB+", "BB", "BB-", "B+", "B", "B-", "CCC+", "CCC", "CCC-", "CC", "C")
rating_flags <- function(r) {
  k <- match(toupper(trimws(r)), .SCALE)
  list(ig = as.numeric(!is.na(k) & k <= 10), hy = as.numeric(!is.na(k) & k >= 11))
}

# ------------------------------------------------------------------ stats
winsor <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
cap_top <- function(x, p = 0.99) pmin(x, quantile(x, p, na.rm = TRUE))
# Strictly above P75 (reproduces the Table C1 shares, e.g. 0.203 for affected share)
top_q <- function(x) ifelse(is.na(x), NA_real_, as.numeric(x > quantile(x, .75, na.rm = TRUE)))
bottom_q <- function(x) ifelse(is.na(x), NA_real_, as.numeric(x <= quantile(x, .25, na.rm = TRUE)))
# Quartiles by rank so ties at zero split evenly (Table 7 group sizes)
rank_quartile <- function(x) {
  out <- rep(NA_real_, length(x)); ok <- !is.na(x)
  r <- rank(x[ok], ties.method = "first")
  out[ok] <- ceiling(4 * r / sum(ok))
  out
}
has <- function(d, v) v %in% names(d) && sum(!is.na(d[[v]])) > 30 && uniqueN(na.omit(d[[v]])) > 1

# ------------------------------------------------------------------ regression
# OLS with fixed effects as dummies; OLS or CR1 cluster-robust standard errors.
reg <- function(d, y, xs, fe = NULL, cluster = NULL) {
  cols <- unique(c(y, xs, fe, cluster))
  dd <- as.data.frame(d)[, cols, drop = FALSE]
  for (cc in setdiff(cols, c(fe, cluster))) dd[[cc]] <- as.numeric(dd[[cc]])
  dd <- dd[stats::complete.cases(dd) & is.finite(rowSums(as.matrix(dd[, c(y, xs)]))), , drop = FALSE]
  keep <- xs[vapply(xs, function(x) {
    v <- dd[[x]]
    if (length(unique(v)) <= 1) return(FALSE)
    if (!is.null(fe) && max(tapply(v, dd[[fe]], function(z) length(unique(z)))) <= 1) return(FALSE)
    TRUE
  }, logical(1))]
  rhs <- c(if (length(keep)) keep else "1", if (!is.null(fe)) sprintf("factor(%s)", fe))
  m <- lm(reformulate(rhs, y), data = dd)
  b <- coef(m); ok <- !is.na(b)
  X <- model.matrix(m)[, ok, drop = FALSE]
  if (is.null(cluster)) {
    V <- vcov(m)[ok, ok]
  } else {
    e <- resid(m); g <- dd[[cluster]]; n <- nrow(X); k <- ncol(X); G <- length(unique(g))
    XtXi <- solve(crossprod(X))
    meat <- Reduce(`+`, lapply(split(seq_len(n), g), function(i) {
      s <- crossprod(X[i, , drop = FALSE], e[i]); s %*% t(s) }))
    V <- (G / (G - 1)) * ((n - 1) / (n - k)) * XtXi %*% meat %*% XtXi
  }
  b <- b[ok]; se <- sqrt(diag(V)); t <- b / se
  dfr <- m$df.residual
  p <- if (is.null(cluster)) 2 * pt(-abs(t), dfr) else 2 * pt(-abs(t), length(unique(dd[[cluster]])) - 1)
  list(b = b, se = se, t = t, p = p, V = V, n = nrow(dd), r2a = summary(m)$adj.r.squared,
       df = dfr, xs = xs, kept = intersect(keep, names(b)))
}
B <- function(r, x) if (x %in% names(r$b)) unname(r$b[x]) else NA_real_
# One-sided p-value for H1: coef(a) > coef(b)
one_sided_diff <- function(r, a, b) {
  if (!all(c(a, b) %in% names(r$b))) return(NA_real_)
  d <- r$b[a] - r$b[b]; s <- sqrt(r$V[a, a] + r$V[b, b] - 2 * r$V[a, b])
  unname(1 - pt(d / s, r$df))
}
stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))

# Paper-style table: coefficient row with stars, t-statistic row in parentheses.
reg_table <- function(regs, rows, labels = CFG$labels, extra = NULL, col_names = NULL) {
  lab <- function(x) if (!is.na(labels[x])) labels[[x]] else x
  out <- list()
  for (x in rows) {
    if (!any(vapply(regs, function(r) x %in% names(r$b), logical(1)))) next
    cr <- c(lab(x)); tr <- c("")
    for (r in regs) {
      if (x %in% names(r$b)) {
        cr <- c(cr, sprintf("%.3f%s", r$b[x], stars(r$p[x])))
        tr <- c(tr, sprintf("(%.2f)", r$t[x]))
      } else { cr <- c(cr, ""); tr <- c(tr, "") }
    }
    out[[length(out) + 1]] <- cr; out[[length(out) + 1]] <- tr
  }
  for (k in names(extra)) out[[length(out) + 1]] <- c(k, extra[[k]])
  out[[length(out) + 1]] <- c("Observations", vapply(regs, function(r) format(r$n, big.mark = ","), ""))
  out[[length(out) + 1]] <- c("Adjusted R2", vapply(regs, function(r) sprintf("%.3f", r$r2a), ""))
  df <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(df) <- c("", if (is.null(col_names)) sprintf("(%d)", seq_along(regs)) else col_names)
  df
}

summary_stats <- function(d, cols, labels = CFG$labels) {
  rows <- lapply(cols, function(c) {
    s <- as.numeric(d[[c]]); s <- s[!is.na(s)]
    if (!length(s)) return(NULL)
    q <- quantile(s, c(.01, .05, .25, .5, .75, .95, .99), names = FALSE)
    data.frame(Variable = if (!is.na(labels[c])) labels[[c]] else c, N = length(s), Min = min(s),
               P1 = q[1], P5 = q[2], P25 = q[3], Median = q[4], Mean = mean(s), P75 = q[5],
               P95 = q[6], P99 = q[7], Max = max(s), SD = sd(s))
  })
  do.call(rbind, rows)
}

# ------------------------------------------------------------------ output
.ALL <- new.env()
.ALL$tabs <- list()
to_md <- function(df) {
  f <- function(v) {
    if (!is.numeric(v)) return(ifelse(is.na(v), "", as.character(v)))
    whole <- all(is.na(v) | v == round(v))
    ifelse(is.na(v), "", formatC(v, format = "f", digits = if (whole) 0 else 3, big.mark = ","))
  }
  body <- apply(as.data.frame(lapply(df, f), stringsAsFactors = FALSE), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(paste0("| ", paste(names(df), collapse = " | "), " |"),
          paste0("|", strrep("---|", ncol(df))), body), collapse = "\n")
}
save_table <- function(df, name, title, note = "") {
  fwrite(df, file.path(CFG$tables, paste0(name, ".csv")))
  writeLines(c(paste("##", title), "", if (nzchar(note)) c(note, ""), to_md(df)),
             file.path(CFG$tables, paste0(name, ".md")))
  .ALL$tabs[[name]] <- list(title = title, note = note, df = df)
  cat(sprintf("  %s: %s\n", name, title))
}
write_workbook <- function() {
  wb <- openxlsx::createWorkbook()
  md <- c()
  for (nm in names(.ALL$tabs)) {
    t <- .ALL$tabs[[nm]]; sh <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, t$title, startRow = 1)
    openxlsx::writeData(wb, sh, substr(t$note, 1, 250), startRow = 2)
    openxlsx::writeData(wb, sh, t$df, startRow = 3)
    openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(t$df)), widths = "auto")
    md <- c(md, paste("##", t$title), "", t$note, "", to_md(t$df), "")
  }
  openxlsx::saveWorkbook(wb, file.path(CFG$tables, "all_tables.xlsx"), overwrite = TRUE)
  writeLines(md, file.path(CFG$tables, "all_tables.md"))
  cat("  wrote all_tables.xlsx and all_tables.md\n")
}
