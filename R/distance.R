# ============================================================================
# distance.R -- dissimilarity matrix for a measurement table.
#   * "gower"      : cluster::daisy(metric = "gower") -- exactly the call used in the
#                    paper. Numeric columns are divided by their range, binary columns
#                    can be asymmetric (joint absences ignored), factors are nominal.
#   * "euclidean" / "manhattan": stats::dist() after the chosen normalisation.
# ============================================================================

#' Distance matrix for a table
#'
#' @param tbl Output of [st_table_from_matrix()].
#' @param specs Data frame from [st_default_specs()] (possibly edited): columns
#'   `idx`, `name`, `type` (num, numlog, bin, binsym, cat, skip) and `weight`.
#' @param rows Row numbers of `tbl$rows` to use (default all).
#' @param metric `"gower"`, `"euclidean"` or `"manhattan"`.
#' @param norm Normalisation for numeric columns when metric is not Gower:
#'   `"minmax"`, `"zscore"` or `"none"`. (Gower always scales by the range.)
#' @return list(d, warnings, normalized, used)
#' @export
st_table_distance <- function(tbl, specs, rows = seq_len(nrow(tbl$rows)),
                              metric = c("gower", "euclidean", "manhattan"),
                              norm = c("minmax", "zscore", "none")) {
  metric <- match.arg(metric); norm <- match.arg(norm)
  warnings <- character()
  samples <- st_table_samples(tbl)[rows]
  cols <- list(); types <- character(); w <- numeric(); nm <- character()
  add <- function(x, type, weight, name) {
    cols[[length(cols) + 1]] <<- x; types <<- c(types, type); w <<- c(w, weight); nm <<- c(nm, name)
  }
  for (i in seq_len(nrow(specs))) {
    sp <- specs[i, ]
    if (sp$type == "skip" || !isTRUE(sp$weight > 0)) next
    raw <- tbl$rows[rows, sp$idx]
    if (sp$type %in% c("num", "numlog")) {
      x <- st_to_num(raw); x[st_is_na(raw)] <- NA
      if (sp$type == "numlog") {
        bad <- !is.na(x) & x <= 0
        if (any(bad)) warnings <- c(warnings, sprintf("\"%s\": %d values <= 0 cannot be log-transformed and are treated as missing.", sp$name, sum(bad)))
        x <- ifelse(!is.na(x) & x > 0, log10(x), NA_real_)
      }
      if (sum(!is.na(x)) < 2) { warnings <- c(warnings, sprintf("\"%s\": fewer than 2 valid numbers, not used.", sp$name)); next }
      add(x, "numeric", sp$weight, sp$name)
    } else if (sp$type %in% c("bin", "binsym")) {
      b <- st_to_binary(raw)
      if (!b$ok) { warnings <- c(warnings, sprintf("\"%s\": cannot be read as binary, not used. Check its type.", sp$name)); next }
      if (!is.null(b$mapping)) warnings <- c(warnings, sprintf("\"%s\": non-0/1 values read as binary (smaller = 0, larger = 1): %s", sp$name, paste(b$mapping, collapse = " / ")))
      add(b$values, if (sp$type == "bin") "asymm" else "symm", sp$weight, sp$name)
    } else if (sp$type == "cat") {
      x <- trimws(as.character(raw)); x[st_is_na(raw)] <- NA
      add(factor(x), "nominal", sp$weight, sp$name)
    }
  }
  if (!length(cols)) stop("No usable variables. Check the column types and weights.", call. = FALSE)
  nm <- make.unique(nm, sep = "_")
  df <- as.data.frame(stats::setNames(cols, nm), check.names = FALSE, stringsAsFactors = FALSE)
  rownames(df) <- samples

  ## numeric columns after normalisation (shown in the Excel export)
  eff_norm <- if (metric == "gower") "minmax" else norm
  scale_num <- function(x) {
    if (eff_norm == "minmax") { r <- diff(range(x, na.rm = TRUE)); if (r > 0) (x - min(x, na.rm = TRUE)) / r else x * 0 }
    else if (eff_norm == "zscore") { s <- stats::sd(x, na.rm = TRUE); if (isTRUE(s > 0)) (x - mean(x, na.rm = TRUE)) / s else x * 0 }
    else x
  }
  is_num <- types == "numeric"
  normalized <- if (any(is_num)) as.data.frame(lapply(df[is_num], scale_num), check.names = FALSE) else NULL
  if (!is.null(normalized)) rownames(normalized) <- samples

  if (metric == "gower") {
    tl <- list()
    if (any(types == "asymm")) tl$asymm <- nm[types == "asymm"]
    if (any(types == "symm"))  tl$symm  <- nm[types == "symm"]
    d <- withCallingHandlers(
      cluster::daisy(df, metric = "gower", type = tl, weights = w),
      warning = function(cond) {
        if (grepl("binary variable", conditionMessage(cond))) invokeRestart("muffleWarning")
      })
  } else {
    feats <- list()
    for (j in seq_along(cols)) {
      x <- df[[j]]
      if (types[j] == "numeric") feats[[length(feats) + 1]] <- scale_num(x) * (if (metric == "euclidean") sqrt(w[j]) else w[j])
      else if (types[j] %in% c("asymm", "symm")) feats[[length(feats) + 1]] <- x * (if (metric == "euclidean") sqrt(w[j]) else w[j])
      else for (l in levels(x)) feats[[length(feats) + 1]] <- as.numeric(x == l) * (if (metric == "euclidean") sqrt(w[j]) else w[j])
    }
    X <- do.call(cbind, feats); rownames(X) <- samples
    d <- stats::dist(X, method = metric)
  }
  attr(d, "Labels") <- samples
  list(d = d, warnings = warnings, normalized = normalized, used = length(cols))
}
