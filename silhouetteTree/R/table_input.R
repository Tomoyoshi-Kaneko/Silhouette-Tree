# ============================================================================
# table_input.R -- reading a measurement table (CSV / TSV / Excel / pasted text)
# and guessing the type of every column.
# ============================================================================

.na_words <- c("", "na", "n/a", "nan", "null", "#n/a", "undefined")
.truthy <- c("1", "yes", "y", "true", "t", "+", "pos", "positive", "\u25cb", "\u25ef", "\u3042\u308a", "\u6709", "\u967d\u6027", "present")
.falsy  <- c("0", "no", "n", "false", "f", "-", "neg", "negative", "\u00d7", "\u2715", "\u306a\u3057", "\u7121", "\u9670\u6027", "absent")

st_is_na <- function(x) is.na(x) | tolower(trimws(as.character(x))) %in% .na_words

st_to_num <- function(x) {
  x <- trimws(as.character(x))
  ok <- !is.na(x) & grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x)
  out <- rep(NA_real_, length(x)); out[ok] <- as.numeric(x[ok]); out
}

## Delimiter guess for pasted / CSV text: tab, comma or semicolon.
.guess_delim <- function(txt) {
  head <- paste(utils::head(strsplit(txt, "\r?\n")[[1]], 5), collapse = "\n")
  cnt <- function(ch) lengths(regmatches(head, gregexpr(ch, head, fixed = TRUE)))
  t <- cnt("\t"); c <- cnt(","); s <- cnt(";")
  if (t > 0 && t >= c && t >= s) "\t" else if (s > c) ";" else ","
}

#' Parse delimited text into a character matrix (first row = header)
#' @inheritParams st_params
#' @export
st_matrix_from_text <- function(txt) {
  txt <- sub("^\ufeff", "", txt)
  if (!nzchar(trimws(txt))) return(NULL)
  delim <- .guess_delim(txt)
  df <- utils::read.table(text = txt, sep = delim, header = FALSE, quote = "\"", comment.char = "",
                          colClasses = "character", fill = TRUE, strip.white = TRUE,
                          stringsAsFactors = FALSE, na.strings = character(0), blank.lines.skip = TRUE,
                          check.names = FALSE, encoding = "UTF-8")
  M <- as.matrix(df)
  M[] <- enc2utf8(M); Encoding(M) <- "UTF-8"
  M
}

#' Read a table file (.csv, .tsv, .txt, .xlsx, .xls) into a character matrix
#' @param sheet Sheet name or number (Excel only).
#' @inheritParams st_params
#' @export
st_read_table_file <- function(path, sheet = 1) {
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    df <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text"))
    M <- as.matrix(df); M[is.na(M)] <- ""; dimnames(M) <- NULL
    return(M)
  }
  st_matrix_from_text(st_read_text_guess(path))
}

#' Split a character matrix into header + rows (optionally transposed)
#' @inheritParams st_params
#' @export
st_table_from_matrix <- function(M, transpose = FALSE, label_col = 1) {
  if (is.null(M) || nrow(M) < 1) return(NULL)
  if (transpose) M <- t(M)
  header <- .as_utf8(trimws(M[1, ]))
  blank <- which(is.na(header) | header == "")
  header[blank] <- paste0("col", blank)
  rows <- M[-1, , drop = FALSE]
  if (nrow(rows)) {
    keep <- apply(rows, 1, function(r) any(!st_is_na(r)))
    rows <- rows[keep, , drop = FALSE]
  }
  label_col <- max(1L, min(as.integer(label_col), length(header)))
  list(header = header, rows = rows, label_col = label_col)
}

## Sample names from the label column, made unique.
st_table_samples <- function(tbl) {
  nm <- trimws(as.character(tbl$rows[, tbl$label_col]))
  nm[is.na(nm) | nm == ""] <- paste0("row_", which(is.na(nm) | nm == ""))
  .as_utf8(make.unique(nm, sep = "_"))
}

#' Guess the type of one column
#' @return list(type = "num" | "bin" | "cat" | "skip", reason = "")
#' @inheritParams st_params
#' @export
st_detect_column <- function(vals) {
  non <- vals[!st_is_na(vals)]
  if (!length(non)) return(list(type = "skip", reason = "empty"))
  nums <- st_to_num(non); allnum <- !anyNA(nums)
  u <- if (allnum) unique(nums) else unique(trimws(non))
  if (length(u) <= 1) return(list(type = "skip", reason = "const"))
  if (allnum) return(list(type = if (all(u %in% c(0, 1))) "bin" else "num", reason = ""))
  low <- tolower(u)
  if (length(u) == 2 && all(low %in% c(.truthy, .falsy)) && any(low %in% .truthy) && any(low %in% .falsy))
    return(list(type = "bin", reason = ""))
  if (length(u) == length(non) && length(non) > 2) return(list(type = "skip", reason = "id"))
  list(type = "cat", reason = "")
}

#' Default column specification (type + weight) for a table
#' @inheritParams st_params
#' @export
st_default_specs <- function(tbl) {
  idx <- setdiff(seq_along(tbl$header), tbl$label_col)
  if (!length(idx)) return(data.frame(idx = integer(), name = character(), type = character(), weight = numeric(), reason = character(), stringsAsFactors = FALSE))
  det <- lapply(idx, function(i) st_detect_column(tbl$rows[, i]))
  data.frame(idx = idx, name = tbl$header[idx],
             type = vapply(det, `[[`, "", "type"), weight = 1,
             reason = vapply(det, `[[`, "", "reason"), stringsAsFactors = FALSE)
}

## Column -> 0/1 (NA kept). Handles 0/1, yes/no, true/false, +/-, any two values.
st_to_binary <- function(vals) {
  na <- st_is_na(vals); v <- trimws(as.character(vals)); low <- tolower(v)
  if (all(unique(low[!na]) %in% c(.truthy, .falsy)))
    return(list(ok = TRUE, mapping = NULL, values = ifelse(na, NA_real_, as.numeric(low %in% .truthy))))
  uu <- unique(v[!na])
  if (length(uu) <= 2) {
    nums <- st_to_num(uu)
    sorted <- if (!anyNA(nums)) uu[order(nums)] else sort(uu)
    hi <- sorted[length(sorted)]
    return(list(ok = TRUE, mapping = sorted,
                values = ifelse(na, NA_real_, as.numeric(length(sorted) == 2 & v == hi))))
  }
  list(ok = FALSE, mapping = NULL, values = rep(NA_real_, length(vals)))
}

## One-line description of a column for the "Variables" table.
st_column_summary <- function(vals, type) {
  na <- st_is_na(vals); nna <- sum(na)
  txt <- if (type %in% c("num", "numlog")) {
    x <- st_to_num(vals)[!na]; x <- x[is.finite(x)]
    if (length(x)) sprintf("%s to %s", format(min(x), digits = 4), format(max(x), digits = 4)) else "-"
  } else if (type %in% c("bin", "binsym")) {
    b <- st_to_binary(vals)
    if (b$ok) sprintf("1: %d / 0: %d", sum(b$values == 1, na.rm = TRUE), sum(b$values == 0, na.rm = TRUE)) else "?"
  } else if (type == "cat") {
    sprintf("%d levels", length(unique(trimws(vals[!na]))))
  } else ""
  if (nna) txt <- paste0(txt, sprintf(", missing %d", nna))
  txt
}
