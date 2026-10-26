# ============================================================================
# utils.R -- small helpers shared by the other files
# ============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

## Categorical colours used for clusters (identical to the web version).
## Cluster 9 and above share the last (grey) colour; the number printed in each
## tip badge always identifies the cluster.
.st_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4",
                 "#008300", "#6250d6", "#e34948", "#80858c")

st_cluster_colors <- function(k) {
  idx <- seq_len(k); idx[idx > 8] <- 9
  .st_palette[idx]
}

## Text colour (dark/white) that stays readable on a given fill colour.
.ink_on <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  L <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(L > 0.62, "#14161a", "#ffffff")
}

.fmt3 <- function(x) formatC(x, format = "f", digits = 3)

## A short, stable string for a piece of text (used to give dynamic Shiny
## inputs ids that change when the loaded table changes).
.simple_hash <- function(x) {
  x <- paste(x, collapse = "|")
  codes <- utf8ToInt(substr(x, 1, 5000))
  if (!length(codes)) return("0")
  as.character(sum(codes * ((seq_along(codes) %% 251) + 1)) %% 1e9)
}

## Read a text file, coping with UTF-8 (with or without BOM) and the Shift_JIS
## (CP932) that Japanese versions of Excel use when saving CSV.
st_read_text_guess <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  if (length(raw) >= 3 && identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) {
    txt <- iconv(rawToChar(raw), from = "CP932", to = "UTF-8")
    if (is.na(txt)) txt <- iconv(rawToChar(raw), from = "latin1", to = "UTF-8")
  }
  txt
}

## Mark valid UTF-8 text as UTF-8 (string functions such as trimws() can drop the mark,
## which garbles non-ASCII sample names such as "\u03a6Wec172" when R runs in a C locale).
.as_utf8 <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & validUTF8(x)
  Encoding(x[ok]) <- "UTF-8"
  x
}
