# ============================================================================
# export.R -- Excel / CSV / Newick output
# ============================================================================

#' Newick string of the hclust dendrogram (branch length = height difference)
#' @inheritParams st_params
#' @export
st_hclust_newick <- function(res) {
  hc <- res$hc; n <- res$n; H <- hc$height
  q <- function(s) if (grepl("[\\s(),:;'\\[\\]]", s, perl = TRUE)) paste0("'", gsub("'", "''", s), "'") else s
  txt <- character(n - 1); hh <- numeric(n - 1)
  part <- function(v) if (v < 0) list(s = q(hc$labels[-v]), h = 0) else list(s = txt[v], h = hh[v])
  for (s in seq_len(n - 1)) {
    a <- part(hc$merge[s, 1]); b <- part(hc$merge[s, 2])
    txt[s] <- sprintf("(%s:%s,%s:%s)", a$s, format(H[s] - a$h, digits = 10), b$s, format(H[s] - b$h, digits = 10))
    hh[s] <- H[s]
  }
  paste0(txt[n - 1], ";")
}

#' Wide per-sample table including the cluster of every tested k
#' @inheritParams st_params
#' @export
st_samples_table_full <- function(res) {
  tab <- st_samples_table(res)
  idx <- match(tab$sample, res$labels)
  names(tab)[2] <- sprintf("cluster (k=%d)", res$k)
  for (j in seq_along(res$ks)) tab[[paste0("cluster_k", res$ks[j])]] <- paste0("C", res$parts[[j]][idx])
  tab
}

#' Write the results to an Excel workbook
#'
#' Sheets: Summary, Silhouette_by_k, Samples, Distance_matrix, Normalized (tables only),
#' Dendrogram_Newick.
#' @inheritParams st_params
#' @export
st_write_workbook <- function(res, path) {
  wb <- openxlsx::createWorkbook()
  put <- function(name, df, rn = FALSE) { openxlsx::addWorksheet(wb, name); openxlsx::writeData(wb, name, df, rowNames = rn); openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df) + rn), widths = "auto") }
  summ <- data.frame(item = c("input", "distance", "normalization", "linkage", "samples", "k tested", "optimal k", "k used", "mean silhouette (k used)"),
                     value = c(res$kind, if (res$kind == "tree") "patristic (cophenetic) distance" else res$metric,
                               if (res$kind == "tree") "" else if (res$metric == "gower") "range (built into Gower)" else res$norm,
                               res$linkage, res$n, paste0("2-", res$kmax), res$best, res$k, round(res$sil_avg, 6)), stringsAsFactors = FALSE)
  put("Summary", summ)
  put("Silhouette_by_k", data.frame(k = res$ks, mean_silhouette_width = round(res$avg, 6)))
  put("Samples", st_samples_table_full(res))
  put("Distance_matrix", as.data.frame(round(as.matrix(res$d), 8)), rn = TRUE)
  if (!is.null(res$normalized)) put("Normalized", round(res$normalized, 6), rn = TRUE)
  put("Dendrogram_Newick", data.frame(newick = st_hclust_newick(res)))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Save a ggplot as PNG or SVG
#' @inheritParams st_params
#' @export
st_save_plot <- function(plot, file, format = c("png", "svg"), width = 10, height = 8, dpi = 300) {
  format <- match.arg(format)
  if (format == "svg") {
    dev <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg
    ggplot2::ggsave(file, plot, device = dev, width = width, height = height)
  } else {
    ggplot2::ggsave(file, plot, device = "png", width = width, height = height, dpi = dpi, bg = "white")
  }
  invisible(file)
}
