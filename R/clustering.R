# ============================================================================
# clustering.R -- hierarchical clustering + silhouette analysis.
# Follows the paper's script: hclust(<dist>, method = "ward.D2"); for every
# k = 2..kmax: silhouette(cutree(hc, k), <dist>); optimal k = argmax of the mean
# silhouette width.
# ============================================================================

#' Hierarchical clustering and silhouette analysis for a distance matrix
#'
#' @param d A `dist` object.
#' @param linkage Any of `"ward.D2"`, `"ward.D"`, `"complete"`, `"average"`,
#'   `"single"`, `"mcquitty"`.
#' @param kmax Largest number of clusters to test (capped at n - 1; the paper used 9).
#' @return A list of class `st_analysis`.
#' @export
st_run_analysis <- function(d, linkage = "ward.D2", kmax = 9) {
  n <- attr(d, "Size")
  if (n < 3) stop(sprintf("Fewer than 3 samples (%d).", n), call. = FALSE)
  fixed <- 0L
  if (anyNA(d)) { fixed <- sum(is.na(d)); d[is.na(d)] <- 0 }
  if (any(!is.finite(d))) stop("The distance matrix contains values that cannot be computed.", call. = FALSE)
  d[] <- signif(as.vector(d), 12)
  kmax <- max(2L, min(as.integer(kmax), n - 1L))
  hc <- stats::hclust(d, method = linkage)
  ks <- 2:kmax
  parts <- lapply(ks, function(k) stats::cutree(hc, k))
  sils <- lapply(parts, function(cl) cluster::silhouette(cl, d))
  avg <- vapply(sils, function(s) mean(s[, "sil_width"]), numeric(1))
  structure(list(d = d, hc = hc, n = n, labels = attr(d, "Labels"), linkage = linkage, kmax = kmax,
                 ks = ks, avg = avg, best = ks[which.max(avg)], parts = parts, sils = sils,
                 fixed_na = fixed), class = "st_analysis")
}

## Attach a chosen k (NULL / "auto" = the optimum) and its clusters.
st_select_k <- function(a, k = NULL) {
  if (is.null(k) || identical(k, "auto") || is.na(suppressWarnings(as.integer(k))) || !(as.integer(k) %in% a$ks)) {
    a$k_auto <- TRUE; k <- a$best
  } else { a$k_auto <- FALSE; k <- as.integer(k) }
  a$k <- k
  a$clusters <- as.integer(a$parts[[match(k, a$ks)]])
  a$sil <- a$sils[[match(k, a$ks)]]
  a$sil_avg <- a$avg[match(k, a$ks)]
  a
}

## Kaufman & Rousseeuw's rule of thumb for the mean silhouette width.
st_interpret_silhouette <- function(avg) {
  if (avg > 0.70) "strong structure" else if (avg > 0.50) "reasonable structure" else if (avg > 0.25) "weak structure" else "no substantial structure"
}

#' Per-sample results (cluster, silhouette width, nearest other cluster)
#' @inheritParams st_params
#' @export
st_samples_table <- function(res) {
  s <- res$sil
  nb <- as.integer(s[, "neighbor"])
  sizes <- table(res$clusters)
  nb[sizes[as.character(res$clusters)] <= 1] <- NA
  out <- data.frame(sample = res$labels, cluster = paste0("C", res$clusters),
                    silhouette_width = round(as.numeric(s[, "sil_width"]), 6),
                    nearest_other_cluster = ifelse(is.na(nb), "", paste0("C", nb)), stringsAsFactors = FALSE)
  out[order(res$clusters, -out$silhouette_width), , drop = FALSE]
}
