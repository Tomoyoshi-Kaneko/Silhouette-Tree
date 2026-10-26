# ============================================================================
# plotting.R -- ggplot2 figures (same layout as the web version)
#   plot_dendrogram()          hclust dendrogram, coloured by cluster, with cut line
#   plot_phylogeny()           the original tree (branch lengths as given), coloured by cluster
#   plot_silhouette_k()        mean silhouette width vs number of clusters
#   plot_silhouette_samples()  silhouette width of every sample
# Requires ggplot2.
# ============================================================================

.st_grey <- "#6b7178"

.st_theme <- function(font_size = 12) {
  ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#e6e8eb"),
                   axis.title = ggplot2::element_text(colour = "#464b52"),
                   axis.text = ggplot2::element_text(colour = "#464b52"),
                   plot.title = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(colour = "#6b7178"))
}

## colour of a cluster id (0 = mixed -> grey)
.cl_col <- function(cl, k) { cols <- st_cluster_colors(k); ifelse(cl > 0, cols[pmax(cl, 1)], .st_grey) }

## right-hand margin (pt) needed for tip labels
.label_margin <- function(labels, font_size) max(nchar(labels, type = "width")) * font_size * 0.62 + 46

.tip_layer <- function(tips, font_size, k, xlab, xbadge) {
  cols <- st_cluster_colors(k)
  list(
    ggplot2::geom_point(data = tips, ggplot2::aes(x = xbadge, y = y), shape = 21, size = font_size * 0.52,
                        fill = cols[tips$cl], colour = "white", stroke = 0.6),
    ggplot2::geom_text(data = tips, ggplot2::aes(x = xbadge, y = y, label = cl), colour = .ink_on(cols[tips$cl]),
                       size = font_size * 0.28, fontface = "bold"),
    ggplot2::geom_text(data = tips, ggplot2::aes(x = xlab, y = y, label = label), hjust = 0,
                       size = font_size / ggplot2::.pt, colour = "#17191c")
  )
}

#' Dendrogram of the hierarchical clustering, coloured by cluster
#'
#' @param res An `st_analysis` (see [st_analyze_tree()] / [st_analyze_table()]).
#' @param show_cut Draw the dashed line where the tree is cut into k clusters.
#' @param font_size Base font size.
#' @export
plot_dendrogram <- function(res, show_cut = TRUE, font_size = 12) {
  hc <- res$hc; n <- res$n; k <- res$k; cl <- res$clusters
  H <- hc$height; xr <- max(H); if (xr <= 0) xr <- 1
  pos <- integer(n); pos[hc$order] <- seq_len(n)
  ny <- numeric(n - 1); ncl <- integer(n - 1)
  m <- 4 * (n - 1)
  sx <- sxe <- sy <- sye <- numeric(m); scl <- integer(m); q <- 0L
  ch <- function(v) if (v < 0) list(y = pos[-v], x = 0, cl = cl[-v]) else list(y = ny[v], x = H[v], cl = ncl[v])
  for (s in seq_len(n - 1)) {
    a <- ch(hc$merge[s, 1]); b <- ch(hc$merge[s, 2])
    ny[s] <- (a$y + b$y) / 2
    ncl[s] <- if (a$cl == b$cl && a$cl > 0) a$cl else 0L
    for (c in list(a, b)) {
      sx[q + 1] <- H[s]; sxe[q + 1] <- H[s]; sy[q + 1] <- ny[s]; sye[q + 1] <- c$y; scl[q + 1] <- c$cl
      sx[q + 2] <- c$x;  sxe[q + 2] <- H[s]; sy[q + 2] <- c$y;  sye[q + 2] <- c$y; scl[q + 2] <- c$cl
      q <- q + 2L
    }
  }
  segs <- data.frame(x = sx[1:q], xend = sxe[1:q], y = sy[1:q], yend = sye[1:q], cl = scl[1:q])
  tips <- data.frame(y = pos, cl = cl, label = res$labels)
  xbadge <- -0.035 * xr; xlab <- -0.075 * xr
  g <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = segs, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          colour = .cl_col(segs$cl, k), linewidth = 0.75, lineend = "round")
  if (show_cut && k <= n - 1) {
    cut_h <- (H[n - k] + H[n - k + 1]) / 2
    g <- g + ggplot2::geom_vline(xintercept = cut_h, linetype = "dashed", colour = "#464b52", linewidth = 0.4) +
      ggplot2::annotate("text", x = cut_h, y = 0.2, label = paste0("k = ", k), size = font_size * 0.28, colour = "#464b52", vjust = 1)
  }
  g + .tip_layer(tips, font_size, k, xlab, xbadge) +
    ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0.02, 0))) +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(add = c(1.2, 0.8))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = paste0(res$linkage, " \u00b7 distance (merge height)"), y = NULL,
                  caption = "Colour and the number in the circle show the cluster.") +
    .st_theme(font_size) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "#6b7178"),
                   plot.margin = ggplot2::margin(10, .label_margin(res$labels, font_size), 8, 8))
}

#' The original phylogeny (branch lengths as given), coloured by cluster
#'
#' Only available for `kind == "tree"` results.
#' @param show_support Print the node labels (bootstrap values etc.).
#' @inheritParams st_params
#' @export
plot_phylogeny <- function(res, show_support = TRUE, font_size = 12) {
  tr <- res$tree
  if (is.null(tr)) stop("plot_phylogeny() needs an analysis made from a tree.", call. = FALSE)
  k <- res$k
  tr <- ape::reorder.phylo(tr, "cladewise")
  ntip <- length(tr$tip.label); N <- ntip + tr$Nnode
  edge <- tr$edge
  kids <- split(edge[, 2], edge[, 1])
  y <- numeric(N); cl <- integer(N)
  tip_order <- edge[edge[, 2] <= ntip, 2]; y[tip_order] <- seq_along(tip_order)
  cl_tip <- res$clusters[match(tr$tip.label, res$labels)]
  cl[seq_len(ntip)] <- cl_tip
  fill_internal <- function(v) {
    if (v <= ntip) return(invisible())
    for (c in kids[[as.character(v)]]) fill_internal(c)
    cs <- kids[[as.character(v)]]
    y[v] <<- (min(y[cs]) + max(y[cs])) / 2
    cl[v] <<- if (all(cl[cs] == cl[cs[1]]) && cl[cs[1]] > 0) cl[cs[1]] else 0L
  }
  root <- ntip + 1L; fill_internal(root)
  x <- ape::node.depth.edgelength(tr); xr <- max(x[seq_len(ntip)]); if (xr <= 0) xr <- 1
  par <- edge[, 1]; chd <- edge[, 2]
  segs <- rbind(data.frame(x = x[par], xend = x[par], y = y[par], yend = y[chd], cl = cl[chd]),
                data.frame(x = x[par], xend = x[chd], y = y[chd], yend = y[chd], cl = cl[chd]))
  tips <- data.frame(y = y[seq_len(ntip)], cl = cl_tip, label = tr$tip.label, x = x[seq_len(ntip)])
  xbadge <- xr * 1.04; xlab <- xr * 1.08
  lead <- tips[tips$x < xr * 0.995, , drop = FALSE]
  g <- ggplot2::ggplot()
  if (nrow(lead)) g <- g + ggplot2::geom_segment(data = lead, ggplot2::aes(x = x, xend = xr * 1.02, y = y, yend = y),
                                                  colour = "#c8ccd1", linetype = "dotted")
  g <- g + ggplot2::geom_segment(data = segs, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                                 colour = .cl_col(segs$cl, k), linewidth = 0.75, lineend = "round")
  if (show_support && !is.null(tr$node.label)) {
    nl <- data.frame(x = x[ntip + seq_len(tr$Nnode)], y = y[ntip + seq_len(tr$Nnode)], label = tr$node.label, stringsAsFactors = FALSE)
    nl <- nl[-1, , drop = FALSE]; nl <- nl[!is.na(nl$label) & nl$label != "", , drop = FALSE]
    if (nrow(nl)) g <- g + ggplot2::geom_text(data = nl, ggplot2::aes(x = x, y = y, label = label), hjust = 1.1, vjust = -0.5,
                                              size = font_size * 0.24, colour = "#6b7178")
  }
  g + .tip_layer(tips, font_size, k, xlab, xbadge) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(add = c(1, 1))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "distance from the root (branch length)", y = NULL,
                  caption = "Colour and the number in the circle show the cluster.") +
    .st_theme(font_size) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "#6b7178"),
                   plot.margin = ggplot2::margin(10, .label_margin(tr$tip.label, font_size), 8, 8))
}

#' Mean silhouette width for every k
#' @inheritParams st_params
#' @export
plot_silhouette_k <- function(res, font_size = 12) {
  df <- data.frame(k = res$ks, avg = res$avg)
  best <- df[df$k == res$best, ]; sel <- df[df$k == res$k, ]
  acc <- "#8e1728"; blue <- .st_palette[1]
  g <- ggplot2::ggplot(df, ggplot2::aes(k, avg)) +
    ggplot2::geom_line(colour = blue, linewidth = 0.9) +
    ggplot2::geom_point(colour = blue, size = 3) +
    ggplot2::geom_point(data = best, shape = 21, size = 7, stroke = 1.2, colour = acc, fill = NA) +
    ggplot2::annotate("text", x = best$k, y = best$avg, label = sprintf("max at k=%d (%s)", best$k, .fmt3(best$avg)),
                      vjust = -2.1, fontface = "bold", size = font_size * 0.28)
  if (res$k != res$best) g <- g + ggplot2::geom_point(data = sel, size = 5, colour = acc)
  g + ggplot2::scale_x_continuous(breaks = df$k) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.2))) +
    ggplot2::labs(x = "number of clusters k", y = "mean silhouette width") +
    .st_theme(font_size)
}

#' Silhouette width of every sample
#' @inheritParams st_params
#' @export
plot_silhouette_samples <- function(res, font_size = 12) {
  tab <- st_samples_table(res); k <- res$k
  tab$sample <- factor(tab$sample, levels = rev(tab$sample))
  tab$cluster_n <- as.integer(sub("^C", "", tab$cluster))
  cols <- st_cluster_colors(k)
  lev <- vapply(seq_len(k), function(i) {
    s <- tab$silhouette_width[tab$cluster_n == i]
    sprintf("C%d  n=%d  mean %s", i, length(s), .fmt3(mean(s)))
  }, "")
  tab$grp <- factor(lev[tab$cluster_n], levels = lev)
  ggplot2::ggplot(tab, ggplot2::aes(x = silhouette_width, y = sample, fill = grp)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = res$sil_avg, linetype = "dashed", colour = "#17191c") +
    ggplot2::annotate("text", x = res$sil_avg, y = nlevels(tab$sample) + 0.9, label = paste("mean", .fmt3(res$sil_avg)),
                      size = font_size * 0.26, vjust = 0) +
    ggplot2::scale_fill_manual(values = stats::setNames(cols, lev), name = NULL) +
    ggplot2::scale_x_continuous(limits = c(min(0, min(tab$silhouette_width) - 0.02), 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(k / 3), byrow = TRUE)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "silhouette width", y = NULL,
                  caption = "Close to 1 fits its cluster well; negative means another cluster is closer.") +
    .st_theme(font_size) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(18, 10, 8, 8))
}

## Suggested plot height in inches for n rows
st_plot_height <- function(n, font_size = 12, extra = 1.4) max(3.5, n * font_size * 0.026 + extra)
