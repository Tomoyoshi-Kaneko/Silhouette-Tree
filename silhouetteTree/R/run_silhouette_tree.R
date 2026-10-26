# ============================================================================
# run_silhouette_tree.R -- end-to-end entry points (used by the Shiny app and
# usable directly from R).
# ============================================================================

#' Analyse a Newick tree
#'
#' Patristic distances between tips -> hclust -> silhouette width for k = 2..kmax.
#'
#' @param tree An `ape::phylo` (see [st_read_newick()]).
#' @param exclude Tip names to leave out.
#' @param warnings Character vector of warnings to carry into the result.
#' @param linkage,kmax,k See [run_silhouette_tree()].
#' @return An `st_analysis` list with extra fields `kind = "tree"`, `tree`, `warnings`.
#' @export
st_analyze_tree <- function(tree, exclude = character(), linkage = "ward.D2", kmax = 9, k = NULL, warnings = character()) {
  keep <- setdiff(tree$tip.label, exclude)
  tr <- st_prune_tree(tree, keep)
  a <- st_run_analysis(st_tree_distance(tr), linkage = linkage, kmax = kmax)
  a <- st_select_k(a, k)
  if (a$fixed_na) warnings <- c(warnings, sprintf("%d sample pairs had nothing comparable; their distance is set to 0.", a$fixed_na))
  if (a$n > 500) warnings <- c(warnings, sprintf("Many samples (n=%d); this may take a while.", a$n))
  a$kind <- "tree"; a$tree <- tr; a$normalized <- NULL; a$used <- NA_integer_
  a$metric <- "patristic"; a$norm <- ""; a$warnings <- warnings
  a
}

#' Analyse a measurement table
#'
#' @param tbl Output of [st_table_from_matrix()].
#' @param specs Column types/weights ([st_default_specs()], possibly edited).
#' @param exclude Sample names to leave out.
#' @param warnings Character vector of warnings to carry into the result.
#' @param metric,norm See [st_table_distance()].
#' @param linkage,kmax,k See [run_silhouette_tree()].
#' @export
st_analyze_table <- function(tbl, specs, exclude = character(), metric = "gower", norm = "minmax",
                             linkage = "ward.D2", kmax = 9, k = NULL, warnings = character()) {
  samples <- st_table_samples(tbl)
  rows <- which(!(samples %in% exclude))
  if (length(rows) < 3) stop(sprintf("Fewer than 3 samples (%d). Un-exclude some samples or check the data.", length(rows)), call. = FALSE)
  if (!nrow(tbl$rows)) stop("The table has no data rows. Use row 1 as headers and one sample per row.", call. = FALSE)
  dd <- st_table_distance(tbl, specs, rows = rows, metric = metric, norm = norm)
  a <- st_run_analysis(dd$d, linkage = linkage, kmax = kmax)
  a <- st_select_k(a, k)
  w <- c(warnings, dd$warnings)
  if (a$fixed_na) w <- c(w, sprintf("%d sample pairs had nothing comparable; their distance is set to 0.", a$fixed_na))
  if (a$n > 500) w <- c(w, sprintf("Many samples (n=%d); this may take a while.", a$n))
  a$kind <- "table"; a$tree <- NULL; a$normalized <- dd$normalized; a$used <- dd$used
  a$metric <- metric; a$norm <- norm; a$warnings <- w
  a
}

#' One-call analysis from a file or text
#'
#' @param input Path to a Newick file / table file, or the text itself. For
#'   `type = "table"` a data frame (first column = sample names) is also accepted.
#' @param type `"newick"` or `"table"`.
#' @param exclude Sample names to leave out of the analysis (e.g. an outgroup).
#' @param linkage Linkage method of [stats::hclust()] (default `"ward.D2"`).
#' @param kmax Largest number of clusters to test (default 9).
#' @param k Number of clusters to use; `NULL` (default) uses the one with the largest mean silhouette width.
#' @param metric `"gower"` (default), `"euclidean"` or `"manhattan"` (tables only).
#' @param norm Normalisation for Euclidean / Manhattan: `"minmax"`, `"zscore"` or `"none"`.
#'   Gower distance normalises numeric columns by their range by itself.
#' @param label_col,transpose See [st_table_from_matrix()].
#' @param sheet Sheet name or number for Excel files.
#' @param col_types Optional named character vector overriding the detected column types,
#'   e.g. `c(onset = "num", host_A = "bin", note = "skip")`.
#' @examples
#' \dontrun{
#' res <- run_silhouette_tree(st_demo_tree(), type = "newick")
#' res$best                # optimal k
#' st_samples_table(res)   # cluster + silhouette width per sample
#' plot_dendrogram(res)
#' }
#' @export
run_silhouette_tree <- function(input, type = c("newick", "table"), exclude = character(), linkage = "ward.D2",
                                kmax = 9, k = NULL, metric = "gower", norm = "minmax", label_col = 1,
                                transpose = FALSE, sheet = 1, col_types = NULL) {
  type <- match.arg(type)
  if (type == "newick") {
    p <- st_read_newick(input)
    return(st_analyze_tree(p$tree, exclude = exclude, linkage = linkage, kmax = kmax, k = k, warnings = p$warnings))
  }
  M <- if (is.data.frame(input)) { m <- rbind(colnames(input), as.matrix(data.frame(lapply(input, as.character), check.names = FALSE))); dimnames(m) <- NULL; m }
       else if (length(input) == 1 && !grepl("[,\t\n]", input) && file.exists(input)) st_read_table_file(input, sheet = sheet)
       else st_matrix_from_text(paste(input, collapse = "\n"))
  tbl <- st_table_from_matrix(M, transpose = transpose, label_col = label_col)
  specs <- st_default_specs(tbl)
  if (!is.null(col_types)) for (nm in names(col_types)) specs$type[specs$name == nm] <- col_types[[nm]]
  st_analyze_table(tbl, specs, exclude = exclude, metric = metric, norm = norm, linkage = linkage, kmax = kmax, k = k)
}
