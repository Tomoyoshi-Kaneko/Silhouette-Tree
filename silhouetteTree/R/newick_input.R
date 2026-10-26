# ============================================================================
# newick_input.R -- reading phylogenetic trees (Newick) and turning them into
# the distance matrix used for clustering (patristic / cophenetic distance).
# ============================================================================

#' Read a Newick tree
#'
#' @param x Either a path to a Newick file (.nwk, .newick, .ntw, .tree, .txt ...)
#'   or the Newick text itself.
#' @return A list with `tree` (an `ape::phylo`) and `warnings` (character).
#' @export
st_read_newick <- function(x) {
  warnings <- character()
  txt <- if (length(x) == 1 && !grepl("[(;]", x) && file.exists(x)) st_read_text_guess(x) else paste(x, collapse = "\n")
  txt <- gsub("\\[[^]]*\\]", "", txt)               # [comments]
  txt <- trimws(txt)
  if (!nzchar(txt) || !grepl("(", txt, fixed = TRUE)) stop("No Newick tree found in the input.", call. = FALSE)
  if (!grepl(";\\s*$", txt)) txt <- paste0(txt, ";")
  tr <- tryCatch(suppressWarnings(ape::read.tree(text = txt)),
                 error = function(e) stop("Could not read the Newick text (", conditionMessage(e),
                                          "). Check that parentheses and the final ';' are complete.", call. = FALSE))
  if (is.null(tr)) stop("Could not read the Newick text. Check that parentheses and the final ';' are complete.", call. = FALSE)
  if (inherits(tr, "multiPhylo")) {
    warnings <- c(warnings, sprintf("The file holds %d trees. Only the first is used.", length(tr)))
    tr <- tr[[1]]
  }
  tr <- ape::collapse.singles(tr)
  if (length(tr$tip.label) < 3) stop(sprintf("Fewer than 3 tips (%d). At least 3 are needed.", length(tr$tip.label)), call. = FALSE)
  if (is.null(tr$edge.length)) {
    tr$edge.length <- rep(1, nrow(tr$edge))
    warnings <- c(warnings, "No branch lengths found; every branch is treated as length 1.")
  } else if (anyNA(tr$edge.length)) {
    nb <- sum(is.na(tr$edge.length)); tr$edge.length[is.na(tr$edge.length)] <- 0
    warnings <- c(warnings, sprintf("%d branches had no length and are treated as 0.", nb))
  }
  if (anyDuplicated(tr$tip.label)) {
    nd <- sum(duplicated(tr$tip.label)); tr$tip.label <- make.unique(tr$tip.label, sep = "_")
    warnings <- c(warnings, sprintf("%d duplicate tip names were numbered.", nd))
  }
  list(tree = tr, warnings = warnings)
}

## Patristic distance between tips = ape::cophenetic.phylo (as in the paper's script).
## Rounded to 12 significant digits so that mathematically equal distances compare
## equal (floating-point noise would otherwise decide ties arbitrarily).
st_tree_distance <- function(tree) {
  cp <- ape::cophenetic.phylo(tree)
  d <- stats::as.dist(cp)
  d[] <- signif(as.vector(d), 12)
  attr(d, "Labels") <- rownames(cp)
  d
}

## Remove tips; remaining patristic distances are unchanged.
st_prune_tree <- function(tree, keep) {
  if (length(keep) < 3) stop(sprintf("Fewer than 3 samples (%d). Un-exclude some samples or check the data.", length(keep)), call. = FALSE)
  if (length(keep) == length(tree$tip.label)) return(tree)
  ape::keep.tip(tree, keep)
}
