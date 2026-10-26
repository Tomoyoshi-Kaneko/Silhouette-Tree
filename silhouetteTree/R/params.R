#' Arguments shared by several functions
#'
#' @param res Result of [run_silhouette_tree()].
#' @param txt Text of a delimited table (copied from Excel, or the contents of a CSV/TSV file).
#' @param path Path of the file to read or write.
#' @param M Character matrix of a table as returned by [st_matrix_from_text()] / [st_read_table_file()].
#' @param transpose Logical; swap rows and columns (samples run across columns).
#' @param label_col Index of the column holding the sample names.
#' @param plot A ggplot object.
#' @param file Output file path.
#' @param format `"png"` or `"svg"`.
#' @param width,height Size in inches.
#' @param dpi Resolution for PNG.
#' @param font_size Base font size.
#' @param tbl Table object from [st_table_from_matrix()].
#' @param vals Character vector of the values of one column.
#' @name st_params
#' @keywords internal
NULL
