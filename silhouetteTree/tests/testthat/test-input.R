test_that("Newick reader handles comments, missing semicolon and no branch lengths", {
  p <- st_read_newick("((A,B),(C,D)) [comment]")
  expect_equal(length(p$tree$tip.label), 4)
  expect_true(length(p$warnings) >= 1)
  expect_error(st_read_newick("(A,B);"))
})

test_that("column types are detected", {
  tb <- st_table_from_matrix(st_matrix_from_text(st_demo_measurements()), transpose = FALSE, label_col = 1)
  sp <- st_default_specs(tb)
  expect_equal(sp$type[sp$name == "onset_min"], "num")
  expect_equal(sp$type[sp$name == "host_1"], "bin")
})

test_that("all-zero asymmetric pairs do not break clustering", {
  txt <- "s,a,b\nx,0,0\ny,0,0\nz,1,1\nw,1,0"
  r <- suppressWarnings(run_silhouette_tree(txt, type = "table", kmax = 3))
  expect_true(is.finite(max(r$avg)))
})
