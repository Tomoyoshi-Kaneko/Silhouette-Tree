# Reference values reproduce the published analysis (Kaneko et al. 2025, Table 2)
# and agree with the browser version of this tool.

test_that("paper Table 2 gives k = 6 with mean silhouette 0.823", {
  r <- run_silhouette_tree(st_demo_paper_table(), type = "table", col_types = NULL)
  expect_equal(r$best, 6)
  expect_equal(round(max(r$avg), 3), 0.823)
})

test_that("demo tree gives k = 5 with mean silhouette 0.866", {
  r <- run_silhouette_tree(st_demo_tree(), type = "newick")
  expect_equal(r$best, 5)
  expect_equal(round(max(r$avg), 3), 0.866)
})

test_that("demo measurements give k = 4 with Gower distance", {
  r <- run_silhouette_tree(st_demo_measurements(), type = "table")
  expect_equal(r$best, 4)
  expect_equal(round(max(r$avg), 3), 0.791)
})

test_that("normalisation matters for Euclidean distance", {
  r_none <- run_silhouette_tree(st_demo_measurements(), type = "table", metric = "euclidean", norm = "none")
  r_mm   <- run_silhouette_tree(st_demo_measurements(), type = "table", metric = "euclidean", norm = "minmax")
  expect_equal(r_none$best, 3)
  expect_false(isTRUE(all.equal(r_none$avg, r_mm$avg)))
})

test_that("manual k and exclusion work", {
  r <- run_silhouette_tree(st_demo_tree(), type = "newick", k = 3, exclude = "Sample_12")
  expect_equal(r$k, 3)
  expect_false(r$k_auto)
  expect_equal(r$n, 11)
  tb <- st_samples_table(r)
  expect_equal(nrow(tb), 11)
  expect_true(all(tb$silhouette_width >= -1 & tb$silhouette_width <= 1))
})

test_that("bad input gives readable errors", {
  expect_error(run_silhouette_tree("not a tree", type = "newick"))
  expect_error(run_silhouette_tree("a,b\n1,2", type = "table"))
})

test_that("plots and exports build", {
  r <- run_silhouette_tree(st_demo_tree(), type = "newick")
  expect_s3_class(plot_dendrogram(r), "ggplot")
  expect_s3_class(plot_phylogeny(r), "ggplot")
  expect_s3_class(plot_silhouette_k(r), "ggplot")
  expect_s3_class(plot_silhouette_samples(r), "ggplot")
  f <- tempfile(fileext = ".xlsx")
  st_write_workbook(r, f)
  expect_true(file.exists(f))
  expect_match(st_hclust_newick(r), ";$")
  png <- tempfile(fileext = ".png")
  st_save_plot(plot_dendrogram(r), png, format = "png", width = 6, height = 5)
  expect_gt(file.size(png), 1000)
})
