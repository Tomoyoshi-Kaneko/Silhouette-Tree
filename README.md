# silhouetteTree

[日本語版 README](README.ja.md) · [Specification / 仕様書 (EN/JA)](docs/specification.md)

Choose the number of clusters by **silhouette width** and draw a **cluster-coloured tree** — from a Newick tree file or from your own table of measurements — with a point-and-click Shiny app (no code needed) or a few lines of R.

The computation follows the analysis scripts of
Kaneko T, Osaka T, Inagaki M, Habe K, Okabe T, Tsuneda S. *From phenotype to receptor: validating physiological clustering of Escherichia coli phages through comprehensive receptor analysis.* J Virol (2025). doi:[10.1128/jvi.01061-25](https://doi.org/10.1128/jvi.01061-25)

| Step | What is done |
|---|---|
| Distance (tree) | path length between tips, `ape::cophenetic.phylo()` |
| Distance (table) | Gower distance, `cluster::daisy(metric = "gower")`: numeric columns are divided by their range (built-in normalisation), 0/1 columns can be asymmetric (shared 0 ignored), categorical columns are compared by match. Euclidean / Manhattan with min–max or z-score normalisation are also offered. |
| Clustering | `hclust()` — `ward.D2` by default (the paper's script; the main text mentions complete linkage), other linkages selectable |
| Choosing k | mean silhouette width of `cutree(hc, k)` for k = 2 … 9 (adjustable); the k with the largest mean is proposed, and you can override it |
| Output | cluster-coloured dendrogram or original phylogeny, silhouette-vs-k plot, per-sample silhouette plot, Excel workbook (summary, all k, samples, distance matrix, normalised data, dendrogram in Newick), PNG / SVG |

Rule of thumb for the mean silhouette width (Kaufman & Rousseeuw): > 0.70 strong structure, 0.51–0.70 reasonable, 0.26–0.50 weak, ≤ 0.25 none. A one-member cluster gets silhouette 0, so a distant outgroup lowers the mean — exclude it in the app if you only care about the rest.

## Input formats

**Tree** — Newick text in any of `.nwk .newick .ntw .tree .txt`, or pasted. Branch lengths are needed (without them, every branch counts as 1 and a warning is shown). Support values on internal nodes are shown on the original-phylogeny view.

**Table** — CSV / TSV / Excel (`.xlsx`, `.xls`) or pasted from Excel. Row 1 = column names, one sample per row, choose the sample-name column (or tick "swap rows and columns"). Column types are detected automatically and can be changed in the *Variables* tab:

| Type | Use for |
|---|---|
| Numeric | measurements (time, titre, concentration …) |
| Numeric, log10 | values spanning orders of magnitude (e.g. 1.9E-09) |
| Binary, joint 0s ignored | presence / absence where "both absent" says nothing (host range, gene requirement) |
| Binary, joint 0s count | two-sided yes/no |
| Categorical | labels |
| Do not use | IDs, notes |

Constant columns and ID-like columns are left out automatically. Missing values are allowed.

## Use the app

Online (once deployed, see below) — just open the URL:
https://01a17622-d74b-7e87-157c-aa3f6c7bfcf0.share.connect.posit.cloud/

```r
install.packages(c("shiny", "DT", "ggplot2", "openxlsx", "readxl", "ape", "cluster", "svglite"))
# from the package root:
shiny::runApp("inst/shiny_app")
# or, after installing the package:
# remotes::install_github("YOUR-NAME/silhouetteTree"); silhouetteTree::run_app()
```

## Use from R

```r
library(silhouetteTree)          # or source() the files in R/

# tree
res <- run_silhouette_tree("tree.nwk", type = "newick")
# table (Gower; numeric columns are range-normalised automatically)
res <- run_silhouette_tree("table.xlsx", type = "table")
# Euclidean with z-score normalisation, fixed k = 4, one sample left out
res <- run_silhouette_tree("table.csv", type = "table", metric = "euclidean",
                           norm = "zscore", k = 4, exclude = "T5")

res$best; max(res$avg)           # optimal k and its mean silhouette
st_samples_table(res)            # cluster and silhouette width per sample
plot_dendrogram(res); plot_phylogeny(res)
plot_silhouette_k(res); plot_silhouette_samples(res)
st_write_workbook(res, "results.xlsx")
```

## Checks

`tests/testthat` reproduces the published result (paper Table 2: k = 6, mean silhouette 0.823) and the other reference values; run with `testthat::test_dir("tests/testthat")` after sourcing/installing the package. The browser version of this tool (single HTML file) gives identical numbers: 311 of 312 numerical checks (merge order, heights, cutree, silhouette widths, distances) matched R exactly; the remaining one was a floating-point tie that is now resolved by rounding distances to 12 significant digits.

## Sample files

`inst/extdata/`: `paper_table2.csv` (genes required by 16 phages, from the paper, CC BY 4.0), `demo_tree.nwk` and `demo_measurements.csv` (fictional).

## Use of AI assistance

The scripts in this repository were drafted with the assistance of an AI model (Claude, Anthropic), using the structure of the author's earlier package [susiR](https://github.com/) as a template, and were reviewed by the author. The numerical results were checked against the published analysis (see *Checks*). Please report any problem you find as a GitHub issue.

## License

MIT © 2026 Tomoyoshi Kaneko
