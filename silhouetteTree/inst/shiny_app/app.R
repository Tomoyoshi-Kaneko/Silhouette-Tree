# ============================================================================
# app.R -- Silhouette Tree Shiny application
#
# Full workflow without writing any R code: load a Newick tree or a measurement
# table, choose distance / normalisation, find the number of clusters by mean
# silhouette width, and get cluster-coloured trees, silhouette plots and an Excel
# workbook. Computation follows the analysis scripts of Kaneko et al. (J Virol
# 2025, doi:10.1128/jvi.01061-25): cluster::daisy(metric = "gower"), hclust
# ("ward.D2"), cluster::silhouette over k = 2..9, ape::cophenetic.phylo for trees.
# ============================================================================

## --- Load core functions ----------------------------------------------------
## Same convention as the susiR app: in development mode shiny sets the working
## directory to this app's folder, so the package's R/ folder is two levels up.
.core_dir <- file.path("..", "..", "R")
if (dir.exists(.core_dir)) {
  for (.f in list.files(.core_dir, pattern = "\\.R$", full.names = TRUE)) source(.f, encoding = "UTF-8")
} else if (requireNamespace("silhouetteTree", quietly = TRUE)) {
  # Installed-package mode: make the package's internal helpers visible as well.
  .ns <- asNamespace("silhouetteTree")
  for (.n in ls(.ns, all.names = TRUE)) assign(.n, get(.n, envir = .ns), envir = globalenv())
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(ggplot2)
  library(openxlsx)
  library(readxl)
  library(ape)
  library(cluster)
  if (requireNamespace("svglite", quietly = TRUE)) library(svglite)
})

options(shiny.maxRequestSize = 30 * 1024^2)

TYPE_CHOICES <- c(
  "Numeric / \u6570\u5024" = "num",
  "Numeric, log10 / \u6570\u5024 (\u5bfe\u6570)" = "numlog",
  "Binary 0/1, joint 0s ignored / \u4e8c\u5024 (0\u540c\u58eb\u306f\u7121\u8996)" = "bin",
  "Binary 0/1, joint 0s count / \u4e8c\u5024 (0\u540c\u58eb\u3082\u4e00\u81f4)" = "binsym",
  "Categorical / \u30ab\u30c6\u30b4\u30ea" = "cat",
  "Do not use / \u4f7f\u308f\u306a\u3044" = "skip")
SKIP_REASON <- c(const = "left out: all values equal", id = "left out: every value differs (an ID column?)", empty = "left out: empty")

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; align-items: center;",
      div(
        h2("Silhouette Tree", style = "margin: 0; font-weight: bold; color: #8E1728;"),  # Waseda enji
        h5("choose the number of clusters by silhouette width and colour the tree by cluster / \u30b7\u30eb\u30a8\u30c3\u30c8\u4fc2\u6570\u3067\u30af\u30e9\u30b9\u30bf\u30fc\u6570\u3092\u6c7a\u3081\u3001\u7cfb\u7d71\u6a39\u3092\u8272\u5206\u3051",
           style = "margin: 0; color: #555;")
      )
    ),
    windowTitle = "Silhouette Tree"
  ),
  tags$head(tags$style(HTML("
    summary.st-toggle {
      cursor: pointer; padding: 8px 12px; margin-bottom: 8px;
      background-color: #e8eef7; border: 1px solid #9fb3d1; border-radius: 5px;
      font-weight: bold; display: block;
    }
    summary.st-toggle:hover { background-color: #d8e2f2; }
    summary.st-toggle::after { content: '  \u25be click to expand / collapse'; font-weight: normal; color: #555; }
    details[open] > summary.st-toggle::after { content: '  \u25b4 click to collapse'; }
    .st-sum { display: flex; flex-wrap: wrap; border: 1px solid #d6d9dd; border-radius: 6px; margin-bottom: 12px; overflow: hidden; }
    .st-sum > div { flex: 1 1 150px; padding: 8px 14px; border-right: 1px solid #d6d9dd; min-width: 140px; }
    .st-sum > div:last-child { border-right: 0; }
    .st-sum .k { font-size: 12px; color: #6b7178; }
    .st-sum .v { font-family: monospace; font-size: 22px; font-weight: 500; }
    .st-sum .s { font-size: 12px; color: #464b52; }
    .st-warn { background: #fff4dc; color: #5c4300; border-radius: 6px; padding: 8px 14px; margin-bottom: 12px; font-size: 13px; }
    .st-warn ul { margin: 4px 0 0; padding-left: 18px; }
    .st-var-row { border-bottom: 1px solid #eceef0; padding: 4px 0; }
    .st-var-row .form-group { margin-bottom: 0; }
    .st-var-name { font-family: monospace; padding-top: 7px; word-break: break-all; }
    .st-var-sum { color: #6b7178; font-size: 12px; padding-top: 8px; }
  "))),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      radioButtons("mode", "1. Input type / \u30c7\u30fc\u30bf\u306e\u7a2e\u985e",
                   choices = c("Tree file, Newick (.nwk) / \u7cfb\u7d71\u6a39\u30d5\u30a1\u30a4\u30eb" = "newick",
                               "Measurement table, CSV or Excel / \u6e2c\u5b9a\u5024\u306e\u8868" = "table")),
      hr(),
      h4("2. Load data / \u30c7\u30fc\u30bf\u3092\u8aad\u307f\u8fbc\u3080"),

      conditionalPanel(
        "input.mode == 'newick'",
        fileInput("tree_file", "Newick file (.nwk .newick .ntw .tree .txt)", accept = c(".nwk", ".newick", ".ntk", ".ntw", ".tree", ".tre", ".txt", ".nex")),
        textAreaInput("tree_text", "or paste Newick text / \u307e\u305f\u306f\u8cbc\u308a\u4ed8\u3051", value = st_demo_tree(), rows = 5, width = "100%"),
        helpText("Distances between samples are computed as the path length along the tree (patristic / cophenetic distance), as in the paper."),
        actionButton("demo_tree", "Load demo tree (fictional data)", class = "btn-secondary")
      ),
      conditionalPanel(
        "input.mode == 'table'",
        fileInput("tbl_file", "Table file (.csv .tsv .txt .xlsx .xls)", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
        uiOutput("sheet_ui"),
        textAreaInput("tbl_text", "or paste a table (copying from Excel works) / \u307e\u305f\u306f\u8868\u3092\u8cbc\u308a\u4ed8\u3051", value = "", rows = 5, width = "100%"),
        helpText("Row 1 = column names; one sample per row; the sample-name column is chosen below. Numeric, 0/1 and categorical columns can be mixed."),
        uiOutput("label_ui"),
        checkboxInput("transpose", "Swap rows and columns (samples run across columns) / \u884c\u3068\u5217\u3092\u5165\u308c\u66ff\u3048\u308b", value = FALSE),
        actionButton("demo_paper", "Load paper Table 2 (real data)", class = "btn-secondary"),
        actionButton("demo_meas", "Load demo measurements (fictional)", class = "btn-secondary")
      ),

      hr(),
      h4("3. Settings / \u8a2d\u5b9a"),
      conditionalPanel(
        "input.mode == 'table'",
        selectInput("metric", "Distance / \u8ddd\u96e2\u306e\u8a08\u7b97",
                    choices = c("Gower (mixed numeric, 0/1, categorical)" = "gower", "Euclidean" = "euclidean", "Manhattan" = "manhattan")),
        conditionalPanel(
          "input.metric == 'gower'",
          helpText("Gower divides every numeric column by its range (max \u2212 min), so columns with large values cannot dominate. No further normalisation is needed. / Gower\u3067\u306f\u6570\u5024\u5217\u3092\u7bc4\u56f2\u3067\u5272\u308b\u305f\u3081\u3001\u5024\u306e\u5927\u304d\u3044\u5217\u306b\u5f15\u304d\u305a\u3089\u308c\u307e\u305b\u3093\u3002")
        ),
        conditionalPanel(
          "input.metric != 'gower'",
          selectInput("norm", "Normalise numeric columns / \u6b63\u898f\u5316",
                      choices = c("Min\u2013max (scale to 0\u20131)" = "minmax", "Z-score (mean 0, SD 1)" = "zscore", "None" = "none")),
          helpText("Without normalisation, columns with large values decide most of the distance.")
        )
      ),
      selectInput("linkage", "Linkage (hclust)", choices = c("ward.D2", "complete", "average", "single", "mcquitty", "ward.D"), selected = "ward.D2"),
      helpText("The paper's R script uses ward.D2 (the main text mentions complete linkage)."),
      fluidRow(
        column(6, numericInput("kmax", "Largest k to test", value = 9, min = 2, max = 30, step = 1)),
        column(6, uiOutput("k_ui"))
      ),
      conditionalPanel(
        "input.mode == 'newick'",
        radioButtons("style", "Tree to draw / \u63cf\u304f\u6a39",
                     choices = c("Dendrogram (clustering result) / \u6a39\u5f62\u56f3" = "hclust", "Original phylogeny (branch lengths as given) / \u5143\u306e\u7cfb\u7d71\u6a39" = "phylo")),
        conditionalPanel("input.style == 'phylo'", checkboxInput("show_support", "Show support values on branches", value = TRUE))
      ),
      conditionalPanel("!(input.mode == 'newick' && input.style == 'phylo')", checkboxInput("show_cut", "Show the cut line", value = TRUE)),
      numericInput("font_size", "Font size", value = 12, min = 8, max = 24, step = 1),
      uiOutput("exclude_ui"),

      hr(),
      tags$details(
        tags$summary(class = "st-toggle", "Export settings: image size & PNG/SVG format"),
        br(),
        numericInput("export_width", "Width (inches)", value = 10, min = 2, step = 0.5),
        numericInput("export_height", "Height (inches)", value = 8, min = 2, step = 0.5),
        helpText("Increase the height when there are many samples."),
        selectInput("export_format", "Image format", choices = c("PNG" = "png", "SVG (vector, editable)" = "svg"))
      )
    ),

    mainPanel(
      width = 8,
      uiOutput("summary_ui"),
      tabsetPanel(
        id = "tabs",
        tabPanel("Tree",
                 br(), plotOutput("tree_plot", height = "auto"), br(),
                 downloadButton("dl_tree", "Download tree"),
                 downloadButton("dl_nwk", "Download dendrogram (Newick)")),
        tabPanel("Silhouette by k",
                 br(),
                 helpText("Click a point to use that k. / \u70b9\u3092\u30af\u30ea\u30c3\u30af\u3059\u308b\u3068\u305d\u306ek\u3092\u63a1\u7528\u3057\u307e\u3059\u3002"),
                 plotOutput("k_plot", height = "380px", click = "k_click"),
                 downloadButton("dl_k", "Download this plot"),
                 br(), br(), DTOutput("k_table")),
        tabPanel("Silhouette per sample",
                 br(),
                 helpText("Bar = silhouette width of each sample at the k in use (colour = cluster, dashed line = overall mean). Near 1: fits its cluster well; near 0: on the border between two clusters; negative: closer to another cluster, so check it. A cluster with one member has width 0 and shows no bar. The 'Results table' tab lists the nearest other cluster for each sample. / \u30b5\u30f3\u30d7\u30eb\u3054\u3068\u306e\u30b7\u30eb\u30a8\u30c3\u30c8\u4fc2\u6570\u3002\u0031\u306b\u8fd1\u3044\u307b\u3069\u30af\u30e9\u30b9\u30bf\u30fc\u306b\u30d4\u30c3\u30bf\u30ea\u3001\u0030\u4ed8\u8fd1\u306f\u5883\u76ee\u3001\u8ca0\u306f\u5225\u306e\u30af\u30e9\u30b9\u30bf\u30fc\u306e\u307b\u3046\u304c\u8fd1\u3044\uff08\u8981\u78ba\u8a8d\uff09\u3002"),
                 plotOutput("sil_plot", height = "auto"), br(),
                 downloadButton("dl_sil", "Download this plot")),
        tabPanel("Results table",
                 br(), DTOutput("samples_table"), br(),
                 downloadButton("dl_xlsx", "Download results (Excel)"),
                 downloadButton("dl_csv", "Download table (CSV)")),
        tabPanel("Variables (table input)",
                 br(),
                 helpText("Column types are detected automatically. Change them if needed; a weight of 0 leaves the column out. / \u578b\u306f\u81ea\u52d5\u5224\u5b9a\u3067\u3059\u3002\u5fc5\u8981\u306a\u3089\u5909\u66f4\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
                 uiOutput("vars_ui")),
        tabPanel("How to read",
                 br(),
                 h4("Silhouette width"),
                 p("For every sample, the silhouette width compares its mean distance to the other members of its own cluster (a) with its mean distance to the nearest other cluster (b): s = (b \u2212 a) / max(a, b). It runs from \u22121 to 1; values near 1 mean the sample sits well inside its cluster, values near 0 mean it lies between two clusters, and negative values suggest it may belong elsewhere. A cluster with a single member gets s = 0."),
                 p("The tool clusters hierarchically for k = 2, 3, \u2026 and picks the k whose mean silhouette width is largest. As a rule of thumb (Kaufman & Rousseeuw): above 0.70 strong structure, 0.51\u20130.70 reasonable, 0.26\u20130.50 weak, 0.25 or below no substantial structure."),
                 h4("Inputs"),
                 tags$ul(
                   tags$li("Tree file: distances are path lengths between tips along the tree (ape::cophenetic.phylo)."),
                   tags$li("Table: Gower distance (cluster::daisy) handles numeric, binary and categorical columns together. Numeric columns are divided by their range, so absolute magnitudes do not matter. Binary 0/1 columns can be treated as asymmetric (a shared 0 carries no information, as for infection / host-range data)."),
                   tags$li("Euclidean / Manhattan: choose a normalisation (min\u2013max or z-score) so that columns with large values do not dominate.")),
                 h4("Tips"),
                 tags$ul(
                   tags$li("A distant outgroup becomes its own one-member cluster. Exclude it (Settings \u2192 Exclude samples) if you only want the structure among the rest."),
                   tags$li("Sample pairs with nothing comparable (all columns missing or joint zeros in asymmetric binary columns only) get distance 0; a warning is shown.")),
                 h4("Reference"),
                 p("Method as used in: Kaneko T, Osaka T, Inagaki M, Habe K, Okabe T, Tsuneda S. From phenotype to receptor: validating physiological clustering of Escherichia coli phages through comprehensive receptor analysis. J Virol (2025). doi:10.1128/jvi.01061-25."))
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(xlsx = NULL, xlsx_name = NULL)
  err <- function(e) structure(list(error = conditionMessage(e)), class = "st_error")
  is_err <- function(x) inherits(x, "st_error")

  ## ---- input handling ----------------------------------------------------
  observeEvent(input$demo_tree, updateTextAreaInput(session, "tree_text", value = st_demo_tree()))
  observeEvent(input$tree_file, {
    txt <- tryCatch(st_read_text_guess(input$tree_file$datapath), error = function(e) "")
    updateTextAreaInput(session, "tree_text", value = txt)
  })
  observeEvent(input$tbl_file, {
    p <- input$tbl_file$datapath; nm <- input$tbl_file$name
    if (grepl("\\.xlsx?$", nm, ignore.case = TRUE)) {
      rv$xlsx <- p; rv$xlsx_name <- nm
    } else {
      rv$xlsx <- NULL
      updateTextAreaInput(session, "tbl_text", value = tryCatch(st_read_text_guess(p), error = function(e) ""))
    }
  })
  observeEvent(input$tbl_text, { rv$xlsx <- NULL }, ignoreInit = TRUE)
  observeEvent(input$demo_paper, { rv$xlsx <- NULL; updateTextAreaInput(session, "tbl_text", value = st_demo_paper_table()) })
  observeEvent(input$demo_meas, { rv$xlsx <- NULL; updateTextAreaInput(session, "tbl_text", value = st_demo_measurements()) })

  output$sheet_ui <- renderUI({
    req(rv$xlsx)
    sn <- tryCatch(readxl::excel_sheets(rv$xlsx), error = function(e) character())
    if (length(sn) < 1) return(NULL)
    tagList(helpText(paste("Excel file:", rv$xlsx_name)), if (length(sn) > 1) selectInput("sheet", "Sheet / \u30b7\u30fc\u30c8", choices = sn, selected = sn[1]))
  })

  ## ---- parsed inputs -----------------------------------------------------
  parsed_tree <- reactive({
    tryCatch(st_read_newick(input$tree_text %||% ""), error = err)
  })
  table_matrix <- reactive({
    tryCatch({
      M <- if (!is.null(rv$xlsx)) st_read_table_file(rv$xlsx, sheet = input$sheet %||% 1) else st_matrix_from_text(input$tbl_text %||% "")
      M
    }, error = err)
  })
  headers <- reactive({
    M <- table_matrix(); if (is.null(M) || is_err(M)) return(NULL)
    tb <- st_table_from_matrix(M, transpose = isTRUE(input$transpose), label_col = 1)
    tb$header
  })
  output$label_ui <- renderUI({
    h <- headers(); req(h)
    sel <- isolate(input$label_col)
    selectInput("label_col", "Sample-name column / \u30b5\u30f3\u30d7\u30eb\u540d\u306e\u5217", choices = stats::setNames(seq_along(h), h),
                selected = if (!is.null(sel) && as.integer(sel) <= length(h)) sel else 1)
  })
  tbl <- reactive({
    M <- table_matrix(); req(!is.null(M), !is_err(M))
    tryCatch(st_table_from_matrix(M, transpose = isTRUE(input$transpose), label_col = as.integer(input$label_col %||% 1)), error = err)
  })
  specs_default <- reactive({ tb <- tbl(); req(!is_err(tb)); st_default_specs(tb) })
  tbl_key <- reactive({ tb <- tbl(); req(!is_err(tb)); .simple_hash(c(tb$header, nrow(tb$rows), tb$label_col, tb$rows[1, ])) })

  specs <- reactive({
    d <- specs_default(); key <- tbl_key()
    for (i in seq_len(nrow(d))) {
      ty <- input[[paste0("vt", key, "_", i)]]; w <- input[[paste0("vw", key, "_", i)]]
      if (!is.null(ty) && ty %in% TYPE_CHOICES) { d$type[i] <- ty; d$reason[i] <- "" }
      if (!is.null(w) && !is.na(w) && w >= 0) d$weight[i] <- w
    }
    d
  })

  all_names <- reactive({
    if (input$mode == "newick") { p <- parsed_tree(); if (is_err(p)) character() else p$tree$tip.label }
    else { tb <- tbl(); if (is_err(tb)) character() else st_table_samples(tb) }
  })
  output$exclude_ui <- renderUI({
    nm <- all_names()
    if (length(nm) < 4) return(NULL)
    tags$details(
      tags$summary(class = "st-toggle", "Exclude samples / \u9664\u5916\u3059\u308b\u30b5\u30f3\u30d7\u30eb"),
      helpText("Checked samples are left out of the calculation and the figures (e.g. an outgroup)."),
      div(style = "max-height: 220px; overflow-y: auto;",
          checkboxGroupInput("exclude", NULL, choices = nm, selected = intersect(isolate(input$exclude), nm)))
    )
  })

  ## ---- analysis ------------------------------------------------------------
  base_res <- reactive({
    kmax <- input$kmax; req(!is.null(kmax), !is.na(kmax))
    kmax <- max(2, min(30, round(kmax)))
    ex <- input$exclude %||% character()
    tryCatch({
      if (input$mode == "newick") {
        p <- parsed_tree(); if (is_err(p)) return(p)
        st_analyze_tree(p$tree, exclude = ex, linkage = input$linkage, kmax = kmax, warnings = p$warnings)
      } else {
        tb <- tbl(); if (is_err(tb)) return(tb)
        M <- table_matrix(); if (is.null(M)) stop("Load a table first / \u8868\u3092\u8aad\u307f\u8fbc\u3093\u3067\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
        st_analyze_table(tb, specs(), exclude = ex, metric = input$metric %||% "gower", norm = input$norm %||% "minmax",
                         linkage = input$linkage, kmax = kmax)
      }
    }, error = err)
  })
  res <- reactive({
    b <- base_res()
    validate(need(!is_err(b), if (is_err(b)) b$error else ""))
    st_select_k(b, input$k %||% "auto")
  })

  output$k_ui <- renderUI({
    b <- base_res(); req(!is_err(b))
    sel <- isolate(input$k)
    ch <- c("Auto (max silhouette)" = "auto", stats::setNames(as.character(b$ks), b$ks))
    selectInput("k", "k to use / \u63a1\u7528\u3059\u308bk", choices = ch, selected = if (!is.null(sel) && sel %in% ch) sel else "auto")
  })
  observeEvent(input$k_click, {
    b <- base_res(); req(!is_err(b))
    k <- round(input$k_click$x)
    if (k %in% b$ks) updateSelectInput(session, "k", selected = as.character(k))
  })

  ## ---- summary -----------------------------------------------------------------
  output$summary_ui <- renderUI({
    b <- base_res()
    if (is_err(b)) return(div(class = "st-warn", style = "background:#fde8e8;color:#7a1313;", strong(b$error)))
    r <- res()
    how <- if (r$kind == "tree") "distance = path length on the tree" else sprintf("%s distance, %d variables", c(gower = "Gower", euclidean = "Euclidean", manhattan = "Manhattan")[[r$metric]], r$used)
    tagList(
      if (length(r$warnings)) div(class = "st-warn", strong("Please check"), tags$ul(lapply(r$warnings, tags$li))),
      div(class = "st-sum",
          div(div(class = "k", "Samples"), div(class = "v", r$n), div(class = "s", how)),
          div(div(class = "k", "Optimal k"), div(class = "v", r$best), div(class = "s", sprintf("mean silhouette %s", .fmt3(max(r$avg))))),
          div(div(class = "k", "k in use"), div(class = "v", r$k), div(class = "s", if (r$k_auto) "chosen automatically" else "chosen by you")),
          div(div(class = "k", "Silhouette (k in use)"), div(class = "v", .fmt3(r$sil_avg)), div(class = "s", st_interpret_silhouette(r$sil_avg))))
    )
  })

  ## ---- plots ---------------------------------------------------------------
  fs <- reactive({ f <- input$font_size; if (is.null(f) || is.na(f)) 12 else f })
  tree_gg <- reactive({
    r <- res()
    if (input$mode == "newick" && identical(input$style, "phylo") && r$kind == "tree")
      plot_phylogeny(r, show_support = isTRUE(input$show_support), font_size = fs())
    else plot_dendrogram(r, show_cut = isTRUE(input$show_cut), font_size = fs())
  })
  sz <- function(extra) { r <- tryCatch(res(), error = function(e) NULL); if (is.null(r)) 300 else round(st_plot_height(r$n, fs(), extra) * 96) }
  output$tree_plot <- renderPlot(tree_gg(), height = function() sz(1.6), res = 96)
  k_gg <- reactive(plot_silhouette_k(res(), font_size = fs()))
  output$k_plot <- renderPlot(k_gg(), res = 96)
  sil_gg <- reactive(plot_silhouette_samples(res(), font_size = fs()))
  output$sil_plot <- renderPlot(sil_gg(), height = function() sz(2.4), res = 96)

  output$k_table <- renderDT({
    r <- res()
    datatable(data.frame(k = r$ks, mean_silhouette_width = round(r$avg, 4), optimal = ifelse(r$ks == r$best, "\u2605", "")),
              rownames = FALSE, options = list(dom = "t", pageLength = 40))
  })
  output$samples_table <- renderDT({
    datatable(st_samples_table(res()), rownames = FALSE, options = list(pageLength = 25))
  })

  ## ---- variables table ---------------------------------------------------------
  output$vars_ui <- renderUI({
    if (input$mode != "table") return(helpText("Only used for table input. / \u8868\u306e\u5165\u529b\u3067\u306e\u307f\u4f7f\u3044\u307e\u3059\u3002"))
    tb <- tbl(); if (is_err(tb)) return(helpText(tb$error))
    d <- specs_default(); key <- tbl_key()
    if (!nrow(d)) return(helpText("No variable columns found."))
    tagList(
      fluidRow(column(3, strong("Column")), column(5, strong("Type")), column(1, strong("Weight")), column(3, strong("Contents"))),
      lapply(seq_len(nrow(d)), function(i) {
        sm <- st_column_summary(tb$rows[, d$idx[i]], d$type[i])
        if (d$type[i] == "skip" && nzchar(d$reason[i])) sm <- SKIP_REASON[[d$reason[i]]]
        div(class = "st-var-row", fluidRow(
          column(3, div(class = "st-var-name", d$name[i])),
          column(5, selectInput(paste0("vt", key, "_", i), NULL, choices = TYPE_CHOICES, selected = d$type[i], width = "100%")),
          column(1, numericInput(paste0("vw", key, "_", i), NULL, value = d$weight[i], min = 0, step = 0.5, width = "100%")),
          column(3, div(class = "st-var-sum", sm))))
      })
    )
  })

  ## ---- downloads ---------------------------------------------------------------
  fmt <- reactive(input$export_format %||% "png")
  save_gg <- function(p, file) st_save_plot(p, file, format = fmt(), width = input$export_width %||% 10, height = input$export_height %||% 8)
  output$dl_tree <- downloadHandler(filename = function() paste0("silhouette-tree_tree.", fmt()), content = function(file) save_gg(tree_gg(), file))
  output$dl_k <- downloadHandler(filename = function() paste0("silhouette-tree_silhouette-vs-k.", fmt()), content = function(file) save_gg(k_gg(), file))
  output$dl_sil <- downloadHandler(filename = function() paste0("silhouette-tree_silhouette-samples.", fmt()), content = function(file) save_gg(sil_gg(), file))
  output$dl_nwk <- downloadHandler(filename = function() "silhouette-tree_dendrogram.nwk",
                                   content = function(file) writeLines(st_hclust_newick(res()), file))
  output$dl_xlsx <- downloadHandler(filename = function() "silhouette-tree_results.xlsx", content = function(file) st_write_workbook(res(), file))
  output$dl_csv <- downloadHandler(filename = function() "silhouette-tree_samples.csv", content = function(file) {
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), file)   # BOM so that Excel opens it as UTF-8
    suppressWarnings(utils::write.table(st_samples_table_full(res()), file, append = TRUE, sep = ",", row.names = FALSE, qmethod = "double", fileEncoding = "UTF-8"))
  })
}

shinyApp(ui, server)
