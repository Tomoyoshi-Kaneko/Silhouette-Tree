# ============================================================================
# demo_data.R -- small example inputs (also written to inst/extdata/)
# ============================================================================

#' Fictional demo tree (Newick text)
#' @export
st_demo_tree <- function() {
  "((((Sample_01:0.010,Sample_02:0.012)100:0.020,Sample_03:0.025)98:0.150,(Sample_04:0.020,Sample_05:0.018)100:0.120)95:0.600,(((Sample_06:0.010,Sample_07:0.011)100:0.030,Sample_08:0.040)99:0.200,((Sample_09:0.020,Sample_10:0.025)100:0.100,(Sample_11:0.015,Sample_12:0.012)100:0.090)90:0.250)97:0.500);"
}

#' Table 2 of Kaneko et al. (J Virol 2025): genes required by each phage (real data, CC BY 4.0)
#' @export
st_demo_paper_table <- function() {
  paste(c(
    "phage,waaV,waaW,waaY,waaT,fihD,yhaH,nfrB,tolA",
    "\u03a6Wec172,1,1,0,0,0,0,0,0", "\u03a6Wec174,1,1,1,0,0,0,0,0", "\u03a6Wec177,1,1,1,0,0,0,0,0",
    "\u03a6Wec179,0,0,0,1,1,0,0,0", "\u03a6Wec181,0,0,0,1,1,0,0,0", "\u03a6Wec186,0,0,0,1,1,0,0,0",
    "\u03a6Wec187,0,0,1,0,0,0,0,0", "\u03a6Wec188,0,0,0,1,0,1,0,0", "\u03a6Wec190,0,0,0,1,0,1,0,0",
    "\u03a6Wec189,0,0,1,0,0,0,1,1", "\u03a6Wec191,0,0,1,0,0,0,1,1", "\u03a6Wec193,0,0,1,0,0,0,1,1",
    "\u03a6Wec196,0,0,1,0,0,0,1,1", "\u03a6Wec270,0,0,1,0,0,0,1,1", "\u03a6Wec272,0,0,1,0,0,0,1,1",
    "T5,0,0,0,0,0,0,0,0"), collapse = "\n")
}

#' Fictional mixed table (numeric columns with very different scales + 0/1 host range)
#' @export
st_demo_measurements <- function() {
  paste(c(
    "sample,onset_min,duration_min,adsorption_const,burst_size,host_1,host_2,host_3,host_4",
    "Demo_01,14.2,125,1.93E-09,58,1,1,1,0", "Demo_02,18.1,122,2.12E-09,61,1,1,0,0",
    "Demo_03,9.9,103,1.73E-09,57,1,0,0,0", "Demo_04,15.9,124,1.80E-09,70,1,1,0,0",
    "Demo_05,31.0,307,6.57E-09,154,0,1,1,0", "Demo_06,29.6,316,6.38E-09,128,0,1,1,0",
    "Demo_07,31.7,290,6.45E-09,149,0,1,1,0", "Demo_08,36.3,319,6.11E-09,131,0,1,1,0",
    "Demo_09,44.1,86,1.19E-09,255,0,0,0,1", "Demo_10,41.3,72,1.05E-09,296,0,0,0,1",
    "Demo_11,47.7,93,1.24E-09,340,0,0,1,1", "Demo_12,49.9,94,7.04E-10,300,0,0,1,1"), collapse = "\n")
}
