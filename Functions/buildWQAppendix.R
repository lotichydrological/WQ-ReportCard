#' Build water quality appendix word document
#' 
#' @description Builds the fundamental elements of an appendix containing water quality information generated from the water quality retrieval code.
#' Inserts .pngs of monitoring site tables, hydrographs, and standard exceedance bar charts to a word document, retrieves the parameters of interest
#' that have been flagged as "Poor" or "Concern", and inserts reach description.
#' 
#' @param segmnent String of the desired segment name to summarize.
#' @param params List of parameters flagged as "Poor" or "Concern" by summaryCalcs function.
#' @param reach_description String of the narrative description of the 303(b) segment.
#' 
#' @usage buildWQAppendix(segment, params, reach_description)
#' 
#' @export

library(officer)
library(magrittr)
library(png)

buildWQAppendix <- function(segment, params, reach_description){
  # define path to save results to
  doc_path = "Output/WQ_appendix.docx"
  
  # define paths of table, qPlot, and bar chart and retrieve image dimensions
  table_path = paste("Figures/site_tables/",segment,"_MonSites.png", sep="")
  dims1 <- attributes(png::readPNG(table_path))$dim/72
  q_path = paste("Figures/qPlots/",segment,"_Q_plot.png", sep = "")
  bar_path = paste("Figures/bar_charts/",segment,"_summary_plot.png", sep = "")
  
  
  # define italic text type
  italic <- fp_text(
    color = "black",
    font.size = 8,
    bold = FALSE,
    italic = TRUE
    )
  
  # define smaller text type
  smaller <- fp_text(
    color = "black",
    font.size = 8,
    bold = FALSE,
    italic = FALSE
    )
  
  # define custom paragraph formatting
  par_center <- fp_par(text.align = "center")
  par_justify <- fp_par(text.align = "justify")

  wq_doc <- read_docx(path = doc_path) %>%
    # add segment header and section titles
    body_add_par(value = segment, style = "heading 3") %>%
    body_add_fpar(fpar(ftext("Reach Description:", prop = italic), fp_p = par_justify)) %>%
    body_add_fpar(fpar(ftext(reach_description, prop = smaller), fp_p = par_justify)) %>%
    body_add_fpar(fpar(ftext("Designated Uses:", prop = italic), fp_p = par_justify)) %>%
    body_add_par(value = "Summary", style = "Normal") %>%
    body_add_par(value = "Regulatory status", style = "Normal") %>%
    body_add_par(value = "Parameters of interest", style = "Normal") %>%
    print(wq_doc, target = doc_path)
  
  # grab parameters of concern list and write to file
  wq_doc <- read_docx(path = doc_path) 
  for(n in 1:length(params)){
    wq_doc <- wq_doc %>% body_add_fpar(fpar(ftext(params[n], prop = italic), fp_p = par_justify) )
    } 
  print(wq_doc, target = doc_path)
  
  # add data quality and representativeness section and figures
  wq_doc <- read_docx(path = doc_path) %>%
    body_add_par(value = "Data quality and representativeness", style = "Normal") %>%
    body_add_fpar(fpar(ftext(paste("Table X: Sampling locations for ", segment,".", sep = ""), prop = italic), fp_p = par_center) ) %>%
    # load in images of the tables, q plots, and bar charts
    body_add_img(src = table_path, width = dims1[2]/2, height = dims1[1]/2,  style = "centered") %>%
    body_add_img(src = q_path, width = 5, height = 2, style = "centered") %>%
    body_add_fpar(fpar(ftext(paste("Figure X. Sampling dates hydrograph, ",segment,". Observed streamflow (blue line) derived from nearest available USGS gauges may not be directly representative for the segment but provides general sense of hydrological conditions present during sampling.", sep = ""), prop = italic), 
                               fp_p = par_center) ) %>%
    body_add_img(src = bar_path, width = 5, height = 1.75, style = "centered") %>%
    body_add_fpar(fpar(ftext(paste("Figure X. Water quality Indicators summary, ",segment,". Graphs summarize the count of individual parameter ratings in each Report Card assessment class (Good, Acceptable, Concern, Poor, Low Resolution, Data Gap).", sep = ""), prop = italic), fp_p = par_center) ) %>%
    print(wq_doc, target = doc_path)
    }
  