# LIBRARIES --------------------------------------------------------------------
library(shiny); library(shinyWidgets); library(plotly); 
library(tidyverse); library(janitor); library(DT); library(rlc); 
library(ggplot2); library(Rtsne); library(umap); library(shinyscreenshot)
libs <- c("readr", "magrittr", "dplyr", "data.table", "bslib", "highcharter", 
          "tidyr", "shinyjs", "RColorBrewer")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

source("global.R")

# GLOBAL VARIABLES -------------------------------------------------------------
base_dt <- loadData()
perplexity_values <- c(10,20,30,40,50,60,70,80,90,100)
symptom_list <- colOpts(base_dt, c("Symptom_1", "Symptom_2", "Symptom_3"))
treatment_list <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy")
# tsne_column_choices <- c("m1", "m2", "m3", "m4")

dr_col_choices <- list.files("dr/tsne") %>% gsub(".Rds", "", .) %>% 
  grep("i", ., invert = TRUE, value = TRUE)
dr_full_names <- lapply(dr_col_choices, function(x) {
  readRDS(paste0("dr/tsne/", x, ".Rds")) %>% paste0(., collapse = ", ")
}) %>% unlist()

col_list <- list(
  seq = RColorBrewer::brewer.pal.info %>% filter(category == "seq") %>% rownames(), 
  qual = RColorBrewer::brewer.pal.info %>% filter(category == "qual") %>% rownames())

cnames <- c(setNames(col_list$seq, paste(col_list$seq, "(Seq)")), 
            setNames(col_list$qual, paste(col_list$qual, "(Qual)")))

col_encoding <- colnames(base_dt)[-ncol(base_dt)][-1] %>% setNames(., nm = gsub("_", " ", .))

# UI logic
ui <- page_fillable(
  navset_card_underline(
    id = "main_nav", title = "BTVis", 
    nav_panel(title = "Home", value = "home",
              layout_column_wrap(
                width = 1/2,
                card(
                  textOutput("legendHint"),
                  textOutput("activeFilters"),
                  textOutput("groupCount"),
                  layout_column_wrap(
                    width = 1/2,
                    pickerInput("sel_symptoms", "Select symptoms:", choices = symptom_list,
                                selected = symptom_list, multiple = TRUE,
                                options = pickerOptions(actionsBox = TRUE, size = length(symptom_list),
                                                        selectAllText = "All selected", dropupAuto = TRUE)),
                    pickerInput("sel_treat", "Select treatments:", choices = treatment_list,
                                selected = treatment_list, multiple = TRUE,
                                options = pickerOptions(actionsBox = TRUE, size = length(treatment_list),
                                                        selectAllText = "All selected", dropupAuto = TRUE))
                  ),
                  layout_column_wrap(
                    width = 1/3,
                    radioGroupButtons(inputId = "sel_mri", label = "MRI result", 
                                      size = "sm", direction = "horizontal", selected = "Both", 
                                      choices = c("Positive" = "Yes", "Negative" = "No", "Both")),
                    radioGroupButtons(inputId = "sel_famhist", label = "Family History", 
                                      size = "sm", direction = "horizontal",
                                      choices = c("Yes", "No", "Both"), selected = "Both"), 
                    layout_column_wrap(
                      width = 1/2, 
                      actionButton(inputId = "screenshot", label = NULL, 
                                   icon = icon("camera"), width = "70px"),
                      downloadButton(outputId = "downloadTbl", label = NULL, icon = icon("download")))    
                  )
                ),
                card(highchartOutput("densPlot")),
                card(highchartOutput("heatPlot")),
                card(highchartOutput("scatPlot"))
              )
    ),
    nav_panel(
      title = "Dimensionality reduction",
      layout_columns(
        col_widths = c(3,6,3), 
        layout_columns(
          col_widths = 12, row_heights = c(2,10),
          card(
            card_header("Dimensionality reduction type"),
            prettyRadioButtons(inputId = "dr_type", label = "Select a technique", 
                               choices = c("t-SNE", "UMAP", "PCA"), 
                               selected = "t-SNE", status = "primary", shape = "round", 
                               bigger = TRUE, fill = TRUE, thick = TRUE, inline = TRUE)
          ), 
          card(
            card_header("Use saved runs"),
            selectInput(inputId = "dr_columns", width = "400px", 
                        choices = setNames(dr_col_choices, nm = gsub("_", " ", dr_full_names)), 
                        label = "Columns selected for input into dimensionality reduction methods"),
            selectInput(inputId = "color_dr", label = "Color encoding for plot",
                        choices = col_encoding, selected = "Tumor_Size"),
            selectInput(inputId = "color_palette", label = "Color palette", choices = cnames, selected = "YlGnBu"),
            uiOutput("DRSelected")
          )
        ), 
        card(plotly::plotlyOutput("DRPlot", width = "auto", height = "auto")), 
        
        layout_columns(
          col_widths = 12, row_heights = c(3,9), 
          card(textOutput("DRExplanation")), 
          card(card_header("New run"), fill = FALSE, useShinyjs(),
               card_body(
                 paste0("Select columns of data to input into a new run. ", 
                        "This may take some time. Data will be saved and available for plotting ", 
                        "when app is next refreshed."),
                 materialSwitch(inputId = "new_run_tog", label = "See inputs for new run:", 
                                value = FALSE, status = "success")), 
               card_body(uiOutput("DRNewInputs"))
          )
        )
      )
    )
  )
)

