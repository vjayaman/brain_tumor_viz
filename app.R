library(shiny); library(shinyWidgets); library(shinydashboard)
libs <- c("tidyverse", "dplyr", "magrittr", "readr", "ggplot2", "janitor", 
          "data.table", "DT", "rlc", "bslib", "plotly", "highcharter")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

source("functions.R")

# ref 1: https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph/48890125#48890125
# ref 2: https://www.datanovia.com/en/lessons/highchart-interactive-density-and-histogram-plots-in-r/
# ref 3: https://www.geeksforgeeks.org/r-language/t-distributed-stochastic-neighbor-embedding-t-sne-using-r/

base_dt <- loadData()
perplexity_values <- c(10,20,30,40,50,60,70,80,90,100)
symptom_list <- colOpts(base_dt, c("Symptom_1", "Symptom_2", "Symptom_3"))
treatment_list <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy")
a1 <- c("m1", "m2", "m3", "m4")

# UI logic
ui <- page_fillable(
  
  navset_card_underline(
    nav_panel("Home",
      layout_column_wrap(
        width = 1/2, 
        card(highchartOutput("densPlot")),
        card(
          fluidRow(
            column(6, 
                   pickerInput(inputId = "sym", label = "Select symptoms:", multiple = TRUE, 
                               choices = symptom_list, selected = symptom_list,
                               options = pickerOptions(actionsBox = TRUE, size = length(symptom_list), 
                                                       selectAllText = "All selected", dropupAuto = TRUE)), 
                   checkboxGroupInput(inputId = "mri", label = "MRI result:",
                                      choices = colOpts(base_dt, "MRI_Result"), 
                                      selected = colOpts(base_dt, "MRI_Result"), inline = TRUE), 
                   
                   checkboxGroupInput(inputId = "fam", label = "Family history:", inline = TRUE, 
                                      choices = colOpts(base_dt, "Family_History"), 
                                      selected = colOpts(base_dt, "Family_History"))
                   ), 
            column(6, p("Treatments:\n"), chkBoxGroupBin("rad", "Radiation"),
                   chkBoxGroupBin("surg", "Surgery"), chkBoxGroupBin("chemo", "Chemotherapy"))
            ), 
          fluidRow(uiOutput("Notes"))
        ), 
        card(highchartOutput("heatPlot")), 
        card(highchartOutput("scatPlot"))
      )      
    ), 
    nav_panel("Dimensionality Reduction", 
              page_sidebar(sidebar = sidebar(
                selectInput(inputId = "tsne_columns", label = "Columns input into t-SNE", width = "400px",
                            choices = a1 %>% setNames(
                              lapply(a1, function(m) {
                                readRDS(paste0("tsne_out/", m, ".Rds")) %>% paste0(collapse = ", ")
                              }))), 
                selectInput(inputId = "color_dr", label = "Color encoding for t-SNE plot", 
                            choices = c("Gender", `Tumor Type` = "Tumor_Type", `Brain lobe (Location)` = "Location", 
                                        "Histology", "Stage"), selected = "Gender"),
                sliderInput(inputId = "perp", label = "Perplexity value", 
                            min = 10, max = 100, step = 10, animate = TRUE, value = 50)), 
                plotlyOutput("dimredPlot")
              ))
  )
)



# Server logic
server <- function(input, output) {

  baseData <- reactive({
    bt <- loadData()
    if (length(input$sym) > 0) {
      bt %<>% filter(Symptom_1 %in% input$sym) %>% 
        filter(Symptom_2 %in% input$sym) %>% filter(Symptom_3 %in% input$sym)
    }
    
    bt <- checkBinary(bt, input$rad, "Radiation_Treatment") %>% 
      checkBinary(., input$surg, "Surgery_Performed") %>% 
      checkBinary(., input$chemo, "Chemotherapy")
    
    if (length(input$mri) > 0) {bt %<>% filter(MRI_Result %in% input$mri)}
    if (length(input$fam) > 0) {bt %<>% filter(Family_History %in% input$fam)}
    
    if (nrow(bt) == 0) {loadData()}else {bt}
  })

  # ref 2  
  output$densPlot <- renderHighchart({
    mdata <- baseData() %>% filter(Gender == "Male") %>% pull(Age) %>% density()
    fdata <- baseData() %>% filter(Gender == "Female") %>% pull(Age) %>% density()
    
    # ref 1
    legDP <- JS("function(event) {Shiny.onInputChange('legendItemClick', [this.name, this.visible]);}")
    
    hchart(mdata, type = "area", color = "steelblue", name = "Male") %>% 
      hc_add_series(fdata, type = "area", color = "darkgreen", name = "Female") %>% 
      hc_title(text = "Population density by age, grouped by gender") %>% 
      hc_xAxis(title = list(text = "Age")) %>% 
      hc_yAxis(title = list(text = "Population Density")) %>% 
      hc_plotOptions(series = list(events = list(legendItemClick = legDP)))
  })
  
  age_gender <- reactiveValues(Male = TRUE, Female = TRUE)
  observeEvent(input$legendItemClick, {
    age_gender[[input$legendItemClick[1]]] <<- !age_gender[[input$legendItemClick[1]]]
  })
  
  output$Notes <- renderUI({
    HTML(paste("Number of rows after filtering: ", nrow(baseData()), "<br>", 
               "If filtering options result in empty set, the last adjusted option ", 
               "is reset to all options selected. <br>", sep = ""))
  })
  
  
  output$heatPlot <- renderHighchart({
    mt <- baseData() %>% filterByGen(., age_gender) %>% 
      select(all_of(c("Location", "Stage", "Tumor_Size"))) %>% group_by(Location, Stage) %>% 
      summarise(med_size = median(Tumor_Size))#, .groups = c("Location", "Stage"))
    pt <- pivot_wider(mt, names_from = c("Stage"), values_from = "med_size") %>% as.data.frame()
    rownames(pt) <- pt[,"Location"]
    pt <- as.matrix(pt[1:4,2:5])
    
    # ref 1
    clickHM <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
    
    hchart(pt) %>% 
      hc_legend(enabled = TRUE) %>%
      hc_title(text = "Brain Location vs Stage") %>%
      hc_colorAxis(minColor = "yellow", maxColor = "red") %>% 
      hc_plotOptions(series = list(events = list(click = clickHM)))
  })
  
  # ref 1
  makeReactiveBinding("quadHM")
  observeEvent(input$Clicked, {
    quadHM <<- strsplit(paste0(input$Clicked), split="~") %>% unlist %>% trimws
    quadHM <- list("stage" = quadHM[1], "loc" = quadHM[2])
    print(paste0(quadHM, ", "))
  })
  
  output$selectedElem <- renderText({
    if (length(quadHM) > 0) {
      paste0("Selected brain location: ", quadHM[2], ", stage: ", quadHM[1])
    }
  })

  output$scatPlot <- renderHighchart({
    stage_x <<- quadHM[1]
    loc_y <<- quadHM[2]
    
    if (length(stage_x) > 0) {
      toscatter <- baseData() %>% filterByGen(., age_gender) %>% 
        filter(Location == loc_y) %>% filter(Stage == stage_x) %>%
        select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology")))
      
      hchart(toscatter, type = "scatter", 
             hcaes(x = Tumor_Growth_Rate, y = Survival_Rate, group = Histology)) %>% 
        hc_legend(enabled = TRUE) %>% 
        hc_xAxis(title = list(text = "Tumor Growth Rate")) %>% 
        hc_yAxis(title = list(text = "Survival Rate")) %>% 
        hc_title(text = paste0("Tumor growth vs survival rate for stage ", 
                               stage_x, " and the ", loc_y, " lobe")) %>% 
        hc_plotOptions(series = list(
          states = list(inactive = list(opacity = 0.2))))
    }
  })
  
  # Tab 2: Dimensionality Reduction and Clustering -----------------------------
  output$dimredPlot <- renderPlotly({
    # ref 3
    colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE")
    
    run_x <- as.character(input$tsne_columns)
    br_red <- loadData() %>% select(all_of(readRDS(paste0("tsne_out/", run_x, ".Rds"))))
    
    char_cols <- sapply(br_red, class) %>% grep("character", ., , value = TRUE) %>% names()
    br_mat <- br_red %>% mutate(across(all_of(char_cols), ~ as.numeric(as.factor(.x)))) %>%
      as.matrix() %>% unique() %>% scale() %>% unique()
    
    n_iter <- 3000
    n_dims <- 3
    
    tsne_out <- list.files("tsne_out", full.names = TRUE, pattern = run_x)
    pvals <- perplexity_values %>% formatC(., flag = "0", width = 3) %>% paste0("p", ., ".Rds")
    tsne_res <- lapply(pvals, function(p) {
      grep(pattern = p, x = tsne_out, value = TRUE) %>% readRDS()}) %>% set_names(., pvals)
    
    i <- which(perplexity_values == as.numeric(as.character(input$perp)))
    
    color_column <- as.data.frame(baseData())[, input$color_dr]
    
    br_res <- tsne_res[[i]]$res
    br_dat <- data.frame(br_res$Y)
    br_dat$ColorBy <- color_column[!duplicated(br_mat)]
    colnames(br_dat) <- c("Dim1", "Dim2", "Dim3", "ColorBy")
    plot_ly(br_dat, x = ~Dim1, y = ~Dim2, z = ~Dim3,type = "scatter3d", mode = "markers", 
            colors = colors, marker = list(size = 6), color = ~ColorBy)
  })
}

# Run app
shinyApp(ui = ui, server = server)
