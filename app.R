library(shiny); library(shinyWidgets)
libs <- c("tidyverse", "dplyr", "magrittr", "readr", "ggplot2", 
          "janitor", "data.table", "DT", "rlc", "bslib", "plotly", 
          "highcharter")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

source("functions.R")

# ref 1: https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph/48890125#48890125
# ref 2: https://www.datanovia.com/en/lessons/highchart-interactive-density-and-histogram-plots-in-r/

bt <- loadData()
symptom_list <- unique(c(bt$Symptom_1, bt$Symptom_2, bt$Symptom_3))


ui <- page_fillable(
  layout_columns(
    card(highchartOutput("densPlot")),
    layout_columns(
      card(textOutput("WIP_Notes"), 
           pickerInput(inputId = "sel_symptoms", label = "Select symptoms:", 
                       choices = symptom_list, multiple = TRUE, 
                       options = pickerOptions(
                         actionsBox = TRUE, size = length(symptom_list), selectAllText = "All selected", dropupAuto = TRUE)
                       ), 
           pickerInput(inputId = "sel_treat", label = "Select treatments:", 
                       choices = c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy"), 
                       multiple = TRUE, 
                       options = pickerOptions(
                         actionsBox = TRUE, size = 3, selectAllText = "All selected", dropupAuto = TRUE)
                       ), 
           checkboxGroupInput(inputId = "sel_mri", label = "MRI result",
             choices = unique(bt$MRI_Result),
             selected = unique(bt$MRI_Result),
             inline = TRUE), 
           checkboxGroupInput(inputId = "sel_famhist", label = "Family history",
                              choices = sort(unique(bt$Family_History), decreasing = TRUE), 
                              selected = sort(unique(bt$Family_History), decreasing = TRUE), 
                              inline = TRUE)
           )
      # card(card_header("C3"), textOutput("selectedElem")),
      # col_widths = c(12, 12)
    ),
    card(highchartOutput("heatPlot")),
    card(highchartOutput("scatPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$WIP_Notes <- renderText({
    print("These inputs not yet linked to rest of app, placeholders for now")
  })
  
  # ref 2  
  output$densPlot <- renderHighchart({
    mdata <- bt %>% filter(Gender == "Male") %>% pull(Age) %>% density()
    fdata <- bt %>% filter(Gender == "Female") %>% pull(Age) %>% density()
    
    hchart(mdata, type = "area", color = "steelblue", name = "Male") %>% 
      hc_add_series(fdata, type = "area", color = "darkgreen", name = "Female") %>% 
      hc_title(text = "Population density by age, grouped by gender") %>% 
      hc_xAxis(title = list(text = "Age")) %>% 
      hc_yAxis(title = list(text = "Population Density"))
  })
  
  
  output$heatPlot <- renderHighchart({
    mt <- bt %>% select(all_of(c("Location", "Stage", "Tumor_Size"))) %>% 
      group_by(Location, Stage) %>% summarise(med_size = median(Tumor_Size))
    pt <- pivot_wider(mt, names_from = c("Stage"), values_from = "med_size") %>% as.data.frame()
    rownames(pt) <- pt[,"Location"]
    pt <- as.matrix(pt[1:4,2:5])
    
    # ref 1
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
    
    hchart(pt) %>% 
      hc_legend(enabled = TRUE) %>%
      hc_title(text = "Brain Location vs Stage") %>%
      hc_colorAxis(minColor = "yellow", maxColor = "red") %>% 
      hc_plotOptions(series = list(events = list(click = ClickFunction)))
  })
  
  # ref 1
  makeReactiveBinding("clickedHM")
  observeEvent(input$Clicked, {
    clickedHM <<- strsplit(paste0(input$Clicked), split="~") %>% unlist %>% trimws
    clickedHM <- list("stage" = clickedHM[1], "loc" = clickedHM[2])
  })
  
  output$selectedElem <- renderText({
    if (length(clickedHM) > 0) {
      paste0("Selected brain location: ", clickedHM[2], ", stage: ", clickedHM[1])
    }
  })

  output$scatPlot <- renderHighchart({
    stage_x <<- clickedHM[1]
    loc_y <<- clickedHM[2]
    
    if (length(stage_x) > 0) {
      toscatter <- bt %>%
        filter(Location == loc_y) %>%
        filter(Stage == stage_x) %>%
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
}

# Run application 
shinyApp(ui = ui, server = server)
