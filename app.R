library(shiny); library(shinyWidgets); library(shinyalert)
libs <- c("tidyverse", "dplyr", "magrittr", "readr", "ggplot2", "janitor", 
          "data.table", "DT", "rlc", "bslib", "plotly", "highcharter", "tidyr")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

source("functions.R")

# ref 1: https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph/48890125#48890125
# ref 2: https://www.datanovia.com/en/lessons/highchart-interactive-density-and-histogram-plots-in-r/
# ref 3: https://www.geeksforgeeks.org/r-language/t-distributed-stochastic-neighbor-embedding-t-sne-using-r/
# ref 4: https://stackoverflow.com/questions/67625405/is-there-a-way-to-put-labels-next-to-an-input-box-in-shiny

base_dt <- loadData()
perplexity_values <- c(10,20,30,40,50,60,70,80,90,100)
symptom_list <- colOpts(base_dt, c("Symptom_1", "Symptom_2", "Symptom_3"))
treatment_list <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy")
tsne_column_choices <- c("m1", "m2", "m3", "m4")

# UI logic
ui <- page_fillable(
  navset_card_underline(
    nav_panel("Home",
      layout_column_wrap(
        width = 1/2,
        card(highchartOutput("densPlot")),
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
            width = 1/2,
            radioGroupButtons(inputId = "sel_mri", label = "MRI result", 
                              size = "sm", direction = "horizontal", selected = "Both", 
                              choices = c("Positive" = "Yes", "Negative" = "No", "Both")),
            radioGroupButtons(inputId = "sel_famhist", label = "Family History", 
                              size = "sm", direction = "horizontal",
                              choices = c("Yes", "No", "Both"), selected = "Both")
          )
        ),
        card(highchartOutput("heatPlot")),
        card(highchartOutput("scatPlot"))
      )
    ),
    nav_panel("Dimensionality Reduction",
              page_sidebar(sidebar = sidebar(
                selectInput(inputId = "tsne_columns", label = "Columns input into t-SNE", width = "400px",
                            choices = tsne_column_choices),
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

  # Reactive value for selected gender
  selected_gender <- reactiveVal(NULL)
  selected_histology <- reactiveVal(NULL)
  quadHM <- reactiveVal(NULL)

  output$legendHint <- renderText({
    "Tip: click legend items to filter charts. Click the same item again to clear."
  })

  output$activeFilters <- renderText({
    active_parts <- c()

    if (!is.null(selected_gender())) {
      active_parts <- c(active_parts, paste("Gender:", selected_gender()))
    }

    if (!is.null(selected_histology())) {
      active_parts <- c(active_parts, paste("Histology:", selected_histology()))
    }

    if (!is.null(quadHM())) {
      active_parts <- c(
        active_parts,
        paste("Stage/Location:", paste(quadHM()$stage, quadHM()$loc, sep = " / "))
      )
    }

    if (length(active_parts) == 0) {
      "Active legend filters: none"
    } else {
      paste("Active legend filters:", paste(active_parts, collapse = " | "))
    }
  })

  output$groupCount <- renderText({
    group_n <- nrow(filtered_selected_group_data())

    if (group_n == 0) {
      "Number of people in the selected group: 0"
    } else {
      paste("Number of people in the selected group:", group_n)
    }
  })

  filtered_base_data <- reactive({
    filtered <- base_dt

    symptom_filter_active <- !is.null(input$sel_symptoms) &&
      length(input$sel_symptoms) > 0 &&
      !setequal(input$sel_symptoms, symptom_list)

    if (symptom_filter_active) {
      filtered <- filtered %>%
        filter(if_any(c(Symptom_1, Symptom_2, Symptom_3), ~ .x %in% input$sel_symptoms))
    } else if (!is.null(input$sel_symptoms) && length(input$sel_symptoms) == 0) {
      filtered <- filtered[0, ]
    }

    treatment_filter_active <- !is.null(input$sel_treat) &&
      length(input$sel_treat) > 0 &&
      !setequal(input$sel_treat, treatment_list)

    if (treatment_filter_active) {
      filtered <- filtered %>%
        filter(if_any(all_of(input$sel_treat), ~ .x == "Yes"))
    } else if (!is.null(input$sel_treat) && length(input$sel_treat) == 0) {
      filtered <- filtered[0, ]
    }

    filtered %>%
      checkYNBoth(., input$sel_mri, "MRI_Result", true_v = "Positive", false_v = "Negative") %>% 
      checkYNBoth(., input$sel_famhist, "Family_History")
  })

  filtered_cross_data <- reactive({
    filtered <- filtered_base_data()

    if (!is.null(selected_histology())) {
      filtered <- filtered %>% filter(Histology == selected_histology())
    }

    filtered
  })

  filtered_plot_data <- reactive({
    filtered <- filtered_cross_data()

    if (!is.null(selected_gender())) {
      filtered <- filtered %>% filter(Gender == selected_gender())
    }

    filtered
  })

  filtered_density_data <- reactive({
    filtered <- filtered_cross_data()
    clicked_vals <- quadHM()

    if (!is.null(clicked_vals)) {
      filtered <- filtered %>%
        filter(Location == clicked_vals$loc) %>%
        filter(Stage == clicked_vals$stage)
    }

    filtered
  })

  filtered_selected_group_data <- reactive({
    filtered <- filtered_density_data()

    if (!is.null(selected_gender())) {
      filtered <- filtered %>% filter(Gender == selected_gender())
    }

    filtered
  })

  filtered_scatter_data <- reactive({
    filtered <- filtered_base_data()

    if (!is.null(selected_gender())) {
      filtered <- filtered %>% filter(Gender == selected_gender())
    }

    filtered
  })

  filteredData <- reactive({
    filtered <- filtered_base_data()

    if (!is.null(selected_gender())) {
      filtered <- filtered %>% filter(Gender == selected_gender())
    }

    if (nrow(filtered) == 0) {
      base_dt
    } else {
      filtered
    }
  })

  # ref 2  
  output$densPlot <- renderHighchart({
    base_data <- filtered_density_data()
    
    male_data <- base_data %>% filter(Gender == "Male") %>% pull(Age) %>% 
      build_density_data(., "Male", base_data)
    female_data <- base_data %>% filter(Gender == "Female") %>% pull(Age) %>% 
      build_density_data(., "Female", base_data)
    
    dens_chart <- highchart() %>%
      hc_chart(events = list(
        load = JS("function() {
          this.legend.allItems.forEach(function(item) {
            var symbol = item.legendSymbol || (item.legendItem && item.legendItem.symbol);
            if (symbol) {
              symbol.attr({
                stroke: (item.options && item.options.lineColor) || item.color,
                'stroke-width': 2
              });
            }
          });
        }"),
        render = JS("function() {
          this.legend.allItems.forEach(function(item) {
            var symbol = item.legendSymbol || (item.legendItem && item.legendItem.symbol);
            if (symbol) {
              symbol.attr({
                stroke: (item.options && item.options.lineColor) || item.color,
                'stroke-width': 2
              });
            }
          });
        }")
      )) %>%
      hc_legend(
        itemStyle = list(cursor = "pointer"),
        itemHoverStyle = list(fontWeight = "600")
      ) %>%
      hc_title(text = paste0(
        "Population density by age, grouped by gender",
        if (!is.null(selected_histology())) paste0(" (", selected_histology(), ")") else "",
        if (!is.null(quadHM())) paste0(" - ", quadHM()$stage, " / ", quadHM()$loc) else ""
      )) %>%
      hc_xAxis(title = list(text = "Age")) %>%
      hc_yAxis(title = list(text = "Population Density")) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = paste0(
          "Age: {point.age_rounded}<br>",
          "Most prevalent symptom: {point.top_symptom}<br>",
          "Most prevalent treatment: {point.top_treatment}<br>",
          "Number of people: {point.count}<br>",
          "Density: {point.y:.4f}"
        )
      ) %>%
      hc_plotOptions(series = list(
        events = list(
          legendItemClick = JS("function(event) {
            Shiny.onInputChange('legendClick', {
              value: this.name,
              nonce: Date.now()
            });
            return false;
          }")
        )
      ))

    if (!is.null(male_data)) {
      dens_chart <- dens_chart %>%
        hc_add_series(
          data = male_data, type = "area", color = "rgba(184, 222, 244, 0.1)",
          lineColor = "rgb(33, 57, 72)", lineWidth = 2, name = "Male"
        )
    }
    
    if (!is.null(female_data)) {
      dens_chart <- dens_chart %>%
        hc_add_series(
          data = female_data, type = "area", color = "rgba(246, 145, 237, 0.2)",
          lineColor = "rgb(146, 73, 140)", lineWidth = 2, name = "Female"
        )
    }
    
    if (is.null(male_data) && is.null(female_data)) {
      dens_chart <- dens_chart %>%
        hc_subtitle(text = "No density data available for the current filters.")
    }

    dens_chart
  })
  
  observeEvent(input$legendClick, {
    clicked_gender <- input$legendClick$value

    if (identical(selected_gender(), clicked_gender)) {
      selected_gender(NULL)
    } else {
      selected_gender(clicked_gender)
    }
  })

  observeEvent(input$scatLegendClick, {
    clicked_histology <- input$scatLegendClick$value

    if (identical(selected_histology(), clicked_histology)) {
      selected_histology(NULL)
    } else {
      selected_histology(clicked_histology)
    }
  })

  output$heatPlot <- renderHighchart({
    filtered_bt <- filtered_plot_data()

    if (nrow(filtered_bt) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Brain Location vs Stage") %>%
        hc_subtitle(text = "No data available for the current filters.")
      )
    }

    size_breaks <- pretty(range(filtered_bt$Tumor_Size, na.rm = TRUE), n = 6)

    if (length(unique(size_breaks)) < 2) {
      size_breaks <- c(min(filtered_bt$Tumor_Size, na.rm = TRUE), max(filtered_bt$Tumor_Size, na.rm = TRUE))
    }

    size_labels <- paste0(
      format(head(size_breaks, -1), nsmall = 1, trim = TRUE),
      " - ",
      format(tail(size_breaks, -1), nsmall = 1, trim = TRUE)
    )

    binned_bt <- filtered_bt %>%
      mutate(
        size_bin = cut(
          Tumor_Size,
          breaks = size_breaks,
          include.lowest = TRUE,
          labels = size_labels
        )
      )

    mode_bins <- binned_bt %>%
      count(Location, Stage, size_bin, name = "bin_count") %>%
      group_by(Location, Stage) %>%
      slice_max(order_by = bin_count, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(bin_index = match(as.character(size_bin), size_labels) - 1L)

    mt <- binned_bt %>%
      group_by(Location, Stage) %>%
      summarise(med_size = median(Tumor_Size), .groups = "drop") %>%
      left_join(mode_bins, by = c("Location", "Stage")) %>%
      complete(Location, Stage,
        fill = list(med_size = NA_real_, size_bin = NA, bin_count = 0L, bin_index = NA_integer_)
      )

    location_levels <- sort(unique(mt$Location))
    stage_levels <- sort(unique(mt$Stage))
    bin_colors <- grDevices::colorRampPalette(c("#FFF7BC", "#D7301F"))(length(size_labels))

    heatmap_data <- mt %>%
      mutate(
        x = match(Stage, stage_levels) - 1L,
        y = match(Location, location_levels) - 1L,
        value = bin_index,
        median_label = ifelse(is.na(med_size), "NA", format(round(med_size, 2), nsmall = 2)),
        range_label = ifelse(is.na(size_bin), "No data", as.character(size_bin))
      ) %>%
      transmute(x, y, value, name = paste(Stage, Location, sep = " ~ "),
        stage = Stage, location = Location, median_label, range_label)

    heatmap_points <- lapply(seq_len(nrow(heatmap_data)), function(i) {
      as.list(heatmap_data[i, ])
    })

    data_classes <- lapply(seq_along(size_labels), function(i) {
      list(
        from = i - 1,
        to = i - 1,
        name = size_labels[i],
        color = bin_colors[i]
      )
    })
    
    # ref 1
    clickHM <- JS("function(event) {
      Shiny.onInputChange('Clicked', {
        value: event.point.name,
        nonce: Date.now()
      });
    }")
    
    highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_legend(
        enabled = TRUE,
        title = list(text = "Most common tumor size range")
      ) %>%
      hc_title(text = paste0(
        "Brain Location vs Stage",
        if (!is.null(selected_gender())) paste0(" (", selected_gender(), ")") else "",
        if (!is.null(selected_histology())) paste0(" - ", selected_histology()) else ""
      )) %>%
      hc_xAxis(categories = stage_levels, title = list(text = "Stage")) %>%
      hc_yAxis(categories = location_levels, title = list(text = "Location")) %>%
      hc_colorAxis(dataClasses = data_classes) %>% 
      hc_tooltip(
        useHTML = TRUE, headerFormat = "",
        pointFormat = paste0(
          "<b>{point.location}, {point.stage}</b><br>",
          "Most common size range: {point.range_label}<br>",
          "Median tumor size: {point.median_label}"
        )
      ) %>%
      hc_add_series(
        data = heatmap_points, name = "",
        borderWidth = 1, events = list(click = clickHM)) %>%
      hc_plotOptions(series = list(events = list(click = clickHM)))
  })
  
  # ref 1
  observeEvent(input$Clicked, {
    quad_vals <- strsplit(paste0(input$Clicked$value), split = "~") %>% unlist() %>% trimws()
    clicked_cell <- list(stage = quad_vals[1], loc = quad_vals[2])

    if (identical(quadHM(), clicked_cell)) {
      quadHM(NULL)
    } else {
      quadHM(clicked_cell)
    }
  })

  output$scatPlot <- renderHighchart({
    clicked_vals <- quadHM()

    if (is.null(clicked_vals)) {
      return(
        highchart() %>%
          hc_title(text = "Tumor growth vs survival rate") %>%
          hc_subtitle(text = "Click a heatmap cell to view the scatter plot for that stage and brain location.")
      )
    }

    toscatter <- filtered_scatter_data() %>%
      filter(Location == clicked_vals$loc) %>%
      filter(Stage == clicked_vals$stage)

    if (nrow(toscatter) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Tumor growth vs survival rate") %>%
          hc_subtitle(text = "No data available for the selected heatmap cell and current filters.")
      )
    }

    toscatter <- toscatter %>%
      select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology")))

    scatter_chart <- highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_legend(
        enabled = TRUE,
        itemStyle = list(cursor = "pointer"),
        itemHoverStyle = list(fontWeight = "600")
      ) %>%
      hc_xAxis(title = list(text = "Tumor Growth Rate")) %>%
      hc_yAxis(title = list(text = "Survival Rate")) %>%
      hc_title(text = paste0(
        "Tumor growth vs survival rate for stage ",
        clicked_vals$stage, " and the ", clicked_vals$loc, " lobe",
        if (!is.null(selected_gender())) paste0(" (", selected_gender(), ")") else "",
        if (!is.null(selected_histology())) paste0(" - ", selected_histology()) else ""
      )) %>%
      hc_plotOptions(series = list(
        events = list(
          legendItemClick = JS("function(event) {
            Shiny.onInputChange('scatLegendClick', {
              value: this.name,
              nonce: Date.now()
            });
            return false;
          }")
        ),
        states = list(inactive = list(opacity = 1))
      ))

    for (hist_name in sort(unique(toscatter$Histology))) {
      hist_data <- toscatter %>%
        filter(Histology == hist_name) %>%
        transmute(x = Tumor_Growth_Rate, y = Survival_Rate)

      is_selected <- is.null(selected_histology()) || identical(selected_histology(), hist_name)

      scatter_chart <- scatter_chart %>%
        hc_add_series(
          data = list_parse2(hist_data),
          name = hist_name,
          type = "scatter",
          opacity = if (is_selected) 1 else 0.15,
          marker = list(
            radius = if (is_selected) 4 else 3,
            states = list(
              hover = list(
                enabled = TRUE,
                radiusPlus = 1
              )
            )
          )
        )
    }

    scatter_chart
  })
  
  # Tab 2: Dimensionality Reduction and Clustering -----------------------------
  output$dimredPlot <- renderPlotly({
    # ref 3
    colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE")
    
    run_x <- as.character(input$tsne_columns)
    br_red <- base_dt %>% select(all_of(readRDS(paste0("tsne_out/", run_x, ".Rds"))))
    
    char_cols <- sapply(br_red, class) %>% grep("character", ., , value = TRUE) %>% names()
    br_mat <- br_red %>% mutate(across(all_of(char_cols), ~ as.numeric(as.factor(.x)))) %>%
      as.matrix() %>% unique() %>% scale() %>% unique()
    
    pval <- formatC(as.numeric(input$perp), flag = "0", width = 3)
    tsne_file <- paste0("tsne_out/", run_x, "_p", pval, ".Rds")
    
    color_column <- as.data.frame(filteredData())[, input$color_dr]
    
    br_res <- readRDS(tsne_file)$res
    br_dat <- data.frame(br_res$Y)
    br_dat$ColorBy <- color_column[!duplicated(br_mat)]
    colnames(br_dat) <- c("Dim1", "Dim2", "Dim3", "ColorBy")
    plot_ly(br_dat, x = ~Dim1, y = ~Dim2, z = ~Dim3,type = "scatter3d", mode = "markers", 
            colors = colors, marker = list(size = 6), color = ~ColorBy)
  })
}

# Run app
shinyApp(ui = ui, server = server)
