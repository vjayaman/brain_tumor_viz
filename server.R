# LIBRARIES --------------------------------------------------------------------
library(shiny); library(shinyWidgets); 
libs <- c("tidyverse", "readr", "ggplot2", "janitor", "magrittr", "dplyr",
          "data.table", "DT", "rlc", "bslib", "plotly", "highcharter", "tidyr", 
          "umap", "shinyjs", "Rtsne", "RColorBrewer")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

base_dt <- loadData()

symptom_list <- colOpts(base_dt, c("Symptom_1", "Symptom_2", "Symptom_3"))
treatment_list <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy")

# Server logic
server <- function(input, output) {
  
  # Reactive value for selected gender
  selected_gender <- reactiveVal(NULL)
  selected_histology <- reactiveVal(NULL)
  quadHM <- reactiveVal(NULL)
  tsne_cache <- reactiveVal(list())
  
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
  
  observeEvent(input$screenshot, {
    screenshot(filename = "homepage", download = TRUE)
  })
  
  output$downloadTbl <- downloadHandler(
    filename = "filtered_base_table.csv", 
    content = function(con) {write.csv(filtered_base_data(), con)}
  )
  
  
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
    
    dens_chart <- highchart() |>
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
      )) |>
      hc_legend(
        itemStyle = list(cursor = "pointer"),
        itemHoverStyle = list(fontWeight = "600")
      ) |>
      hc_title(text = paste0(
        "Population density by age, grouped by gender",
        if (!is.null(selected_histology())) paste0(" (", selected_histology(), ")") else "",
        if (!is.null(quadHM())) paste0(" - ", quadHM()$stage, " / ", quadHM()$loc) else ""
      )) |>
      hc_xAxis(title = list(text = "Age")) |>
      hc_yAxis(title = list(text = "Population Density")) |>
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
      ) |>
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
      dens_chart <- dens_chart |>
        hc_add_series(
          data = male_data, type = "area", color = "rgba(184, 222, 244, 0.1)",
          lineColor = "rgb(33, 57, 72)", lineWidth = 2, name = "Male"
        )
    }
    
    if (!is.null(female_data)) {
      dens_chart <- dens_chart |>
        hc_add_series(
          data = female_data, type = "area", color = "rgba(246, 145, 237, 0.2)",
          lineColor = "rgb(146, 73, 140)", lineWidth = 2, name = "Female"
        )
    }
    
    if (is.null(male_data) && is.null(female_data)) {
      dens_chart <- dens_chart |>
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
        highchart() |>
          hc_title(text = "Brain Location vs Stage") |>
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
    
    highchart() |>
      hc_chart(type = "heatmap") |>
      hc_legend(
        enabled = TRUE,
        title = list(text = "Most common tumor size range")
      ) |>
      hc_title(text = paste0(
        "Brain Location vs Stage",
        if (!is.null(selected_gender())) paste0(" (", selected_gender(), ")") else "",
        if (!is.null(selected_histology())) paste0(" - ", selected_histology()) else ""
      )) %>%
      hc_xAxis(categories = stage_levels, title = list(text = "Stage")) |>
      hc_yAxis(categories = location_levels, title = list(text = "Location")) |>
      hc_colorAxis(dataClasses = data_classes) |>
      hc_tooltip(
        useHTML = TRUE, headerFormat = "",
        pointFormat = paste0(
          "<b>{point.location}, {point.stage}</b><br>",
          "Most common size range: {point.range_label}<br>",
          "Median tumor size: {point.median_label}"
        )
      ) |>
      hc_add_series(
        data = heatmap_points, name = "",
        borderWidth = 1, events = list(click = clickHM)) |>
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
        highchart() |>
          hc_title(text = "Tumor growth vs survival rate") |>
          hc_subtitle(text = "Click a heatmap cell to view the scatter plot for that stage and brain location.")
      )
    }
    
    toscatter <- filtered_scatter_data() %>%
      filter(Location == clicked_vals$loc) %>%
      filter(Stage == clicked_vals$stage)
    
    if (nrow(toscatter) == 0) {
      return(
        highchart() |>
          hc_title(text = "Tumor growth vs survival rate") |>
          hc_subtitle(text = "No data available for the selected heatmap cell and current filters.")
      )
    }
    
    toscatter <- toscatter %>%
      select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology")))
    
    scatter_chart <- highchart() |>
      hc_chart(type = "scatter") |>
      hc_legend(
        enabled = TRUE,
        itemStyle = list(cursor = "pointer"),
        itemHoverStyle = list(fontWeight = "600")
      ) |>
      hc_xAxis(title = list(text = "Tumor Growth Rate")) |>
      hc_yAxis(title = list(text = "Survival Rate")) |>
      hc_title(text = paste0(
        "Tumor growth vs survival rate for stage ",
        clicked_vals$stage, " and the ", clicked_vals$loc, " lobe",
        if (!is.null(selected_gender())) paste0(" (", selected_gender(), ")") else "",
        if (!is.null(selected_histology())) paste0(" - ", selected_histology()) else ""
      )) |>
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
      
      scatter_chart <- scatter_chart |>
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
  
  output$dimredPlotContainer <- renderUI({
    if (!identical(input$main_nav, "dimred")) {
      return(NULL)
    }
    
    plotlyOutput("dimredPlot")
  })
  
  # Tab 2: Dimensionality Reduction and Clustering ------------------------------------------------
  # -----------------------------------------------------------------------------------------------
  output$DRExplanation <- renderText({
    if (input$dr_type == "t-SNE") {
      paste0(
        "t-SNE (t-Distributed Stochastic Neighbor Embedding) is a nonlinear ", 
        "dimensionality reduction technique. A focus is on understanding ", 
        "the structure of local neighborhoods in the data")
    }else if (input$dr_type == "PCA") {
      paste0(
        "PCA (Principal Component Analysis) is a linear dimensionality reduction ", 
        "technique. A focus is on understanding the structure of global ", 
        "variance in the data")
    }else if (input$dr_type == "UMAP") {
      paste0(
        "UMAP (Uniform Manifold Approximation and Projection) is a nonlinear ", 
        "technique for dimensionality reduction. A focus is on understanding ", 
        "the structure of local neighborhoods in the data")
    }
  })
  
  output$DRSelected <- renderUI({
    if (input$dr_type == "t-SNE") {
      tagList(sliderInput(inputId = "perp", label = "Perplexity value",
                          min = 10, max = 100, step = 10, animate = TRUE, value = 50))
    }
  })
  
  observeEvent(input$new_run_tog, {
    if (input$new_run_tog) {
      shinyjs::show(id = "DRNewInputs")
    }else {
      shinyjs::hide(id = "DRNewInputs")
    }
  })
  
  output$DRNewInputs <- renderUI({
    tagList(
      pickerInput("new_dr_cols", paste0("Columns for input into ", input$dr_type, ":"), 
                  choices = colnames(select(base_dt, -Patient_ID, -`.row_id`)),
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE, size = length(symptom_list),
                                          selectAllText = "All selected", dropupAuto = TRUE)),
      if(input$dr_type == "t-SNE") {
        numericInput(inputId = "new_run_iter", label = "Number of iterations", value = 3000, min = 500, max = 3000)}, 
      actionButton(inputId = "new_run_btn", label = "Start new run")
    )
  })
  
  observeEvent(input$new_run_btn, {
    if (length(input$new_dr_cols) > 3) {
      
      inp_cols <- input$new_dr_cols
      br <- loadData() %>% select(-Patient_ID)
      br_red <- br %>% select(all_of(inp_cols))
      char_cols <- sapply(br_red, class) %>% grep("character", ., , value = TRUE) %>% names()
      br_mat <- br_red %>% 
        mutate(across(all_of(char_cols), ~ as.numeric(as.factor(.x)))) %>% 
        as.matrix() %>% unique() %>% scale() %>% unique()
      
      
      if (input$dr_type == "t-SNE") {
        n_iter <- input$new_run_iter
        m_name <- list.files("dr/tsne/") %>% strsplit(., "_") %>% unlist() %>% 
          grep("set", ., value = TRUE) %>% gsub(".Rds", "", .) %>% unique() %>% 
          gsub("set", "", .) %>% as.numeric() %>% max() %>% sum(., 1) %>% paste0("set", .)
        saveRDS(inp_cols, paste0("dr/tsne/", m_name, ".Rds"))
        
        perps <- c(10,20,30,40,50,60,70,80,90,100)
        withProgress(
          message = "Collecting t-SNE results ...", 
          detail = "Varied perplexity values", min = min(perps), max = max(perps), {
            for (p in perps) {
              incProgress(p/max(p))
              br_res <- Rtsne(br_mat, dims = 3, perplexity = p, verbose = TRUE,
                              max_iter = n_iter, normalize = TRUE, num_threads = 0)
              list(n_iter, 3, perp = p, res = br_res) %>%
                saveRDS(., paste0("dr/tsne/", m_name, "_i", n_iter, "_d3_p", p, ".Rds"))
            }
          })
        
      }else if (input$dr_type == "UMAP") {
        m_name <- list.files("dr/tsne/") %>% strsplit(., "_") %>% unlist() %>% 
          grep("set", ., value = TRUE) %>% gsub(".Rds", "", .) %>% unique() %>% 
          gsub("set", "", .) %>% as.numeric() %>% max() %>% sum(., 1) %>% paste0("set", .)
        saveRDS(inp_cols, paste0("dr/umap/", m_name, ".Rds"))
        
        withProgress(
          message = "Collecting UMAP results ...", 
          min = 0, max = 1, value = 0.5, {
            um <- umap(br_mat, n_neighbors = 100, metric = "euclidean", init = "spectral",
                       verbose = TRUE, n_components = 3, n_epochs = 600)
          })
        saveRDS(um, paste0("dr/umap/", m_name, "_nn100_d3.Rds"))
        
      }else if (input$dr_type == "PCA") {
        m_name <- list.files("dr/pca/") %>% strsplit(., "_") %>% unlist() %>% 
          grep("set", ., value = TRUE) %>% gsub(".Rds", "", .) %>% unique() %>% 
          gsub("set", "", .) %>% as.numeric() %>% max() %>% sum(., 1) %>% paste0("set", .)
        saveRDS(inp_cols, paste0("dr/pca/", m_name, ".Rds"))
        pca_res <- prcomp(br_mat)
        saveRDS(pca_res, paste0("dr/pca/", m_name, "_res.Rds"))
      }  
    }else {
      showModal(modalDialog(easyClose = TRUE, "Insufficient data selected for a run."))
    }
  })
  
  # # Tab 2: Dimensionality Reduction and Clustering -----------------------------
  tsne_cache <- reactiveVal(list())
  
  plotSavedTSNE <- reactive({
    cache_key <- paste(input$dr_columns, formatC(as.numeric(input$perp), flag = "0", width = 3), sep = "_")
    cached_tsne <- tsne_cache()
    if (is.null(cached_tsne[[cache_key]])) {
      cached_tsne[[cache_key]] <- load_tsne_embedding(input$dr_columns, input$perp)
      tsne_cache(cached_tsne)
    }
    with_color <- base_dt %>% select(.row_id, ColorBy = all_of(input$color_dr))
    cached_tsne[[cache_key]] %>% inner_join(with_color, by = ".row_id") %>% return()
  })
  
  output$DRPlot <- bindEvent(renderPlotly({
    dr_res <- data.frame()
    br_color <- pull(base_dt, input$color_dr)
    
    if (input$dr_type == "t-SNE") {
      dr_res <- plotSavedTSNE()
      
    }else if (input$dr_type == "UMAP") {
      um <- readRDS(paste0("dr/umap/", input$dr_columns, "_nn100_d3_epoch600.Rds"))
      dr_res <- um$layout %>% set_colnames(c("Dim1", "Dim2", "Dim3")) %>% 
        as.data.frame() %>% mutate(ColorBy = br_color)
      
    }else if (input$dr_type == "PCA") {
      pca_res <- paste0("dr/pca/", input$dr_columns, "_res.Rds") %>% readRDS()
      dr_res <- as.data.frame(pca_res$x) %>% select(c("PC1", "PC2", "PC3")) %>% 
        set_colnames(c("Dim1", "Dim2", "Dim3")) %>% mutate(ColorBy = br_color)
    }
    
    validate(need(nrow(dr_res) > 0, paste0("No ", input$dr_type, " points are available for the current filters.")))
    
    col_palette <- RColorBrewer::brewer.pal(n = 8, name = input$color_palette)
    cols <- colorRampPalette(col_palette)(length(unique(br_color)))
    
    plot_ly(dr_res, x = ~Dim1, y = ~Dim2, z = ~Dim3, type = "scatter3d", mode = "markers", 
            colors = cols, marker = list(size = 6), color = ~ColorBy) %>% 
      layout(legend=list(title=list(text=input$color_dr)))
    
  }), input$dr_type, input$dr_columns, input$perp, input$color_dr, 
  input$color_palette, ignoreInit = TRUE)
  
  
  
}
