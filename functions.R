# Global variables -------------------------------------------------------------
tsne_inputs <- c("Age, tumor size, survival rate, tumor growth rate, location" = "m1", 
                 "Tumor size, survival rate, tumor growth rate, radiation treatment, surgery performed, chemotherapy, MRI result" = "m2", 
                 "Age, gender, tumor type, tumor size, location, histology, stage, symptom 1, symptom 2, symptom 3, radiation treatment, surgery performed, chemotherapy, survival rate, tumor growth rate, family history, MRI result" = "m3", 
                 "Tumor type, tumor size, location, stage, symptom 1, symptom_2, symptom 3, radiation treatment, surgery performed, chemotherapy, survival rate, tumor growth rate, MRI result, follow up required" = "m4")

# UI functions -----------------------------------------------------------------
chkBoxGroupBin <- function(id, lbl) {
  checkboxGroupInput(inputId = id, label = lbl, inline = TRUE, 
                     choices = c("Yes","No"), selected = c("Yes","No"))  
}

radGrpBtns <- function(id, lbl, chc = c("Yes", "No", "Both")) {
  radioGroupButtons(inputId = id, label = lbl, size = "sm", direction = "horizontal",
                    choices = chc, selected = last(chc))
}

# ------------------------------------------------------------------------------
loadData <- function() {
  read.csv("brain_tumor_dataset.csv") %>% as_tibble %>% arrange(-Tumor_Growth_Rate)
}

colOpts <- function(df, colx) {
  df %>% select(all_of(colx)) %>% pull() %>% unique() %>% 
    sort(., decreasing = TRUE)
}

checkYNBoth <- function(base_tbl, inp, colx, true_v = "Yes", false_v = "No") {
  if (inp == "Yes") {
    base_tbl <- filter(base_tbl, .data[[colx]] == true_v)
  }else if (inp == "No") {
    base_tbl <- filter(base_tbl, .data[[colx]] == false_v)
  }
  # otherwise no change: "Both" and other implies do not filter, use all data
  return(base_tbl)
}

filterByGen <- function(df, ag) {
  if (ag$Male & !ag$Female) {
    df %<>% filter(Gender == "Male")
  }else if (!ag$Male & ag$Female) {
    df %<>% filter(Gender == "Female")
  }
  return(df)
}


# DENSITY PLOT FUNCTIONS -------------------------------------------------------
get_age_profile <- function(data, gender_label, age_value) {
  age_subset <- data %>% filter(Gender == gender_label, Age == age_value)
  
  symptom_vals <- age_subset %>%
    select(Symptom_1, Symptom_2, Symptom_3) %>%
    unlist(use.names = FALSE)
  symptom_vals <- symptom_vals[!is.na(symptom_vals) & nzchar(symptom_vals)]
  
  top_symptom <- if (length(symptom_vals) > 0) {
    names(sort(table(symptom_vals), decreasing = TRUE))[1]
  } else {
    "No symptom data"
  }
  
  treatment_counts <- c(
    Radiation = sum(age_subset$Radiation_Treatment == "Yes", na.rm = TRUE),
    Surgery = sum(age_subset$Surgery_Performed == "Yes", na.rm = TRUE),
    Chemotherapy = sum(age_subset$Chemotherapy == "Yes", na.rm = TRUE)
  )
  
  top_treatment <- if (length(age_subset$Age) > 0 && max(treatment_counts) > 0) {
    names(treatment_counts)[which.max(treatment_counts)]
  } else {
    "No treatment data"
  }
  
  list(symptom = top_symptom, treatment = top_treatment)
}



build_density_data <- function(age_values, gender_label, base_data) {
  if (length(age_values) < 2) {
    return(NULL)
  }
  
  density_data <- density(age_values)
  age_counts <- table(age_values)
  density_df <- data.frame(
    x = density_data$x,
    y = density_data$y,
    age_rounded = round(density_data$x),
    count = sapply(round(density_data$x), function(a) {
      if (as.character(a) %in% names(age_counts)) as.integer(age_counts[[as.character(a)]]) else 0
    })
  )
  
  mapply(function(x, y, age_r, cnt) {
    age_profile <- get_age_profile(base_data, gender_label, age_r)
    list(
      x = x,
      y = y,
      age_rounded = age_r,
      count = cnt,
      top_symptom = age_profile$symptom,
      top_treatment = age_profile$treatment
    )
  }, density_df$x, density_df$y, density_df$age_rounded, density_df$count, SIMPLIFY = FALSE)
}



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
  hc_title(text = "Population density by age, grouped by gender") %>%
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
          Shiny.onInputChange('legendClick', this.name);
          return false; // Prevent default toggle behavior
        }")
    )
  ))