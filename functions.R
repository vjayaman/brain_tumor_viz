# Global variables -------------------------------------------------------------
tsne_inputs <- c("Age, tumor size, survival rate, tumor growth rate, location" = "m1", 
                 "Tumor size, survival rate, tumor growth rate, radiation treatment, surgery performed, chemotherapy, MRI result" = "m2", 
                 "Age, gender, tumor type, tumor size, location, histology, stage, symptom 1, symptom 2, symptom 3, radiation treatment, surgery performed, chemotherapy, survival rate, tumor growth rate, family history, MRI result" = "m3", 
                 "Tumor type, tumor size, location, stage, symptom 1, symptom_2, symptom 3, radiation treatment, surgery performed, chemotherapy, survival rate, tumor growth rate, MRI result, follow up required" = "m4")

# UI functions -----------------------------------------------------------------
chkBoxGroupBin <- function(id, lbl) {
  checkboxGroupInput(inputId = id, label = lbl, inline = TRUE, choices = c("Yes","No"), selected = c("Yes","No"))  
}

# ------------------------------------------------------------------------------
toScatter <- function(base, x, y) {
  base %>% 
    filter(Location == y) %>% 
    filter(Stage == x) %>% 
    select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology"))) %>% return()
}

loadData <- function() {
  read.csv("brain_tumor_dataset.csv") %>%
    as_tibble %>% arrange(-Tumor_Growth_Rate)
}

colOpts <- function(df, colx) {
  df %>% select(all_of(colx)) %>% pull() %>% unique() %>% sort(., decreasing = TRUE)
}

checkBinary <- function(base_tbl, inp, colx) {
  if (("Yes" %in% inp) & !("No" %in% inp)) {
    base_tbl <- filter(base_tbl, .data[[colx]] == "Yes")
  }else if (("No" %in% inp) & !("Yes" %in% inp)) {
    base_tbl <- filter(base_tbl, .data[[colx]] == "Yes")
  }
#   otherwise no change
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

# hc_tooltip(formatter = JS("function(){return this.y + ' years';}"))
# hc_add_event_point(event = "click")