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


# hc_tooltip(formatter = JS("function(){return this.y + ' years';}"))
# hc_add_event_point(event = "click")