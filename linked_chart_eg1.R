
libs <- c("tidyverse", "dplyr", "magrittr", "readr", "ggplot2", 
          "janitor", "data.table", "DT", "rlc")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

base <- bt <- read.csv("brain_tumour/brain_tumor_dataset.csv") %>%
  as_tibble %>% arrange(-Tumor_Growth_Rate)
# DT::datatable(bt, rownames = FALSE)

pt <- bt %>% 
  select(all_of(c("Location", "Stage", "Tumor_Size"))) %>% 
  group_by(Location, Stage) %>% 
  summarise(med_size = median(Tumor_Size)) %>% 
  pivot_wider(names_from = c("Stage"), values_from = "med_size") %>%
  as.data.frame()
rownames(pt) <- pt[,"Location"]
pt <- as.matrix(pt[1:4,2:5])

# pheatmap(pt, cluster_rows = FALSE, cluster_cols = FALSE, 
#          color = hcl.colors(50, "YlOrRd"), legend = TRUE, 
#          main = "Brain Location vs Stage")# scale = "column")

# stage_x <- "I"
# loc_y <- "Frontal"
# 
# test1 <- toscatter <- bt %>% 
#   filter(Location == loc_y) %>% 
#   filter(Stage == stage_x) %>% 
#   select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology")))
# 
# ggplot(toscatter, aes(x = Tumor_Growth_Rate, y = Survival_Rate, color = Histology)) + 
#   geom_point()

# ggplot(bt, aes(x = Age, color = Family_History,
#                fill = Family_History, alpha = 1)) +
#   ylab("Density") + geom_density()


# ag <- bt %>% select(Age, Gender) %>% 
#   group_by(Gender, Age) %>% count()


toScatter <- function(bt, x, y) {
  bt %>% 
    filter(Location == y) %>% 
    filter(Stage == x) %>% 
    select(all_of(c("Tumor_Growth_Rate", "Survival_Rate", "Histology"))) %>% return()
}


# reference: 
#   https://anders-biostat.github.io/linked-charts/rlc/#jupyter-notebooks

openPage( useViewer=TRUE, layout="table1x2" )

stage_x <- "I"
loc_y <- "Frontal"
scat <- toScatter(bt, stage_x, loc_y)

lc_heatmap(
  dat(
    value = pt, 
    palette = hcl.colors(50, "YlOrRd"),
    on_click = function(k) {
      # non-local scope, assignment operator
      stage_x <<- colnames(pt)[k[1]]
      loc_y <<- rownames(pt)[k[2]]
      scat <<- toScatter(bt, stage_x, loc_y)
      updateCharts( "A2" )
    }
  ),
  place = "A1"
)


lc_scatter(
  dat(
    x = scat$Tumor_Growth_Rate,
    y = scat$Survival_Rate, 
    size = 1.2, 
    colorValue = scat$Histology,
    axisTitleX = "Tumor Growth Rate", 
    axisTitleY = "Survival Rate", 
    title = paste0("Location: ", loc_y, ", stage: ", stage_x)
  ),
  place = "A2"
)

# lc_dens(
#   dat(
#     value = bt$Age, 
#     colorValue = bt$Gender
#   ), 
#   place = "A3"
# )
