library(shiny); library(shinyWidgets)
libs <- c("tidyverse", "dplyr", "magrittr", "readr", "ggplot2", 
          "janitor", "data.table", "DT", "rlc", "bslib", "plotly", 
          "highcharter", "Rtsne", "factoextra")
y <- suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

source("functions.R")

# ----- tSNE -------------------------------------------------------------------
set.seed(42)
# ref: https://www.geeksforgeeks.org/r-language/t-distributed-stochastic-neighbor-embedding-t-sne-using-r/
colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE")

br <- loadData()
br_red <- br %>% select(Age, Tumor_Size, Survival_Rate, Tumor_Growth_Rate, Location)
# br_red <- br %>% select(-Patient_ID, -Gender)

char_cols <- sapply(br_red, class) %>% grep("character", ., , value = TRUE) %>% names()
br_mat <- br_red %>% 
  mutate(across(all_of(char_cols), ~ as.numeric(as.factor(.x)))) %>% 
  as.matrix() %>% unique() %>% scale() %>% unique()

# iter = 5000, dims = 3, perp = 50 gives distinct clusters
n_iter <- 3000
n_dims <- 3
perplexity_values <- c(10,20,30,40,50,60,70,80,90,100)

# for (p in perplexity_values) {
#   print(paste0("Perplexity value: ", p))
#   br_res <- Rtsne(br_mat, dims = n_dims, perplexity = p, verbose = TRUE,
#                   max_iter = n_iter, normalize = TRUE, num_threads = 0)
#   list(n_iter, n_dims, perp = p, res = br_res) %>%
#     saveRDS(., paste0("tsne_out/i", n_iter, "_d", n_dims, "_p", p, ".Rds"))
# 
# }

tsne_res <- lapply(perplexity_values, function(p) {
  paste0("tsne_out/i", n_iter, "_d", n_dims, "_p", p, ".Rds") %>% readRDS()  
}) %>% set_names(., paste0("p", perplexity_values))

i <- 10
br_res <- tsne_res[[i]]$res
br_dat <- data.frame(br_res$Y)

color_column <- br %>% pull("Gender")
br_dat$ColorBy <- color_column[!duplicated(br_mat)]
colnames(br_dat) <- c("Dim1", "Dim2", "Dim3", "ColorBy")
plot_ly(br_dat, x = ~Dim1, y = ~Dim2, z = ~Dim3,
        type = "scatter3d", mode = "markers", colors = colors, 
        marker = list(size = 6), color = ~ColorBy)


set.seed(100)
br_kmeans <- kmeans(br_res$Y, centers = 3, iter.max = 3000, algorithm = "Lloyd")
fviz_cluster(br_kmeans, br_res$Y)


# br_dat$ColorBy <- findInterval(br_dat$ColorBy, c(0.5,1,1.5,2,2.5,3)) %>% as.character()
# saveRDS(br_res, paste0("stand_nogen_dims-", n_dims, "_perp-", n_perp, "_iter-", n_iter, ".Rds"))

