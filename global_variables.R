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
  seq = brewer.pal.info %>% filter(category == "seq") %>% rownames(), 
  qual = brewer.pal.info %>% filter(category == "qual") %>% rownames())

cnames <- c(setNames(col_list$seq, paste(col_list$seq, "(Seq)")), 
            setNames(col_list$qual, paste(col_list$qual, "(Qual)")))

col_encoding <- colnames(base_dt)[-ncol(base_dt)][-1] %>% setNames(., nm = gsub("_", " ", .))