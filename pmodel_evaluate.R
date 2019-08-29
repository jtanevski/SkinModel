# hello kitty
library(readr)
library(dplyr)
library(randomForest)
library(purrr)
library(caret)

evaluate <- function(query, model, pca = FALSE) {
  colnames(query) <- c(paste0("sig", seq(length(query) - 14)), paste0("spk", seq(14)))
  
  #use the rotation vector from the first model, as all should be identical
  if(!is.null(model[[1]]$preProcess)){
    query <- scale(query, center = model[[1]]$preProcess$mean, scale = model[[1]]$preProcess$std) %*% 
      model[[1]]$preProcess$rotation
    colnames(query) <- paste0("PC",seq(model[[1]]$preProcess$numComp))
  }

  t0 <- Sys.time()
  predictions <- model %>% map(~ predict(.x$finalModel, query, predict.all = TRUE))
  t1 <- Sys.time()
  print(t1 - t0)
  
  predictions
}

load("journal/nina_data_vsi.RData")

# transpose galore
result <- read_csv("test_nina_pca.txt", col_types = cols(), col_names = FALSE) %>%
  apply(1, function(q) evaluate(as.data.frame(t(q)), model))

i <- 1
result %>% sapply(function(r) {
  names(r) <- c("F", "Bd1", "depi", "mel", "oxy", "der", "Bd", "A", "Be", "oxy2", "amp", "pot", "amp1", "pot1")
  write.csv(t(r %>% map_dfr(~ .x$individual)), paste0("output_data_", i, ".csv"))
  i <<- i + 1
})
