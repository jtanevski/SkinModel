#hello kitty
library(readr)
library(randomForest)
library(purrr)

evaluate <- function(query,model) {
  colnames(query) <- c(paste0("sig",seq(lengrth(query)-14)), paste0("spk",seq(14)))
  
  t0<-Sys.time()
  predictions <- model %>% map(~predict(.x,query,predict.all=TRUE))
  t1<-Sys.time()
  print(t1-t0)
  predictions
}

load("path_to_model.RData")

#transpose galore 
result <- read_csv("input_data.csv",col_types = cols(), col_names = FALSE) %>% 
  apply(1,function(q) evaluate(as.data.frame(t(q)),model))

i <- 1
result %>% sapply(function(r) {
  names(r) <- c("F","Bd1","depi","mel","oxy","der","Bd","A","Be","oxy2","amp","pot","amp1","pot1")
  write.csv(t(r %>% map_dfr(~.x$individual)),paste0("output_data_",i,".csv"))
  i <<- i+1
})





