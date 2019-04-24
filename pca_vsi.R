library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(purrr)

set.seed(42)

datapath <- "eu_conference/nina_data_vsi"

d <- read_csv(paste0(datapath,".txt"))

 pcs <- d %>% select(-seq(15)) %>% prcomp(center = TRUE, scale. = TRUE)
# # 
 pct <- 1
#

#otherwise fix the number of components
components <- min(which(summary(pcs)$importance[3, ] >= pct))
#
# # the first 11 components explain the entire variance of the data
transformed.data <- d %>% select(2:15) %>% bind_cols(as_tibble(pcs$x) %>%
   select(1:components))

targets <- colnames(d)[2:15]
 
features <- paste0("PC", seq(components), collapse = "+")

#
model <- targets %>% map(~ randomForest(as.formula(paste0(.x, "~", features)),
  data = transformed.data,
  ntree = 100, importance = T
))

save(model, pcs, file = paste0(datapath,"_pcamodel.RData"))

folds <- createFolds(seq(nrow(d)), k = 10)

defaulteval <- folds %>% map(function(fold) {
  print("Folding")
  targets %>%
    map(~ mean(t(d[-fold, .x]))) %>%
    imap_dbl(~ sqrt(sum((.x - d[fold, .y])^2) / length(fold)))
})

defaultstats <- defaulteval %>% map_dfr(~.x)
print("Done")

rfeval <- folds %>% map(function(fold) {
  print("Folding")

  train_data <- d %>%
    select(-seq(15)) %>%
    slice(-fold) %>%
    prcomp(center = TRUE, scale. = TRUE)
  
  components <- min(which(summary(train_data)$importance[3, ] >= pct))
  
  #components <- 40
  
  pca_fold <- d %>%
    select(2:15) %>%
    slice(-fold) %>%
    bind_cols(as_tibble(train_data$x) %>% select(1:components))

  test_data <- d %>%
    select(-seq(15)) %>%
    slice(fold)
    
  test_data <- as_tibble(scale(test_data, center = train_data$center, scale = train_data$scale) %*% train_data$rotation) %>% 
    select(1:components)
  
  features <- paste0("PC", seq(components), collapse = "+")

  predictions <- targets %>%
    map(~ randomForest(as.formula(paste0(.x, "~", features)), data = pca_fold, ntree = 100)) %>%
    map(~ predict(.x, test_data))
  
  write_csv(cbind(d[fold, 2:15], predictions), path = paste0(datapath,"_predictions.csv"), append = TRUE, col_names = FALSE)
  predictions %>% imap_dbl(~ sqrt(sum((.x - d[fold, .y+1])^2) / length(fold)))
})

rfstats <- rfeval %>% map_dfr(~.x)

tmean <- folds %>% map_dfr(function(fold){
  targets %>% map_dbl(~mean(t(d[-fold,.x])))  
})

nrmse <- rfstats/tmean

rrmse <- rfstats/defaultstats

grmse <- rfstats %>% colMeans
gnrmse <- nrmse %>% colMeans
grrmse <- rrmse %>% colMeans



write("RMSE",file=paste0(datapath,"_perf.txt"),append=TRUE)
write.table(t(rfstats %>% rowMeans),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(apply(rfstats,1,sd)),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(c(mean(grmse),sd(grmse))),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)

write("NRMSE",file=paste0(datapath,"_perf.txt"),append=T)
write.table(t(nrmse %>% rowMeans),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(apply(nrmse,1,sd)),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(c(mean(gnrmse),sd(gnrmse))),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)

write("RRMSE",file=paste0(datapath,"_perf.txt"),append=T)
write.table(t(rrmse %>% rowMeans),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(apply(rrmse,1,sd)),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)
write.table(t(c(mean(grrmse),sd(grrmse))),file = paste0(datapath,"_perf.txt"), col.names =  FALSE, row.names = FALSE, append=TRUE)

