library(readr)
library(caret)
library(randomForest)
library(purrr)
library(corrplot)
library(dplyr)

set.seed(42)

datapath <- commandArgs(TRUE)[1]

d <- read_csv(paste0(datapath, ".txt"), col_types = cols())[, -1]
folds <- createFolds(seq(nrow(d)), k = 10)
targets <- colnames(d)[1:14]
features <- paste(colnames(d)[15:ncol(d)], collapse = "+")

outls <- targets %>% map(function(target){
  pulled <- d %>% pull(target) %>% hist(breaks = "FD", plot = FALSE)
  bottom <- pulled$breaks[which.max(pulled$counts)]
  top <- pulled$breaks[which.max(pulled$counts) + 1]
  which(d %>% pull(target) <= top & d %>% pull(target) > bottom)
})


cts <- table(unlist(outls))
to.remove <- as.numeric(names(which(cts > 10)))

#par(mfrow = c(3,5))
#targets %>% walk(function(target){
#  pulled <- d %>% slice(-to.remove) %>% pull(target) %>%  hist(breaks = "FD", plot = FALSE)
#  plot(pulled, main = "")
#})

#filter out most frequent parameter values
d <- d %>% slice(-to.remove)



# pdf(file = paste0(datapath, "_dist.pdf"), width = 10.5, height = 8.5)
# par(mfrow = c(3, 5))
# seq(14) %>% walk(~ plot(density(t(d[, .x])), main = targets[.x]))
# corrplot(cor(d[, 1:14]), type = "upper", diag = F)
# dev.off()
# 
# model <- targets %>% map(~ randomForest(as.formula(paste0(.x, "~", features)),
#   data = d, ntree = 100, importance = T
# ))
# 
# save(model, file = paste0(datapath, ".RData"))


#
defaulteval <- folds %>% map(function(fold) {
  print("Folding")
  targets %>%
    map(~ mean(t(d[-fold, .x]))) %>%
    imap_dbl(~ sqrt(sum((.x - d[fold, .y])^2) / length(fold)))
})

defaultstats <- defaulteval %>% map_dfr(~.x)

rfeval <- folds %>% map(function(fold) {
  print("Folding")
  predictions <- targets %>%
    map(~ randomForest(as.formula(paste0(.x, "~", features)), data = d[-fold, ], ntree = 100)) %>%
    map(~ predict(.x, d[fold, 15:ncol(d)]))
  write_csv(cbind(d[fold, 1:14], predictions), path = paste0(datapath, "_predictions.csv"), append = TRUE, col_names = FALSE)
  predictions %>% imap_dbl(~ sqrt(sum((.x - d[fold, .y])^2) / length(fold)))
})

rfstats <- rfeval %>% map_dfr(~.x)

tmean <- folds %>% map_dfr(function(fold) {
  targets %>% map_dbl(~ mean(t(d[-fold, .x])))
})

nrmse <- rfstats / tmean

rrmse <- rfstats / defaultstats

grmse <- rfstats %>% colMeans()
gnrmse <- nrmse %>% colMeans()
grrmse <- rrmse %>% colMeans()

write("RMSE", file = paste0(datapath, "_perf.txt"), append = TRUE)
write.table(t(rfstats %>% rowMeans()), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(apply(rfstats, 1, sd)), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(c(mean(grmse), sd(grmse))), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)

write("NRMSE", file = paste0(datapath, "_perf.txt"), append = T)
write.table(t(nrmse %>% rowMeans()), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(apply(nrmse, 1, sd)), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(c(mean(gnrmse), sd(gnrmse))), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)

write("RRMSE", file = paste0(datapath, "_perf.txt"), append = T)
write.table(t(rrmse %>% rowMeans()), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(apply(rrmse, 1, sd)), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(t(c(mean(grrmse), sd(grrmse))), file = paste0(datapath, "_perf.txt"), col.names = FALSE, row.names = FALSE, append = TRUE)


# imps <- model %>% map_dfc(~importance(.x)[,2])

# imps %>% apply(2,order,decreasing=T) %>% rowMeans()

# imps %>% apply(2,function(p){
#   (p-min(p))/(max(p)-min(p))
# })

# heatmap(imps)

# p <- read_csv("data2_predictions.csv", col_names = FALSE)
#
# seq(14) %>% walk(function(.x){
#   pdf(file = paste0("set1_2/",targets[.x],".pdf"),width=8,height=5)
#   #smoothScatter(t(p[,.x]),t(p[,.x+14]),xlab="True value", ylab="Predicted value",main=targets[.x], nrpoints=0, colramp=colorRampPalette(c("white","grey10")))
#   smoothScatter(t(p[,.x]),t(p[,.x+14]),xlab="True value", ylab="Predicted value",main=targets[.x], pch='.', nrpoints=0, colramp=colorRampPalette(c("white","lightyellow", "darkseagreen1","royalblue")))
#   #samp <- sample(nrow(p),1000)
#   points(t(p[,.x]),t(p[,.x+14]),pch='.')
#   #plot(t(p[,.x]),t(p[,.x+14]),xlab="True value",ylab="Predicted value",main=targets[.x],pch='.')
#   dev.off()
#
# }
# )
