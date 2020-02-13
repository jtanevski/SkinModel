library(readr)
library(caret)
library(randomForest)
library(purrr)
library(furrr)
library(corrplot)
library(dplyr)
library(ggplot2)

set.seed(42)
plan(multiprocess, workers = 5)

## Example
# datapath <- commandArgs(TRUE)[1]
# train_model(datapath)
#
# load(paste0(datapath, ".RData"))
# model_stats(model)


train_model <- function(datapath) {
  d <- read_csv(paste0(datapath, ".txt"), col_types = cols())[, -1]

  targets <- colnames(d)[1:14]
  features <- paste(colnames(d)[15:ncol(d)], collapse = "+")


  # trim data
  outls <- targets %>% map(function(target) {
    pulled <- d %>%
      pull(target) %>%
      hist(breaks = "FD", plot = FALSE)
    bottom <- pulled$breaks[which.max(pulled$counts)]
    top <- pulled$breaks[which.max(pulled$counts) + 1]
    which(d %>% pull(target) <= top & d %>% pull(target) > bottom)
  })


  cts <- table(unlist(outls))
  to.remove <- as.numeric(names(which(cts > 10)))

  # plot distributions and correlation
  targets %>% walk(function(target) {
    pulled <- d %>%
      slice(-to.remove) %>%
      pull(target)

    histg <- ggplot(data.frame(pulled), aes(x = pulled)) +
      geom_histogram(binwidth = 2 * IQR(pulled) / length(pulled)^(1 / 3), color = "black", fill = "white") +
      xlab(target) +
      ylab("Count") +
      xlim(c(0, tail(pretty(pulled), 1))) +
      theme_classic(base_size = 18)

    ggsave(file = paste0(datapath, "_dist_", target, ".pdf"), plot = histg, width = 8, height = 6)
  })
  pdf(file = paste0(datapath, "_corrplot.pdf"), width = 8, height = 6, pointsize = 18)
  corrplot(cor(d %>% slice(-to.remove) %>% select(targets)), type = "upper", diag = F)
  dev.off()

  # filter out most frequent parameter values
  d <- d %>% slice(-to.remove)

  # folds <- createFolds(seq(nrow(d)), k = 10)

  flength <- ncol(d) - length(targets)
  # fit model parameters
  if (flength < 200) {
    mtries <- data.frame(mtry = c(
      1, floor(c(0.1, 0.25, 0.5, 1) * flength),
      floor(sqrt(flength))
    ))
    model <- targets %>% future_map(function(target) {
      train(as.formula(paste0(target, "~", features)), d,
        method = "rf",
        ntree = 100, metric = "RMSE", tuneGrid = mtries,
        trControl = trainControl(
          method = "cv", number = 10,
          savePredictions = "final"
        )
      )
    })
  } else {
    # PCA
    model <- targets %>% future_map(function(target) {
      flength <- 11
      mtries <- data.frame(mtry = c(
        1, floor(c(0.1, 0.25, 0.5, 1) * flength),
        floor(sqrt(flength))
      ))
      train(as.formula(paste0(target, "~", features)), d,
        method = "rf",
        ntree = 100, metric = "RMSE", tuneGrid = mtries,
        trControl = trainControl(
          method = "cv", number = 10,
          savePredictions = "final",
          preProcOptions = list(pcaComp = 11)
        ),
        preProcess = "pca"
      )
    })
  }

  save(model, file = paste0(datapath, ".RData"))
  return(model)
}


model_stats <- function(model) {
  model %>% map_dfr(function(m) {
    stats <- unique(m$pred$Resample) %>% map_dfr(function(fold) {
      fold.idx <- m$pred %>%
        filter(Resample == fold) %>%
        pull(rowIndex)

      rmse <- m$pred %>%
        group_by(Resample) %>%
        summarise(rmse = RMSE(pred, obs)) %>%
        pull(rmse)

      means <- m$trainingData %>%
        slice(-fold.idx) %>%
        pull(.outcome) %>%
        mean()

      default.stats <- (means - m$trainingData %>% slice(fold.idx) %>% pull(.outcome))^2 %>%
        mean() %>%
        sqrt()

      rho <- m$pred %>%
        group_by(Resample) %>%
        summarise(rho = cor(pred, obs)) %>%
        pull(rho)

      tibble(rmse = rmse, nrmse = rmse / means, rrmse = rmse / default.stats, rho = rho)
    })

    tibble(
      parameter = deparse(m$terms[[2]]),
      rmse.mean = mean(stats$rmse), rmse.sd = sd(stats$rmse),
      nrmse.mean = mean(stats$nrmse), nrmse.sd = sd(stats$nrmse),
      rrmse.mean = mean(stats$rrmse), rrmse.sd = sd(stats$rrmse),
      rho.mean = mean(stats$rho), rho.sd = sd(stats$rho)
    )
  })
}


prediction_scatter_plots <- function(model, folder) {
  p <- model %>%
    map_dfc(~ tibble(!!.x$terms[[2]] := .x$pred$obs)) %>%
    cbind(model %>% map_dfc(~ tibble(!!.x$terms[[2]] := .x$pred$pred)))

  seq(14) %>% walk(function(.x) {
    pdf(file = paste0(folder, colnames(p)[.x], ".pdf"), width = 5, height = 5.5, pointsize = 18)
    cmax <- max(c(t(p[, .x]), t(p[, .x + 14])))
    tval <- t(p[, .x])
    pval <- t(p[, .x + 14])
    smoothScatter(tval, pval,
      xlab = "True value", ylab = "Predicted value",
      xlim = c(0, cmax), ylim = c(0, cmax),
      main = colnames(p)[.x], pch = ".", nrpoints = 0,
      colramp = colorRampPalette(c("white", "lightyellow", "darkseagreen1", "royalblue"))
    )
    points(tval, pval, pch = ".")
    dev.off()
  })
}

optimal_number_features <- function(model) {
  print(table(model %>% map_dbl(~ .x$bestTune[1, 1])))
  print(table(model %>% map_dbl(~ .x$bestTune[1, 1] / length(.x$coefnames))))
}
