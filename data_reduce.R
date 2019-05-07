library(purrr)

outls <- targets %>% map(function(target){
  pulled <- d %>% pull(target) %>% hist(breaks = "FD", plot = FALSE)
  bottom <- pulled$breaks[which.max(pulled$counts)]
  top <- pulled$breaks[which.max(pulled$counts) + 1]
  which(d %>% pull(target) <= top & d %>% pull(target) > bottom)
})


cts <- table(unlist(outls))
to.remove <- as.numeric(names(which(cts == 12)))
