library(demest)
library(tidyverse)
library(abind)

ww.raw <- read.csv("worker2014.csv", header = TRUE)
# head(ww.raw)
# summary(ww.raw)

ww.arr <- array(NA, dim = c(length(unique(ww.raw$ETA)),
                            length(unique(ww.raw$SESSO)),
                            length(unique(ww.raw$Regione))))
dimnames(ww.arr) <- list(age = sort(unique(ww.raw$ETA)),
                         sex = unique(ww.raw$SESSO),
                         region = unique(ww.raw$Regione))

for(a in 1:length(unique(ww.raw$ETA))){
  ww.raw1 <- ww.raw[ww.raw$ETA == dimnames(ww.arr)[[1]][a], c(2, 4, 5)]
  for(s in 1:length(unique(ww.raw$SESSO))){
    ww.raw2 <- ww.raw1[ww.raw1$SESSO==dimnames(ww.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(ww.raw$Regione))){
      ww.raw3 <- ww.raw2[ww.raw2$Regione == dimnames(ww.arr)[[3]][r], 1]
      ww.arr[a, s, r] <- ww.raw3
    }
  }
}

italy.workers.1Y <- round(ww.arr[ ,2:1, ])
dimnames(italy.workers.1Y) <- list(age = sort(unique(ww.raw$ETA)),
                                   sex = dimnames(italy.popn.reg)$sex,
                                   region = dimnames(italy.popn.reg)$region[-c(5, 6)])

italy.workers1 <- Counts(italy.workers.1Y) %>%
  subarray(age != "65") %>%
  collapseIntervals(dimension = "age", width = 5)

italy.workers2 <- as.array(italy.workers1)
italy.workers <- abind(italy.workers2, italy.workers.1Y[51, , ], along = 1)
dimnames(italy.workers) <- list(age = c(dimnames(italy.workers2)[[1]], "65+"),
                                sex = dimnames(italy.popn.reg)$sex,
                                region = dimnames(italy.popn.reg)$region[-c(5, 6)])

