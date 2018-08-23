setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw\\Istat")

library(tidyverse)
library(demest)

cens.raw <- read.csv("Census2011.csv")
# head(cens.raw)
# summary(cens.raw)
# colnames(cens.raw)
unique(cens.raw$Età)
# unique(cens.raw$Età.per.anno.di.nascita.e.anni.compiuti)
# unique(cens.raw$Tipo.di.edificio)
# unique(cens.raw$Anno.di.Censimento)
# unique(cens.raw$Territorio)
# unique(cens.raw$Tipo.dato)
# unique(cens.raw$Flags)

anni <- c("fino a 4 anni", "5-9 anni", "10-14 anni", "15-19 anni",
          "20-24 anni", "25-29 anni", "30-34 anni", "35-39 anni",
          "40-44 anni", "45-49 anni", "50-54 anni", "55-59 anni",
          "60-64 anni", "65-69 anni", "70-74 anni", "75-79 anni",
          "80-84 anni", "85-89 anni","90-94 anni", "95-99 anni",
          "100 anni e più")

cens.proc  <- cens.raw %>%
  filter(Stato.civile == "totale") %>%
  select(Età, Territorio, Sesso,X0) %>%
  filter(Età %in% anni)%>%
  filter(Sesso %in% c("maschi", "femmine"))

cens.proc$Età <- gsub("[^0-9\\-]", "", cens.proc$Età)
cens.proc$Età[cens.proc$Età=="4"] <- "0-4"
unique(cens.proc$Età)
age <- c("0-4","5-9","10-14",
         "15-19","20-24","25-29",
         "30-34","35-39","40-44",
         "45-49","50-54", "55-59",
         "60-64","65-69","70-74","75-79",
         "80-84","85-89","90-94","95-99","100")
summary(cens.proc)


cens.proc.arr <- array(NA, dim = c(length(unique(cens.proc$Età )),
                                length(unique(cens.proc$Territorio)),
                                length(unique(cens.proc$Sesso))))
dimnames(cens.proc.arr) <- list(age = age,
                             region = unique(cens.proc$Territorio),
                             sex = unique(cens.proc$Sesso))


for(a in 1:length(unique(age)) ){
  cens.proc1 <- cens.proc[cens.proc$Età == dimnames(cens.proc.arr)[[1]][a], c(2, 3, 4)]
  for(s in 1:length(unique(cens.proc$Territorio)) ){
    cens.proc2 <- cens.proc1[cens.proc1$Territorio==dimnames(cens.proc.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(cens.proc$Sesso)) ){
      cens.proc3 <- cens.proc2[cens.proc2$Sesso == dimnames(cens.proc.arr)[[3]][r], 2]
      cens.proc.arr[a, s, r] <- cens.proc3
    }
  }
}
cens.proc.arr1 <- aperm(cens.proc.arr, c(1,3,2))
dimnames(cens.proc.arr1)[[3]]
library(demItaly)
cens.proc.arr2 <- cens.proc.arr1[,,c(1,4,2,10,5,6,7,8,3,9,11,12,13,15,14,16,17,18,19,20,21,22)]
dimnames(cens.proc.arr2)[[3]]<-dimnames(italy.popn.reg)[[3]]
dimnames(cens.proc.arr2)[[2]] <-dimnames(italy.popn.reg)[[2]]
italy.popn.census <- cens.proc.arr2


