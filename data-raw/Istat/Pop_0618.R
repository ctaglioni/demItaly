setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw\\Istat")

library(tidyverse)
library(demest)

pop0211raw <- read.csv("Pop_res_ricostr0211.csv")

summary(pop0211raw)
pop0211 <- pop0211raw %>%
  select(- 1 ,- TIPO_DATO15 , -Tipo.dato, -Classe.di.etÃ., - SEXISTAT1, -CITTADINANZA,
         -Seleziona.periodo, -Flag.Codes, - Flags) %>%
  filter(Cittadinanza=="totale") %>%
  select(-Cittadinanza)%>%
  filter(Sesso!= "totale") %>%
  filter(TIME %in% c(2006:2011)) %>%
  filter(ETA1 != "TOTAL")

pop0211$ETA1 <- as.numeric(gsub("[^0-9\\-]", "", pop0211$ETA1))
unique(pop0211$ETA1)

pop0211.arr <- array(NA, dim = c(length(unique(pop0211$ETA1 )),
                                 length(unique(pop0211$Sesso)),
                                   length(unique(pop0211$Territorio)),
                                 length(unique(pop0211$TIME))))

age <- 0:100
dimnames(pop0211.arr) <- list(age = age,
                              sex = unique(pop0211$Sesso),
                              region = unique(pop0211$Territorio),
                              time = unique(pop0211$TIME))


for(a in 1:length(unique(age)) ){
  pop02111 <- pop0211[pop0211$ETA1 == dimnames(pop0211.arr)[[1]][a], c(1, 3, 4, 5)]
  for(s in 1:length(unique(pop0211$Sesso)) ){
    pop02112 <- pop02111[pop02111$Sesso==dimnames(pop0211.arr)[[2]][s], c(1,3,4)]
    for(r in 1:length(unique(pop0211$Territorio)) ){
      pop02113 <- pop02112[pop02112$Territorio == dimnames(pop0211.arr)[[3]][r], 2:3]
      for(y in 1:length(unique(pop0211$TIME)) ){
        pop02114 <- pop02113[pop02113$TIME == dimnames(pop0211.arr)[[4]][y], 2]
        pop0211.arr[a, s, r, y] <- pop02114
    }
  }
}
}

dimnames(pop0211.arr)[[3]]
dimnames(italy.births.reg)[[1]]
pop0211.arr1 <- pop0211.arr[,,c(1,2,4,5,6,7,8,9,3,10:22),]
dimnames(pop0211.arr1)[[3]] <- dimnames(italy.births.reg)[[1]]
dimnames(pop0211.arr1)[[2]] <- dimnames(italy.popn.reg)[[2]]

pop0211ok <- as.array(Counts(pop0211.arr1[,,,], dimscales = c(time="Points", age ="Intervals"))%>%
  collapseIntervals("age", breaks=c(seq(0,100,by=5))))

###--------------------------------------

pop1218raw <- read.csv("Pop_res_1218.csv")

summary(pop1218raw)

pop1218 <- pop1218raw %>%
  select(-1, -TIPO_DATO15, -Tipo.di.indicatore.demografico, -SEXISTAT1,
         - EtÃ., -STATCIV2, -Seleziona.periodo, -Flag.Codes, -Flags)%>%
  filter(Stato.civile == "totale") %>%
  select(- Stato.civile) %>%
  filter( Sesso != "totale") %>%
  filter(ETA1 !="TOTAL")

pop1218$ETA1 <- as.numeric(gsub("[^0-9\\-]", "", pop1218$ETA1))
unique(pop1218$ETA1)

pop1218.arr <- array(NA, dim = c(length(unique(pop1218$ETA1 )),
                                 length(unique(pop1218$Sesso)),
                                 length(unique(pop1218$Territorio)),
                                 length(unique(pop1218$TIME))))

age <- 0:100
dimnames(pop1218.arr) <- list(age = age,
                              sex = unique(pop1218$Sesso),
                              region = unique(pop1218$Territorio),
                              time = unique(pop1218$TIME))


for(a in 1:length(unique(age)) ){
  pop12181 <- pop1218[pop1218$ETA1 == dimnames(pop1218.arr)[[1]][a], c(1, 2, 4, 5)]
  for(s in 1:length(unique(pop1218$Sesso)) ){
    pop12182 <- pop12181[pop12181$Sesso==dimnames(pop1218.arr)[[2]][s], c(1,3,4)]
    for(r in 1:length(unique(pop1218$Territorio)) ){
      pop12183 <- pop12182[pop12182$Territorio == dimnames(pop1218.arr)[[3]][r], 2:3]
      for(y in 1:length(unique(pop1218$TIME)) ){
        pop12184 <- pop12183[pop12183$TIME == dimnames(pop1218.arr)[[4]][y], 2]
        pop1218.arr[a, s, r, y] <- pop12184
      }
    }
  }
}

dimnames(pop1218.arr1)[[3]]
dimnames(italy.popn.reg)[[3]]
pop1218.arr1 <- pop1218.arr[,,c(1,2,4,5,6,7,8,9,3,10:22),]
dimnames(pop1218.arr1)[[3]] <- dimnames(italy.popn.reg)[[3]]
dimnames(pop1218.arr1)[[2]] <- dimnames(italy.popn.reg)[[2]]

pop1218ok <- as.array(Counts(pop1218.arr1[,,,], dimscales = c(time="Points", age ="Intervals"))%>%
                        collapseIntervals("age", breaks=c(seq(0,100,by=5))))

library(abind)
pop0618 <-abind(pop0211ok, pop1218ok, along=4)
dimnames(pop0618) <- list(age = dimnames(pop0618)[[1]],
                          sex = dimnames(pop0618)[[2]],
                          region = dimnames(pop0618)[[3]],
                          time = dimnames(pop0618)[[4]])

italy.popn.rev <- pop0618
