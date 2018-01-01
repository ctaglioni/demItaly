#-----------------------
# NZ_electoral
#-----------------------

library(stringr)
library(dplyr)

setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw\\NZ_electoral")
files<- list.files(full.names = TRUE)
electoral <- lapply(files, read.csv)
names(electoral)<- gsub(".*_*_(.*).csv", "\\1", names)
names(electoral)[[65]] <- "EntireCountry"

select<- function(data){
  data[,c("Est.Eligible.Population", "Total.Enrolled")]
}

elec_data<- lapply(electoral, select)

dimn <- list(age = electoral[[1]]$Age,
              pop = c("Population", "Enrolled"),
             region=names(electoral))
library(abind)
enrolled<- array(NA, dim=c(12,2,65), dimnames = dimn)
  for(i in 1:65){
 enrolled[,,i]<-abind(elec_data[[i]])
  }

clean_numbers <- function(x) str_replace_all(x, "[,]","")

nz.enrolled.reg <- array(NA, dim=c(12,2,65), dimnames = dimn)
for(i in 1:dim(enrolled)[3]){
  nz.enrolled.reg[,,i] <- as.numeric(clean_numbers(enrolled[,,i]))
}

