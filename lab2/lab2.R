library("readxl")
library(plyr)
library(readr)
library(dplyr)

setwd("C:\\Proteomics\\secondLab\\data")

#Задание 13

df <- read_excel("air_flows.xlsx")

myFunc1 <- function(data){
  
  df <- df[-c(1:4), ]
  colnames(df) <- c(df[1, "2.33 ПЕРЕВОЗКИ ПАССАЖИРОВ МЕЖДУ ОТДЕЛЬНЫМИ ПАРАМИ ГОРОДОВ ВО ВНУТРЕННЕМ СООБЩЕНИИ"],df[1,"...2"],df[1,"...3"],df[1,"...4"],df[1,"...5"])
  df <- df[-c(1), ]
  x=df$Город2[df$Город1 %in% c(data)]
  
  df <- transform(df,`2000` = as.numeric(`2000`))
  
  cities <- distinct(df,Город2)
  citiesSum <- list(c(0))
  
  for(i in 1:nrow(cities)){
    citiesSum[[i]] <- c(sum(df$`X2000`[df$Город2 %in% c(cities[i,"Город2"])])+sum(df$`X2005`[df$Город2 %in% c(cities[i,"Город2"])])+sum(df$`X2006`[df$Город2 %in% c(cities[i,"Город2"])]))
  }
  
  cities$count <- c(citiesSum)
  
  print(x)
  print(cities)
}

#Задание 26

df <- read_csv(file = "Payment_and_Value_of_Care-Hospital.csv")

myFunc <- function(data){
  
  df <- distinct(df,`Facility ID`, City, State, `County Name`)
  
  x=df$`Facility ID`[df$City %in% c(data)]
  if (!length(x)){
    x=df$`Facility ID`[df$State %in% c(data)]
    if (!length(x)){
      x=df$`Facility ID`[df$`County Name` %in% c(data)]
    }
  }
  if(length(x)==0){
    return ("Incorrect input")
  }
  return(length(x))
}

