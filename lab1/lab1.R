setwd("C:\\Proteomics")

#Задание 1

fix_column <- function(column){
  fixed_column <- sub(' ', '', column)
  fixed_column <- as.numeric(fixed_column)
  if(any(is.na(fixed_column))){
    return(column)
  }else{
  return(fixed_column)
  }
}

fix_data <- function(df){
  fixed_df <- lapply(df, fix_column)
  return(data.frame(fixed_df))
}

df <- read.csv(file = "data\\lab1_e1.csv")
fixed_df <- fix_data(df)
print(fixed_df)

#Задание 2

get_id <- function(dfs){
  new_df <- Reduce(function(x, y) merge(x, y, by='id'), dfs)
  result <- data.frame(id=new_df$id, mean_temp=rowMeans(new_df[, -1]))
  return(result)
}

load("data\\lab1_e2.Rdata")
result_table <- get_id(all_data)
print(result_table)