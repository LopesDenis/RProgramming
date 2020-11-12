

pollutantmean <- function( directory="specdata", pollutant="sulfate", id = 1:332){
  # 'directory' is a character vector
  
  # Descrition of values
  #   Directory Name
  #   Pollutant, name of variable
  #   id of file to load
  
  
  # initial value 
  data_temp_1 = data.frame()
  

  # join data 
    for (n_id in id) {
      local = paste(c("./",directory,"/",sprintf("%03d",n_id),".csv"), collapse = "")
      data_temp_1  <- rbind(data_temp_1, read.csv(local,sep = ","),all = FALSE)
    }
  
  #sub_set data
  data_temp_2 = data_temp_1[pollutant]
  
  #mean withOut NA
  mean(data_temp_2[!is.na(data_temp_2)])

  
  
  
  #test 
  # id = 3:6
  # n_id = 4
  # teste = vector( )
  # 
  #mean(data1_sub[!is.na(data1_sub)])
  
  
  
} #polluntantmean



###################################################################

complete <- function( directory="specdata", id = 1:332){
  # complete function return a data frame with if and complete case
  #     of each file
  
  mylist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  
  # initial value 
  nobs <- numeric()
  
 
  # calculate data
  for (n_id in id) {
    data_temp_2 <-read.csv(mylist[n_id])
    mySum  <-  sum(complete.cases(data_temp_2))
    nobs <- c(nobs,mySum)

   }
  data.frame(id, nobs)
  
  
} #complete




###################################################################

corr <- function( directory="specdata", threshold = 0){
  # complete function return a data frame with if and complete case
  #     of each file
  
  mylist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  df <- complete(directory)
  
  ids <- df[df["nobs"]>threshold , ]$id
  
  # initial value 
  corrr <- numeric()

  # calculate data
  for (n_id in ids) {
    mydata <- read.csv(mylist[n_id])
    dff <- mydata[complete.cases(mydata),]
    corrr <- c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  
  return(corrr)  
  

} #corr





###################################################################

complete_errado <- function( directory="specdata", id = 1:332){
  # complete function return a data frame with if and complete case
  #     of each file
  
  
  
  # initial value 
  data_temp_1 <-  data.frame() # data.frame(matrix(ncol = 2, nrow = 0))
  
  
  
  # calculate data
  for (n_id in id) {
    local = paste(c("./",directory,"/",sprintf("%03d",n_id),".csv"), collapse = "")
    
    data_temp_2 <-read.csv(local,sep = ",")
    
    #bind data (id, n complete case)
    data_temp_1  <- rbind(data_temp_1, c(n_id,sum(complete.cases(data_temp_2))))
  }
  
  names(data_temp_1) = c("id", "nobs")
  
  #return
  data_temp_1
  
  
  ####  test    #### 
  
  
  
} #complete



#############################################
# Programming Assignment 3

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

#denis
names(outcome)

hist(outcome[,11]) 



