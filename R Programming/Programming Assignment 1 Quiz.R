####
# Part 1
###
## Write a function named 'pollutantmean' that calculates the mean of a pollutant
## (sulfate or nitrate) across a specified list of monitors. The function
## 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
## particulate matter data from the directory specified in the 'directory' argument
## and returns the mean of the pollutant across all of the monitors,
## ignoring any missing values coded as NA


pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  #source("obsFileName.R")
  allData <- numeric()
  
  for (i in id) {
    filename <- obsFileName(directory, i)
    
    data <- read.csv(filename)
    
    if (pollutant == "sulfate") {
      allData <- c(allData, data$sulfate)
    } else if (pollutant == "nitrate") {
      allData <- c(allData, data$nitrate)
    }
  }
  
  mean(allData, na.rm=TRUE)
}


pollutantmean(directory = '/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', pollutant = 'sulfate', id = 34)
pollutantmean(directory = '/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', pollutant = 'nitrate')

####
# Part 2
###
## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
## The function should return a data frame where the first column is the name of the file and the second column is the number
## of complete cases. A prototype of this function follows

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  comp <- data.frame(id=numeric(), nobs=numeric())
  
  for (i in id) {
    filename <- obsFileName(directory, i)
    data <- read.csv(filename)
    comp <- rbind(comp, data.frame(id=i, nobs=nrow(data[complete.cases(data), ])))
  }
  
  comp
}

complete(directory = '/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', c(2, 4, 8, 10, 12))
cc <- complete('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 54)
print(cc$nobs)


RNGversion("3.5.1")  
set.seed(42)
cc <- complete('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
####
# Part 3
###
## Write a function that takes a directory of data files and a threshold
## for complete cases and calculates the correlation between sulfate and
## nitrate for monitor locations where the number of completely observed
## cases (on all variables) is greater than the threshold. The function
## should return a vector of correlations for the monitors that meet the
## threshold requirement. If no monitors meet the threshold requirement,
## then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations
  
  #source("complete.R")
  #source("obsFileName.R")
  observations <- complete(directory, 1:332)
  sulfate <- numeric()
  nitrate <- numeric()
  result <- numeric()
  
  for (i in observations$id[observations$nobs > threshold]) {
    filename <- obsFileName(directory, i)
    data <- read.csv(filename)
    result <- c(result, cor(data$sulfate, data$nitrate, use="complete.obs"))
  }
  
  result
}

cr <- corr(directory = '/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 150)
head(cr)
summary(cr)


cr <- corr('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata')                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 2000)                
n <- length(cr)                
cr <- corr('/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/R Programing/specdata', 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
