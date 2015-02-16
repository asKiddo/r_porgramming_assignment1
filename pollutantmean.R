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
    
    ##Declare variables
    x <- vector()
    
    for(i in id) {
        ## Generate file name based on length of id
        file_name <- if(i < 10) {
            paste(directory,"/00",toString(i),".csv",sep = "")
        } else if(i < 100) {
            paste(directory,"/0",toString(i),".csv",sep = "")
        } else {
            paste(directory,"/",toString(i),".csv",sep = "")
        }
        
        ## Read file and remove NAs
        temp <- read.csv(file_name)
        x <- c(x,temp[!is.na(temp[pollutant]),pollutant])
    }
    
    ## Calculate mean
    mean(x)
}