complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ## Declare variables
    comp <- data.frame()
    x <- 1
    
    for(i in id) {
        ## Generate file name based on length of id
        file_name <- if(i < 10) {
            paste(directory,"/00",toString(i),".csv",sep = "")
        } else if(i < 100) {
            paste(directory,"/0",toString(i),".csv",sep = "")
        } else {
            paste(directory,"/",toString(i),".csv",sep = "")
        }
        
        temp <- read.csv(file_name)
        
        ## Store file name & nobs
        comp[x,1] <- i
        comp[x,2] <- sum(complete.cases(temp))
        x <- x + 1
    }
    
    ## Add column names
    names(comp) <- c("id","nobs")
    comp
}