corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## Define variables
    x <- vector()
    
    ## Calculate number of complete entries for each file in directory
    comp <- complete(directory)
    
    ## Loop through all files that are above threshold
    id <- comp[(comp[,2]>threshold),1]
    for(i in id) {
        ## Generate file name based on length of id
        file_name <- if(i < 10) {
            paste(directory,"/00",toString(i),".csv",sep = "")
        } else if(i < 100) {
            paste(directory,"/0",toString(i),".csv",sep = "")
        } else {
            paste(directory,"/",toString(i),".csv",sep = "")
        }
        
        ## Read file
        temp <- read.csv(file_name)
        
        ## Calculate covariance
        x <- c(x,cor(temp["nitrate"],temp["sulfate"],use = "complete.obs"))
    }
    
    x
}