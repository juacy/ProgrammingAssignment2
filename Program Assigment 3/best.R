best <- function(state, outcome) {
        #reading the file with data
        dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
        ## Check that state and outcome are valid
        if(!(state %in% unique(dataframe[,'State']))){
                stop('invalid state')        
        }
        if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
                stop('invalid outcome')
        }
        ##chossing the colunm by index
        column <- c(11,17,23)
        names(column) <- c('heart attack', 'heart failure', 'pneumonia')
        ## selecting only the columns name, state and outcome, removing na values
        dataframe <- dataframe[,c(2,7,column[outcome])]
        ## transfoming the outcome column in numeric
        options(warn = -1)
        dataframe[,3] <- as.numeric(dataframe[,3])
        options(warn = 0)
        ##selecting only the hospitals in the state
        dataframe <- dataframe[which(dataframe[,2]==state),]
        ## best hospital value
        value <- min(dataframe[,3], na.rm = TRUE)
        ## Return hospital name in that state with lowest 30-day death
        best <- dataframe[which(dataframe[,3]==value),1]
        sort(best)[1]
        ## rate
}


