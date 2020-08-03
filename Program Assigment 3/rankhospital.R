rankhospital <- function(state, outcome, num = "best") {
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
        ## selecting only the columns name, state and outcome
        dataframe <- dataframe[,c(2,7,column[outcome])]
        ## transfoming the outcome column in numeric
        options(warn = -1)
        dataframe[,3] <- as.numeric(dataframe[,3])
        options(warn = 0)
        ## remove NA values
        dataframe <- dataframe[complete.cases(dataframe), ]
        ##selecting only the hospitals in the state
        dataframe <- dataframe[which(dataframe[,2]==state),]
        ## Ordering the hospitals
        dataframe <- dataframe[order(dataframe[,3],dataframe[,1]),]
        ##adjusting the num
        if(num=='best'){
                num <- 1
        }
        if(num=='worst'){
                num <- nrow(dataframe)
        }
        ## if num is bigger than the numbers of hospitals
        if(num > nrow(dataframe)){
                return(NA)
        }
        ## Return hospital name in that state with the given rank
        dataframe[num,1]
        ## 30-day death rate
}