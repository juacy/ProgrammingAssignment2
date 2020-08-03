rankall <- function(outcome, num = "best") {
        ## reading the file with data
        dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
        ## defing a vector with states
        states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
                    "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN",
                    "TX","UT","VT","VI","VA","WA","WV","WI","WY","GU")
        states <- sort(states)
        ##create a dataframe for the return
        rankhospital <- data.frame(hospital=NA, states, row.names = states) 
        ## Check that outcome is valid
        if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
                stop('invalid outcome')
        }
        ##choosing the colunm by index
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
        ## adjusting the num for the best
        if(num=='best'){
                num <- 1
        }
        ## For each state, find the hospital of the given rank
        for(state in states){
                aux <- num
                ##selecting only the hospitals in the state
                dataframe_state <- dataframe[which(dataframe[,2]==state),]
                ## Ordering the hospitals
                dataframe_state <- dataframe_state[order(dataframe_state[,3],dataframe_state[,1]),]
                ## adjusting the num for worst and other cases without best
                if(aux=='worst'){
                        aux <- nrow(dataframe_state)
                }
                if(aux>nrow(dataframe_state)){
                        next()
                }
                rankhospital[rankhospital$states==state,1]<-dataframe_state[aux,1]
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rankhospital
}
