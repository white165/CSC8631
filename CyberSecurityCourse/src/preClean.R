
cleanQuizData <- function(quiz, courseStartDate){
  
  #convert the course start date to seconds
  cs = as.numeric(as.POSIXct(courseStartDate ))
  
  #renaming the df and its columns
  colnames(quiz) = c("id", "qq", "qt", "wn", "sn", "qn", "r", "cr", "t", "ans")
  
  quiz = select(quiz, -c(qt, cr)) #removing the columns that are not needed, or give no info
  
  #displaying the date in seconds and removing substituting the date in which the course started (standardisinng to the start of the course)
  quiz$t = as.numeric(as.POSIXct(quiz$t))-cs
  
  return(quiz)
}





# This func cleans the data and returns a data frame with the following:
#
#  id - the student number
#  numQues - number of different questions answered by the student
#  numCorr - number of correct answers 
#  numAns - number of answers 
#  ft - final time a question was answered 
#  st - first time a question was answered
#  dt - delta t time between ft and st
#
quizStatClean <- function(quizStat){
  
  #create a dataframe with unique user id
  quizData <- data.frame(id = unique(quizStat$id), numAns="", numQues="", numCorr="", ft="", st="", dt="")
  
  for(i in 1:nrow(quizData)){
    count = 0 #count the number of occurrences (i.e. question attempts)
    count2 = 0 #reset the number of correct answers 
    count3 = 0 # resets the number of different questions answered 
    question = ""
    flag = 1
    
    #loops the number unique id values 
    for(j in 1:nrow(quizStat)){
      
      if(quizData$id[i] == quizStat$id[j]){
        count = count+1
        if(flag == 1){
          quizData$st[i] = quizStat$t[j] #store the FIRST time student answered question 
          flag = 0 
        }
        if(quizStat$ans[j] == "true"){
          count2 = count2+1
        }
        if(quizStat$qq[j] != question){
          question = quizStat$qq[j] 
          count3 = count3+1
        }
      }
    }
    quizData$numQues[i] = count3#store the number of different questions answered 
    quizData$numCorr[i] = count2 # store the number of correct answers 
    quizData$numAns[i] = count #store the number of attempts 
    quizData$ft[i] = quizStat$t[count] #store the LAST time student answered question
    
  }
  #quizData$dt = quizData$ft - quizData$st
  
  return(quizData)
}


