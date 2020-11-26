# Example preprocessing script.


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
    quizData$tot = (as.numeric(quizData$numQues) / max(as.numeric(quizData$numQues)))
    
  }


  quizData$dt <- (as.numeric(quizData$ft) - as.numeric(quizData$st)) 
  quizData$dt <- Mod(quizData$dt)
  quizData$acc = (as.numeric(quizData$numCorr)/as.numeric(quizData$numAns))
  quizData$scr = (as.numeric(quizData$numQues)/as.numeric(quizData$numAns))
  
  return(quizData)
}

quizStat1 <- cleanQuizData(cyber.security.1_question.response, "2016-09-05")
quizStat2 <- cleanQuizData(cyber.security.2_question.response, "2017-03-20")
quizStat3 <- cleanQuizData(cyber.security.3_question.response, "2017-09-18")
quizStat4 <- cleanQuizData(cyber.security.4_question.response, "2017-11-13")
quizStat5 <- cleanQuizData(cyber.security.5_question.response, "2018-02-05")
quizStat6 <- cleanQuizData(cyber.security.6_question.response, "2018-06-11")
quizStat7 <- cleanQuizData(cyber.security.7_question.response, "2018-09-10")


quizStatClean1 <- quizStatClean(quizStat1)
quizStatClean2 <- quizStatClean(quizStat2)
quizStatClean3 <- quizStatClean(quizStat3)
quizStatClean4 <- quizStatClean(quizStat4)
quizStatClean5 <- quizStatClean(quizStat5)
quizStatClean6 <- quizStatClean(quizStat6)
quizStatClean7 <- quizStatClean(quizStat7)


cache('quizStat1')
cache('quizStat2')
cache('quizStat3')
cache('quizStat4')
cache('quizStat5')
cache('quizStat6')
cache('quizStat7')

cache('quizStatClean1')
cache('quizStatClean2')
cache('quizStatClean3')
cache('quizStatClean4')
cache('quizStatClean5')
cache('quizStatClean6')
cache('quizStatClean7')
