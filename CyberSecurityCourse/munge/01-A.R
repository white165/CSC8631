# Example preprocessing script.


quizStu <- function(quiz, courseStartDate){
  
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
#
quizStuPre <- function(quizStat){
  
  #create a data frame with unique user id
  quizData <- data.frame(id = unique(quizStat$id), 
                         numAns="", 
                         numQues="", 
                         numCorr="", 
                         ft="", 
                         st="")
  
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
  
  return(quizData)
}

quizStuCon <- function(quizData){
  quizData <- data.frame(quizData,
                tot = (as.numeric(quizData$numQues) / max(as.numeric(quizData$numQues))),
                dt <- Mod((as.numeric(quizData$ft) - as.numeric(quizData$st))),
                acc <- (as.numeric(quizData$numCorr)/as.numeric(quizData$numAns)),
                scr <- (as.numeric(quizData$numQues)/as.numeric(quizData$numAns))
                )
  return(quizData)
  
}



quizStu1 <- quizStu(cyber.security.1_question.response, "2016-09-05")
quizStu2 <- quizStu(cyber.security.2_question.response, "2017-03-20")
quizStu3 <- quizStu(cyber.security.3_question.response, "2017-09-18")
quizStu4 <- quizStu(cyber.security.4_question.response, "2017-11-13")
quizStu5 <- quizStu(cyber.security.5_question.response, "2018-02-05")
quizStu6 <- quizStu(cyber.security.6_question.response, "2018-06-11")
quizStu7 <- quizStu(cyber.security.7_question.response, "2018-09-10")


quizStuPre1 <- quizStuPre(quizStu1)
quizStuPre2 <- quizStuPre(quizStu2)
quizStuPre3 <- quizStuPre(quizStu3)
quizStuPre4 <- quizStuPre(quizStu4)
quizStuPre5 <- quizStuPre(quizStu5)
quizStuPre6 <- quizStuPre(quizStu6)
quizStuPre7 <- quizStuPre(quizStu7)


quizStuCon1 <- quizStuCon(quizStuPre1)
quizStuCon2 <- quizStuCon(quizStuPre2)
quizStuCon3 <- quizStuCon(quizStuPre3)
quizStuCon4 <- quizStuCon(quizStuPre4)
quizStuCon5 <- quizStuCon(quizStuPre5)
quizStuCon6 <- quizStuCon(quizStuPre6)
quizStuCon7 <- quizStuCon(quizStuPre7)


cache('quizStu1')
cache('quizStu2')
cache('quizStu3')
cache('quizStu4')
cache('quizStu5')
cache('quizStu6')
cache('quizStu7')

cache('quizStuPre1')
cache('quizStuPre2')
cache('quizStuPre3')
cache('quizStuPre4')
cache('quizStuPre5')
cache('quizStuPre6')
cache('quizStuPre7')

cache('quizStuCon1')
cache('quizStuCon2')
cache('quizStuCon3')
cache('quizStuCon4')
cache('quizStuCon5')
cache('quizStuCon6')
cache('quizStuCon7')
