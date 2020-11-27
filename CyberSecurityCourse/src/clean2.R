




# This function cleans the data and returns a data frame with the following:
#
#  qq - question number
#  numStu - number of students that answered the question 
#  numCorr - number of correct answers 
#  numAns - number of answers 
#  wn - week number
#  acc - 
#  scr - ratio of 
#  tot - ratio of students that completed the question 



#
quizCleanQQ <- function(quizStat){
  
  #create a dataframe with unique user qq
  quizData <- data.frame(qq = unique(quizStat$qq), numAns="", numStu="", numCorr="", wn="")
  
  for(i in 1:nrow(quizData)){
    count = 0 #count the number of occurrences (i.e. question attempts)
    count2 = 0 #reset the number of correct answers 
    count3 = 0 # resets the number of different questions answered 
    student = ""
    flag = 1
    
    #loops the number unique qq values 
    for(j in 1:nrow(quizStat)){
      
      if(quizData$qq[i] == quizStat$qq[j]){
        count = count+1
        if(flag == 1){
          quizData$wn[i] = quizStat$wn[j] #store the FIRST time student answered question 
          flag = 0 
        }
        if(quizStat$ans[j] == "true"){
          count2 = count2+1
        }
        if(quizStat$id[j] != student){
          student = quizStat$id[j] 
          count3 = count3+1
        }
      }
    }
    quizData$numStu[i] = count3#store the number of different questions answered 
    quizData$numCorr[i] = count2 # store the number of correct answers 
    quizData$numAns[i] = count #store the number of attempts 
    
    
  }
  
  quizData$tot = (as.numeric(quizData$numStu) / max(as.numeric(quizData$numStu)))
  quizData$acc = (as.numeric(quizData$numCorr)/as.numeric(quizData$numAns))
  quizData$scr = (as.numeric(quizData$numStu)/as.numeric(quizData$numAns))
  
  return(quizData)
}


quizCleanQQ1 <-quizCleanQQ(quizStat1)
quizCleanQQ2 <-quizCleanQQ(quizStat2)
quizCleanQQ3 <-quizCleanQQ(quizStat3)
quizCleanQQ4 <-quizCleanQQ(quizStat4)
quizCleanQQ5 <-quizCleanQQ(quizStat5)
quizCleanQQ6 <-quizCleanQQ(quizStat6)
quizCleanQQ7 <-quizCleanQQ(quizStat7)



quizCleanQQ1 <-CharToNum(quizCleanQQ1, 2, 8)
quizCleanQQ2 <-CharToNum(quizCleanQQ2, 2, 8)
quizCleanQQ3 <-CharToNum(quizCleanQQ3, 2, 8)
quizCleanQQ4 <-CharToNum(quizCleanQQ4, 2, 8)
quizCleanQQ5 <-CharToNum(quizCleanQQ5, 2, 8)
quizCleanQQ6 <-CharToNum(quizCleanQQ6, 2, 8)
quizCleanQQ7 <-CharToNum(quizCleanQQ7, 2, 8)


cache('quizCleanQQ1')
cache('quizCleanQQ2')
cache('quizCleanQQ3')
cache('quizCleanQQ4')
cache('quizCleanQQ5')
cache('quizCleanQQ6')
cache('quizCleanQQ7')

