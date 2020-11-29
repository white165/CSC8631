




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
quizQuePre <- function(quizStat){
  
  #create a dataframe with unique user qq
  quizData <- data.frame(qq = unique(quizStat$qq), numAns="", numStu="", numCorr="", wn="", sn="", qn="")
  
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
          quizData$sn[i] = quizStat$sn[j]
          quizData$qn[i] = quizStat$qn[j]
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

  return(quizData)
}


quizQuePre1 <-quizQuePre(quizStat1)
quizQuePre2 <-quizQuePre(quizStat2)
quizQuePre3 <-quizQuePre(quizStat3)
quizQuePre4 <-quizQuePre(quizStat4)
quizQuePre5 <-quizQuePre(quizStat5)
quizQuePre6 <-quizQuePre(quizStat6)
quizQuePre7 <-quizQuePre(quizStat7)


cache('quizQuePre1')
cache('quizQuePre2')
cache('quizQuePre3')
cache('quizQuePre4')
cache('quizQuePre5')
cache('quizQuePre6')
cache('quizQuePre7')

#converts the df variables to a num other than id
dfToNum <- function(data){
  df <- data
  df <- select(df, -c(qq))
  df <- as.data.frame(sapply(df, as.numeric))
  df <- data.frame(qq=data$qq, df )
  return(df)
}

quizQueCon <- function(quizData) { 
  quizData <- data.frame(quizData,
                        tot = (as.numeric(quizData$numStu) / max(as.numeric(quizData$numStu))),
                        acc = (as.numeric(quizData$numCorr)/as.numeric(quizData$numAns)),
                        scr = (as.numeric(quizData$numStu)/as.numeric(quizData$numAns))
                        ) 
  return(quizData)
}

quizQueCon1 <-quizQueCon(quizQuePre1)
quizQueCon2 <-quizQueCon(quizQuePre2)
quizQueCon3 <-quizQueCon(quizQuePre3)
quizQueCon4 <-quizQueCon(quizQuePre4)
quizQueCon5 <-quizQueCon(quizQuePre5)
quizQueCon6 <-quizQueCon(quizQuePre6)
quizQueCon7 <-quizQueCon(quizQuePre7)


quizQueCon1 <-dfToNum(quizQueCon1)
quizQueCon2 <-dfToNum(quizQueCon2)
quizQueCon3 <-dfToNum(quizQueCon3)
quizQueCon4 <-dfToNum(quizQueCon4)
quizQueCon5 <-dfToNum(quizQueCon5)
quizQueCon6 <-dfToNum(quizQueCon6)
quizQueCon7 <-dfToNum(quizQueCon7)


cache('quizQueCon1')
cache('quizQueCon2')
cache('quizQueCon3')
cache('quizQueCon4')
cache('quizQueCon5')
cache('quizQueCon6')
cache('quizQueCon7')




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

quizQueCon1$numStu <- normalize(quizQueCon1$numStu)
quizQueCon2$numStu <- normalize(quizQueCon2$numStu)
quizQueCon3$numStu <- normalize(quizQueCon3$numStu)
quizQueCon4$numStu <- normalize(quizQueCon4$numStu)
quizQueCon5$numStu <- normalize(quizQueCon5$numStu)
quizQueCon6$numStu <- normalize(quizQueCon6$numStu)
quizQueCon7$numStu <- normalize(quizQueCon7$numStu)



# run one was ignored as the section question varied from the other 
df1 <- data.frame(run=1, quizQueCon1)
df2 <- data.frame(run=2, quizQueCon2)
df3 <- data.frame(run=3, quizQueCon3)
df4 <- data.frame(run=4, quizQueCon4)
df5 <- data.frame(run=5, quizQueCon5)
df6 <- data.frame(run=6, quizQueCon6)
df7 <- data.frame(run=7, quizQueCon7)
quizQueMod <- rbind( df1, df2,df3, df4, df5, df6, df7)

cache('quizQueMod')


quizQueMod1 <- rbind(df2,df3, df4, df5, df6, df7)

cache('quizQueMod1')






