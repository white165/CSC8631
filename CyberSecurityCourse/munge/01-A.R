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








# This function pre-prosesses the quiz.response with reference to the quiz question  
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


#converts all columns to a num - other than qq
dfToNum <- function(data){
  df <- data
  df <- select(df, -c(qq))
  df <- as.data.frame(sapply(df, as.numeric))
  df <- data.frame(qq=data$qq, df )
  return(df)
}


#add the derivations to the constructed df
quizQueCon <- function(quizData) { 
  quizData <- data.frame(quizData,
                         tot = (as.numeric(quizData$numStu) / max(as.numeric(quizData$numStu))),
                         acc = (as.numeric(quizData$numCorr)/as.numeric(quizData$numAns)),
                         scr = (as.numeric(quizData$numStu)/as.numeric(quizData$numAns))
                         ) 
  return(quizData)
}


#normalise function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#2nd stage of pre-processing - 1st stage was done in PrePro1.R to get quiStat
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


# 1st stage of construction - add derivations 
quizQueCon1 <-quizQueCon(quizQuePre1)
quizQueCon2 <-quizQueCon(quizQuePre2)
quizQueCon3 <-quizQueCon(quizQuePre3)
quizQueCon4 <-quizQueCon(quizQuePre4)
quizQueCon5 <-quizQueCon(quizQuePre5)
quizQueCon6 <-quizQueCon(quizQuePre6)
quizQueCon7 <-quizQueCon(quizQuePre7)

# 2nd stage of construction - convert columns to number 
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





#normalise the all the runs
quizQueCon1$numStu <- normalize(quizQueCon1$numStu)
quizQueCon2$numStu <- normalize(quizQueCon2$numStu)
quizQueCon3$numStu <- normalize(quizQueCon3$numStu)
quizQueCon4$numStu <- normalize(quizQueCon4$numStu)
quizQueCon5$numStu <- normalize(quizQueCon5$numStu)
quizQueCon6$numStu <- normalize(quizQueCon6$numStu)
quizQueCon7$numStu <- normalize(quizQueCon7$numStu)



#modeling df with all runs
df1 <- data.frame(run=1, quizQueCon1)
df2 <- data.frame(run=2, quizQueCon2)
df3 <- data.frame(run=3, quizQueCon3)
df4 <- data.frame(run=4, quizQueCon4)
df5 <- data.frame(run=5, quizQueCon5)
df6 <- data.frame(run=6, quizQueCon6)
df7 <- data.frame(run=7, quizQueCon7)
quizQueMod <- rbind( df1, df2,df3, df4, df5, df6, df7)

cache('quizQueMod')

#modeling df without run one
quizQueMod1 <- rbind(df2,df3, df4, df5, df6, df7)

cache('quizQueMod1')







