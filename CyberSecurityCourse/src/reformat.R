

CharToNum <- function(quizData) {
  
  for(i in 2:9){
    quizData[,i] <- as.numeric(quizData[,i])
  }
  return(quizData)
}
  
quizStatClean1 <-CharToNum(quizStatClean1)
quizStatClean2 <-CharToNum(quizStatClean2)
quizStatClean3 <-CharToNum(quizStatClean3)
quizStatClean4 <-CharToNum(quizStatClean4)
quizStatClean5 <-CharToNum(quizStatClean5)
quizStatClean6 <-CharToNum(quizStatClean6)
quizStatClean7 <-CharToNum(quizStatClean7)

quizStatClean1 <- quizStatClean1[!(quizStatClean1$numAns == max(quizStatClean1$numAns)), ]
quizStatClean1 <- quizStatClean1[!(quizStatClean1$numAns == max(quizStatClean1$numAns)), ]

cache('quizStatClean1')
cache('quizStatClean2')
cache('quizStatClean3')
cache('quizStatClean4')
cache('quizStatClean5')
cache('quizStatClean6')
cache('quizStatClean7')
