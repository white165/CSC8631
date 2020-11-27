

CharToNum <- function(quizData, x, y) {
  
  for(i in x:y){
    quizData[,i] <- as.numeric(quizData[,i])
  }
  return(quizData)
}
  
quizStatClean1 <-CharToNum(quizStatClean1, 2, 9)
quizStatClean2 <-CharToNum(quizStatClean2, 2, 9)
quizStatClean3 <-CharToNum(quizStatClean3, 2, 9)
quizStatClean4 <-CharToNum(quizStatClean4, 2, 9)
quizStatClean5 <-CharToNum(quizStatClean5, 2, 9)
quizStatClean6 <-CharToNum(quizStatClean6, 2, 9)
quizStatClean7 <-CharToNum(quizStatClean7, 2, 9)

quizStatClean1 <- quizStatClean1[!(quizStatClean1$numAns == max(quizStatClean1$numAns)), ]
quizStatClean1 <- quizStatClean1[!(quizStatClean1$numAns == max(quizStatClean1$numAns)), ]

cache('quizStatClean1')
cache('quizStatClean2')
cache('quizStatClean3')
cache('quizStatClean4')
cache('quizStatClean5')
cache('quizStatClean6')
cache('quizStatClean7')








          