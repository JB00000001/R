#Compare triplets: https://www.hackerrank.com/challenges/compare-the-triplets
A <- c(5,6,7)
B <- c(3,6,10)
#option 1: direct comparison
print("Results using direct comparison")
score_A <- sum(A>B)
score_B <- sum(A<B)
sprintf("score A= %i and score B=%i",score_A,score_B)
#option 2: function
print("Results using function")
calc_score <- function(A,B){
  scores<-c(0,0)
  for ( i in 1:length(A))  {
     if( A[i]>B[i] ){
          scores[1]<-scores[1]+1
         }
     else if (A[i] <B[i]){
         scores[2]<-scores[2]+1 
         }
    }
   scores
}
print(calc_score(A,B))


# VERY BIG SUM: https://www.hackerrank.com/challenges/a-very-big-sum
n<-5
input<-c(1000000001, 1000000002, 1000000003, 1000000004, 1000000005)

big_sum <- function(n,input,showsteps){
  result<-0
  for (i in 1:n){
      result<-result + input[i]
      if (showsteps==TRUE){
         print(i)
         print(result,digits=10)
      }
  }
  result
}

print(big_sum(n,input,showsteps=TRUE),digits=10)

#Diagonal Difference: https://www.hackerrank.com/challenges/diagonal-difference

n<-3
M=matrix(c(11, 2, 4,     4, 5, 6,    10, 8, -12),nrow=n,ncol=n)

diag_diff <-function(M,n,debug){
   diag1 <- 0
   diag2 <- 0
   for (i in 1:n){
      diag1 <- diag1 + M[i,i]
      diag2 <- diag2 + M[n+1-i,i]
      if (debug==TRUE){
       cat(sprintf("step i=%i: diagonal1=%i and diagonal2=%i\n",i,diag1,diag2))
       }
   }
   return( abs(diag1-diag2) )
}
print(diag_diff(M,n,debug=TRUE))


#birthday-cake-candles: https://www.hackerrank.com/challenges/birthday-cake-candles
n<-4
candles<-c(3,2,1,3)

blown_candles<-function(n,candles){
    result<-0
    themax<-max(candles)
    for (i in 1:length(candles) ){
        if(candles[i]==themax){
          result<-result+1
        }   
    } 
    return(result)
}

print(blown_candles(n,candles))




#Implementaion -> grading: https://www.hackerrank.com/challenges/grading
n<-4
grades<-c(73, 67, 38, 33)

round_grades<-function(n,grades){
   rounded_grades<-grades 
   for (i in 1:length(grades)) {
      cat(sprintf("grade#=%i",grades[i]))
      if  ( (grades[i]<38 ) | (grades[i]%%5<=2)  ) {
        rounded_grades[i]<-grades[i]
        print("no rounding")
      }
      else {
        rounded_grades[i]<-ceiling(grades[i]/5)*5
        print("rounding")        
      }
   }
   rounded_grades
}

print(round_grades(n,grades))



#sorting: big-sorting: https://www.hackerrank.com/challenges/big-sorting
n<-6
to_sort<-c(31415926535897932384626433832795, 1, 3, 10 ,3, 5)
sorted<-to_sort[order(to_sort)]
print(sorted,digits=20)


#sorting: The Full Counting Sort: https://www.hackerrank.com/challenges/countingsort4
n<-20
nums<-c(0,6,0,6,4,0,6,0, 6, 0, 4, 3,0,1,5,1,2,4,2,4) 
words<-c("ab","cd","ef","gh","ij","ab" ,"cd" ,"ef" ,"gh" ,"ij","that" ,"be" ,"to" ,"be" ,"question" ,"or" ,"not" ,"is" ,"to" ,"the")
rank<-seq(from=1, to=20)
df=data.frame(v1=nums,v2=words,v3=rank)
df2<-df[order(df$v1),  ]
#function that takes a dataframe with numbers in first column and chars in second and only ranks
#when numbers are equal based on a third column rank
order_advanced<-function(df){

}

#Gaming array: https://www.hackerrank.com/contests/hourrank-15/challenges/an-interesting-game-1
#input
nrgames<-2
game<-list()
game1<-c(5,2,6,3,4)
game2<-c(3,1)
game<-list(game1=game1,game2=game2)

game_array <- function(game,nrgames,debug){
  for (i in 1:nrgames){  
    #initialisation    
    found <- FALSE
    helpgame <- game[[i]]
    winner<-0

    while (found == FALSE){
       winner<-winner+1
       if (debug==TRUE){ 
          cat(sprintf("iteration #=%i",winner))
       }
       #save only elements up until maximum
       max_index <- which( helpgame==max(helpgame) )
       if (debug==TRUE){ 
          cat(sprintf("max_index #=%i",max_index))
       }
       helpgame <- helpgame[ 1:  (max_index-1)]
       if (debug==TRUE){ 
          print(helpgame)
       }
       if (max_index == 1){
         found <- TRUE
        }
       if (winner>10){
          break
          print("infinite loop")
       }
    }
    if (winner%%2==0){
       print("ANDY")
    }
    else{
       print("BOB")
    } 
  }
}

game_array(game,nrgames,debug=TRUE)


#sum of squares

sum_squares <-function(x){
result<-x**2
}
input <- c(2,3,6,1)
sum_squares(input)


#new year chaos https://www.hackerrank.com/challenges/new-year-chaos
nr_games<-2
game1<-c(2, 1, 5, 3, 4)
game2<-c(2, 5, 1, 3, 4)


NY_chaos<-function(game){
   bribes <- 0
   ctr<-1
   for ( i in game  ) { 
        ctr<-ctr+1
        if (ctr==length(game)){
          return(bribes)
          break
        }
        if (  i > game[ctr]  ){

          bribes <- bribes + ( i-game[ctr] )
          if ( bribes > 3 ) {
            write("Too chaotic",stdout())
            invisible(bribes)
            break        
          }
        }
   }
}

NY_chaos(game1)
NY_chaos(game2)










