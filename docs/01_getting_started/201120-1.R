EOQ
Q=sqrt((2*D*K)/h)
Q

round(1.25,0)

roll<- function() {
  die<-(1:6)
  dice<- sample(die,size=,replace=TRUE)
  sum(dice)
  }

roll2<- function(faces=1:6) {  die<-(1:6)
  dice<- sample(die,size=2,replace=TRUE)
  sum(dice)}

roll4<- function(faces=1:6) {  die<-(1:6)
dice<- sample(die,size=2,replace=TRUE,prob = c(0,0,0,0,0.5,0.5))
sum(dice)}

EOQ<- function(D=1000){
  sqrt(2*D*K/h)
}

probabilities_vector<-c(1/6,1/6,1/6,1/6)

results <- replications(n=100, exrp=roll4(), )
hist(results)
