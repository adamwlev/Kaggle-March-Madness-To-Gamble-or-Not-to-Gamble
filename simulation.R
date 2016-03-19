set.seed(200)
numofcomp<-900 ##number of competitors
numofsims<-700 ##number of simulations
numofclasses<-30 ##number of classes of accuracy
trueprobs<-rbeta(63,.65,.65) ##true probabilities of the games
assessments<-matrix(0,63,numofcomp) 

cap<-function(x){
	x[x<0]<-.001
	x[x>1]<-.999
	return(x)
}

perturb<-function(x,a){
	return(x+rnorm(length(x),0,a))
}

make_inaccurate<-function(x,a){
	return(a*runif(length(x))+(1-a)*x)
}

extremeize<-function(x,a){
	x[x>.5]<- x[x>.5]+(1-x[x>.5])*a
	x[x<.5]<- x[x<.5]-x[x<.5]*a
	return(x)
}

logloss<-function(x,y){
	return (-1/length(x)*sum((1-y)*log(1-x)+y*log(x)))
}

money<-numeric(numofcomp)
placement<-numeric(numofcomp)
for (s in 1:numofsims){
	print(s)
	scores<-numeric(numofcomp) ##vector for scores
	outcomes<-rbinom(63,1,trueprobs) ##draw to simulate game outcomes
	# fakeprobs<-matrix(0,63,numofclasses)
	# for (i in 1:numofclasses){
		# fakeprobs[,i]<-make_inaccurate(trueprobs,(i-1)/100)
	# }
	for (i in 1:numofcomp){
		#assessments[,i]<-cap(extremeize(trueprobs,(i-1)/899))
		#assessments[,i]<-cap(perturb(trueprobs,(i-1)/2500))
		#assessments[,i]<-cap(extremeize(fakeprobs[,(i-1)%/%numofclasses+1],(i-1)%%numofclasses/(numofclasses-1)))
		#assessments[,i]<-cap(perturb(fakeprobs[,(i-1)%/%numofclasses+1],(i-1)%%30/83))
		#assessments are trueprobs with some noise - bigger i is, the less accurate they are
		scores[i]<-logloss(assessments[,i],outcomes)
	}
	score<-sort(scores,index.return=TRUE)$ix
	placement<-placement+rank(scores)
	money[score[1]]<-money[score[1]]+10000
	money[score[2]]<-money[score[2]]+6000
	money[score[3]]<-money[score[3]]+4000
	money[score[4]]<-money[score[4]]+3000
	money[score[5]]<-money[score[5]]+2000
	##record the outcomes for winning competitors
}
money<-money/numofsims
placement<-placement/numofsims