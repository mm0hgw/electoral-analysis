rrom<-function(n,pc,tc=0,tv=1,cc=0,cv=2) {
  ## Input: 
  ## n = number of variates to produce
  ## pc = probability of a contaminant
  ## tc = true distribution centre
  ## tv = true distribution variance 
  ## cc = contaminating distribution center 
  ## cv = contaminating distribution variance
  ##
  ## Output: 
  ## rs = variates from contaminated distribution
k <- rbinom(n,1,1-pc)
return(c(rnorm(n-k,tc,tv),rnorm(k,cc,cv)))
}

# NOTE: A general Fixed Outlier Model function provided for two normal distributions
rfom<-function(n,nc,tc=0,tv=1,cc=0,cv=2){
  ## Input: 
  ## n = number of variates to produce
  ## nc = number of contaminants
  ## tc = true distribution centre
  ## tv = true distribution variance
  ## cc = contaminating distribution center 
  ## cv = contaminating distribution variance
  ##
  ## Output: 
  ## rs = variates from contaminated distribution
  return(unlist(lapply(c(rep(1,n-nc),rep(0,nc)), function(x) {ifelse(x,rnorm(1,tc,tv),rnorm(1,cc,cv))})))
}

# NOTE: For simplicity, general standardized laplace and logistic distributions are provided
#rslogis<-function(n,l=0,s=1){
  ## Input: 
  ## n = number of variates
  ## l = location
  ## s = scale
#return((rlogis(n,l,s)-l)/(3.14159265358979323846/(sqrt(3)*s)))
#}
#rslaplace<-function(n,l=0,s=1){
  ## Input: 
  ## n = number of variates
  ## l = location
  ## s = scale
#return((rlaplace(n,l,s)-l)/(sqrt(2)*s))
#}

rslogis <- function(n=n,l=0,s=1,lb=0,ub=1){
    ## Input:
    ## n = number of variates
    ## l = location
    ## s = scale
    ## lb = lower bound of Uniform
    ## ub = upper bound of Uniform
    X <- runif(n,lb,ub)
    Y <- log(X/(1-X))
    return((Y-l)/(3.14159265358979323846/(sqrt(3)*s)))
}


rslaplace <- function(n=n,l=0,s=1,ex.rate1=1,ex.rate2=1){
    ## Input:
    ## n = number of variates
    ## l = location
    ## s = scale
    ## ex.rate1 = rate 1 of exponential distribution
    ## ex.rate2 = rate 2 of exponential distribution
    X <- rexp(n, ex.rate1)
    Y <- rexp(n, ex.rate2)
    W <- X - Y
    return((W-l)/(sqrt(2)*s))
}








utst.sta <- function(X,Y,md=0){ #Unequal Variance Two Sample T Test Statistic
  # A function that computes the two sample t-test statistic under null hypothesis of 
  # default mean difference 0
  t <- mean(X)-mean(Y);
  t <- t-md;
  t <- t/(sqrt((var(X)/length(X))+(var(Y)/length(Y))));
  return(t)
}

var.pool <- function(X,Y){
  return((var(X)*(length(X)-1) + var(Y)*(length(Y)-1))/(length(X)+length(Y)-2))
  #varsp <- ((length(X)-1)/(length(X)+length(Y)-2))*var(X);
  #varsp <- varsp + ((length(Y)-1)/(length(X)+length(Y)-2))*var(Y);
  #return(varsp)
}

etst.sta <- function(X,Y,md=0){ #Equal Variance Two Sample T Test Statistic Calculation
  varsp <- var.pool(X,Y)
  d <- sqrt(varsp*((1/length(X))+(1/length(Y))))
  n <- mean(X)-mean(Y)-md
  return(n/d)
}

evft.sta <- function(X,Y){ #F Test for testing equality of variance
  return(var(X)/var(Y))
}

tconf.uneq <- function(X,Y,dfm=df.min,cl=0.95){ #Computes T-confidence interval when unequal variances
	mdiff = mean(X)-mean(Y)
	tvl = qt((1-cl)/2,dfm(X,Y))
	se = sqrt((var(X)/length(X))+(var(Y)/length(Y)))
  return(c(mdiff + tvl*se,mdiff - tvl*se))
    
}

tconf.eq <- function(X,Y,dfm=df.min,cl=0.95){
	mdiff=mean(X)-mean(Y)
	tvl=qt((1-cl)/2,dfm(X,Y))
	se=sqrt(var.pool(X,Y)*(1/length(X) + 1/length(Y)))
	return(c(mdiff + tvl*se,mdiff - tvl*se))
}


tconf.ft <- function(X,Y,dfm=df.min,tcl=0.95,fcl=0.05){
  f.sta <- var(X)/var(Y)
  f.crit.low <- qf(fcl/2,df1=length(X)-1,df2=length(Y)-1)
  f.crit.high <- qf(1-(fcl)/2,df1=length(X)-1,df2=length(Y)-1)
  if((f.sta > f.crit.high) | (f.sta < f.crit.low)){
    return(tconf.uneq(X,Y,dfm=dfm,cl=tcl))
  } else {
    return(tconf.eq(X,Y,dfm=dfm,cl=tcl))
  }
}


df.min <- function(X,Y){ 
  return(min(length(X)-1,length(Y)-1))
}

df.sat <- function(X,Y){
  k <- (var(X)/length(X))+(var(Y)/length(Y));
  k <- k*k;
  d <- (1/(length(X)-1))*((var(X)/length(X))**2);
  d <- d+(1/(length(Y)-1))*((var(Y)/length(Y))**2);
  k <- k/d;
  return(k);
}

df.tot <- function(X,Y){
  return(length(X)+length(Y)-2)
}



M <- 10000

modelsx <- c("snorm","fom2","rom2","logist","laplace")
modelsy <- c("snorm","fom2","rom2","logist","laplace")


varratios <- c(0.2,0.25,1/3,0.5,1,3,5)
#sampsizex <- c(5,10,20,30,50)
sampsizex <- c(10,20,30)
#sampsizey <- c(5,10,20,30,50)
sampsizey <- c(10,20,30)
dfestimtec <- c(df.sat)
procedures <- c("uneq","eq","ft1","ft2","ft3")
dfile <- file("SimulationData.csv", 'w')
#ftestconflvls <- c(0.01,0.05,0.1)
#function(n,pc,tc=0,tv=1,cc=0,cv=2)
#function(n,nc,tc=0,tv=1,cc=0,cv=2)
#function(n,l=0,s=1)
data <- list()
for(modx in modelsx){
for(mody in modelsy){
for (ssy in sampsizey){
for (ssx in sampsizex){
		for (rat in varratios){
			wu <- 0
			cpu <- 0
			we <- 0
			cpe <- 0
			wf1 <- 0
			cpf1 <- 0
			wf2 <- 0
			cpf2 <- 0
			wf3 <- 0
			cpf3 <- 0
			for (i in 1:M){
                X <- switch(modx,
                snorm = rnorm(ssx,0,1),
rom2 = rrom(ssx,0.2,0,1,0,4),
fom2 = rfom(ssx,2,0,1,0,4),
logist = rslogis(ssx,0,1),
laplace = rslaplace(ssx,0,1),
stop("Model Not Defined")
)
Y <- switch(mody,
snorm = rnorm(ssy,0,1),
rom2 = rrom(ssy,0.2,0,1,0,4),
fom2 = rfom(ssy,2,0,1,0,4),
logist = rslogis(ssy,0,1),
laplace = rslaplace(ssy,0,1),
stop("Model Not Defined")
)
				Y <- Y/sqrt(rat)

				cl.u <- tconf.uneq(X,Y)
				cl <- cl.u

				#Compute the Width and See if 0 on interval
				wu <- wu+cl[2]-cl[1]
				cpu <- cpu+(cl[2]*cl[1]<0)

				cl <- tconf.eq(X,Y)

				#Compute the Width and See if 0 on interval
				we <- we+cl[2]-cl[1]
				cpe <- cpe+(cl[2]*cl[1]<0)
				cl <- tconf.ft(X,Y,fcl=0.01)

				#Compute the Width and See if 0 on interval
				wf1 <- wf1+cl[2]-cl[1]
				cpf1 <- cpf1+(cl[2]*cl[1]<0)
				if (cl == cl.u){
					wf2 <- wf2+cl[2]-cl[1]
					cpf2 < cpf2+(cl[2]*cl[1]<0)
					wf3 <- wf3+cl[2]-cl[1]
					cpf3 <- cpf3+(cl[2]*cl[1]<0)
					next()
				}
				cl <- tconf.ft(X,Y,fcl=0.05)

				#Compute the Width and See if 0 on interval
				wf2 <- wf2+cl[2]-cl[1]
				cpf2 <- cpf2+(cl[2]*cl[1]<0)
				if (cl == cl.u){
					wf3 <- wf3+cl[2]-cl[1]
					cpf3 <- cpf3+(cl[2]*cl[1]<0)
					next()
				}
				cl <- tconf.ft(X,Y,fcl=0.10)

				#Compute the Width and See if 0 on interval
				wf3 <- wf3+cl[2]-cl[1]
				cpf3 <- cpf3+(cl[2]*cl[1]<0)
			}
write(sprintf("%10s,%10s,%5f,%5f,%5f,%10f,%10f,%10f,%10f,%10f,%10f,%10f,%10f,%10f,%10f",modx,mody,ssx,ssy,rat,wu/M,we/M,wf1/M,wf2/M,wf3/M,cpu/M,cpe/M,cpf1/M,cpf3/M,cpf3/M),dfile,append=TRUE)
data<-rbind(data,c(ssy,ssx,modx,mody,rat,wu/M,we/M,wf1/M,wf2/M,wf3/M,cpu/M,cpe/M,cpf1/M,cpf3/M,cpf3/M))

	#		cat(sizex, " ", sizex, " ", rat, " ", we/M, " ", cpe/M, '\n')
	}
}


#cat(".")
}
cat(".")
}
#cat("\n")
cat("\n")
}