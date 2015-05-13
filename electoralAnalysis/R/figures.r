library(limit)
#	Figures for paper

# Figure one
# Normalised overlaid density curves of every ballot in the database

# custom sd
custom_sd <- function(x,center=mean(x)){
	sqrt(mean((x-center)^2))
}

# normalise a sample
normalise <- function(x,center=mean(x)){
	return((x-center)/custom_sd(x,center))
}

# calculate density object from ballot object (sample mean variant)
b2d_s <- function(b){
	density(normalise(b$V/b$N))
}

# calculate density object from ballot object (population mean variant)
b2d_p <- function(b){
	density(normalise(b$V/b$N,center=sum(b$V)/sum(b$N)))
}

# find 2d ranges of a list of density objects
d_range <- function(d_list){
	x_range <- range(foreach(d_obj=d_list,.combine=c)%do%{
		range(d_obj$x)
	})
	y_range <- range(foreach(d_obj=d_list,.combine=c)%do%{
		range(d_obj$y)
	})
	out<-rbind(x_range,y_range)
	colnames(out)<-c("min","max")
	out
}

# plot a list of density objects, use names
plot_density_list <- function(d_list,xlab="",main=""){
	#sanity
	if(length(names(d_list))==0){
		print("list objects are not named")
		return()
	}
 
	# setup
	limits <- d_range(d_list)
	x_lim <- limits["x_range",]
	y_lim <- limits["y_range",]
	g_domain <- seq(min(x_lim),max(x_lim),length.out=512)
	g_x <- g_domain
	g_y <- dnorm(x=g_domain)
	col_seq <- seq(2,length.out=length(d_list))
	
	# create Gaussian plot space with limits
	plot(x=g_x,y=g_y,xlim=x_lim,ylim=y_lim,type="l",xlab=xlab,ylab="Density",main=main)

	foreach(d_obj=d_list,d_col=col_seq)%do%{
		lines(d_obj,col=d_col)
	}
	legend("topleft",legend=c("Gaussian",names(d_list)),col=c(1,col_seq),pch=1)

}

figure_one <- function(b_list){
	#sanity
	if(length(names(d_list))==0){
		print("list objects are not named")
		return()
	}
	
	d_list <- lapply(b_list,FUN=b2d_s)
	plot_density_list(d_list,xlab="Normalised turnout",main="Normalised to sample mean")
}

figure_two <- function(b_list){
	#sanity
	if(length(names(d_list))==0){
		print("list objects are not named")
		return()
	}
	
	d_list <- lapply(b_list,FUN=b2d_p)
	plot_density_list(d_list,xlab="Normalised turnout",main="Normalised to population mean")
}

plot_density_list_b <- function(d_list,d_list2,xlab="",main=""){
	#sanity
	if(length(names(d_list))==0){
		print("list objects are not named")
		return()
	}
 
	# setup
	limits <- d_range(d_list)
	x_lim <- limits["x_range",]
	y_lim <- limits["y_range",]
	g_domain <- seq(min(x_lim),max(x_lim),length.out=512)
	g_x <- g_domain
	g_y <- dnorm(x=g_domain)
	col_seq <- seq(2,length.out=length(d_list))
	
	# create Gaussian plot space with limits
	plot(x=g_x,y=g_y,xlim=x_lim,ylim=y_lim,type="l",xlab=xlab,ylab="Density",main=main)

	foreach(d=d_list,d2=d_list2,d_col=col_seq)%do%{
		lines(d,col=d_col)
		lines(d2,col=d_col)
	}
	legend("topleft",legend=c("Gaussian",names(d_list)),col=c(1,col_seq),pch=1)

}

figure_one_b <- function(b_list){
	#sanity
	if(length(names(d_list))==0){
		print("list objects are not named")
		return()
	}
	
	d_list <- lapply(b_list,FUN=b2d_s)
	d_list2 <- lapply(b_list,FUN=b2d_p)
	plot_density_list_b(d_list,d_list2,xlab="Normalised turnout",main="Normalised to sample mean and population mean")
}


# Table One

table_one_line <- function(ballot){
	n <- length(ballot$N)
	a <- ballot$V/ballot$N
	pop_mean <- sum(ballot$V)/sum(ballot$N)
	pop_sd <- custom_sd(a,center=pop_mean)
	samp_mean <- mean(a)
	samp_sd <- sd(a)
	deviance <- abs(samp_mean - pop_mean)
	votes <- sum(ballot$V)
	electorate <- sum(ballot$N)
	c(n=n,
		SampMean=samp_mean,
#		SampSD=samp_sd,
		PopMean=pop_mean,
#		PopSD=pop_sd,
		AbsDeviance=deviance,
		Votes=votes,
		Electorate=electorate)
}

table_one <- function(b_list){
	#sanity
	if(length(names(b_list))==0){
		print("list objects are not named")
		return()
	}

	out<-foreach(b=b_list,.combine=rbind)%do%{
		table_one_line(b)
	}
	rownames(out)<-names(b_list)
	return(out)
}

#
# Unstuffer
#

# We assume, since stuffing inflates turnout, that ballot areas above 
# the sample mean are more likely to contain stuffed ballots. 

# ballot remover for ballots where the sample mean is higher than the 
# population mean. Here, the valid area with the loewest electorate
# has the ballot that will reduce deviation most.
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
ballot_s_remover <- function(b){
	return(b)
}

# ballot remover for ballots where the population mean is higher than 
# the sample mean. Here, the valid area with the highest electorate
# has the ballot that will reduce deviation most.
ballot_p_remover <- function(b){
	N <- b$N
	a <- b$V/N
	samp_mean <- mean(a)
	mask <- a<samp_mean
	N[mask] <- 0
	i <- which.max(N)
	b$V[i] <- b$V[i]-1
	return(b)
}

# while loop container, high sample mean variant
ballot_unstuffer <- function(b_in){
	b<-b_in
	if(sum(b$V)/sum(b$N)<mean(b$V/b$N)){
		condition<-function(W,V){sum(W)/sum(V)<mean(W/V)}
		mask_fn<-function(x){max(x)+1}
		select_fn<-which.min
	}else{
		condition<-function(W,V){sum(W)/sum(V)>mean(W/V)}
		mask_fn<-function(x){min(x)-1}
		select_fn<-which.max
	}

	while(condition(b$V,b$N)){
		a_mask <- b$V/b$N<sum(b$V)/sum(b$N)
		v_mask <- b$W/b$V<sum(b$W)/sum(b$V)
		N<-b$N
		mask<-a_mask|v_mask
		if(sum(mask)==length(mask)){
			mask<-a_mask
		}
		N[mask]<-mask_fn(N)
		i <- select_fn(N)
		b$V[i] <- b$V[i]-1
		b$W[i] <- b$W[i]-1
	}
	b$a<-b$V/b$N
	b$v<-b$W/b$V
	cat(paste(sum(b_in$V)-sum(b$V),"ballots removed ",
		(sum(b_in$V)-sum(b$V))/sum(b$N),"turnout change",
		(sum(b_in$W)-sum(b$W))/sum(b$V),"%W change\n"))
	return(b)
}

# while loop container, high population mean variant
ballot_unstuffer_p <- function(b){
	pop_mean <- sum(b$W)/sum(b$V)
	samp_mean <- mean(b$W/b$V)
	while(samp_mean<pop_mean){
		b <- ballot_p_remover(b)
		pop_mean <- sum(b$W)/sum(b$N)
		samp_mean <- mean(b$W/b$V)
	}
	return(b)
}

# grand unified ballot unstuffer
#ballot_unstuffer <- function(b){
#	pop_mean <- sum(b$W)/sum(b$V)
#	samp_mean <- mean(b$W/b$V)
#	if(pop_mean>samp_mean){
#		return(ballot_unstuffer_p(b))
#	}
#	if(samp_mean>pop_mean){
#		return(ballot_unstuffer_s(b))
#	}
#	return(b)
#}

# list application of ballot unstuffer
ballot_unstuffer_list <- function(b_list){
	out <- mclapply(b_list,
		FUN=ballot_unstuffer,
		mc.preschedule=FALSE,
		mc.cores=detectCores(),
		mc.silent=TRUE)
	out
}

chisq_test <- function(b){
	d <- density(normalise(b$V/b$N,center=sum(b$V)/sum(b$N)))
	y1 <- d$y
	y2 <- dnorm(d$x)
	sum((y1-y2)^2)
}

chisq_unstuffer <- function(b){
	n <- length(b$N)
	b1 <- b
	flag <- TRUE
	last <- chisq_test(b1)
	while(flag==TRUE){
		xlist<-foreach(i=icount(n),.combine=c)%do%{
			b2 <- b1
			b2$V[i] <- b2$V[i]-1
			out<-chisq_test(b2)
			return(out)
		}
		if(min(xlist)>last){
			flag=FALSE
		}else{
			j <- which.min(xlist)
			b1$V[j] <- b1$V[j]-1
			last<-min(xlist)
		}
	}
	b1
}

chisq_unstuffer_list <- function(b_list){
	out <- mclapply(b_list,FUN=chisq_unstuffer,mc.cores=detectCores(),mc.preschedule=FALSE)
	out
}
