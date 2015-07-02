#
#	electoral analysis tools
#

source("mod/cluster.r")
source("mod/import.r")

#custom sd calculator based upon "Eq. S1" and "Eq. S2"
#in Klimek et al. (2009)
custom_sd <- function(x,center=mean(x)){
        sqrt(mean((x-center)^2))  
}
 
# specialised division that doesn't break on edge cases
calculate_a <- function(V,N){
                # create edge case masks
        mask0 <- N==0 # These will become 0 instead of NaN
        mask1 <- V>N  # These will become 1 instead of >1 
        a<-V/N
        a[mask0]<-0
        a[mask1]<-1
        return(a)  
}
 
# specialised subtraction handling edge cases
calculate_np <- function(total,postal){
        out <- total - postal
        out[postal==0] <- 0  
        out
}

# return turnout with mu normalised to 0 and sigma normalised to 1 
calculate_normalised_a <- function(V,N){
        a<-calculate_a(V,N)
        pop_mean <- calculate_a(sum(V),sum(N))
        pop_sd <- custom_sd(a,pop_mean)
        (a-pop_mean)/pop_sd
}
 
# apply normalised turnout to triple sub ballots
normalised_a <- function(b){
        total<- calculate_normalised_a(b$V,b$N)
        mask <- !is.na(b$NP)
        postal<- calculate_normalised_a(b$VP[mask],b$NP[mask])
        nonp <- calculate_normalised_a(
                calculate_np(b$V[mask],b$VP[mask]),
                calculate_np(b$N[mask],b$NP[mask]))
        list(total,postal,nonp)
}

# perform chisq test on density curve of normalised sample
custom_chisq <- function(d,...){
        sum((d$y-dnorm(d$x))^2)
}

# cook a list of files using the normalised turnout technique
cook_files <- function(files=list_csv_files(),breakdown=F){
        out<-foreach(file=files,   
                .combine=c,   
                .inorder=F,   
                .options.multicore=mcoptions,
                .export=c("normalised_a","custom_chisq")
        ) %dopar% {
                b<-read_ballot(file,do.cook=F)
                a<-normalised_a(b)
		if(breakdown==T){
                	names(a)<-paste(file,c("total","postal","non-postal"))
                	out<-unlist(lapply(mclapply(a,density),custom_chisq))
		}else{
			out<-custom_chisq(density(unlist(a)))
			names(out)<-file
		}
		out
        }
	out[order(out)]
}

main <- function(){
	pdf(file="chisq.pdf")
	sample<-cook_files(list_csv_files())
	write.csv(file="chisq.csv",sample)
	plot(density(sample))
	plot(ecdf(sample))
	dev.off()
}

plot_with_normal <- function(d,...){
	plot(d,...)
	lines(x=d$x,y=dnorm(d$x),col="magenta")
}

long_plot <- function(){
	pdf(file="longplot.pdf")
	file_list <- list_csv_files()
	sample<-lapply(file_list,FUN=function(x){
		out<-unlist(normalised_a(read_ballot(x,do.cook=F)))

		out
	})
	densities<-lapply(sample,density)
	names(densities)<-file_list
	foreach(d=densities,n=file_list)%do%{
		plot_with_normal(d,main=n)
	}
	dev.off()
}

# initialise cluster
if(!exists("cl")){
	cl <- makeCustomCluster()
}
