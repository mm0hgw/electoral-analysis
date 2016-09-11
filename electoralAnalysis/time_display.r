# setup data read options
require(moments)
require(gdata)
options(stringsAsFactors=FALSE)

# Load data

# Load GE2005 data
GE2005<-read.xls(sheet=3,"data/org/General-election-2005---postal-voting,-proxies-and-spoilts.xls",stringsAsFactors=FALSE)[-seq(1,4),seq(1,4)]
colnames(GE2005)<-c("Region","Name","N","V")
GE2005$N<-as.numeric(GE2005$N)
GE2005$V<-as.numeric(GE2005$V)
GE2005S<-GE2005[GE2005$Region=="Scotland",]
GE2005R<-read.xls("data/org/Generalelection2005_A-Zconstituencyresults_18784-13893__E__N__S__W__.xls",stringsAsFactors=FALSE)[-seq(1,7),seq(1,5)]
GE2005S<-cbind(GE2005S,SNP=as.numeric(gsub(",","",(GE2005R[GE2005R$X.2=="SNP",])[,5])))

# Load GE2010 data
GE2010<-read.xls("data/org/GE2010-results-flatfile-website.xls")[-651,]
colnames(GE2010)[c(5,6)]<-c("N","V")
GE2010S<-GE2010[GE2010$Region=="Scotland",]
GE2010S<-GE2010S[,colSums(!is.na(GE2010S))!=0]

# Load GE2015 data
GE2015<-read.csv("data/org/2015-UK-General-election-data-collated-results-Constituency.csv")[,c(2,3,5,6)]
colnames(GE2015)[c(3,4)]<-c("N","V")
GE2015R<-read.csv("data/org/2015-UK-General-election-data-collated-results-Results.csv")
GE2015S<-cbind(GE2015[grep("S",GE2015$Constituency.ID),],SNP=GE2015R[grep("SNP",GE2015R$Description.on.ballot.paper),]$Votes)

# Load SIR2014 data
SIR2014<-read.csv("data/SIR2014.csv")

# Load SP2007 data
SP2007<-read.xls("data/org/Scotland-Parl-Elec-2007-Turnout-Electorate-Constituency.xls")[seq(2,74),c(2,3,5)]
colnames(SP2007)<-c("Name","N","V")
SP2007$N<-as.numeric(gsub(",","",SP2007$N))
SP2007$V<-as.numeric(gsub(",","",SP2007$V))
SP2007R<-read.xls("data/org/Scotland-Parl-2007-Candidate-Constituency.xls")[,c(4,5)]
SP2007<-cbind(SP2007,SNP=as.numeric(gsub(",","",SP2007R[grep("SNP",SP2007R[,1]),2])))

#Load SP2011 data
SP2011<-read.xls("data/org/SP-2011-results-flat-file-WEB.xlsx")[,c(1,2,4,10)]
colnames(SP2011)[c(2,3)]<-c("N","V")
SP2011$N<-as.numeric(gsub(",","",SP2011$N))
SP2011$V<-as.numeric(gsub(",","",SP2011$V))
SP2011$SNP<-as.numeric(gsub(",","",SP2011$SNP))

# Load SP2016 data
SP2016<-read.csv("data/org/RESULTS Constituency - Electoral Data and Results - Scottish Parliament elections - May 2016.csv")[seq(2,74),c(2,4,6,13)]
colnames(SP2016)<-c("Name","N","V","SNP")
SP2016$N<-as.numeric(gsub(",","",SP2016$N))
SP2016$V<-as.numeric(gsub(",","",SP2016$V))
SP2016$SNP<-as.numeric(gsub(",","",SP2016$SNP))

# Load EUR2016 data
EUR2016<-read.csv("data/org/EU-referendum-result-data.csv")[,c(5,6,11,12,13)]
colnames(EUR2016)[c(2,3)]<-c("N","V")

# Define GE trend line
uf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V-GE2005S$SNP)/sum(GE2005S$N),
		sum(GE2010RS$V-GE2010RS$SNP)/sum(GE2010RS$N),
		sum(GE2015S$V-GE2015S$SNP)/sum(GE2015S$N)
	)
)

nf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$SNP)/sum(GE2005S$N),
		sum(GE2010RS$SNP)/sum(GE2010RS$N),
		sum(GE2015S$SNP)/sum(GE2015S$N)
	)
)

vf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V)/sum(GE2005S$N),
		sum(GE2010RS$V)/sum(GE2010RS$N),
		sum(GE2015S$V)/sum(GE2015S$N)
	)
)

ur_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V-GE2005S$SNP),
		sum(GE2010RS$V-GE2010RS$SNP),
		sum(GE2015S$V-GE2015S$SNP)
	)
)

nr_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$SNP),
		sum(GE2010RS$SNP),
		sum(GE2015S$SNP)
	)
)

vr_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V),
		sum(GE2010RS$V),
		sum(GE2015S$V)
	)
)

# Define SIR trend line
uf_SIR_line<-list(x=2014,y=sum(SIR2014$No)/sum(SIR2014$N))
nf_SIR_line<-list(x=2014,y=sum(SIR2014$Yes)/sum(SIR2014$N))
vf_SIR_line<-list(x=2014,y=sum(SIR2014$V)/sum(SIR2014$N))
ur_SIR_line<-list(x=2014,y=sum(SIR2014$No))
nr_SIR_line<-list(x=2014,y=sum(SIR2014$Yes))
vr_SIR_line<-list(x=2014,y=sum(SIR2014$V))

# SP trend lines
uf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V-SP2007$SNP)/sum(SP2007$N),
		sum(SP2011$V-SP2011$SNP)/sum(SP2011$N),
		sum(SP2016$V-SP2016$SNP)/sum(SP2016$N)
	)
)

nf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$SNP)/sum(SP2007$N),
		sum(SP2011$SNP)/sum(SP2011$N),
		sum(SP2016$SNP)/sum(SP2016$N)
	)
)

vf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V)/sum(SP2007$N),
		sum(SP2011$V)/sum(SP2011$N),
		sum(SP2016$V)/sum(SP2016$N)
	)
)

ur_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V-SP2007$SNP),
		sum(SP2011$V-SP2011$SNP),
		sum(SP2016$V-SP2016$SNP)
	)
)

nr_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$SNP),
		sum(SP2011$SNP),
		sum(SP2016$SNP)
	)
)

vr_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V),
		sum(SP2011$V),
		sum(SP2016$V)
	)
)

# define line groups

vr<-list(GE.turnout=vr_GE_line,SIR.turnout=vr_SIR_line,SP.turnout=vr_SP_line)
ur<-list(GE.unionist=ur_GE_line,SIR.unionist=ur_SIR_line,SP.unionist=ur_SP_line)
nr<-list(GE.nationalist=nr_GE_line,SIR.nationalist=nr_SIR_line,SP.nationalist=nr_SP_line)
vf<-list(GE.turnout=vf_GE_line,SIR.turnout=vf_SIR_line,SP.turnout=vf_SP_line)
uf<-list(GE.unionist=uf_GE_line,SIR.unionist=uf_SIR_line,SP.unionist=uf_SP_line)
nf<-list(GE.nationalist=nf_GE_line,SIR.nationalist=nf_SIR_line,SP.nationalist=nf_SP_line)

GE_fn <- function(b){
	list(overall.turnout=b$V,
		SNP.turnout=b$SNP,
		nonSNP.turnout=b$V-b$SNP
	)
}

GE_tag_fn <- function(GE,name,FUN){
	f<-a_fn_fn(GE$N)
	out<-lapply(
		GE_fn(GE),
		function(b){FUN(f(b))}
	)
	names(out) <- gsub("^",paste(name,".",sep=""),names(out))
	out
}

GE_list_fn <- function(GE_list,FUN=density){
	foreach(GE=GE_list,n=names(GE_list),.combine=c)%do%{
		out<-GE_tag_fn(GE,n,FUN)
	}
}

GE_list <- list(GE2005S=GE2005S,
	GE2010S=GE2010S,
	GE2015S=GE2015S,
	SP2007=SP2007,
	SP2011=SP2011,
	SP2016=SP2016
)

uGE_list <- list(GE2005=GE2005,
	GE2010=GE2010,
	GE2015=GE2015
)

GE_skewness <- unlist(GE_list_fn(GE_list,FUN=skewness))

S_fn <- a_fn_fn(SIR2014$N)

SIR_fn <-function(FUN){ list(SIR2014.overall.turnout=FUN(S_fn(SIR2014$V)),
	SIR2014.Yes.turnout=FUN(S_fn(SIR2014$Yes)),
	SIR2014.No.turnout=FUN(S_fn(SIR2014$No))
)}

SIR_sum<-c(SIR2014.overall.turnout=sum(SIR2014$V),
	SIR2014.Yes.turnout=sum(SIR2014$Yes),
	SIR2014.No.turnout=sum(SIR2014$No)
)

SIR_a<-c(SIR2014.overall.turnout=sum(SIR2014$V)/sum(SIR2014$N),
	SIR2014.Yes.turnout=sum(SIR2014$Yes)/sum(SIR2014$N),
	SIR2014.No.turnout=sum(SIR2014$No)/sum(SIR2014$N)
)


SIR<-SIR_fn(density)
SIR_skewness<-SIR_fn(skewness)

GE<-GE_list_fn(GE_list)

d_a<-c(SIR_a,GE_a)
d_list<-c(SIR,GE)
d_skewness<-unlist(c(SIR_skewness,GE_skewness))

chisq_fn <- function(d){
	sum((d$y-dnorm(d$x))^2)
}

d_chisq<-unlist(lapply(d_list,chisq_fn))

sum_fn<-function(GE){
	out<-colSums(GE[,c("V","SNP")])
	out<-c(out,out["V"]-out["SNP"])
	names(out)[3]<-"nonSNP"
	out
}

GE_sum<-foreach(d=lapply(GE_list,sum_fn),n=names(GE_list),.combine=c)%do%{ 
	out<-d
	names(out)<-gsub("^",paste(n,".",sep=""),names(out))
	out
}

GE_a_fn<-function(GE){
	out<-colSums(GE[,c("V","SNP")])/sum(GE[,"N"])
	out<-c(out,out[1]-out[2])
	names(out)[3]<-"nonSNP"
	out
}

GE_a<-foreach(d=lapply(GE_list,GE_a_fn),n=names(GE_list),.combine=c)%do%{ 
	out<-d
	names(out)<-gsub("^",paste(n,".",sep=""),names(out))
	out
}

d_sum<-c(SIR_sum,GE_sum)
d_a<-c(SIR_a,GE_a)

# plot chart
chart <- function(line_list,...){
	n<-length(line_list)
	if(n==0){return()}
	yl<-foreach(l=line_list,.combine=limits)%do%{limits(l$y)}
	xl<-foreach(l=line_list,.combine=limits)%do%{limits(l$x)}
	plot(line_list[[1]],xlim=xl,ylim=yl,...)
	foreach(l=line_list,i=seq(n))%do%{lines(l,col=i,pch=i,...)}
	legend("topleft",legend=names(line_list),col=seq(n),pch=seq(n))
}

r_chart<-function(){chart(c(nr,vr,ur),type="b",lwd=5)}

f_chart<-function(){chart(c(nf,vf,uf),type="b",lwd=5)}

chisq_chart <- function(n=6,decreasing=TRUE){
	chart(d_list[head(order(d_chisq,decreasing=decreasing),n)],lwd=5)
}

EU_GE_line <- list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005$V)/sum(GE2005$N),
		sum(GE2010$V)/sum(GE2010$N),
		sum(GE2015$V)/sum(GE2015$N)
	)
)
