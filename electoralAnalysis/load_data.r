# setup data read options
require(gdata)
require(plyr)
require(foreach)
options(stringsAsFactors=FALSE)

banned_parties = "(^Ind|^$|^Speaker$|^no description$)"

ban_parties <- function(x){
	ban_list <- grep(banned_parties,x)
	if(length(ban_list)==0)return(x)
	x[!(x%in%ban_list)]
}

# ballot trimming function.
# Independent candidates aren't a party
# Parties need at least 7 candidates
trim_ballot<-function(b){
	out<-b[,colSums(b!=0)>5]
	bl<-grep(banned_parties,colnames(out))
	if (length(bl)>0) out<-out[,-bl]
	as.data.frame(out)
}

trim_white_space <- function(s){
	gsub(" *$","",s)
}

trim_number <- function(s){
	as.numeric(gsub("^$","0",gsub(",* *","",s)))
}

# (b) ballot needs counting areas as rownames
# (r) result needs format [,c("PANo","party","votes")] 
result_sorter <- function(b,r){
	k<-factor(r[,2])
	parties<-ban_parties(levels(k)[count(k)[,2]>5])
	
	out<-foreach(p=parties,
		.combine=cbind
	)%do%{
		n <- k %in% p
		o <- rep(0,nrow(b))
		mask <- r[n,1]
		o[mask] <- trim_number(r[n,3])
		o
	}
	colnames(out)<-parties
	as.data.frame(out)
}

# Load data

read_GE2005 <- function(){
		# Load region data
	GE2005_reg<-read.xls(sheet=3,
		"data/org/General-election-2005---postal-voting,-proxies-and-spoilts.xls"
	)[c(-seq(1,4)),1]

		# read constituency/electorate, party, vote tally, candidate name columns
	GE2005R<-read.xls(
		"data/org/Generalelection2005_A-Zconstituencyresults_18784-13893__E__N__S__W__.xls"
	)[-seq(1,7),c(1,4,5)]

		# read constituency start indices
	index<-grep("\\[\\d*\\]",GE2005R[,1])

		# read names
	GE2005_rn<-gsub(" *\\[\\d+]$","",GE2005R[index,1])

		# read electorate
	GE2005_N1<-GE2005R[index+1,1]
	GE2005_N1[GE2005_N1==""]<-GE2005R[index[GE2005_N1==""]+2,1]
	GE2005_N<-trim_number(GE2005_N1)
	rm(GE2005_N1)

		# read constituency end indices
	index2<-grep("^-+$",GE2005R[,3])

		# strip unused rows
	GE2005R<-GE2005R[
		-c(	unlist(lapply(	seq(length(index)-1),
					function(x){
						seq(index2[x],index[x+1]-1)
					}
				)
			),
			seq(index2[length(index2)],nrow(GE2005R))
		),
	]

		# convert data
	GE2005R[,2]<-trim_white_space(GE2005R[,2])
	GE2005R[,3]<-trim_number(GE2005R[,3])

		# correct indices
	index<-grep("\\[\\d*\\]",GE2005R[,1])
	key<-function(i)sum(i>=index)
	GE2005R[,1]<-sapply(seq(nrow(GE2005R)),key)

		# read turnout data
	GE2005_V <- as.vector(unlist(lapply(split(GE2005R[,3],
				sapply(seq(nrow(GE2005R)),
					key)
				),
				sum
			)
		)
	)

		# construct data frame		
	GE2005C<-data.frame(Region=as.factor(GE2005_reg),
		N=GE2005_N,
		V=GE2005_V,
		row.names=GE2005_rn
	)
	trim_ballot(cbind(GE2005C,result_sorter(GE2005C,GE2005R)))
}

#GE2005S<-GE2005[
#	GE2005$Region=="Scotland",
#	colSums(GE2005[GE2005$Region=="Scotland",]!=0)>5
#]

read_GE2010 <- function(){
		# Load GE2010 data
	GE2010<-read.xls(
		"data/org/GE2010-results-flatfile-website.xls"
	)[-651,-c(1,4)]
	GE2010[is.na(GE2010)]<-0
	rownames(GE2010)<-trim_white_space(GE2010[,1])
	colnames(GE2010)[c(3,4)]<-c("N","V")
	trim_ballot(GE2010[,-1])
}

GE2015_fix<-list(
	SDLP="SDLP",
	Socialist="Socialist Labour",
	UKIP="(UK *I *P|UK Independence Party)",
	Conservative="Conservative",
	Labour="Labour",
	SNP="SNP",
	Lib.Dem="Liberal Democrat",
	Green="Green Party",
	Pirate="Pirate"
)


# Load GE2015 data
read_GE2015C <- function(){
	GE2015C<-read.csv(
		"data/org/2015-UK-General-election-data-collated-results-Constituency.csv"
	)[seq(1,650),c(2,3,5,6)]
	colnames(GE2015C)<-c("Name","ID","N","V")
	rownames(GE2015C)<-trim_white_space(GE2015C$Name)
	GE2015C$V<-trim_number(GE2015C$V)
	GE2015C$N<-trim_number(GE2015C$N)
	return(GE2015C[,-c(1)])
}

read_GE2015R <- function(){
	Sys.setlocale('LC_ALL','C')
	GE2015R<-read.csv(
		"data/org/2015-UK-General-election-data-collated-results-Results.csv"
	)[-3972,c(5,3,6)]
	GE2015R[,2]<-trim_white_space(GE2015R[,2])
	foreach(p=GE2015_fix,
		n=names(GE2015_fix)
	)%do%{
		GE2015R[grep(p,GE2015R[,2]),2]<-n
	}
	return(GE2015R)
}

read_GE2015 <- function(){
	GE2015C<-read_GE2015C()
	GE2015R<-read_GE2015R()
	GE2015R2<-trim_ballot(result_sorter(GE2015C,GE2015R))
	GE2015R3<-cbind(GE2015C,GE2015R2)
	return(GE2015R3)
}
	# Load SIR2014 data
read_SIR2014<-function()read.csv("data/SIR2014.csv")[,-c(1,2,6,7,seq(9,15))]

	# Load SP2007 data
read_SP2007<-function(){
	read_SP2007C<-function(){
		SP2007C<-read.xls(
			"data/org/Scotland-Parl-Elec-2007-Turnout-Electorate-Constituency.xls"
		)[seq(2,74),c(2,3,5)]
		rownames(SP2007C)<-trim_white_space(SP2007C[,1])
		SP2007C<-SP2007C[,-1]
		colnames(SP2007C)<-c("N","V")
		SP2007C$N<-trim_number(SP2007C$N)
		SP2007C$V<-trim_number(SP2007C$V)
		return(SP2007C)
	}
	read_SP2007R<-function(){
		SP2007R<-read.xls(
			"data/org/Scotland-Parl-2007-Candidate-Constituency.xls"
		)[-1,c(1,4,5)]
		colnames(SP2007R)<-c("PANo","Party","V")
		SP2007R[,1]<-trim_white_space(gsub("\\.","",SP2007R[,1]))
		SP2007R[,2]<-trim_white_space(SP2007R[,2])
		SP2007R[,3]<-trim_number(SP2007R[,3])
		for(i in seq(nrow(SP2007R))){
			if(SP2007R[i,1]==""){
				SP2007R[i,1]<-SP2007R[i-1,1]
			}
		}
		SP2007R[,1]<-trim_number(SP2007R[,1])
		return(SP2007R)
	}	
	SP2007C<-read_SP2007C()
	SP2007R<-read_SP2007R()
	SP2007R2<-result_sorter(SP2007C,SP2007R)
	SP2007<-trim_ballot(
		cbind(	SP2007C,
			SP2007R2
		)
	)
	return(SP2007)
}


	#Load SP2011 data
read_SP2011<-function(){
	SP2011<-read.xls(
		"data/org/SP-2011-results-flat-file-WEB.xlsx"
	)[,-c(3,5,6,11,12)]
	rownames(SP2011)<-SP2011[,1]
	SP2011<-SP2011[,-1]
	colnames(SP2011)[c(1,2)]<-c("N","V")
	for(i in seq(ncol(SP2011))){
		SP2011[,i]<-trim_number(SP2011[,i])
	}
	return(SP2011)
}

	# Load SP2016 data
read_SP2016<-function(){
	SP2016<-read.csv(
		"data/org/RESULTS Constituency - Electoral Data and Results - Scottish Parliament elections - May 2016.csv"
	)[seq(2,74),c(2,3,4,6,10,11,12,13)]
	colnames(SP2016)<-c("Name","Region","N","V","CON","LAB","LIB","SNP")
	rownames(SP2016)<-SP2016[,1]
	SP2016<-SP2016[,-1]
	for(i in seq(2,ncol(SP2016))){
		SP2016[,i]<-trim_number(SP2016[,i])
	}
	SP2016
}

	# Load EUR2016 data
read_EUR2016<-function(){
	EUR2016<-read.csv(
		"data/org/EU-referendum-result-data.csv"
	)[,c(3,5,6,11,12,13)]
	colnames(EUR2016)[c(3,4)]<-c("N","V")
	EUR2016
}

	# Load VSR2011 data
read_VSR2011<-function(){
	VSR2011<-read.csv(
		"data/org/Voting-System-Referendum-results.csv"
	)[,c(2,4,5,12,7,9)]
	colnames(VSR2011)[c(3,4)]<-c("N","V")
	VSR2011$N<-as.numeric(gsub(",","",VSR2011$N))
	VSR2011$V<-as.numeric(gsub(",","",VSR2011$V))
	VSR2011$Yes<-as.numeric(gsub(",","",VSR2011$Yes))
	VSR2011$No<-as.numeric(gsub(",","",VSR2011$No))
	VSR2011
}

read_Q80 <- function(){
	fix<-function(x)trim_number(gsub("\302\240","",x))
	Q80R<-read.csv("data/Quebec 1980.csv")
	Q80<-data.frame(
		Name=Q80R$Name,
		N=fix(Q80R$N),
		V=fix(Q80R$V),
		Non=fix(Q80R$Non),
		Oui=fix(Q80R$Oui)
	)
	Q80
}

read_Q92 <- function(){
	fix<-function(x)trim_number(gsub("\302\240","",x))
	Q92R<-read.csv("data/Quebec 1992.csv")
	Q92<-data.frame(
		Name=Q92R$Name,
		N=fix(Q92R$N),
		V=fix(Q92R$V),
		Non=fix(Q92R$Non),
		Oui=fix(Q92R$Oui)
	)
	Q92
}

read_Q95 <- function(){
	fix<-function(x)trim_number(gsub("\302\240","",x))
	Q95R<-read.csv("data/Quebec 1995.csv")
	Q95<-data.frame(
		Name=Q95R$Name,
		N=fix(Q95R$N),
		V=fix(Q95R$V),
		Non=fix(Q95R$Non),
		Oui=fix(Q95R$Oui)
	)
	Q95
}

if(!exists("UK_list")){
	UK_list <- list(GE2005=read_GE2005(),
		GE2010=read_GE2010(),
		GE2015=read_GE2015(),
		VSR2011=read_VSR2011(),
		EUR2016=read_EUR2016()
	)
}

if(!exists("SC_list")){
	SC_list <- list(
		GE2005S=trim_ballot(UK_list$GE2005[UK_list$GE2005$Region=="Scotland",]),
		GE2010S=trim_ballot(UK_list$GE2010[UK_list$GE2010$Region=="Scotland",]),
		GE2015S=trim_ballot(UK_list$GE2015[grep("S",UK_list$GE2015$ID),]),
		SP2007=read_SP2007(),
		SP2011=read_SP2011(),
		SP2016=read_SP2016(),
		VSR2011S=trim_ballot(UK_list$VSR2011[UK_list$VSR2011$Region=="Scotland",]),
		SIR2014=read_SIR2014(),
		EUR2016S=trim_ballot(UK_list$EUR2016[UK_list$EUR2016$Region=="Scotland",])
	)
}

if(!exists("Q_list")){
	Q_list <- list(
		Q1980=read_Q80(),
		Q1992=read_Q92(),
		Q1995=read_Q95()
	)
}