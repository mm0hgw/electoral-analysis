# setup data read options
require(gdata)
options(stringsAsFactors=FALSE)

# ballot trimming function.
# Independent candidates aren't a party
# Parties need at least 7 candidates
trim_ballot<-function(b){
	n<-colSums(b[,sapply(b[1,],is.numeric)])!=0
	n[sapply(names(n)[n],
		function(x){
			sum(b[,x]!=0)<7
		}
	)]<-FALSE
	n[grep("^Ind.$",names(n))]<-FALSE
	n[grep("^Independent$",names(n))]<-FALSE
	b[,c(colnames(b)[!sapply(b[1,],is.numeric)],
		names(n)[n]
	)]
}

trim_white_space <- function(s){
	gsub(" *$","",s)
}

trim_number <- function(s){
	as.numeric(gsub("^$","0",gsub(",* *","",s)))
}

# Load data

# Load GE2005 data
if(!exists("GE2005")){
	GE2005<-read.xls(sheet=3,
		"data/org/General-election-2005---postal-voting,-proxies-and-spoilts.xls"
	)[-seq(1,4),seq(1,4)]
	colnames(GE2005)<-c("Region","Name","N","V")
	GE2005$N<-as.numeric(GE2005$N)
	GE2005$V<-as.numeric(GE2005$V)
}

if(!exists("GE2005S")){
	GE2005R<-read.xls(
		"data/org/Generalelection2005_A-Zconstituencyresults_18784-13893__E__N__S__W__.xls"
	)[-seq(1,7),seq(1,5)]
	GE2005S<-cbind(GE2005[GE2005$Region=="Scotland",],
		SNP=as.numeric(gsub(",","",(GE2005R[GE2005R$X.2=="SNP",])[,5]))
	)
	GE2005S<-cbind(GE2005S,
		nonSNP=GE2005S$V-GE2005S$SNP
	)
	
	rm("GE2005R")
}

# Load GE2010 data
if(!exists("GE2010")){
	GE2010<-read.xls(
		"data/org/GE2010-results-flatfile-website.xls"
	)[-651,-c(1,4)]
	GE2010[is.na(GE2010)]<-0
	colnames(GE2010)[c(3,4)]<-c("N","V")
	GE2010<-trim_ballot(GE2010)
}

if(!exists("GE2010S")){
	GE2010S<-trim_ballot(GE2010[GE2010$Region=="Scotland",])
	GE2010S<-cbind(GE2010S,
		nonSNP=GE2010S$V-GE2010S$SNP
	)
}

# Load GE2015 data

if(!exists("GE2015")){
	GE2015<-read.csv(
		"data/org/2015-UK-General-election-data-collated-results-Constituency.csv"
	)[,c(2,3,5,6)]
	colnames(GE2015)[c(3,4)]<-c("N","V")
}

if(!exists("GE2015S")){
	GE2015R<-read.csv("data/org/2015-UK-General-election-data-collated-results-Results.csv")
	GE2015S<-cbind(GE2015[grep("S",GE2015$Constituency.ID),],
		SNP=GE2015R[grep("SNP",GE2015R$Description.on.ballot.paper),]$Votes
	)
	GE2015S<-cbind(GE2015S,
		nonSNP=GE2015S$V-GE2015S$SNP
	)

	rm("GE2015R")
}

# Load SIR2014 data
if(!exists("SIR2014")){
	SIR2014<-read.csv("data/SIR2014.csv")
}

# Load SP2007 data
if(!exists("SP2007")){
	SP2007<-read.xls(
		"data/org/Scotland-Parl-Elec-2007-Turnout-Electorate-Constituency.xls"
	)[seq(2,74),c(2,3,5)]
	colnames(SP2007)<-c("Name","N","V")
	SP2007$N<-as.numeric(gsub(",","",SP2007$N))
	SP2007$V<-as.numeric(gsub(",","",SP2007$V))
	rownames(SP2007)<-trim_white_space(SP2007[,1])
	SP2007<-SP2007[,-1]
	SP2007R<-read.xls("data/org/Scotland-Parl-2007-Candidate-Constituency.xls")[-1,-c(1,6)]
	colnames(SP2007R)<-c("Name","Candiate","Party","V")
	SP2007R[,1]<-trim_white_space(SP2007R[,1])
	SP2007R[,3]<-trim_white_space(SP2007R[,3])
	SP2007R[,4]<-trim_number(SP2007R[,4])
	for(i in seq(nrow(SP2007R))){
		if(SP2007R[i,1]==""){
			SP2007R[i,1]<-SP2007R[i-1,1]
		}
	}
	for(party in levels(factor(SP2007R[,3]))){
		if(length(n<-grep(party,SP2007R[,3]))>5){
			SP2007<-cbind(SP2007,
				rep(0,nrow(SP2007))
			)
			colnames(SP2007)[ncol(SP2007)]<-party
			SP2007[SP2007R[n,1],ncol(SP2007)]<-SP2007R[n,4]
		}
	}
	SP2007<-trim_ballot(SP2007)
	#SP2007<-cbind(SP2007,SNP=as.numeric(gsub(",","",SP2007R[grep("SNP",SP2007R[,1]),2])))
	#SP2007<-cbind(SP2007,
	#	nonSNP=SP2007$V-SP2007$SNP
	#)

}

#Load SP2011 data
if(!exists("SP2011")){
	SP2011<-read.xls(
		"data/org/SP-2011-results-flat-file-WEB.xlsx"
	)[,-c(3,5,6,11,12)]
	colnames(SP2011)[c(2,3)]<-c("N","V")
	for(i in seq(2,ncol(SP2011))){
		SP2011[,i]<-trim_number(SP2011[,i])
	}
	SP2011<-cbind(SP2011,
		nonSNP=SP2011$V-SP2011$SNP
	)
}

# Load SP2016 data
if(!exists("SP2016")){
	SP2016<-read.csv(
		"data/org/RESULTS Constituency - Electoral Data and Results - Scottish Parliament elections - May 2016.csv"
	)[seq(2,74),c(2,3,4,6,10,11,12,13)]
	colnames(SP2016)<-c("Name","Region","N","V","CON","LAB","LIB","SNP")
	for(i in seq(3,ncol(SP2016))){
		SP2016[,i]<-trim_number(SP2016[,i])
	}
	SP2016<-cbind(SP2016,
		nonSNP=SP2016$V-SP2016$SNP
	)
}

# Load EUR2016 data
if(!exists("EUR2016")){
	EUR2016<-read.csv(
		"data/org/EU-referendum-result-data.csv"
	)[,c(3,5,6,11,12,13)]
	colnames(EUR2016)[c(3,4)]<-c("N","V")
}

if(!exists("EUR2016S")){
	EUR2016S<-EUR2016[EUR2016$Region=="Scotland",]
}


# Load VSR2011 data
if(!exists("VSR2011")){
	VSR2011<-read.csv(
		"data/org/Voting-System-Referendum-results.csv"
	)[,c(2,4,5,12,7,9)]
	colnames(VSR2011)[c(3,4)]<-c("N","V")
	VSR2011$N<-as.numeric(gsub(",","",VSR2011$N))
	VSR2011$V<-as.numeric(gsub(",","",VSR2011$V))
	VSR2011$Yes<-as.numeric(gsub(",","",VSR2011$Yes))
	VSR2011$No<-as.numeric(gsub(",","",VSR2011$No))
}

if(!exists("VSR2011S")){
	VSR2011S<-VSR2011[grep("Scotland",VSR2011$Region),]
}
