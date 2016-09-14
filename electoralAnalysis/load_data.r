# setup data read options
require(gdata)
options(stringsAsFactors=FALSE)

banned_parties = "(^Ind|^$|^Speaker$)"

# ballot trimming function.
# Independent candidates aren't a party
# Parties need at least 7 candidates
trim_ballot<-function(b){
	n<-sapply(b[1,],is.numeric)
	print(head(n))
	n[sapply(names(n)[n],
		function(x){
			sum(b[,x]==0)>7
		}
	)]<-FALSE
	print(head(n))
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

# (b) ballot needs counting areas as rownames
# (r) result neds format [,c("keyname","party","votes")] 
result_sorter <- function(b,r){
	for(party in levels(factor(r[,2]))){
		n<-grep(party,r[,2])
		if(length(n)>7){
			b<-cbind(b,0)
			colnames(b)[ncol(b)]<-party
			b[r[n,1],ncol(b)]<-trim_number(r[n,3])
		}
	}
	b
}

# Load data

# Load GE2005 data
if(!exists("GE2005")){
	GE2005_reg<-read.xls(sheet=3,
		"data/org/General-election-2005---postal-voting,-proxies-and-spoilts.xls"
	)[c(-seq(1,4)),1]
	
	
#	colnames(GE2005_old)<-c("Region","Name","N","V")
#	GE2005_old$N<-trim_number(GE2005_old$N)
#	GE2005_old$V<-trim_number(GE2005_old$V)
#	rownames(GE2005_old)<-trim_white_space(GE2005_old[,2])
#	GE2005_old<-GE2005_old[,-2]

	# read constituency/electorate, party, vote tally, candidate name columns
	GE2005R<-read.xls(
		"data/org/Generalelection2005_A-Zconstituencyresults_18784-13893__E__N__S__W__.xls"
	)[-seq(1,7),c(1,4,5,3)]

	# read constituency start indices
	index<-grep("\\[\\d*\\]",GE2005R[,1])
	
	# read names
	GE2005_rn<-GE2005R[index,1]

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
	GE2005R[,2]<-factor(trim_white_space(GE2005R[,2]))
	GE2005R[,3]<-trim_number(GE2005R[,3])

	# correct indices
	index<-grep("\\[\\d*\\]",GE2005R[,1])
	rm(index2)

	# read turnout data
	key<-function(i)sum(i>=index)
	GE2005_V <- as.vector(unlist(lapply(split(GE2005R[,3],
				sapply(seq(nrow(GE2005R)),
					key)
				),
				sum
			)
		)
	)
	parties<-levels(GE2005R[,2])
	parties<-parties[-grep(banned_parties,parties)]
	GE2005i<-data.frame(Region=as.factor(GE2005_reg),
		N=GE2005_N,
		V=as.vector(GE2005_V),
		row.names=GE2005_rn
	)
	GE2005<-foreach(
		p=parties,
		.combine=cbind,
		.init=GE2005i
	)%do%{
		i<-grep(p,GE2005R[,2])
		out<-rep(0,length(GE2005_N))
		out[sapply(i,key)]<-GE2005R[i,3]
		out
	}
	colnames(GE2005)[-seq(1,3)]<-parties
#	GE2005<-trim_ballot(GE2005)
}

if(!exists("GE2005S")){
	GE2005S<-GE2005[GE2005$Region=="Scotland",colSums(GE2005[GE2005$Region=="Scotland",]!=0)>5]
	GE2005S<-cbind(GE2005S,
		nonSNP=GE2005S$V-GE2005S$SNP
	)
}

# Load GE2010 data
if(!exists("GE2010")){
	GE2010<-read.xls(
		"data/org/GE2010-results-flatfile-website.xls"
	)[-651,-c(1,4)]
	GE2010[is.na(GE2010)]<-0
	colnames(GE2010)[c(3,4)]<-c("N","V")
#	GE2010<-trim_ballot(GE2010)
}

if(!exists("GE2010S")){
	GE2010S<-GE2010[GE2010$Region=="Scotland",colSums(GE2010[GE2010$Region=="Scotland",]!=0)>5]
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
	rownames(GE2015)<-trim_white_space(GE2015[,1])
	GE2015<-GE2015[,-1]
	GE2015R<-read.csv(
		"data/org/2015-UK-General-election-data-collated-results-Results.csv"
	)[-3972,c(1,2,4,3,6)]
}

if(!exists("GE2015S")){
#	GE2015R<-read.csv("data/org/2015-UK-General-election-data-collated-results-Results.csv")
	GE2015S<-cbind(GE2015[grep("S",GE2015$Constituency.ID),],
		SNP=GE2015R[grep("SNP",GE2015R$Description.on.ballot.paper),]$Votes
	)
	GE2015S<-cbind(GE2015S,
		nonSNP=GE2015S$V-GE2015S$SNP
	)

}

# Load SIR2014 data
if(!exists("SIR2014")){
	SIR2014<-read.csv("data/SIR2014.csv")[,-c(1,2,6,7,seq(9,15))]
}

# Load SP2007 data
if(!exists("SP2007")){
	SP2007<-read.xls(
		"data/org/Scotland-Parl-Elec-2007-Turnout-Electorate-Constituency.xls"
	)[seq(2,74),c(2,3,5)]
	rownames(SP2007)<-trim_white_space(SP2007[,1])
	SP2007<-SP2007[,-1]
	colnames(SP2007)<-c("N","V")
	SP2007$N<-trim_number(SP2007$N)
	SP2007$V<-trim_number(SP2007$V)
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
	SP2007<-result_sorter(SP2007,SP2007R[,c(1,3,4)])
	SP2007<-cbind(SP2007,
		nonSNP=SP2007$V-SP2007$SNP
	)
	SP2007<-trim_ballot(SP2007)
}

#Load SP2011 data
if(!exists("SP2011")){
	SP2011<-read.xls(
		"data/org/SP-2011-results-flat-file-WEB.xlsx"
	)[,-c(3,5,6,11,12)]
	rownames(SP2011)<-SP2011[,1]
	SP2011<-SP2011[,-1]
	colnames(SP2011)[c(1,2)]<-c("N","V")
	for(i in seq(ncol(SP2011))){
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
	rownames(SP2016)<-SP2016[,1]
	SP2016<-SP2016[,-1]
	for(i in seq(2,ncol(SP2016))){
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
