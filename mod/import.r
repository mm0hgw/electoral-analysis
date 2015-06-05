#
#	Import functions
#
require(foreach)

# read custom scrubbed format
read_custom_csv <- function(file){
        bal<-read.csv(file)
        if(is.null(bal$V)||is.null(bal$N)||is.null(bal$VP)||is.null(bal$NP)||is.null(bal$name)){
                print(paste("Crisis! file",file,"makes crisis!"))
                return(NULL)
        }
        if(is.null(bal$Region)){
                Region <- rep("",length(bal$name))
        }else{
                Region <- bal$Region
        }
        ballot<-list(
                name=as.character(bal$name),
                Region=as.factor(Region),
                N=really_strip_whitespace(bal$N),
                V=really_strip_whitespace(bal$V),
                NP=really_strip_whitespace(bal$NP),
                VP=really_strip_whitespace(bal$VP))
        out<-cook_ballot(ballot,file)
	return(out)
}

# find LE2014 files
find_LE2014_files<-function(){
	p<-sub(list.files(path="csv/",pattern="Local elections 2014"),pattern="^",replacement="csv/")
	return(p)
}


#establish horizontal reading key
find_LE2014_key<-function(file="csv/Local elections 2014 - Electoral data - UNITARIES.csv"){
	p<-read.csv(file,header=F,stringsAsFactors=F)[c(1,2),]
	first<-p[1,]
	key<-rep(as.integer(0),6)
	key[1]<-grep(first,pattern="Ward")
	key[2]<-grep(first,pattern="Local Authority")
	second<-p[2,]
	key[3]<-grep(second,pattern="Electorate")
	key[4]<-grep(second,pattern="Total votes cast")
	key[5]<-grep(second,pattern="Number of postal ballot papers issued")
	key[6]<-grep(second,pattern="Number of postal votes included in the count")
	return(key)
}

# whitespace stripping function
really_strip_whitespace<-function(x){
	y<-as.character(x)
	z<-gsub(" |,","",y)
	out<-as.numeric(z)
	if(sum(is.na(out))>0){
		print(z[is.na(out)])
	}
	return(out)
}

read_all_LE2014_files<-function(){
	p<-find_LE2014_files()
	foreach(n=p,
		.combine=rbind,
		.inorder=F,
		.multicombine=T)%dopar%{
		read_LE2014_ballot(n)
	}
}

# import LE2014 ballot file
read_LE2014_ballot <- function(file){
	key <- find_LE2014_key(file)
	if(sum(key==0)>0){
		return()
	}
        p<-read.csv(file,strip.white=T,sep=",",stringsAsFactors=F,header=T)[,key]
	i<-p[,1]!=""
        p2<-p[p[,1]!=""&p[,3]!=""&p[,3]!="0",]
	
        ballot<-list(
                name=p2[,1],
                Region=as.factor(p2[,2]),
                N=really_strip_whitespace(p2[,3]),
                V=really_strip_whitespace(p2[,4]),
                NP=really_strip_whitespace(p2[,5]),
                VP=really_strip_whitespace(p2[,6]))
        return(cook_ballot(ballot,file))
}

# pull custom csv data
read_all_custom_csv<-function(){
	p<-paste("csv/",list.files(path="csv/",pattern="--.csv$"),sep="")
	q<-foreach(n=grep(p,pattern=".csv$",value=T),
		.combine=rbind,
		.inorder=F,
		.multicombine=T)%dopar%{
		read_custom_csv(n)
	}
	return(q)
}

