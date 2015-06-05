#
#	Import functions
#
require(foreach)

# key constant for grepping files
LE2014_key <- c("Ward",
	"Local Authority",
	"Electorate",
	"Total votes cast",
	"Number of postal ballot papers issued",
	"Number of postal votes included in the count")

custom_key <- c("^name$",
	"^Region$",
	"^N$",
	"^V$",
	"^NP$",
	"^VP$")

keys <- cbind(LE2014_key,
	custom_key)

# read headers
read_headers <- function(filename){
	p<-read.csv(filename,header=F,stringsAsFactors=F)[c(1,2),]
	return(p)
}

# rbind into table and return maximums from columns
key_build <- function(...){
	p<-rbind(...)
	l_len<-length(p[1,])
	if(l_len==0){
		return()
	}
	out<-foreach(l=seq(1,l_len),.combine=c,.multicombine=T,.inorder=T)%do%{
		max(p[,l])
	}
	out
}

# grep headers, find key
try_key <- function(filename,key){
	p<-read_headers(filename)
	l_len<-seq(1,length(p[,1]))
	foreach(l=l_len,.combine=key_build,.multicombine=T,.inorder=F)%:%
	foreach(k=key,.combine=c,.multicombine=T,.inorder=T)%do%{
		match<-(grep(pattern=k,x=p[l,]))
		if(any(match)){
			min(match)
		}else{
			0
		}
	}
}

# identify a valid key. Keys are valid without a Region field
valid_key <- function(key){
	if(key[1]!=0&key[2]!=0&key[3]!=0&key[4]!=0&key[5]!=0&key[6]!=0){
		return(TRUE)
	}
	return(FALSE)
}

# collect valid keys
key_collect<-function(...){
	p<-rbind(...)
	p_len<-length(p[,1])
	if(p_len>0){
		mask<-foreach(p_id=seq(1,p_len),.combine=c,.multicombine=T,.inorder=T)%do%{
			valid_key(p[p_id,])
		}
		return(p[mask,])
	}
	return(vector())
}

# find valid keys for provided files
find_key <- function(filenames){
	k_len<-length(keys[1,])
	foreach(file=filenames,.combine=list,.multicombine=T,.inorder=T)%:%
	foreach(key_id=seq(1,k_len),.inorder=F,.combine=key_collect,.multicombine=T)%do%{
		k<-try_key(file,keys[,key_id])
		if(valid_key(k)==T){
			k
		}else{
			0
		}
	}
}

# list all csv files
list_csv_files <- function(){
	paste(sep="","csv/",list.files(path="csv/",pattern=".csv$"))
}

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

assemble_sample <- function(){
	files<-list_csv_files()
	file_keys<-find_key(files)
	l_files<-length(files)
	if(l_files>0){
		foreach(id=seq(1,l_files),.combine=rbind,.multicombine=T)%do%{
			key<-file_keys[[id]]
			out<-vector()
			if(valid_key(key)==T){
				file<-files[[id]]
				out<-read_LE2014_ballot(file,key)
			}
			out
		}
	}
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
	return(out)
}

read_all_LE2014_files<-function(){
	p<-find_LE2014_files()
	foreach(n=p,
		.combine=rbind,
		.inorder=F,
		.multicombine=T)%do%{
		read_LE2014_ballot(n)
	}
}

# import LE2014 ballot file
read_LE2014_ballot <- function(file,key=find_LE2014_key(file)){
	if(!valid_key(key)){
		return()
	}
        p<-read.csv(file,strip.white=T,sep=",",stringsAsFactors=F,header=T)[,key]
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
		.multicombine=T)%do%{
		read_custom_csv(n)
	}
	return(q)
}

