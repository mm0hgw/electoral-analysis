#
#	Import functions
#
require(foreach)

keys <- read.table("keys.tab")

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
	out<-foreach(l=seq(1,l_len),
		.combine=c,
		.multicombine=T,
		.inorder=T,
		.options.multicore=mcoptions
	)%dopar%{
		max(p[,l])
	}
	out
}

# grep headers, find key
try_key <- function(filename,key){
	p<-read_headers(filename)
	l_len<-seq(1,length(p[,1]))
	foreach(l=l_len,
		.combine=key_build,
		.multicombine=T,
		.inorder=F,
		.options.multicore=mcoptions
	)%:%
	foreach(k=key,
		.combine=c,
		.multicombine=T,
		.inorder=T,
		.options.multicore=mcoptions
	)%dopar%{
		match<-(grep(pattern=k,x=p[l,]))
		if(any(match)){
			min(match)
		}else{
			0
		}
	}
}

# identify a valid key.
valid_key <- function(key){
	if(length(key)==6&
		key[1]!=0&
		key[2]!=0&
		key[3]!=0&
		key[4]!=0&
		key[5]!=0&
		key[6]!=0
	){
		return(TRUE)
	}
	return(FALSE)
}

# collect valid keys
key_collect<-function(...){
	p<-rbind(...)
	p_len<-length(p[,1])
	if(p_len>0){
		mask<-foreach(p_id=seq(1,p_len),
			.combine=c,
			.multicombine=T,
			.inorder=T,
			.options.multicore=mcoptions
		)%dopar%{
			valid_key(p[p_id,])
		}
		return(p[mask,])
	}
	return(vector())
}

# find valid keys for provided files
find_key <- function(filenames){
	k_len<-length(keys[1,])
	foreach(file=filenames,
		.combine=list,
		.multicombine=T,
		.inorder=T,
		.options.multicore=mcoptions
	)%:%
	foreach(key_id=seq(1,k_len),
		.inorder=F,
		.combine=key_collect,
		.multicombine=T,
		.options.multicore=mcoptions
	)%dopar%{
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

assemble_sample <- function(){
	files<-list_csv_files()
	file_keys<-find_key(files)
	l_files<-length(files)
	if(l_files>0){
		sort_sample(foreach(id=seq(1,l_files),
			.inorder=F,
			.combine=rbind,
			.multicombine=T,
			.options.multicore=mcoptions
		)%dopar%{
			key<-file_keys[[id]]
			out<-vector()
			if(valid_key(key)==T){
				file<-files[[id]]
				out<-read_ballot(file,key)
			}
			out
		})
	}
}

# whitespace stripping function
really_strip_whitespace<-function(x){
	y<-as.character(x)
	z<-gsub(" |,","",y)
	out<-as.numeric(z)
	return(out)
}

# import ballot file
read_ballot <- function(file,key=find_key(file)){
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
