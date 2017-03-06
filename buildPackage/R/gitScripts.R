hashFinder <- function(line)if(nchar(line[1])==40){TRUE}else{FALSE}
sizeSorter <- function(line)as.integer(line[5])

readHashes <- function(){
	z1<-system('git verify-pack -v .git/objects/pack/pack-*.idx',
		intern=TRUE
	)
	z2<-strsplit(z1,' ')
	z3<-z2[sapply(z2,hashFinder)]
	z4<-z3[order(sapply(z3,sizeSorter),decreasing=TRUE)]
	z4
}

filenameLookup <- function(){
	z1<-system(intern=TRUE,'git rev-list --all --objects')
	z2<-strsplit(z1,' ')
	z3<-z2[sapply(z2,function(x)length(x)==2 && x[2]!='')]
	names(z3)<-sapply(z3,'[',1)
	z3
}
gitSuperRm <- function(fileName){
	system(paste(sep='','git filter-branch --tag-name-filter cat --index-filter \'git rm -r --cached --ignore-unmatch ',fileName,'\' --prune-empty -f -- --all'))
}

gitPurge <- function(){
	system('rm -rf .git/refs/original')
	system('git reflog expire --expire=now --all')
	system('git gc --prune=now')
	system('git gc --aggressive --prune=now')
}

readFilenames <- function(pattern='.*',
	hashes=readHashes(),
	lookupTable=filenameLookup()
){
	z1<-lapply(hashes,
		function(hash){
			lookupTable[[hash[1]]]
		}
	)
	z2<-z1[!sapply(z1,is.null)]
	z3<-sapply(z2,'[',2)
	z4<-ultraCombo::multiUnion(z3)
	z5<-grep(pattern,z4,value=TRUE)
	z5
}
