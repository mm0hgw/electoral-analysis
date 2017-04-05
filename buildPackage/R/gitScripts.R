hashFinder <- function(line)if(nchar(line[1])==40){TRUE}else{FALSE}
sizeSorter <- function(line)as.integer(line[5])

#'ls.git.hashes
#'@param pattern 'character' a regexp to filter output
#'@export
ls.git.hashes <- function(pattern='.*'){
	z1<-system('git verify-pack -v .git/objects/pack/pack-*.idx',
		intern=TRUE
	)
	z2<-strsplit(z1,' ')
	z3<-z2[sapply(z2,function(x)nchar(x[1])==40)]
	z4<-as.integer(sapply(z3,'[',5))
	names(z4)<-sapply(z3,'[',1)
	z5<-z4[grep(pattern,names(z4))]
	sort(z5,decreasing=TRUE)
}

#'ls.git.files
#'@param pattern 'character' a regexp to filter output
#'@export
ls.git.files <- function(pattern='.*'){
	z1<-system(intern=TRUE,'git rev-list --all --objects')
	z2<-strsplit(z1,' ')
	z3<-z2[sapply(z2,length)==2]
	z4<-sapply(z3,'[',2)
	grep(pattern,value=TRUE,sort(unique(z4)))
}

#'hashLookup
#'@export
hashLookup <- function(){
	z1<-system(intern=TRUE,'git rev-list --all --objects')
	z2<-strsplit(z1,' ')
	z3<-z2[sapply(z2,function(x)length(x)==2 && x[2]!='')]
	names(z3)<-sapply(z3,'[',1)
	z3
}

#'gitExpunge
#'@param fileName a 'character' vector of filenames to expunge from repo
#'@export
gitExpunge <- function(fileName,n=200){
	while(length(fileName)>n){
		gitExpunge(head(fileName,n=n))
		fileName <- fileName[-seq(n)]
	}
	system(paste(sep='','git filter-branch --tag-name-filter cat ',
		'--index-filter \'git rm -r --cached --ignore-unmatch ',
		paste(collapse=' ',fileName),'\' --prune-empty -f -- --all')
	)
}

#'gitPurge
#'@export
gitPurge <- function(){
	system('rm -rf .git/refs/original')
	system('git reflog expire --expire=now --all')
	system('git gc --prune=now')
	system('git gc --aggressive --prune=now')
}

#'gitRebase
#'@param from identifier to rebase upon.
#'@param tool mergetool to use
#'@export
gitRebase <- function(from,tool='kdiff3'){
	system(paste('git rebase -i',from))
	while(system(paste(sep='','git mergetool --tool=',tool))==0){
		while(system('git rebase --continue')==0)
			0
	}
}
