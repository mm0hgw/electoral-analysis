
require(gsubfn)

europe_dir <- "~/git/COVID-19/data/cases/europe/europe_situation_updates"

crunch.dir <- function(d,pattern='download'){
	fl <- list.files(d,full.names=TRUE,pattern=pattern)
	fd <-
as.Date(strapplyc(fl,"\\d+-\\d+-\\d+",simplify=TRUE))
	ft <- lapply(seq_along(fl),function(x){
		y <- read.csv(fl[x],stringsAsFactors=FALSE)
		if(ncol(y)==2){
			y<-cbind(rownames(y),y)
		}
		out<-cbind(fd[x],y)
		colnames(out)<-c('date','country','cases','deaths')
		out
})
	fco <- ft[[length(ft)]][,2]
	ftr <- do.call(rbind,ft)
	ftr[,2] <- gsub('_',' ',ftr[,2])
	ftr[,2] <- gsub('\\n','',ftr[,2])
	ftr[,2] <- gsub('United kingdom','United Kingdom',ftr[,2])
	split(ftr,ftr[,2])
}
