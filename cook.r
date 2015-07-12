source("plots.r")
source("chisq.r")

file_list <- list_csv_files()

ballots <- lapply(file_list,FUN=function(x){read_ballot(x,do.cook=F)})
names(ballots) <- file_list
clean_ballots <- mclapply(ballots,remove_ballots,mc.preschedule=T)
diff_ballots <- foreach(b=ballots,c=clean_ballots)%do%{b$V-c$V}
cleaned_pc <- unlist(foreach(d=diff_ballots,b=ballots)%do%{sum(d)/sum(b$N)})

