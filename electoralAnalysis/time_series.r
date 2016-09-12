source("load_data.r")

# Define GE trend lines
uf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V-GE2005S$SNP)/sum(GE2005S$N),
		sum(GE2010RS$V-GE2010RS$SNP)/sum(GE2010RS$N),
		sum(GE2015S$V-GE2015S$SNP)/sum(GE2015S$N)
	)
)

nf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$SNP)/sum(GE2005S$N),
		sum(GE2010RS$SNP)/sum(GE2010RS$N),
		sum(GE2015S$SNP)/sum(GE2015S$N)
	)
)

vf_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V)/sum(GE2005S$N),
		sum(GE2010RS$V)/sum(GE2010RS$N),
		sum(GE2015S$V)/sum(GE2015S$N)
	)
)

ur_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V-GE2005S$SNP),
		sum(GE2010RS$V-GE2010RS$SNP),
		sum(GE2015S$V-GE2015S$SNP)
	)
)

nr_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$SNP),
		sum(GE2010RS$SNP),
		sum(GE2015S$SNP)
	)
)

vr_GE_line<-list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005S$V),
		sum(GE2010RS$V),
		sum(GE2015S$V)
	)
)

# Define SIR trend lines
uf_SIR_line<-list(x=2014,y=sum(SIR2014$No)/sum(SIR2014$N))
nf_SIR_line<-list(x=2014,y=sum(SIR2014$Yes)/sum(SIR2014$N))
vf_SIR_line<-list(x=2014,y=sum(SIR2014$V)/sum(SIR2014$N))
ur_SIR_line<-list(x=2014,y=sum(SIR2014$No))
nr_SIR_line<-list(x=2014,y=sum(SIR2014$Yes))
vr_SIR_line<-list(x=2014,y=sum(SIR2014$V))

# SP trend lines
uf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V-SP2007$SNP)/sum(SP2007$N),
		sum(SP2011$V-SP2011$SNP)/sum(SP2011$N),
		sum(SP2016$V-SP2016$SNP)/sum(SP2016$N)
	)
)

nf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$SNP)/sum(SP2007$N),
		sum(SP2011$SNP)/sum(SP2011$N),
		sum(SP2016$SNP)/sum(SP2016$N)
	)
)

vf_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V)/sum(SP2007$N),
		sum(SP2011$V)/sum(SP2011$N),
		sum(SP2016$V)/sum(SP2016$N)
	)
)

ur_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V-SP2007$SNP),
		sum(SP2011$V-SP2011$SNP),
		sum(SP2016$V-SP2016$SNP)
	)
)

nr_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$SNP),
		sum(SP2011$SNP),
		sum(SP2016$SNP)
	)
)

vr_SP_line <- list(x=c(2007,2011,2016),
	y=c(sum(SP2007$V),
		sum(SP2011$V),
		sum(SP2016$V)
	)
)

# VSR trend lines
VSR_r_line <- list(x=2011,
	y=sum(VSR2011$V)
)
VSR_f_line <- list(x=2011,
	y=sum(VSR2011$V)/sum(VSR2011$N)
)
VSRS_r_line <- list(x=2011,
	y=sum(VSR2011S$V)
)
VSRS_f_line <- list(x=2011,
	y=sum(VSR2011S$V)/sum(VSR2011S$N)
)

# EU trend line

EU_GE_f_line <- list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005$V)/sum(GE2005$N),
		sum(GE2010$V)/sum(GE2010$N),
		sum(GE2015$V)/sum(GE2015$N)
	)
)

EU_R_f_line <- list(
	x=2016,
	y=sum(EUR2016$V)/sum(EUR2016$N)
)

EU_GE_r_line <- list(
	x=c(2005,2010,2015),
	y=c(sum(GE2005$V),
		sum(GE2010$V),
		sum(GE2015$V)
	)
)

EU_R_r_line <- list(
	x=2016,
	y=sum(EUR2016$V)
)

vr_EU_line <- list(
	x=2016,
	y=sum(EUR2016S$V)
)

vf_EU_line <- list(
	x=2016,
	y=sum(EUR2016S$V)/sum(EUR2016S$N)
)

# define SC line groups

SC_r_list<-list(GE.turnout=vr_GE_line,
	SIR.turnout=vr_SIR_line,
	SP.turnout=vr_SP_line,
	VSR.turnout=VSRS_r_line,
	EUR.turnout=vr_EU_line,
	GE.unionist=ur_GE_line,
	SIR.unionist=ur_SIR_line,
	SP.unionist=ur_SP_line,
	GE.nationalist=nr_GE_line,
	SIR.nationalist=nr_SIR_line,
	SP.nationalist=nr_SP_line
)

SC_f_list<-list(GE.turnout=vf_GE_line,
	SIR.turnout=vf_SIR_line,
	SP.turnout=vf_SP_line,
	VSR.turnout=VSRS_f_line,
	EUR.turnout=vf_EU_line,
	GE.unionist=uf_GE_line,
	SIR.unionist=uf_SIR_line,
	SP.unionist=uf_SP_line,
	GE.nationalist=nf_GE_line,
	SIR.nationalist=nf_SIR_line,
	SP.nationalist=nf_SP_line
)

EU_r_list<-list(GE.turnout=EU_GE_r_line,
	EUR.turnout=EU_R_r_line,
	VSR.turnout=VSR_r_line
)

EU_f_list<-list(GE.turnout=EU_GE_f_line,
	EUR.turnout=EU_R_f_line,
	VSR.turnout=VSR_f_line
)

SC_r_chart<-function(){
	chart(SC_r_list,
		lwd=5,
		main="Scotland raw turnout",
		xlab="Year",
		ylab="Number of Voters"
	)
}

SC_f_chart<-function(){
	chart(SC_f_list,
		lwd=5,
		main="Scotland fractional turnout",
		xlab="Year",
		ylab="Fraction of Electorate"
	)
}

EU_r_chart<-function(){
	chart(EU_r_list,
		lwd=5,
		main="UK raw turnout",
		xlab="Year",
		ylab="Number of Voters"
	)
}

EU_f_chart<-function(){
	chart(EU_f_list,
		lwd=5,
		main="UK fractional turnout",
		xlab="Year",
		ylab="Fraction of Electorate"
	)
}

