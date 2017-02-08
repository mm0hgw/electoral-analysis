
RU2011<-read.csv("data/org/Russia2011.csv")

png("test1.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			RU2011[,c(2,3)],
			norm=TRUE
		)
	),
	main="Russian Parliamentary elections 2011 normalised turnout density",
	sub="Explicit Gaussian expectation from formal observation https://arxiv.org/abs/1003.2807 Found fraudulent in http://m.pnas.org/content/109/41/16469.short",
	xlab="Population standard deviations from population mean",
	col=2
)
dev.off()

SIR2014<-read.csv("data/SIR2014.csv")

png("test2.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			SIR2014[,c("N","V")],
			norm=TRUE
		)
	),
	main="Scottish Independence Referendum 2014 normalised turnout density",
	sub="Explicit Gaussian expectation from formal observation https://arxiv.org/abs/1003.2807",
	xlab="Population standard deviations from population mean",
	col=2
)
dev.off()

png("test3.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			RU2011[,c(3,9)],
			norm=TRUE
		)
	),
	main="Russian Parliamentary elections 2011 normalised United Russia vote proportion density",
	sub="Explicit Gaussian expectation from formal observation https://arxiv.org/abs/1003.2807 Found fraudulent in http://m.pnas.org/content/109/41/16469.short The fraudulent points are in the long RHS tail.",
	xlab="Population standard deviations from population mean",
	col=2
)
dev.off()

png("test4.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			SIR2014[,c("V","No")],
			norm=TRUE
		)
	),
	main="Scottish Independence Referendum 2014 normalised vote proportion density",
	sub="Explicit Gaussian expectation from formal observation https://arxiv.org/abs/1003.2807 Density curves are mirror images, thus both are affected by turnout distortion.",
	xlab="Population standard deviations from population mean",
	col=2
)
lines(
	density(
		sbCalculateSample(
			SIR2014[,c("V","Yes")],
			norm=TRUE
		)
	),
	col=3
)
legend("topleft",
	leg=c("No","Yes"),
	col=c(2,3),
	lwd=10
)
dev.off()

png("test5.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			SIR2014[,c("NP","VP")],
			norm=TRUE
		)
	),
	main="Scottish Independence Referendum 2014 normalised turnout density",
	sub="Postal and non-postal ballots have statistical independence and therefore inherit the Gaussian expectation of their sum",
	xlab="Population standard deviations from population mean",
	col=2
)
lines(
	density(
		sbCalculateSample(
			SIR2014[,c("N","V")]-SIR2014[,c("NP","VP")],
			norm=TRUE
		)
	),
	col=3
)
legend("topleft",
	leg=c("postal","non-postal"),
	col=c(2,3),
	lwd=10
)
dev.off()

png("test6.png",width=1920,height=1080)
plot_with_normal(
	density(
		sbCalculateSample(
			SIR2014[,c("N","No")],
			norm=TRUE
		)
	),
	main="Scottish Independence Referendum 2014 normalised turnout density",
	sub="Yes and No ballot totals have statistical independence and therefore inherit the Gaussian expectation of their sum",
	xlab="Population standard deviations from population mean",
	col=2
)
lines(
	density(
		sbCalculateSample(
			SIR2014[,c("N","Yes")],
			norm=TRUE
		)
	),
	col=3
)
legend("topleft",
	leg=c("No","Yes"),
	col=c(2,3),
	lwd=10
)
dev.off()


buildPackage::gitPush(list.files(pattern="test*"),"e")
