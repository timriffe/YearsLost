
# Author: tim
###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/YearsLost/YearsLost")
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//YearsLost//YearsLost")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
	}
}
# this script relies on PCprobs.R . source() that script to get objects:
# qxm, qxf, Pop, Fxm, Fxf
source("R/PCprobs.R")
# but replace Pop...
Pop <- local(get(load("Data/PCmaterials/Pop.Rdata")))
# we need Jan 1 pops

# 
# qxfv <- qxf[,1];qxmv <- qxm[,1];Fxfv <- Fxf[,1];Fxmv <- Fxm[,1]
makeLeslie <- function(qxfv,qxmv,Fxfv,Fxmv){
	N   <- length(qxmv)
	
	# discount ferility for infant mort and mother mort
	mfert           <- Fxmv * (1 - qxmv[1]) * (1 - (qxfv / 2))
	ffert           <- Fxmv * (1 - qxfv[1]) * (1 - (qxfv / 2))
			
	# make the four big corners of the Leslie matrix:
	Fem 			<- rbind(ffert,cbind(diag(1 - qxfv[-N]),0))
	Mal 			<- rbind(0,cbind(diag(1 - qxmv[-N]),0))
	zeros 			<- Fem * 0
	malefert 		<- zeros
	malefert[1, ] 	<- mfert
	
	cbind(rbind(Fem,malefert),
			rbind(zeros,Mal))
}

Pop <- Pop[with(Pop,order(Year,Age)),]


Pinit      <- Pop[Pop$Year==1908,]
ind        <- Pop$Year == 1908

# there is terrible age-heaping in 1908...
# group, then graduate?
Pinit      <- c(Pinit$Female1, Pinit$Male1)

years      <- as.integer(colnames(Fxf))
Horiz      <- length(years)
PprojBase  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojBase[, 1] <- Pinit
for (i in 1:Horiz){
	PprojBase[, i + 1] <- makeLeslie(qxf[, i], 
											 qxm[, i],
											 Fxf[, i],
											 Fxm[, i])  %*% PprojBase[, i]
}

# now for experiment, try 1
qxm2 <- qxm
qxf2 <- qxf

qxm2[,c("1918","1919")] <- rowMeans(qxm[,c("1917","1920")])
qxf2[,c("1918","1919")] <- rowMeans(qxf[,c("1917","1920")])

PprojExperiment1  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojExperiment1[, 1] <- Pinit
for (i in 1:Horiz){
	PprojExperiment1[, i + 1] <- makeLeslie(qxf2[, i], 
			qxm2[, i],
			Fxf[, i],
			Fxm[, i])  %*% PprojExperiment1[, i]
}

#plot(colSums(PprojExperiment1) - colSums(PprojBase))
Diff1 <- PprojExperiment1 - PprojBase

plot(colSums(Diff1))

plot(1908:2014,c(acast(DT, 1~Year, value.var = "Male", sum)))
abline(v=c(1936,1941))

qxf3 <- qxf
qxm3 <- qxm

# next erase civil war
qxm3[,as.character(1936:1941)] <- rowMeans(qxm[,c("1934","1935","1942","1943")])
qxf3[,as.character(1936:1941)] <- rowMeans(qxf[,c("1934","1935","1942","1943")])

PprojExperiment2  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojExperiment2[, 1] <- Pinit
for (i in 1:Horiz){
	PprojExperiment2[, i + 1] <- makeLeslie(qxf3[, i], 
			qxm3[, i],
			Fxf[, i],
			Fxm[, i])  %*% PprojExperiment2[, i]
}



qxf4 <- qxf
qxm4 <- qxm

# now arease both flu and war:
qxm4[,c("1918","1919")] <- rowMeans(qxm[,c("1917","1920")])
qxf4[,c("1918","1919")] <- rowMeans(qxf[,c("1917","1920")])
qxm4[,as.character(1936:1941)] <- rowMeans(qxm[,c("1934","1935","1942","1943")])
qxf4[,as.character(1936:1941)] <- rowMeans(qxf[,c("1934","1935","1942","1943")])

PprojExperiment3  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojExperiment3[, 1] <- Pinit
for (i in 1:Horiz){
	PprojExperiment3[, i + 1] <- makeLeslie(qxf4[, i], 
			qxm4[, i],
			Fxf[, i],
			Fxm[, i])  %*% PprojExperiment3[, i]
}

########################################################################
# a final scenario. What if no shocks, and observed rates always happened 5 years
# earlier? i.e. what are the impacts of slow progress?

# get wider mat, replace early years as necessary
qxf.accell 					<- qxf.future
qxf.accell[,colnames(qxf4)] <- qxf4
qxm.accell 					<- qxm.future
qxm.accell[,colnames(qxm4)] <- qxm4

# add up to 2017:
N <- ncol(qxm.accell)
qxm.accell 				<- cbind(qxm.accell,qxm.accell[,N],qxm.accell[,N],qxm.accell[,N])
qxf.accell 				<- cbind(qxf.accell,qxf.accell[,N],qxf.accell[,N],qxf.accell[,N])
colnames(qxf.accell) 	<- 1908:2017
colnames(qxm.accell) 	<- 1908:2017
#################################################

PprojExperiment4  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojExperiment4[, 1] <- Pinit
for (i in 1:Horiz){
	PprojExperiment4[, i + 1] <- makeLeslie(qxf.accell[, i+5], 
			qxm.accell[, i+5],
			Fxf[, i],
			Fxm[, i])  %*% PprojExperiment4[, i]
}

# get wider mat, replace early years as necessary
qxf.accell2 					<- qxf.future
qxm.accell2 					<- qxm.future


# add up to 2017:
N <- ncol(qxm.accell2)
qxm.accell2 				<- cbind(qxm.accell2,qxm.accell2[,N],qxm.accell2[,N],qxm.accell2[,N])
qxf.accell2 				<- cbind(qxf.accell2,qxf.accell2[,N],qxf.accell2[,N],qxf.accell2[,N])
colnames(qxf.accell2) 		<- 1908:2017
colnames(qxm.accell2) 		<- 1908:2017
#################################################

PprojExperiment5  <- matrix(0, nrow = length(Pinit), ncol = Horiz + 1)
PprojExperiment5[, 1] <- Pinit
for (i in 1:Horiz){
	PprojExperiment5[, i + 1] <- makeLeslie(qxf.accell2[, i+5], 
			qxm.accell2[, i+5],
			Fxf[, i],
			Fxm[, i])  %*% PprojExperiment5[, i]
}


Diff1 <- PprojExperiment1 - PprojBase
Diff2 <- PprojExperiment2 - PprojBase
Diff3 <- PprojExperiment3 - PprojBase
Diff4 <- PprojExperiment4 - PprojBase
Diff5 <- PprojExperiment5 - PprojBase

png("Figures/ScenarioTrends.png")
plot(1908:2013, colSums(Diff4), type = 'l',col="red",ylab="Population", xlab = "year",
		main = "Persons missing from base scenario\n different mortality scenarios",
		sub="base scenario = observed vital rates, without migration")
lines(1908:2013, colSums(Diff5),col = "green",lty=2)
lines(1908:2013, colSums(Diff3),col = "orange",lty=2)
lines(1908:2013, colSums(Diff2),col = "forestgreen",lty=1)
lines(1908:2013, colSums(Diff1),col="blue",lty="22")
text(c(1945,1946,1941,1942,1948),
		c(2e6,1.5e6,767000,491000,250000),
		c("5yrs faster, no war, no flu", "5yrs faster","no war and no flu","no war","no flu"),pos=4)
dev.off()



# next step would be to decompose into original saved, and descendents.
# total more than sum of parts
plot(colSums(Diff3) - colSums(Diff1) + colSums(Diff2))

# Pop pyramids:
library(Pyramid)
N     <- ncol(Diff1)
yrind <- Pop$Year == 2013

png("Figures/DifferencesPyramid.png")
Pyramid(males = PprojBase[112:222,N], 
		females = PprojBase[1:111,N], 
		fill.males = gray(.8), 
		fill.females = gray(.8),
		grid = FALSE,
		verbose = FALSE,
		xlim = c(-2.2,2.2))
mtext("Age structure of difference between Jan 1, 2013 population\nScenario minus base scenario",side=3,line=2)
mtext("base scenario = observed vital rates, without migration",side=1,line=4)
PyramidOutline(males = Diff1[112:222,N], females = Diff1[1:111,N],scale=100,border="blue",lty="22")
PyramidOutline(males = Diff2[112:222,N], females = Diff2[1:111,N],scale=100,border = "forestgreen",lty=1)
PyramidOutline(males = Diff3[112:222,N], females = Diff3[1:111,N],scale=100,border = "orange",lty=2)
PyramidOutline(males = Diff4[112:222,N], females = Diff4[1:111,N],scale=100,border="red")
PyramidOutline(males = Diff5[112:222,N], females = Diff5[1:111,N],scale=100,border = "green",lty=2)
dev.off()

# and total population structure Jan 1, 2013. Not crazy differences
Pyramid(males = PprojBase[112:222,N], 
		females = PprojBase[1:111,N], 
		fill.males = gray(.8), 
		fill.females = gray(.8),
		grid = FALSE,
		verbose = FALSE,
		xlim = c(-1.2,1.2))
mtext("Age structure of Jan 1, 2013 population\nScenario minus base scenario",side=3,line=2)
mtext("base scenario = observed vital rates, without migration",side=1,line=4)
PyramidOutline(males = PprojExperiment1[112:222,N], females = PprojExperiment1[1:111,N],scale=100,border="blue",lty="22")
PyramidOutline(males = PprojExperiment2[112:222,N], females = PprojExperiment2[1:111,N],scale=100,border = "forestgreen",lty=1)
PyramidOutline(males = PprojExperiment3[112:222,N], females = PprojExperiment3[1:111,N],scale=100,border = "orange",lty=2)
PyramidOutline(males = PprojExperiment4[112:222,N], females = PprojExperiment4[1:111,N],scale=100,border="red")
PyramidOutline(males = PprojExperiment5[112:222,N], females = PprojExperiment5[1:111,N],scale=100,border = "green",lty=2)

# but, not standardized populations:
png("Figures/Pyramids2013.png")
Pyramid(males = PprojBase[112:222,N], 
		females = PprojBase[1:111,N], 
		fill.males = gray(.8), 
		fill.females = gray(.8),
		grid = FALSE,
		verbose = FALSE,
		prop = FALSE)
mtext("Age pyramid of Jan 1, 2013 population\nScenario minus base scenario",side=3,line=2)
mtext("base scenario = observed vital rates, without migration",side=1,line=4)
PyramidOutline(males = PprojExperiment1[112:222,N], females = PprojExperiment1[1:111,N],
		scale=sum(PprojExperiment1[,N])/1000,border="blue",lty="22")
PyramidOutline(males = PprojExperiment2[112:222,N], females = PprojExperiment2[1:111,N],
		scale=sum(PprojExperiment2[,N])/1000,border = "forestgreen",lty=1)
PyramidOutline(males = PprojExperiment3[112:222,N], females = PprojExperiment3[1:111,N],
		scale=sum(PprojExperiment3[,N])/1000,border = "orange",lty=2)
PyramidOutline(males = PprojExperiment4[112:222,N], females = PprojExperiment4[1:111,N],
		scale=sum(PprojExperiment4[,N])/1000,border="red")
PyramidOutline(males = PprojExperiment5[112:222,N], females = PprojExperiment5[1:111,N],
		scale=sum(PprojExperiment5[,N])/1000,border = "green",lty=2)
PyramidOutline(males = PprojExperiment5[112:222,N], females = PprojExperiment5[1:111,N],
		scale=sum(PprojExperiment5[,N])/1000,border = "green",lty=2)
PyramidOutline(males = Pop$Male1[yrind], females = Pop$Female1[yrind],
		scale=sum(Pop$Male1[yrind]+ Pop$Female1[yrind])/1000,border = "black",lty=1,lwd=.5)
dev.off()





#
#
#library(Pyramid)
#Pyramid(males = Diff[112:222,ncol(Diff)], females = Diff[1:111,ncol(Diff)],
#		fill.males="gray",fill.females="gray",grid = FALSE)
#mls <- PprojBase[112:222,ncol(Diff)]; fmls <- PprojBase[1:111,ncol(Diff)]
#PyramidOutline(males = mls, females = fmls, 
#		scale = 100 )
#
#for (yr in 12:length(years)){
#	Pyramid(males = Diff[112:222,yr], females = Diff[1:111,yr],
#			fill.males="gray",fill.females="gray",grid = FALSE)
#	Sys.sleep(.5)
#}
#
#
#
#
#Pyramid(males = mls, females = fmls,
#		fill.males="gray",fill.females="gray",grid = FALSE)
#
#group5 <- function(x,age = 1:length(age)-1){
#	age5 <- age - age %% 5
#	tapply(x,age5,sum)
#}
#

