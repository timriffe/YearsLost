
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


colSums(PprojExperiment2)
Diff2 <- PprojExperiment2 - PprojBase
Diff3 <- PprojExperiment3 - PprojBase
plot(1908:2013, colSums(Diff3), type = 'l',col="red",lwd=3)
lines(1908:2013, colSums(Diff2),col = "forestgreen",lty=1)
lines(1908:2013, colSums(Diff1),col="blue",lty="22")

plot(colSums(Diff3) - colSums(Diff1) + colSums(Diff2))

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
