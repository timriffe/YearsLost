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
# Author: tim
###############################################################################

library(HMDHFDplus)
library(reshape2)
#DT            <- readHMDweb("ESP","Deaths_lexis",username=us,password=pw)
#Pop           <- readHMDweb("ESP","Population",username=us,password=pw)
#B             <- readHMDweb("ESP","Births",username=us,password=pw)
#FPC           <- readHFDweb("ESP","asfrVV",username=us,password=pw)
save(DT, file = "Data/PCmaterials/DT.Rdata")
save(Pop, file = "Data/PCmaterials/Pop.Rdata")
save(B, file = "Data/PCmaterials/B.Rdata")
save(FPC, file = "Data/PCmaterials/FPC.Rdata")
# We'll need these for age 0 death probabilities
BM            <- B$Male
BF            <- B$Female

# Add cohort oclumn to pop for selection
Pop$Coh1      <- Pop$Year - Pop$Age - 1
Pop$Coh2      <- Pop$Year - Pop$Age

# add Birthday columns (age obtained in the year)
Pop$Birthday  <- Pop$Age + 1

# append births as Pop with Birthday = 0.
Pop0          <- Pop[Pop$Age == 0, ]
Pop0$Age      <- -1
Pop0$Birthday <- 0

# shift pops
Pop0$Coh1     <- Pop0$Coh2 
Pop0$Female2  <- Pop0$Female1
Pop0$Male2    <- Pop0$Male1

# assign births to pop1
Pop0$Female1  <- BF
Pop0$Male1    <- BM

# append to Pop
Pop           <- rbind(Pop0, Pop)

# determine Lexis shape for deaths
res                           <- DT$Year - DT$Age - DT$Cohort
res[is.na(res)]               <- 0
DT$Lexis                      <- ifelse(res==0,"TL","TU")

# open interval all in lower tri, odd.
# divide by 3?
cnames                        <- c("Female", "Male", "Total")
DT[DT$Age == 110, cnames]     <- DT[DT$Age == 110, cnames] / 3
DT$Cohort[DT$Age==110]        <- DT$Year[DT$Age==110] - 110

# PC death counts
PCf                           <- acast(DT, Year~Cohort, value.var = "Female", sum)
PCm                           <- acast(DT, Year~Cohort, value.var = "Male", sum)

# PC Jan 1 Pop counts, to conform
P1f                           <- acast(Pop[Pop$Age < 110, ], Year~Coh1, value.var = "Female1")
P1m                           <- acast(Pop[Pop$Age < 110, ], Year~Coh1, value.var = "Male1")

# PC death probabilities
qPCf                          <- PCf / P1f
qPCm                          <- PCm / P1m

# can't be > 1 (data quality issue)
# sum((qPCf > 1 | qPCm > 1) & !is.na(qPCf) & !is.na(qPCm)) #17 cases
qPCf[qPCf > 1 & !is.na(qPCf)] <- 1
qPCm[qPCm > 1 & !is.na(qPCm)] <- 1

# no zeros in denom!
# sum((is.nan(qPCf) | is.nan(qPCm)) & !is.na(qPCf) & !is.na(qPCm)) #0 cases
qPCf[is.nan(qPCf)]            <- 1
qPCm[is.nan(qPCm)]            <- 1

# get back to long format
PCqxf                         <- melt(qPCf, varnames = c("Year", "Cohort"), value.name = "qx")
PCqxm                         <- melt(qPCm, varnames = c("Year", "Cohort"), value.name = "qx")

# add age column (middle horizontal (birthday) of PC parallelogram)
PCqxf$Age                     <- PCqxf$Year - PCqxf$Cohort
PCqxm$Age                     <- PCqxm$Year - PCqxm$Cohort

# cut down to positive ages <- 110
PCqxf                         <- PCqxf[PCqxf$Age >= 0 & PCqxf$Age <= 110, ]
PCqxm                         <- PCqxm[PCqxm$Age >= 0 & PCqxm$Age <= 110, ]


# put into AP matrix (of PC values), where Age is actually indexed to the birthday line.
qxf <- acast(PCqxf, Age ~ Year, value.var = "qx")
qxm <- acast(PCqxm, Age ~ Year, value.var = "qx")

# save for re-use later:
save(qxf, file ="Data/qxfPCinAPformat.Rdata")
save(qxm, file ="Data/qxmPCinAPformat.Rdata")



#library(devtools)
#document("/home/tim/git/LexisUtils/LexisUtils")
#install_github("timriffe/LexisUtils/LexisUtils")
plot(1908:2014,tapply(DT$Total,DT$Year,sum))
abline(v=1918)


propFemale     <- BF / (BF+BM)

# minor setback: HFD only starts in 1922, HFC too. INE starts 1941.
# so we'll need to improvise for 1908 to 1921:
FPCAP <- acast(FPC, ARDY~Year, value.var = "ASFR")
#matplot(12:55,FPCAP[,1:6],type='l',col=gray(c(.8,.7,.6,.5,.3,.2)), lwd = c(2,1.6,1.2,.8,.6,.5), lty = 1)

# get shape
FPCAPshape <- t(t(FPCAP) /colSums(FPCAP))
#matplot(12:55,FPCAPshape[,1:6],type='l',col=gray(c(.8,.7,.6,.5,.3,.2)), lwd = c(2,1.6,1.2,.8,.6,.5), lty = 1)
#lines(12:55,rowMeans(FPCAPshape[,1:6]),col="red")

# can try different ranges to see how sensitive it actually is:
pre1922standard <- rowMeans(FPCAPshape[,1:6])




#################################################################################
## would a rescale have made sense for SWE over the Spanish flu years?
## looks like no big deal actually...
#FPC2          <- readHFDweb("SWE","asfrVV",username=us,password=pw)
#FPCAP2 <- acast(FPC2, ARDY~Year, value.var = "ASFR")
#matplot(12:55,FPCAP2[,as.character(1915:1921)],type='l',col=gray(.2), lwd = 1, lty = 1)
#lines(12:55,FPCAP2[,as.character(1918)],col="red")
#lines(12:55,FPCAP2[,as.character(1919)],col="red")
#
#FPCAP2shape <- t(t(FPCAP2) /colSums(FPCAP2))
#matplot(12:55,FPCAP2shape[,as.character(1915:1921)],type='l',col=gray(.2), lwd = 1, lty = 1)
#lines(12:55,FPCAP2shape[,as.character(1918)],col="red")
#lines(12:55,FPCAP2shape[,as.character(1919)],col="red")
