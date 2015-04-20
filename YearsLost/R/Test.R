
Do.This <- FALSE

# this was my first hacky attempt. Things are cleaner now and in separate files...
if (Do.This){
library(DemogBerkeley)
Dx <- readHMD("/home/triffe/DATA/HMD/deaths/Deaths_1x1/USA.Deaths_1x1.txt")
Ex <- readHMD("/home/triffe/DATA/HMD/exposures/Exposures_1x1/USA.Exposures_1x1.txt")

Dxm <- Dx$Male[Dx$Year == 2010]
Exm <- Ex$Male[Ex$Year == 2010]
Dxf <- Dx$Female[Dx$Year == 2010]
Exf <- Ex$Female[Ex$Year == 2010]

Mxm <- Dxm / Exm
Mxf <- Dxf / Exf

lxm <- c(1, exp(-cumsum(Mxm)))
lxf <- c(1, exp(-cumsum(Mxf)))
plot(0:111,lx,type='l')

dxm <- -diff(lxm)
dxf <- -diff(lxf)

# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.

library(devtools)
load_all("/home/triffe/git/Leaves/PlosOne/R/RiffeetalFunctions")

plot(rowSums(Thano(dxm,dxm)))
plot(rowSums(Thano(Dxm,dxm)))
plot(rowSums(Thano(dxf,dxf)))
plot(rowSums(Thano(Dxf,dxf)))
sum(Dxm) # males persons lost
sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) # total male person-years lost due to death
sum(Dxf) # females lost
sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) # total female person-years lost due to death

sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) / sum(Dxm) # lose 16.3 PYL per male
sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) / sum(Dxf)  # lose 13.7 PYL per female
# OK, but if each is repeated over x:x+y?

LDM <- outer(lxm,lxm,"/")
LDF <- outer(lxf,lxf,"/")
LDM[LDM > 1] <- NA
LDF[LDF > 1] <- NA

image(LDM)
image(LDF)
image(LDF / LDM)
#all(LDM == t(1/LDM), na.rm= TRUE)

# ages go down the matrix
head(LD)[,1:6]
plot(LDM[,1])
LDM[,1] == lxm
# so:
plot(rowSums(t(LDM)[2:112,2:112] * Dxm,na.rm=TRUE))
plot(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))


Males   <- t(LDM)[2:112,2:112] * Dxm
Females <- t(LDF)[2:112,2:112] * Dxf
library(Pyramid)
par(mfrow = c(1, 2))
MalesY   <- rowSums(Males, na.rm = TRUE)
FemalesY <- rowSums(Females, na.rm = TRUE)
Pyramid(MalesY,FemalesY, prop = FALSE, grid = FALSE, main = "Potental person-years of life lost due to death\nby age and sex in 2010")

MalesA   <- colSums(Males, na.rm = TRUE)
FemalesA <- colSums(Females, na.rm = TRUE)
Pyramid(MalesA,FemalesA, prop = FALSE, grid = FALSE, main = "Potental person-ages of life lost due to death\nby age and sex in 2010")

graphics.off()
plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)))
PyramidOutline(MalesY,FemalesY, col = "#00000050")
PyramidOutline(MalesA,FemalesA, col = "#FF000050")

# -------------------------------------------------
# now for heterogeneity 1:
aggN <- function(x,N){
    age <- 0:(length(x) - 1)
    tapply(x,age - age %% N, sum, na.rm = TRUE)
}

MalesYH   <- apply(Males, 1, aggN, N = 10)
FemalesYH <- apply(Females, 1, aggN, N = 10)

MalesAH   <- apply(Males, 2, aggN, N = 10)
FemalesAH <- apply(Females, 2, aggN, N = 10)

MalesYH   <- MalesYH[nrow(MalesYH):1,]
FemalesYH <- FemalesYH[nrow(MalesYH):1,]
MalesAH   <- MalesAH[nrow(MalesAH):1,]
FemalesAH <- FemalesAH[nrow(MalesAH):1,]




MalesYHC   <- t(apply(MalesYH,2,cumsum))
FemalesYHC <- t(apply(FemalesYH,2,cumsum))
MalesAHC   <- t(apply(MalesAH,2,cumsum))
FemalesAHC <- t(apply(FemalesAH,2,cumsum))

library(RColorBrewer)
display.brewer.all()
Cols1  <- colorRampPalette(brewer.pal(9,"BuGn"),space="Lab")(ncol(MalesYHC))
plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)),
        main = "Potental person-years of life lost due to death\nby age and sex in 2010", sub = 
                "Color bands indicate potential age attained (darker = younger, lighter = older)")
for (i in ncol(MalesYHC):1){
    PyramidOutline(MalesYHC[,i],FemalesYHC[,i], col = Cols1[i],border=NA)
}
text(0,-10,"Millions",xpd=TRUE)

plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)), main = 
                "Potental person-ages of life lost due to death\nby age (10-year color bands) and sex in 2010")
for (i in ncol(MalesYHC):1){
    PyramidOutline(MalesAHC[,i],FemalesAHC[,i], col = Cols1[i],border=NA)
}
text(0,-10,"Millions",xpd=TRUE)

args(Pyramid)
sum(rowSums(t(LD)[2:112,2:112] * Dx,na.rm=TRUE)) # closer

# COD:

COD <- read.csv("/home/triffe/Desktop/COD_5x1_adjusted_chapters.csv", stringsAsFactors = FALSE)
COD <- COD[COD$Year == 2010, ]
COD$Age <- as.integer(gsub("\\+","",unlist(lapply(strsplit(COD$Age,split = "-"),"[[",1))))

}
#
#require(devtools)
#install_github('rCharts', 'ramnathv')
library(rCharts)


setwd("/home/triffe/git/YearsLost/YearsLost")
# get plotting functions
source("R/Functions.R")

library(DemogBerkeley)
Dx  <- readHMDweb("USA","Deaths_1x1",username = us, password = pw)
Ex  <- readHMDweb("USA","Exposures_1x1",username = us, password = pw)
Px  <- readHMDweb("USA","Population",username = us, password = pw)

Dxm <- Dx$Male[Dx$Year == 2010]
Exm <- Ex$Male[Ex$Year == 2010]
Dxf <- Dx$Female[Dx$Year == 2010]
Exf <- Ex$Female[Ex$Year == 2010]
Pxm <- Px$Male1[Px$Year == 2010]
Pxf <- Px$Female1[Px$Year == 2010]

Mxm <- Dxm / Exm
Mxf <- Dxf / Exf

lxm <- c(1, exp(-cumsum(Mxm)))
lxf <- c(1, exp(-cumsum(Mxf)))

dxm <- lx2dx(lxm)
dxf <- lx2dx(lxf)
# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.

# xlim is shared
xlim     <- max(pretty(c(Pxm, Pxf))) * c(-1, 1)

# Leaf
Males   <- ThanoAgg(Pxm, dxm, FALSE, N = 10)
Females <- ThanoAgg(Pxf, dxf, FALSE, N = 10)

library(reshape2)
MalesL <- melt(-Males, varnames = c("a","y"),value.name = "Population")
FemalesL <- melt(Females, varnames = c("a","y"),value.name = "Population")
MalesL$Sex <- "m"
FemalesL$Sex <- "f"
Pop2        <- rbind(MalesL,FemalesL)
Pop2$Population <- Pop2$Population / sum(abs(Pop2$Population)) * 100

n1 <- nPlot(Population~y, data = Pop2,groups="a",type = 'multiBarHorizontalChart')
n1$chart(stacked = TRUE)
slotNames(n1)
n1$chart

d1 <- dPlot(value~y+a,data=Pop,type="bar",stacked=TRUE)
d1$yAxis(type="addCategoryAxis", orderRule="a")
d1$xAxis(type="addMeasureAxis")



#######################################################
# a quick test for an unrelated paper:
COD <- local(get(load("/data/commons/triffe/git/YearsLost/YearsLost/Data/COD.Rdata")))
names(COD[[1]])
COD[[1]]$Mxmc

redistributeIllDefined <- function(Mxc){
  Mxc[is.na(Mxc)] <- 0
  ID                      <- Mxc[,"Ill defined"]
  TheRest                 <- Mxc[,colnames(Mxc)!="Ill defined"]
  denom                   <- rowSums(TheRest)
  ind0                    <- denom == 0
  TheRest                 <- TheRest + (TheRest/denom) * ID
  TheRest[is.na(TheRest)] <- 0
  TheRest[ind0, ]         <- ID[ind0] / ncol(TheRest)
  TheRest[is.infinite(TheRest)] <- 0
  TheRest
}

Mxmc <- lapply(COD, function(X){
    redistributeIllDefined(X$Mxmc)
  })
Mxfc <- lapply(COD, function(X){
    redistributeIllDefined(X$Mxfc)
  })

Causes <- colnames(Mxmc[[1]])
cause <- "Cancer"
Mxc <- Mxmc
getMinc <- function(cause, Mxc){
  Mc <- do.call(cbind, lapply(Mxc, function(X,cause){
        X[,cause]
      },cause=cause))
  Mc[Mc==0] <- NA
 
  apply(Mc,1,function(x){
      ifelse(all(is.na(x)),0,min(x,na.rm=TRUE))
    })
  
}
Mcm <- do.call(cbind,lapply(Causes, getMinc, Mxc = Mxmc))
Mcf <- do.call(cbind,lapply(Causes, getMinc, Mxc = Mxfc))

Mxm <- rowSums(Mcm)
Mxf <- rowSums(Mcf)
matplot(0:110,Mcm,type='l',log='y')
matplot(0:110,Mcf,type='l',log='y')

sum(exp(-cumsum(Mxm))) # a best-practices e0
sum(exp(-cumsum(Mxf))) 

# the respective all-cause e0s
unlist(lapply(COD, function(X){
    sum(exp(-cumsum(rowSums(X$Mxmc,na.rm=TRUE))))
  }))
unlist(lapply(COD, function(X){
      sum(exp(-cumsum(rowSums(X$Mxfc,na.rm=TRUE))))
    }))

# conclusion: this particular best practices lifetable isn't so great.

#
devtools::load_all("/data/commons/triffe/git/HMDLexis/HMDLexis/HMDLexis")

# LexisDB functions:

#bitops_1.0-6     chron_2.3-45     compiler_3.0.1   data.table_1.9.4
#devtools_1.4.1   digest_0.6.4     evaluate_0.5.5   httr_0.5        
#lubridate_1.3.3  magrittr_1.5     memoise_0.2.1    parallel_3.0.1  
#plyr_1.8.1       pracma_1.7.9     pspline_1.0-16   Rcpp_0.11.3     
#RCurl_1.95-4.4   reshape2_1.4     stringr_0.6.2    tools_3.0.1     
#whisker_0.3-2   
#
#importFrom(compiler,cmpfun)          # in RDC
#importFrom(data.table,data.table)    # in RDC
#importFrom(lubridate,decimal_date)
#importFrom(lubridate,floor_date)
#importFrom(lubridate,yday)
#importFrom(lubridate,ymd)
#importFrom(magrittr,"%>%")
#importFrom(pracma,pchip)
#importFrom(pspline,smooth.Pspline)
#importFrom(reshape2,acast)           # in RDC
#importFrom(reshape2,melt)            # in RDC

# chron, stringr, memoise is a dependency of lubridate
# RCurl, evaluate, whisker is needed by devtools, used to load this example....
# bitops used by RCurl

q0 <- 1 - 98575 / 1e5

D0 <- 21713 # from table 5
(q0^2 * (1- q0))/D0 * 10^8


CODB <- local(get(load("/data/commons/triffe/git/YearsLost/YearsLost/Data/COD.Rdata")))
COD         <- read.csv("/data/commons/triffe/HMDCOD.hg/test/USA/CODoutput/COD_5x1.csv", 
                               stringsAsFactors = FALSE)
unique(COD$Age)
COD         <- COD[COD$Year == 2010 & COD$COD.cat != "All", ]
COD$Age     <- as.integer(gsub("\\+","",unlist(lapply(strsplit(COD$Age,split = "-"),"[[",1))))

#chapter	chapterName	            INED6	INED6Name	Code8	Code8Name
# 1	    Certain infectious diseases	    3	Infectious	1	Infectious
# 2	    Malignant neoplasms	            1	Cancer	    2	Cancer
# 3	    Other neoplasms	                1	Cancer	    2	Cancer
# 4	    Blood and blood-forming orga    2	Other	    3	Other
# 5	    Endocrine, nutrition., metab    4	Other	    3	Other
# 6	    Mental and behavioural diso     4	Other	    8	Mental
# 7	    Diseases of the nervous system	4	Other	    3	Other
# 8	    Heart disease	                2	Cardio	    4	Cardio
# 9	    Cerebrovascular disease	        2	Cardio	    4	Cardio
# 10	Other unsp. dis. circ   	    2	Cardio	    4	Cardio
# 11	Respiratory diseases	        3	Infectious	1	Infectious
# 12	Diseases of the digestive       4	Other	    3	Other
# 13	Diseases of the skin	        4	Other	    3	Other
# 14	Diseases of the musculoske      4	Other	    3	Other
# 15	Diseases of the genitourin      4	Other	    3	Other
# 16	Compli preg,  child	            4	Other	    3	Other
# 17	Certain conditions originating  4	Other	    5	Infant/Cong.
# 18	Congenital malformations/anom   4	Other	    5	Infant/Cong.
# 19	Ill-defined or unknown	        6	Ill defined	6	Ill defined
# 20	External causes	                5	Injuries	7	External
xxx   <- local(get(load("/data/commons/triffe/HMDCOD.hg/RHMDCOD/data/hmdicdranges.rda")))

Codes <- tapply(xxx$chapter,xxx$chapterName, unique)
Names <- names(Codes)
ord   <- order(Codes)
Names <- Names[ord]
Codes <- Codes[ord]
names(Names) <- Codes
Codes2 <- tapply(xxx$cause,xxx$causeName, unique)
Names2 <- names(Codes2)
ord2   <- order(Codes2)
Names2 <- Names2[ord2]
Codes2 <- Codes2[ord2]
names(Names2) <- Codes2
unique(COD$COD.cat)

recvec <- c(1,2,2,3,3,7,2,4,4,4,1,3,3,3,3,3,5,5,3,6)
names(recvec) <- sprintf("%.2d",1:20)
TimCodes <- 1:7
names(TimCodes) <- c("Infectious","Cancer","Other","Cardio","Inf_Cong","External","Mental")

TimNames        <- names(TimCodes)
names(TimNames) <- 1:7

# step 1) go from 92 codes to 92 names, then 92 codes to 20 names and codes, then 8 names and codes.
COD$CauseName <- Names2[COD$COD.cat]

# 2) get 20 codes from 92 codes.
Codes3          <- tapply(xxx$chapter,xxx$cause, unique)
COD$ChapterCode <- Codes3[COD$COD.cat]
COD$Code7       <- recvec[COD$ChapterCode]
COD$Name7       <- TimNames[COD$Code7]
COD$ChapterName <- Names[COD$ChapterCode]

save(COD, file = "/data/commons/triffe/git/YearsLost/YearsLost/Data/USA2010.Rdata")

F92 <- tapply(COD$Deaths.F, COD$CauseName, sum)
M92 <- tapply(COD$Deaths.M, COD$CauseName, sum)
F20 <- tapply(COD$Deaths.F, COD$ChapterName, sum)
M20 <- tapply(COD$Deaths.M, COD$ChapterName, sum)
F7 <- tapply(COD$Deaths.F, COD$Name7, sum)
M7 <- tapply(COD$Deaths.M, COD$Name7, sum)

Out92 <- data.frame(Cause92 = names(F92), Females = F92, Males = M92, stringsAsFactors =FALSE)
Out20 <- data.frame(Cause20 = names(F20), Females = F20, Males = M20, stringsAsFactors =FALSE)
Out7 <- data.frame(Cause7 = names(F7), Females = F7, Males = M7, stringsAsFactors =FALSE)

write.table(Out92, file = "/data/commons/triffe/HMDCOD.hg/test/USA/CODoutput/Out92.csv", sep = ",")
write.table(Out20, file = "/data/commons/triffe/HMDCOD.hg/test/USA/CODoutput/Out20.csv", sep = ",")
write.table(Out7, file = "/data/commons/triffe/HMDCOD.hg/test/USA/CODoutput/Out7.csv", sep = ",")

#
## open file connection
#f <- file("/data/commons/triffe/VS13MORT.DUSMCPUB")
#dir("/data/commons/triffe")
## install.packages("sqldf")
#library(sqldf)
#
## month of death: pos 65, 2 wide
## sex: pos 69, 1
## Age 70, 4
## 39 cause recode: 160, 2
#demo("sqldf-unitTests")
#system.time(
#  US2013 <- sqldf("select 
#      substr(V1, 160, 2) Cause, 
#      substr(V1, 69, 1) Sex, 
#      substr(V1, 70, 4) Age, 
#      substr(V1, 65, 2) Month, 
#      count(*) Deaths 
#      from f 
#      group by  
#      Cause, 
#      Sex, 
#      Age, 
#      Month"))
##user  system elapsed 
##88.518  20.041 515.501  
#dim(US2010)
##[1] 68845     5
#head(US2010)
##  Cause Sex  Age Month Deaths
##1    01   F 1002    08      1
##2    01   F 1007    11      1
##3    01   F 1009    12      1
##4    01   F 1010    10      1
##5    01   F 1017    12      1
##6    01   F 1018    06      1
#
## close the connection
#close(f)
#

# quick test of random thought
# ----------------------------------------------------
HMD <- local(get(load("/home/tim/git/YearsLost/YearsLost/Data/HMD.Rdata")))
names(HMD$USA)

Dxm <- HMD$USA$Dxm
Mxm <- HMD$USA$Mxm
Exm <- Dxm / Mxm

mx2ex <- function(mx){
 lx <-	c(1,exp(cumsum(-mx)))
 Lx <- (lx[1:(length(lx)-1)] + lx[2:length(lx)])/2
 rev(cumsum(rev(Lx)))
}
plot(mx2ex(Mxm))
Exmx2PYLL <- function(Ex,mx){
	Dx <- Ex * mx
	ex <- mx2ex(mx)
	sum(Dx*ex,na.rm=TRUE)
}
PYLL1 <- Exmx2PYLL(Exm, Mxm)
PYLL2 <- Exmx2PYLL(Exm, Mxm/2)
PYLL3 <- Exmx2PYLL(Exm, Mxm*2)

PYL1;PYL2;PYL3
(PYL2/PYL1+PYL1/PYL3)/2

facs <- c(1/rev(seq(1,3,by=.1)),seq(1,3,by=.1))
e0vec <- sapply(facs,function(fac,mx){
			mx2ex(mx*fac)[1]
		},mx=Mxm)
ratiovec <- sapply(facs, function(fac,mx,Ex){
			PYL1 <- Exmx2PYLL(Ex, mx)
			PYL2 <- Exmx2PYLL(Ex, mx*fac)
			PYL2 / PYL1
		},mx=Mxm,Ex=Exm)
plot(facs,ratiovec)
abline(h=1);abline(v=1)
abline(lm(ratiovec~facs))

PYLLvec <- sapply(facs, function(fac,mx,Ex){
			Exmx2PYLL(Ex, mx*fac)
		},mx=Mxm,Ex=Exm)
PYLvec <- sapply(facs, function(fac,mx,Ex){
			sum(Ex * mx2ex(mx*fac))
		},mx=Mxm,Ex=Exm)

plot(facs,PYLLvec / PYLvec,log='x')

PYL1 <- sum(Exm * mx2ex(Mxm))
PYL2 <- sum(Exm * mx2ex(Mxm)/2)
PYL3 <- sum(Exm * mx2ex(Mxm)*2)

PYLL1 / PYL1
PYLL2 / PYL2
(PYLL3 / PYL3) / (PYLL1 / PYL1)
(PYLL1 / PYL1) / (PYLL2 / PYL2)
#abline(v=c(.5,1,2))
#plot(facs,PYLLvec)
#plot(diff(PYLLvec)/PYLLvec[-1] )
# at least with this population exposure...
# interpretation: a 10% decrease / increase is only worth 3.8% decrease/increase in PYLL!
# wow. That's cool. Entropy shows its head.

sum(Exm * mx2ex(Mxm)) / 
sum(Exm * mx2ex(Mxm))

# and what about as a period indicator over the SWE time series?

HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))
SWE <- HMD[HMD$CNTRY == "SWE", ]

# first as a stationary population measure. Then as a stable population measure?
SWE$Wx <- SWE$Lx * SWE$ex
SWE$WDx <- SWE$Wx * SWE$mx
SWE$Rx <- SWE$WDx / SWE$Wx


SWEL <- split(SWE, list(SWE$Year,SWE$Sex))
SWE <- do.call(rbind,lapply(SWEL, function(DAT){
			DAT$WDxC <- rev(cumsum(rev(DAT$WDx)))
			DAT$WxC  <- rev(cumsum(rev(DAT$Wx)))
			DAT$WRx  <- DAT$WDxC  / DAT$WxC
			DAT$WRx[is.infinite(DAT$WRx)] <- NA
			DAT
		}))

library(LexisUtils)
library(reshape2)
LexisMap(acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "WRx") ,log=FALSE)
LexisMap(acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "Rx"),log=TRUE)
LexisMap(acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "WRx") - 
				acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "Rx"),log=FALSE)
plot(acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "WRx")[,1])
# what's the meaning of cumulative... is there a point to this measure?
plot(1751:2011, acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "WRx")[1,])
plot(1751:2011, acast(SWE[SWE$Sex == "m", ],Age~Year,value.var = "Rx")[50,])

plot(DAT$mx,log='y')
lines(rev(cumsum(rev(DAT$WDx)))  / rev(cumsum(rev(DAT$Wx))))







