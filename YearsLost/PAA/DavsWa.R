# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/YearsLost/YearsLost")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
}

# read in HMD USA 2010:

#HMD <- local(get(load("/home/tim/git/YearsLost/YearsLost/Data/HMD.Rdata")))
#HMD$USA

##########################################
# Make figure that jumps from Dx to Wx....

# 2) then a diagram showing a lifeline that starts at x and increases to x+y...

# 3) then the W figure where ages are appropriately shifted up: the cumulative years of life foregone.
#

# savedGained....


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


###############################################################
# 1) deaths, transparent gray.  2 D(a)                        #
###############################################################
# shae xlim with next two plots.
xlim     <- max(pretty(c(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf))))) * c(-1, 1)
pdf("PAA/Figures/f1_Da.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics     <- plotSetup(xlim,lwd=1)
#PyrLevels(DXYM, DXYF, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(Dxm, Dxf, col = "#00000030", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "count (1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
dev.off()

###############################################################
# 2) Remaining lifespans lost. 2 D(y)                         #
###############################################################

#PyrLevels(DXYM, DXYF, revColors = TRUE, colRamp = BrewerRamp("Blues"))
#PyramidOutline(Dxm, Dxf, col = NA, border = gray(.2), lwd = .5)
pdf("PAA/Figures/f2_Dy.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics     <- plotSetup(xlim,lwd=1)
DrawLabels(tics, ylab = "Thano Age",xlab = "count (1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
PyramidOutline(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf)), 
		col = "#00000030", border = gray(.2), lwd = .5)
dev.off()
###############################################################
# 3)Back to deaths          3 D(a)                            #
###############################################################
pdf("PAA/Figures/f3_Da.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics     <- plotSetup(xlim,lwd=1)
#PyrLevels(DXYM, DXYF, revColors = TRUE, colRamp = BrewerRamp("Blues"))
#PyramidOutline(Dxm, Dxf, col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "count (1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
PyramidOutline(Dxm, Dxf, 
		col = "#00000030", border = gray(.2), lwd = .5)
dev.off()
###############################################################
# 4) deaths again, scale changed  4 D(a)                      #
###############################################################

LDM <- makeLD(lxm)
LDF <- makeLD(lxf)
#(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))

Males      <- t(LDM)[2:112,2:112] * Dxm
Females    <- t(LDF)[2:112,2:112] * Dxf

MalesY     <- AggM(Males, TRUE, N = 10)
FemalesY   <- AggM(Females, TRUE, N = 10)
MalesA     <- AggM(Males, FALSE, N = 10)
FemalesA   <- AggM(Females, FALSE, N = 10)

xlim2       <- max(pretty(c(colSums(MalesA), colSums(FemalesA)))) * c(-1, 1)

pdf("PAA/Figures/f4_Da.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics     <- plotSetup(xlim2,lwd=1)
#PyrLevels(DXYM, DXYF, revColors = TRUE, colRamp = BrewerRamp("Blues"))
#PyramidOutline(Dxm, Dxf, col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "count (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
PyramidOutline(Dxm, Dxf, col = "#00000030", border = gray(.2), lwd = .5)
dev.off()

###############################################################
# 4) overlay D(a)e(a)             5 D(a)e(a)                  #
###############################################################

# originally from AgeDeathPyramid.R
#pdf("Figures/YearsSavedGainedxx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
pdf("PAA/Figures/f5_Daea.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics <- plotSetup(xlim2)
#PyrLevels(MalesA, FemalesA, revColors = FALSE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(MalesA), colSums(FemalesA), col = "#00000030", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "years (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
#Drawlegend(tics, N = min(dim(MalesA)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Ages gained")
#PyramidOutline(Dxm, Dxf, col = "#00000030", border = gray(.2), lwd = 1)
dev.off()

###############################################################
# 5) overlay D(a)e(a)             6 Ages won!                 #
###############################################################

# originally from AgeDeathPyramid.R
#pdf("Figures/YearsLostLivedyx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
pdf("PAA/Figures/f6_AgesWon.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics <- plotSetup(xlim2,lwd=1)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = "#00000030", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "years (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

#plot(0:110,colSums(MalesY),type='l',ylim=c(0,6e5))
#lines(0:110,colSums(FemalesY))
#sum(FemalesY)
#sum(Dxf)

##############################################################
# Cause stuff

COD <- local(get(load("/home/tim/git/YearsLost/YearsLost/Data/COD.Rdata")))
names(COD)
USA <- COD$USA

names(USA)


cause <- "Cardio"
XYZ   <- "USA"
# cause-eliminated stuff:
dxfc <- COD[[XYZ]]$Kdxf[,cause]
dxmc <- COD[[XYZ]]$Kdxm[,cause]
lxmc <- COD[[XYZ]]$Klxm[,cause]
lxfc <- COD[[XYZ]]$Klxf[,cause]
# Deaths from cause:
Dxfc <- COD[[XYZ]]$Dxfc[,cause]
Dxmc <- COD[[XYZ]]$Dxmc[,cause]

Males   <- ThanoAgg(Dxmc, dxmc, TRUE, N = 10)
Females <- ThanoAgg(Dxfc, dxfc, TRUE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxmc, dxmc)), rowSums(Thano(Dxfc, dxfc))))) * c(-1, 1)
# originally from AgeDeathPyramid.R

pdf(paste0("PAA/Figures/f7_Dac.pdf"),height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics <- plotSetup(xlim,lwd=1)
#PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(Males), colSums(Females), col = "#00000030", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
#Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
dev.off()

# xlim is shared
LDM <- makeLD(lxmc)
LDF <- makeLD(lxfc)
Males   <- t(LDM)[2:112,2:112] * Dxmc
Females <- t(LDF)[2:112,2:112] * Dxfc

MalesY     <- AggM(Males, TRUE, N = 10)
FemalesY   <- AggM(Females, TRUE, N = 10)
MalesA     <- AggM(Males, FALSE, N = 10)
FemalesA   <- AggM(Females, FALSE, N = 10)
xlim       <- max(pretty(c(colSums(MalesA), colSums(FemalesA)))) * c(-1, 1)

# originally from AgeDeathPyramid.R
pdf(paste0("PAA/Figures/f8_AgesWonc.pdf"),height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim,lwd=1)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = "#00000020", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",
		xlab = "years (1000s)", 
		xlabs = zapsmall(abs(tics$x / 1e3)), cex=.7)
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()


pdf(paste0("PAA/Figures/f9_AgesWoncdec.pdf"),height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim,lwd=1)
PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",
		xlab = "years (1000s)", 
		xlabs = zapsmall(abs(tics$x / 1e3)), cex=.7)
Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()


cause <- "External"
XYZ   <- "USA"
# cause-eliminated stuff:
dxfce <- COD[[XYZ]]$Kdxf[,cause]
dxmce <- COD[[XYZ]]$Kdxm[,cause]
lxmce <- COD[[XYZ]]$Klxm[,cause]
lxfce <- COD[[XYZ]]$Klxf[,cause]
# Deaths from cause:
Dxfce <- COD[[XYZ]]$Dxfc[,cause]
Dxmce <- COD[[XYZ]]$Dxmc[,cause]

# xlim is shared
#xlim     <- max(pretty(c(rowSums(Thano(Dxmc, dxmc)), rowSums(Thano(Dxfc, dxfc))))) * c(-1, 1)
# originally from AgeDeathPyramid.R

# xlim is shared
LDMe     <- makeLD(lxmce)
LDFe     <- makeLD(lxfce)
Malese   <- t(LDMe)[2:112,2:112] * Dxmce
Femalese <- t(LDFe)[2:112,2:112] * Dxfce

MalesYe     <- AggM(Malese, TRUE, N = 10)
FemalesYe   <- AggM(Femalese, TRUE, N = 10)
xlim       <- max(pretty(c(colSums(MalesAe), colSums(FemalesAe)))) * c(-1, 1)


Gxme <- colSums(MalesYe)
Gxfe <- colSums(FemalesYe)
Gxmc <- colSums(MalesY)
Gxfc <- colSums(FemalesY)


# originally from AgeDeathPyramid.R
pdf(paste0("PAA/Figures/f11_AgesWoncomp.pdf"),height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(c(-2,2),lwd=1)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(Gxme, Gxfe, col = "#FF000020", border = gray(.2), lwd = .5, scale = 100)
PyramidOutline(Gxmc, Gxfc, col = "#0000FF20", border = gray(.2), lwd = .5, scale = 100)
DrawLabels(tics, ylab = "Chrono Age",
		xlab = "years (percent)", 
		xlabs = paste(zapsmall(abs(tics$x)),"%"), cex=.7)
text(.8,90,"Cardio",srt=-45,cex=1.2)
text(-.75,50,"External",srt=-45,cex=1.2)
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

##########################################
XYZ2   <- "FRA"
cause <- "Cardio"
# cause-eliminated stuff:
dxfc2 <- COD[[XYZ2]]$Kdxf[,cause]
dxmc2 <- COD[[XYZ2]]$Kdxm[,cause]
lxmc2 <- COD[[XYZ2]]$Klxm[,cause]
lxfc2 <- COD[[XYZ2]]$Klxf[,cause]
# Deaths from cause:
Dxfc2 <- COD[[XYZ2]]$Dxfc[,cause]
Dxmc2 <- COD[[XYZ2]]$Dxmc[,cause]

# xlim is shared
#xlim     <- max(pretty(c(rowSums(Thano(Dxmc, dxmc)), rowSums(Thano(Dxfc, dxfc))))) * c(-1, 1)
# originally from AgeDeathPyramid.R

# xlim is shared
LDM2     <- makeLD(lxmc2)
LDF2     <- makeLD(lxfc2)
Males2   <- t(LDM2)[2:112,2:112] * Dxmc2
Females2 <- t(LDF2)[2:112,2:112] * Dxfc2

MalesY2     <- AggM(Males2, TRUE, N = 10)
FemalesY2   <- AggM(Females2, TRUE, N = 10)

Gxm2 <- colSums(MalesY2)
Gxf2 <- colSums(FemalesY2)
Gxmc <- colSums(MalesY)
Gxfc <- colSums(FemalesY)

pdf("PAA/Figures/f12_AgesWoncountries.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(c(-2.5,2.5),lwd=1)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(Gxm2, Gxf2, col = "#FF000020", border = gray(.2), lwd = .5, scale = 100)
PyramidOutline(Gxmc, Gxfc, col = "#0000FF40", border = gray(.2), lwd = .5, scale = 100)
DrawLabels(tics, ylab = "Chrono Age",
		xlab = "years (percent)", 
		xlabs = paste(zapsmall(abs(tics$x)),"%"), cex=.7)
text(1.75,95,"France",srt=-45,cex=1.2)
text(-1,70,"USA",srt=-45,cex=1.2)
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()


# compare stationary
lxm <- HMD$USA$lxm
lxf <- HMD$USA$lxf
Lxm <- (lxm[1:111] + lxm[2:112])/2
Lxf <- (lxf[1:111] + lxf[2:112])/2

names(COD$USA)
LDMs     <- makeLD(lxmc)
LDFs     <- makeLD(lxfc)
dxms <- -diff(lxmc)
dxfs <- -diff(lxfc)
Maless   <- t(LDMs)[2:112,2:112] * COD$USA$Mxmc[,cause] * Lxm
Femaless <- t(LDFs)[2:112,2:112] * COD$USA$Mxfc[,cause] * Lxf

MalesYs     <- AggM(Maless, TRUE, N = 10)
FemalesYs   <- AggM(Femaless, TRUE, N = 10)

Gxms <- colSums(MalesYs)
Gxfs <- colSums(FemalesYs)

pdf("PAA/Figures/f13_AgesWonstationary.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(c(-2.5,2.5),lwd=1)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(Gxms, Gxfs, col = "#FF000020", border = gray(.2), lwd = .5, scale = 100)
PyramidOutline(Gxmc, Gxfc, col = "#0000FF40", border = gray(.2), lwd = .5, scale = 100)
DrawLabels(tics, ylab = "Chrono Age",
		xlab = "years (percent)", 
		xlabs = paste(zapsmall(abs(tics$x)),"%"), cex=.7)
text(-1,100,"stationary",srt=20,cex=1.2)
text(-1,65,"raw impact",srt=-45,cex=1.2)
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

################################################


library(DemogBerkeley)
Dx <- readHMDweb("USA","Deaths_1x1",username = us, password = pw)
Ex <- readHMDweb("USA","Exposures_1x1",username = us, password = pw)
mxm <- readHMDweb("USA","mltper_1x1",username = us, password = pw)
mxf <- readHMDweb("USA","fltper_1x1",username = us, password = pw)

head(Dx)
Dxm <- acast(Dx, Age~Year, value.var = "Male")
Exm <- acast(Ex, Age~Year, value.var = "Male")
Dxf <- acast(Dx, Age~Year, value.var = "Female")
Exf <- acast(Ex, Age~Year, value.var = "Female")
Mxm <- acast(mxm, Age~Year, value.var = "mx")
Mxf <- acast(mxf, Age~Year, value.var = "mx")

#Mxm <- Dxm / Exm
any(is.na(Mxm))
Gxf <- Gxm <- Exm * 0 #yr <- 261
for (yr in 1:ncol(Exm)){
	
	LXM <- makeLD(mx2lx(Mxm[,yr])[-112])
	LXF <- makeLD(mx2lx(Mxf[,yr])[-112])
	Gxm[,yr] <- colSums(Dxm[,yr] * LXM) /colSums(Exm[,yr] * LXM)
	Gxf[,yr] <- colSums(Dxf[,yr] * LXF) /colSums(Exf[,yr] * LXF)
}

pdf("PAA/Figures/f14_linerate.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.9,.9,.1,.2))
plot(0:110,Mxm[,"2010"],type='l',log='y', col = "#0000FF50", ylim=c(1e-4,1),lty=2,axes=FALSE,xlab="Age",ylab="Rate")
lines(0:110,Mxf[,"2010"],col="#FF000050",lty=2)
lines(0:110,Gxm[,"2010"],col="#0000FF",lwd=2)
lines(0:110,Gxf[,"2010"],col="#FF0000",lwd=2)
segments(-5,c(1e-4,1e-3,1e-2,1e-1,1),-4,c(1e-4,1e-3,1e-2,1e-1,1),xpd=TRUE)
segments(-4,1e-4,-4,1,xpd=TRUE)
text(-5,c(1e-4,1e-3,1e-2,1e-1,1),c(1e-4,1e-3,1e-2,1e-1,1),pos=2,xpd=TRUE,cex=.8)
segments(seq(0,110,10),.5e-4,seq(0,110,10),.45e-4,xpd=TRUE)
segments(0,.5e-4,110,.5e-4,xpd=TRUE)
text(seq(0,110,10),.5e-4,seq(0,110,10),pos=1,xpd=TRUE,cex=.8)
# locator(1)
text(25.5,.0113,"Male")
text(27.5,.0054, "Female")
dev.off()

range(Gxm/Gxf)
LexisMap(Gxm/Gxf,zlim=c(.5,2))
LexisMap(Gxm,zlim=c(.005,1), colramp=)
matplot(Gxm/Gxf,type='l',lty=1,col="#00000030")
#library(LexisUtils)
LexisMap(Gxm,contour=TRUE)

plot(as.integer(colnames(Exm)),apply(Gxm,2,function(mx){
					sum(exp(-cumsum(mx)),na.rm=TRUE)
				}),type='l')

Gxmprime <- Exm * 0
for (yr in 1:ncol(Exm)){
	lx <- mx2lx(Mxm[,yr])
	dx <- -diff(lx)
	Lx <- (lx[1:111] + lx[2:112]) / 2
	LX <- makeLD(Lx)
	
	Gxmprime[,yr] <- colSums(dx * LX) /colSums(Lx * LX)
}
plot(as.integer(colnames(Exm)),apply(Gxmprime,2,function(mx){
					sum(exp(-cumsum(mx)),na.rm=TRUE)
				}),type='l')
LexisMap(Gxmprime,contour=TRUE)
LexisMap(Gxm,contour=TRUE)
matplot(Gxmprime,type='l',lty=1,lwd=1,col="#00000050",log='y')
matplot(Gxm[,1:200],type='l',lty=1,lwd=1,col="#00000050",log='y')

# ratio
ratiovec <- c()
for (yr in 1:ncol(Exm)){
	
	LX <- makeLD(mx2lx(Mxm[,yr])[-112])
	ratiovec[yr] <- sum(colSums(Dxm[,yr] * LX)) / sum(colSums(Exm[,yr] * LX))
}
ratiovecprime <- c()
for (yr in 1:ncol(Exm)){
	lx <- mx2lx(Mxm[,yr])
	dx <- -diff(lx)
	Lx <- (lx[1:111] + lx[2:112]) / 2
	LX <- makeLD(Lx)
	
	ratiovecprime[yr] <- sum(colSums(dx * LX)) / sum(colSums(Lx * LX))
}
plot(1751:2011,ratiovec,type='l')
lines(1751:2011,ratiovecprime,col='red')
