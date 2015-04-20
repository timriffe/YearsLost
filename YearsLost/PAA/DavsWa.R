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
tics     <- plotSetup(xlim)
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
tics     <- plotSetup(xlim)
DrawLabels(tics, ylab = "Thano Age",xlab = "count (1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
PyramidOutline(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf)), 
		col = "#00000030", border = gray(.2), lwd = .5)
dev.off()
###############################################################
# 3)Back to deaths          3 D(a)                            #
###############################################################
pdf("PAA/Figures/f3_Da.pdf",height = 4,width = 4.6)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, .9))
tics     <- plotSetup(xlim)
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
tics     <- plotSetup(xlim2)
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
tics <- plotSetup(xlim2)
#PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = "#00000030", border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Chrono Age",xlab = "years (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
#Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

#plot(0:110,colSums(MalesY),type='l',ylim=c(0,6e5))
#lines(0:110,colSums(FemalesY))
#sum(FemalesY)
#sum(Dxf)



