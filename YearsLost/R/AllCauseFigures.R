# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/YearsLost/YearsLost")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
}
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

# does thanatological decomposition, groups 10-year thanatological age classes within chronological age
Males   <- ThanoAgg(Dxm, dxm, TRUE, N = 10)
Females <- ThanoAgg(Dxf, dxf, TRUE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf))))) * c(-1, 1)
# originally from AgeDeathPyramid.R
pdf("Figures/Deathsxy10.pdf",height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Lived",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
dev.off()

# xlim is shared
xlim     <- max(pretty(c(Dxm, Dxf))) * c(-1, 1)

# Leaf
Males   <- ThanoAgg(Dxm, dxm, FALSE, N = 10)
Females <- ThanoAgg(Dxf, dxf, FALSE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf))))) * c(-1, 1)
# originally from ThanoDeath.R
pdf("Figures/Deathsyx10.pdf",height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = FALSE, colRamp = BrewerRamp("BuGn"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Left",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("BuGn"), label = "Years Lived",revColors = TRUE)
dev.off()


LDM <- makeLD(lxm)
LDF <- makeLD(lxf)
#(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))


Males   <- t(LDM)[2:112,2:112] * Dxm
Females <- t(LDF)[2:112,2:112] * Dxf

MalesY     <- AggM(Males, TRUE, N = 10)
FemalesY   <- AggM(Females, TRUE, N = 10)
MalesA     <- AggM(Males, FALSE, N = 10)
FemalesA   <- AggM(Females, FALSE, N = 10)
xlim       <- max(pretty(c(colSums(MalesA), colSums(FemalesA)))) * c(-1, 1)

# originally from AgeDeathPyramid.R
pdf("Figures/YearsLostLivedyx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Ages gained",xlab = "Years gained (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

# originally from AgeDeathPyramid.R
pdf("Figures/YearsSavedGainedxx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesA, FemalesA, revColors = FALSE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(MalesA), colSums(FemalesA), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Age Saved at",xlab = "Years gained (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
Drawlegend(tics, N = min(dim(MalesA)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Ages gained")
dev.off()