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

# does thanatological decomposition, groups 10-year thanatological age classes within chronological age
Males   <- ThanoAgg(Dxm, dxm, TRUE, N = 10)
Females <- ThanoAgg(Dxf, dxf, TRUE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf))))) * c(-1, 1)

pdf("Figures/Deathsxy10.pdf",height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Lived",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
dev.off()




# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.

#plot(rowSums(Thano(dxm,dxm)))
#plot(rowSums(Thano(Dxm,dxm)))
#plot(rowSums(Thano(dxf,dxf)))
#plot(rowSums(Thano(Dxf,dxf)))
#sum(Dxm) # males persons lost
#sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) # total male person-years lost due to death
#sum(Dxf) # females lost
#sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) # total female person-years lost due to death
#
#sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) / sum(Dxm) # lose 16.3 PYL per male
#sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) / sum(Dxf)  # lose 13.7 PYL per female
# OK, but if each is repeated over x:x+y?

LDM <- makeLD(lxm)
LDF <- makeLD(lxf)
#
#image(LDM)
#image(LDF)
#image(LDF / LDM)
##all(LDM == t(1/LDM), na.rm= TRUE)
#
## ages go down the matrix
#head(LD)[,1:6]
#plot(LDM[,1])
#LDM[,1] == lxm
# so:
#plot(rowSums(t(LDM)[2:112,2:112] * Dxm,na.rm=TRUE))
#plot(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))
#

Males   <- t(LDM)[2:112,2:112] * Dxm
Females <- t(LDF)[2:112,2:112] * Dxf

# funny surface
#HM <- Males + t(Females)
#diag(HM) <- diag(HM) / 2
#image(HM)

MalesY     <- AggM(Males, TRUE, N = 10)
FemalesY   <- AggM(Females, TRUE, N = 10)
MalesA     <- AggM(Males, FALSE, N = 10)
FemalesA   <- AggM(Females, FALSE, N = 10)
xlim       <- max(pretty(c(colSums(MalesA), colSums(FemalesA)))) * c(-1, 1)

#xlim     <- max(pretty(c(colSums(MalesY), colSums(FemalesY)))) * c(-1, 1)

pdf("Figures/YearsLostLivedyx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("BuGn"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Ages gained",xlab = "Years gained (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("BuGn"), revColors = TRUE, label = "Age Saved at")
dev.off()


pdf("Figures/YearsSavedGainedxx10.pdf",height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesA, FemalesA, revColors = FALSE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(MalesA), colSums(FemalesA), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Age Saved at",xlab = "Years gained (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
Drawlegend(tics, N = min(dim(MalesA)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Ages gained")
dev.off()

sum(MalesA)
sum(MalesY)
sum(FemalesA)
sum(FemalesY)
sum(Dxm) 
sum(Dxf)
sum(Dxm) + sum(Dxf)
sum(MalesA) + sum(FemalesA)

# saved-years-weighted mean age at which lives would be saved
sum((rowSums(Males) + rowSums(Females)) * .5:110.5) / sum(Males+Females)

# mean age in which saved lives are lived after saving
sum((colSums(Males) + colSums(Females)) * .5:110.5) / sum(Males+Females)
# i.e., average age of a life once-saved
sum(colSums(Males)* .5:110.5) / sum(Males)
sum(colSums(Females)* .5:110.5) / sum(Females)

plot(rowSums(Males) + rowSums(Females))
plot(colSums(Males) + colSums(Females))

sum(Males[51,])
Dxm[51]
fy50 <- dxm[51:111] / lxm[51]

(sum(fy50 * 50.5:110.5) - 50) * Dxm[51]











