
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

COD <- local(get(load("Data/COD.Rdata")))

# "Cancer"      "Cardio"      "External"    "Ill defined" "Inf/Cong"   
# "Infectious"  "Mental"      "Other"   

names(COD$USA)

cause <- "External"
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

pdf(paste0("Figures/Deathsxy10",XYZ,cause,".pdf"),height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Lived",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
dev.off()


# Leaf
Males   <- ThanoAgg(Dxmc, dxmc, FALSE, N = 10)
Females <- ThanoAgg(Dxfc, dxfc, FALSE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxmc, dxmc)), rowSums(Thano(Dxfc, dxfc))))) * c(-1, 1)
# originally from ThanoDeath.R
pdf(paste0("Figures/Deathsyx10",XYZ,cause,".pdf"),height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = FALSE, colRamp = BrewerRamp("BuGn"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Left",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("BuGn"), label = "Years Lived",revColors = TRUE)
dev.off()

y <- .5:110.5
sum(Dxmc*y)/sum(Dxmc)
sum(Dxfc*y)/sum(Dxfc)
Dcym <- colSums(Males)
Dcyf <- colSums(Females)


names(COD$USA)
LDM <- makeLD(lxmc)
LDF <- makeLD(lxfc)
#(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))


Males   <- t(LDM)[2:112,2:112] * Dxmc
Females <- t(LDF)[2:112,2:112] * Dxfc

MalesY     <- AggM(Males, TRUE, N = 10)
FemalesY   <- AggM(Females, TRUE, N = 10)
MalesA     <- AggM(Males, FALSE, N = 10)
FemalesA   <- AggM(Females, FALSE, N = 10)
xlim       <- max(pretty(c(colSums(MalesA), colSums(FemalesA)))) * c(-1, 1)

# originally from AgeDeathPyramid.R
pdf(paste0("Figures/YearsLostLivedyx10",XYZ,cause,".pdf"),height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesY, FemalesY, revColors = FALSE, colRamp = BrewerRamp("Oranges"))
PyramidOutline(colSums(MalesY), colSums(FemalesY), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Ages gained",
        xlab = "Years gained (1000s)", 
        xlabs = zapsmall(abs(tics$x / 1e3)), cex=.7)
Drawlegend(tics, N = min(dim(MalesY)), colRamp = BrewerRamp("Oranges"), revColors = TRUE, label = "Age Saved at")
dev.off()

# originally from AgeDeathPyramid.R
pdf(paste0("Figures/YearsSavedGainedxx10",XYZ,cause,".pdf"),height = 4,width = 4.6)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .6, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(MalesA, FemalesA, revColors = FALSE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(MalesA), colSums(FemalesA), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Age Saved at",xlab = "Years gained (100s)", 
        xlabs = zapsmall(abs(tics$x / 1e3)), cex = .7)
Drawlegend(tics, N = min(dim(MalesA)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Ages gained")
dev.off()

Males   <- t(LDM)[2:112,2:112] * Dxmc
Females <- t(LDF)[2:112,2:112] * Dxfc

# rowSums = thano, colsums = chrono
plot(colSums(Males))
plot(rowSums(Males))

sum(colSums(Males)*y)/sum(Males)
sum(colSums(Females)*y)/sum(Females)

sum(rowSums(Males)*y)/sum(Males)
sum(rowSums(Females)*y)/sum(Females)


