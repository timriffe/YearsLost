

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
countries <- names(COD)
causes    <- colnames(COD[[countries[1]]]$Dxfc)
# need to loop over cause and country
cause <- "External"
XYZ   <- "USA"
# Deaths from cause:
Dxfc <- COD[[XYZ]]$Dxfc[,cause]
Dxmc <- COD[[XYZ]]$Dxmc[,cause]

Males   <- ThanoAgg(Dxmc, dxmc, TRUE, N = 10)
Females <- ThanoAgg(Dxfc, dxfc, TRUE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxmc, dxmc)), rowSums(Thano(Dxfc, dxfc))))) * c(-1, 1)
# originally from AgeDeathPyramid.R
xlim     <- c(-1.5,1.5)
#pdf(paste0("Figures/Deathsxy10",XYZ,cause,".pdf"),height = 4,width = 4.5)
dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"), scale = 100)
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5, scale = 100)
#DrawLabels(tics, ylab = "Years Lived",xlab = "percent", xlabs = tics$x )
#Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
#dev.off()

# draw with no color legend for now.
XYZ <- "USA"
cause <- "External"
plotXYZcause1 <- function(COD, XYZ, cause, xlim = c(-1.5,1.5)){
  Dxfc <- COD[[XYZ]]$Dxfc[,cause]
  Dxmc <- COD[[XYZ]]$Dxmc[,cause]
  
  Males   <- ThanoAgg(Dxmc, dxmc, TRUE, N = 20)
  Females <- ThanoAgg(Dxfc, dxfc, TRUE, N = 20)
  tics <- plotSetup(xlim)
  PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"), scale = 100)
  PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5, scale = 100)
  invisible(tics)
}
ylabels <- function(tics,cex = .8,left=TRUE){
 
  if (left){
    text(tics$xl, tics$y, tics$y, pos = 2, cex = cex, xpd = TRUE)
  } else {
    text(tics$xr, tics$y, tics$y, pos = 4, cex = cex, xpd = TRUE)
  }
}
xlabels <- function(tics,cex = .8,below=TRUE){
  xdiff <- diff(tics$x[1:2])
  if (below){
    text(tics$x, tics$yb, tics$x, pos = 1, cex = cex, xpd = TRUE)
  } else {
    text(tics$x, tics$yt, tics$x, pos = 3, cex = cex, xpd = TRUE)
  }
}

# try 2x3 matrix of plots
# only put y axes on left side and x axes on bottom (gray background grid for everything, though
graphics.off()

dev.new(height = 8,width = 6)

plotDxy <- function(COD,cause = "External",countries,xlim=c(-1.6,1.6)){
pp <- layout(matrix(c(1,2,3,4,5,6,7,7,7), 3, 3, byrow=FALSE), c(2.5,2.5,1), c(2.5,2.5,2.5,2.5,2.5,2.5,6), T)
#layout.show(pp)
par(xaxs="i",yaxs="i",oma=c(0,1,0,0))
# left side
# top
par(mai = c(.05+.2, .28, .25, .02))
tics <- plotXYZcause1(COD, XYZ = countries[1], cause = cause,xlim=xlim)
ylabels(tics, left = TRUE)
text(0,-12,countries[1],xpd=TRUE)
#mtext(-2,115,"Years Lived",pos=4,xpd=TRUE)
#xlabels(tics,below=FALSE)
# middle
par(mai = c(.15+.2, .28, .15, .02))
tics <- plotXYZcause1(COD, XYZ = countries[2], cause = cause,xlim=xlim)
#ylabels(tics, left = TRUE)
text(0,-12,countries[2],xpd=TRUE)
# bottom
par(mai = c(.25+.2, .28, .05, .02))
tics <- plotXYZcause1(COD, XYZ = countries[3], cause = cause,xlim=xlim)
ylabels(tics, left = TRUE)
xlabels(tics,below=TRUE)
text(0,-12,countries[3],xpd=TRUE)

# right side
#top
par(mai = c(.05+.2, .02, .25, .28))
tics <- plotXYZcause1(COD, XYZ = countries[4], cause = cause,xlim=xlim)
#ylabels(tics, left = FALSE)
xlabels(tics,below=FALSE)
text(0,-12,countries[4],xpd=TRUE)
#middle
par(mai = c(.15+.2, .02, .15, .28))
tics <- plotXYZcause1(COD, XYZ = countries[5], cause = cause,xlim=xlim)
ylabels(tics, left = FALSE)
text(0,-12,countries[5],xpd=TRUE)
# bottom
par(mai = c(.25+.2, .02, .05, .28))
tics <- plotXYZcause1(COD, XYZ = countries[6], cause = cause,xlim=xlim)
#ylabels(tics, left = FALSE)
#xlabels(tics,below=TRUE)
text(0,-12,countries[6],xpd=TRUE)
#sapply(countries[4:6], plotXYZcause1, COD=COD, cause = "External", xlim = c(-1.6,1.6))
par(mai = c(0,0,0,0))
plotSetup(xlim= c(0,1), ylim = c(0,100), bg = NA, grid = FALSE)
N <- 5
yat   <- seq(30,70, length = N + 1)
rect(.2, yat[1:N], .4, yat[2:(N + 1)], 
  col = BrewerRamp("Blues")(N), border = gray(.3), lwd = .5, xpd = TRUE)
text(.4,yat,c(0,20,40,60,80,"100+"),pos=4)
text(.3,75,"Years Left",cex=1.2)
mtext(cause, side=3, line=2, cex=1.5, col="black", outer=FALSE, at = -2.5, padj=1) 
}

getwd()
graphics.off()
folder <- "/data/commons/triffe/git/YearsLost/YearsLost/Figures/Causes"
pdf(file.path(folder,paste0("Dxy",causes[1],".pdf")),height = 8,width = 6)
#dev.new(height = 8,width = 6)
plotDxy(COD,causes[1],countries,xlim=c(-1.6,1.6))
dev.off()
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[2],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[2],countries,xlim=c(-2.8,2.8))
dev.off()
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[3],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[3],countries)
dev.off()
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[4],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[4],countries, xlim=c(-4,4))
dev.off()
#dev.new(height = 8,width = 6)
#plotDxy(COD,causes[5],countries, xlim=c(-20,20))
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[6],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[6],countries, xlim=c(-2.5,2.5))
dev.off()
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[7],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[7],countries, xlim=c(-4,4))
dev.off()
#dev.new(height = 8,width = 6)
pdf(file.path(folder,paste0("Dxy",causes[8],".pdf")),height = 8,width = 6)
plotDxy(COD,causes[8],countries, xlim=c(-2.5,2.5))
dev.off()



plotXYZcause2 <- function(COD, XYZ, cause, xlim = c(-1.5,1.5)){
  Dxfc <- COD[[XYZ]]$Dxfc[,cause]
  Dxmc <- COD[[XYZ]]$Dxmc[,cause]
  
  Males   <- ThanoAgg(Dxmc, dxmc, FALSE, N = 20)
  Females <- ThanoAgg(Dxfc, dxfc, FALSE, N = 20)
  tics <- plotSetup(xlim)
  PyrLevels(Males, Females, revColors = FALSE, colRamp = BrewerRamp("BuGn"), scale = 100)
  PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5, scale = 100)
  invisible(tics)
}
plotDyx <- function(COD,cause = "External",countries,xlim=c(-2,2)){
  pp <- layout(matrix(c(1,2,3,4,5,6,7,7,7), 3, 3, byrow=FALSE), c(2.5,2.5,1), c(2.5,2.5,2.5,2.5,2.5,2.5,6), T)
#layout.show(pp)
  par(xaxs="i",yaxs="i",oma=c(0,1,0,0))
# left side
# top
  par(mai = c(.05+.2, .28, .25, .02))
  tics <- plotXYZcause2(COD, XYZ = countries[1], cause = cause,xlim=xlim)
  ylabels(tics, left = TRUE)
  text(0,-12,countries[1],xpd=TRUE)
#mtext(-2,115,"Years Lived",pos=4,xpd=TRUE)
#xlabels(tics,below=FALSE)
# middle
  par(mai = c(.15+.2, .28, .15, .02))
  tics <- plotXYZcause2(COD, XYZ = countries[2], cause = cause,xlim=xlim)
#ylabels(tics, left = TRUE)
  text(0,-12,countries[2],xpd=TRUE)
# bottom
  par(mai = c(.25+.2, .28, .05, .02))
  tics <- plotXYZcause2(COD, XYZ = countries[3], cause = cause,xlim=xlim)
  ylabels(tics, left = TRUE)
  xlabels(tics,below=TRUE)
  text(0,-12,countries[3],xpd=TRUE)
  
# right side
#top
  par(mai = c(.05+.2, .02, .25, .28))
  tics <- plotXYZcause2(COD, XYZ = countries[4], cause = cause,xlim=xlim)
#ylabels(tics, left = FALSE)
  xlabels(tics,below=FALSE)
  text(0,-12,countries[4],xpd=TRUE)
#middle
  par(mai = c(.15+.2, .02, .15, .28))
  tics <- plotXYZcause2(COD, XYZ = countries[5], cause = cause,xlim=xlim)
  ylabels(tics, left = FALSE)
  text(0,-12,countries[5],xpd=TRUE)
# bottom
  par(mai = c(.25+.2, .02, .05, .28))
  tics <- plotXYZcause2(COD, XYZ = countries[6], cause = cause,xlim=xlim)
#ylabels(tics, left = FALSE)
#xlabels(tics,below=TRUE)
  text(0,-12,countries[6],xpd=TRUE)
#sapply(countries[4:6], plotXYZcause1, COD=COD, cause = "External", xlim = c(-1.6,1.6))
  par(mai = c(0,0,0,0))
  plotSetup(xlim= c(0,1), ylim = c(0,100), bg = NA, grid = FALSE)
  N <- 5
  yat   <- seq(30,70, length = N + 1)
  rect(.2, yat[1:N], .4, yat[2:(N + 1)], 
    col = BrewerRamp("BuGn")(N), border = gray(.3), lwd = .5, xpd = TRUE)
  text(.4,yat,c(0,20,40,60,80,"100+"),pos=4)
  text(.35,75,"Years Lived",cex=1.2,xpd=TRUE)
  mtext(cause, side=3, line=2, cex=1.5, col="black", outer=FALSE, at = -2.5, padj=1) 
}
pdf(file.path(folder,paste0("Dyx",causes[1],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[1],countries,xlim=c(-3,3))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[2],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[2],countries,xlim=c(-5,5))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[3],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[3],countries,xlim=c(-2.2,2.2))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[4],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[4],countries,xlim=c(-7,7))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[5],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[5],countries,xlim=c(-2,2))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[6],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[6],countries,xlim=c(-5,5))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[7],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[7],countries,xlim=c(-8,8))
dev.off()
pdf(file.path(folder,paste0("Dyx",causes[8],".pdf")),height = 8,width = 6)
plotDyx(COD,cause=causes[8],countries,xlim=c(-4.5,4.5))
dev.off()


