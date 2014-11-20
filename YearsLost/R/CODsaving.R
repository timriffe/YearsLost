
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

# load data from /Data/ as produced by R/DataPrep.R
HMD <- local(get(load("Data/HMD.Rdata")))
COD <- local(get(load("Data/COD.Rdata")))



MalesList <- lapply(names(LDm_list), function(CD, LDm_list, Dxmc){
            t(LDm_list[[CD]])[2:112, 2:112] * Dxmc[,CD]
        }, LDm_list = LDm_list, Dxmc = Dxmc)
FemalesList <- lapply(names(LDf_list), function(CD, LDf_list, Dxfc){
            t(LDF_list[[CD]])[2:112, 2:112] * Dxfc[,CD]
        }, LDf_list = LDf_list, Dxfc = Dxfc)
# aggregate into 10-year groups for both margins, both sexes
MalesYL   <- lapply(MalesList,      AggM, Pyr = TRUE)
FemalesYL <- lapply(FemalesList,    AggM, Pyr = TRUE)
MalesAL   <- lapply(MalesList,      AggM, Pyr = FALSE)
FemalesAL <- lapply(FemalesList,    AggM, Pyr = FALSE)
names(FemalesYL) <- names(MalesYL) <- names(MalesAL) <- names(FemalesAL) <- names(LDM_list)
# make multipage pdf to examine results?
#xlim       <- max(pretty(c(colSums(MalesAL), colSums(FemalesA)))) * c(-1, 1)
#xlim       <-max(pretty(c(unlist(lapply(MalesAL, function(X){
#            colSums(X)
#        })),unlist(lapply(FemalesAL, function(X){
#            colSums(X)
#        }))))) * c(-1, 1)

#xlim     <- max(pretty(c(colSums(MalesY), colSums(FemalesY)))) * c(-1, 1)

# all in one figure? grid 5x4?

makePanel <- function(xy,Males,Females,colRamp = BrewerRamp("Blues")){
    X    <- Males[[CD]]
    Y    <- Females[[CD]]
    Tot  <- sum(X) + sum(Y)
    xlim <- max(pretty(c(colSums(X)/Tot,colSums(Y)/Tot))) * c(-1,1) * 100
    tics <- plotSetup(xlim)
    PyrLevels(X, 
            Y, 
            revColors = FALSE, 
            colRamp = colRamp, 
            scale = 100)
    PyramidOutline(
            colSums(X), 
            colSums(Y), 
            col = NA, 
            border = gray(.2), 
            lwd = .5, 
            scale = 100)
}












# redo this to work with proportions, and include totals, along with COD in titles
pdf("Figures/YearsLostLivedyx10COD.pdf",height = 4.5,width = 5)
#dev.new(height = 4.5,width = 4.5) # CD <- "03"
par(xaxs = "i",yaxs = "i", mai = c(.5, .75, 1, 1.25))
lapply(names(MalesAL), function(CD, MalesAL, FemalesAL, DFxc, DMxc){
            X    <- MalesAL[[CD]]
            Y    <- FemalesAL[[CD]]
            Tot  <- sum(X) + sum(Y)
            xlim <- max(pretty(c(colSums(X)/Tot,colSums(Y)/Tot))) * c(-1,1) * 100
            tics <- plotSetup(xlim)
            PyrLevels(X, 
                    Y, 
                    revColors = FALSE, 
                    colRamp = BrewerRamp("Blues"), 
                    scale = 100)
            PyramidOutline(
                    colSums(X), 
                    colSums(Y), 
                    col = NA, 
                    border = gray(.2), 
                    lwd = .5, 
                    scale = 100)
            DrawLabels(tics, 
                    ylab = "Age Saved at", 
                    xlab = "percent", 
                    xlabs = abs(tics$x))
            Drawlegend(tics, 
                    N = min(dim(X)), 
                    colRamp = BrewerRamp("Blues"), 
                    revColors = TRUE, 
                    label = "Ages gained")
            text(min(tics$x) - 2 * diff(tics$x[1:2]), 130, 
                    paste("COD:", CD, 
                            "| Deaths:",round(sum(DFxc[,CD] + DMxc[,CD])/1000)*1000,
                            "| PYL:",round(sum(X + Y)/1000)*1000,
                            "| mean RYL:", round(sum(X + Y) / sum(DFxc[,CD] + DMxc[,CD]),2)
            ),
                    xpd = TRUE, cex = .8, pos = 4)
        }, MalesAL = MalesAL, FemalesAL = FemalesAL, DFxc = DFxc, DMxc = DMxc)
dev.off()




pdf("Figures/YearsSavedGainedxx10COD.pdf",height = 4,width = 5)
#dev.new(height = 4,width = 5) #CD <- '01'
par(xaxs = "i",yaxs = "i", mai = c(.5, .75, 1, 1.25))
lapply(names(MalesYL), function(CD, MalesYL, FemalesYL, DFxc, DMxc){
            X    <- MalesYL[[CD]]
            Y    <- FemalesYL[[CD]]
            Tot  <- sum(X) + sum(Y)
            xlim <- max(pretty(c(colSums(X)/Tot,colSums(Y)/Tot))) * c(-1,1) * 100
           
            tics <- plotSetup(xlim)
            PyrLevels(X, Y, revColors = FALSE, colRamp = BrewerRamp("Blues"), 
                    scale = 100)
            PyramidOutline(colSums(X), colSums(Y), col = NA, border = gray(.2), lwd = .5, 
                    scale = 100)
            DrawLabels(tics, ylab = "Ages gained", xlab = "percent", xlabs =  abs(tics$x))
            Drawlegend(tics, N = min(dim(X)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Age Saved at")
            text(min(tics$x) - 2 * diff(tics$x[1:2]), 130, 
                    paste("COD:", CD, 
                            "| Deaths:",round(sum(DFxc[,CD] + DMxc[,CD])/1000)*1000,
                            "| PYL:",round(sum(X + Y)/1000)*1000,
                            "| mean RYL:", round(sum(X + Y) / sum(DFxc[,CD] + DMxc[,CD]),2)
                    ),
                    xpd = TRUE, cex = .8, pos = 4)
        }, MalesYL = MalesYL, FemalesYL = FemalesYL, DFxc = DFxc, DMxc = DMxc)
dev.off()

# this is experimentation to get standard right-side legend dimensions. continue on Monday
XW <- diff(par("usr")[1:2]) # xunits wide plot region
PW <- round(sum(par("mai")[c(2,4)])/(1-diff(par("plt")[1:2])) * diff(par("plt")[1:2])) # inches wide in plot region
XR <- XW / PW * par("mai")[4] # X width of right margin


#
#
#pdf("Figures/YearsSavedGainedxx10.pdf",height = 4,width = 4.5)
##dev.new(height = 4,width = 4.5)
#par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
#tics <- plotSetup(xlim)
#PyrLevels(MalesA, FemalesA, revColors = FALSE, colRamp = BrewerRamp("Blues"))
#PyramidOutline(colSums(MalesA), colSums(FemalesA), col = NA, border = gray(.2), lwd = .5)
#DrawLabels(tics, ylab = "Age Saved",xlab = "Years gained (Millions)", xlabs = zapsmall(abs(tics$x / 1e6)))
#Drawlegend(tics, N = min(dim(MalesA)), colRamp = BrewerRamp("Blues"), revColors = TRUE, label = "Ages gained")
#dev.off()


