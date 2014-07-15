
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

# cheap lx
lxm <- c(1, exp(-cumsum(Mxm)))
lxf <- c(1, exp(-cumsum(Mxf)))

dxm <- lx2dx(lxm)
dxf <- lx2dx(lxf)

# COD:

COD         <- read.csv("Data/COD_5x1_adjusted_chapters.csv", stringsAsFactors = FALSE)
COD         <- COD[COD$Year == 2010, ]
COD$Age     <- as.integer(gsub("\\+","",unlist(lapply(strsplit(COD$Age,split = "-"),"[[",1))))

expandAbdridged <- function(M){
    do.call(rbind,list(M[1,],
            matrix(M[2,],nrow = 4, ncol = ncol(M), byrow = TRUE),
            do.call(rbind,lapply(
                            apply(
                                    M[3:nrow(M),],1,function(x){
                                        list(matrix(x,nrow = 5, ncol = length(x), byrow = TRUE))
                                    }),"[[",1)
    )))
}

library(reshape2)
MF <- acast(COD, Age ~ COD.chap, value.var = "Rates.F")
MM <- acast(COD, Age ~ COD.chap, value.var = "Rates.M")
MF <- MF[, -ncol(MF)]
MM <- MM[, -ncol(MM)]
# These are rates per 100000. Do all calcs then divide out back to pure rates.
# tried to smooth pattern, not worth it
ind0M     <- MM == 0
ind0F     <- MF == 0
MM[ind0M] <- 1e-8
MF[ind0F] <- 1e-8

# for imputation of 0s later
ind0M1                     <- expandAbdridged(ind0M)[1:101, ]
ind0F1                     <- expandAbdridged(ind0F)[1:101, ]

# ages for fitting
ages                       <- sort(unique(COD$Age))
ages[1:(length(ages) - 1)] <- ages[1:(length(ages) - 1)] + diff(ages) / 2
ages[1]                    <- 0
MF1 <- apply(log(MF),2,function(y){
           exp(splinefun(y~ages)(0:100))      
        })
MM1 <- apply(log(MM),2,function(y){
            exp(splinefun(y~ages)(0:100))
        })
MF1[ind0F1] <- 0
MM1[ind0M1] <- 0

# get Fracs (no need to get rid of 1e5)
MFe <- MF1 / rowSums(MF1)
MMe <- MM1 / rowSums(MM1)

# add on ages 101-110
MFe <- rbind(MFe, t(replicate(10, MFe[nrow(MFe), ])))
MMe <- rbind(MMe, t(replicate(10, MMe[nrow(MMe), ])))

#####################################################################
# deleted frac:
# K = Keep
KF <- 1 - MFe
KM <- 1 - MMe

# deaths from each cause
DFxc <- Dxf * MFe
DMxc <- Dxm * MMe

# Mx of each cause:
Mxmc <- Mxm * MMe
Mxfc <- Mxf * MFe
#rowSums(MMe)
#plot(c(1,exp(-cumsum(rowSums(Mxmc)))), type = 'l', col = "blue", lwd = 2)

# cause-deleted lx (simplified)
Klxm <- apply(Mxm * KM ,2,function(x){
            c(1,exp(-cumsum(x)))
        })
Klxf <- apply(Mxf * KF ,2,function(x){
            c(1,exp(-cumsum(x)))
        })
# cause-deleted dx
Kdxm <- apply(Klxm, 2, lx2dx)
Kdxf <- apply(Klxf, 2, lx2dx)

# sanity-check (16 is pregnancy-related)
# all(Kdxm[,"16"] == dxm) 
# [1] TRUE

# stupid list trick. could be done another way I'm sure
LDM_list <- lapply(apply(Klxm,2,function(x){
                    list(makeLD(x))
                }
             ),"[[",1)
LDF_list <- lapply(apply(Klxf,2,function(x){
                         list(makeLD(x))
                     }
             ),"[[",1)
#LDM <- makeLD(lxm)
#LDF <- makeLD(lxf)

## spread out over years lived further, etc
#MalesList <- lapply(LDM_list, function(LD,Dxm){
#            t(LD)[2:112, 2:112] * Dxm
#        }, Dxm = Dxm)
#FemalesList <- lapply(LDF_list, function(LD,Dxf){
#            t(LD)[2:112, 2:112] * Dxf
#        }, Dxf = Dxf)
MalesList <- lapply(names(LDM_list), function(CD, LDM_list, DMxc){
            t(LDM_list[[CD]])[2:112, 2:112] * DMxc[,CD]
        }, LDM_list = LDM_list, DMxc = DMxc)
FemalesList <- lapply(names(LDF_list), function(CD, LDF_list, DFxc){
            t(LDF_list[[CD]])[2:112, 2:112] * DFxc[,CD]
        }, LDF_list = LDF_list, DFxc = DFxc)
# aggregate into 10-year groups for both margins, both sexes
MalesYL   <- lapply(MalesList, AggM, Pyr = TRUE)
FemalesYL <- lapply(FemalesList, AggM, Pyr = TRUE)
MalesAL   <- lapply(MalesList, AggM, Pyr = FALSE)
FemalesAL <- lapply(FemalesList, AggM, Pyr = FALSE)
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


