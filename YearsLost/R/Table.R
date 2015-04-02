

# the little script just produces a table of the main causes of death (count and prop)
# for the US.

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

A <- cbind(colSums(COD$USA$Dxfc),
colSums(COD$USA$Dxmc))
ID<- A["Ill defined", ]
A <- A[rownames(A) != "Ill defined", ]
A["Other", ] <- A["Other", ] + ID
A <- cbind(A,rowSums(A))
colnames(A) <- c("Female","Male","Total")
rownames(A)[rownames(A) == "Inf_Cong"] <- "Infant"

P <- 100 * prop.table(A,2)

library(xtable)
B <- cbind(A[,1],P[,1],A[,2],P[,2],A[,3],P[,3])
B <- rbind(B,Total=colSums(B))
print(xtable(B,digits=c(0,0,1,0,1,0,1)),floating=FALSE)

# column headers were added by hand after, and rows were rearranged...


# this is a rather ambitious distraction: produce stats for each cause

#now let's get another table:
names(COD[["USA"]])
getMeans <- function(XYZ = "USA",cause = "External"){
 
  # all-cause elements
  Mxm  <- COD[[XYZ]]$Mxm
  Mxf  <- COD[[XYZ]]$Mxf
  lxm  <- COD[[XYZ]]$lxm
  lxf  <- COD[[XYZ]]$lxf
  Lxm  <- (lxm[1:111] + lxm[2:112]) / 2
  Lxf  <- (lxf[1:111] + lxf[2:112]) / 2

  
  # single cause elements (or cause deleted, as the case may be)
  Mxfc <- COD[[XYZ]]$Mxfc[,cause]
  Mxmc <- COD[[XYZ]]$Mxmc[,cause]
  dxfc <- COD[[XYZ]]$Kdxf[,cause]
  dxmc <- COD[[XYZ]]$Kdxm[,cause]
  lxmc <- COD[[XYZ]]$Klxm[,cause]
  lxfc <- COD[[XYZ]]$Klxf[,cause]
# Deaths from cause:
  Dxfc <- COD[[XYZ]]$Dxfc[,cause]
  Dxmc <- COD[[XYZ]]$Dxmc[,cause]
# make useful LD object
  LDM  <- makeLD(lxmc)
  LDF  <- makeLD(lxfc)
  
  # deaths by years lived and left (were they saved)
  # note the dx vectors are cause-deleted, per our assumptions,
  # though this ought not make a huge difference. 
  Males   <- Thano(Dxmc, dxmc)
  Females <- Thano(Dxfc, dxfc)
  y       <- .5:110.5

  # obs pop
  xm <- sum(Dxmc*y)/sum(Dxmc)
  xf <- sum(Dxfc*y)/sum(Dxfc) 
  ym <- sum(rowSums(Males)*y)/sum(Dxmc)
  yf <- sum(rowSums(Females)*y)/sum(Dxfc)
  
  Males2   <- t(LDM)[2:112,2:112] * Dxmc
  Females2 <- t(LDF)[2:112,2:112] * Dxfc
  
  # rowSums = Da*ea, colsums = years lived through
  #plot(colSums(Males2))
  #plot(rowSums(Males2))
  #
  Wm <- sum(rowSums(Males2)*y)/sum(Males2)
  Wf <- sum(rowSums(Females2)*y)/sum(Females2)
  #
  Am <- sum(colSums(Males2)*y)/sum(Males2)
  Af <- sum(colSums(Females2)*y)/sum(Females2)

  Obs <- cbind(xm,xf,ym,yf,Wm,Wf,Am,Af)

# stationary pop (of same size?)- not key, the pop is just weights.
 PM        <- sum(rowSums(COD[[XYZ]]$Dxmc) / Mxm)
 PF        <- sum(rowSums(COD[[XYZ]]$Dxfc) / Mxf)
 DxmcS     <- Mxmc * (Lxm/sum(Lxm)) * PM
 DxfcS     <- Mxfc * (Lxf/sum(Lxf)) * PF
 
 MalesS    <- Thano(DxmcS, dxmc)
 FemalesS  <- Thano(DxfcS, dxfc)
 
 xmS       <- sum(DxmcS*y)/sum(DxmcS)
 xfS       <- sum(DxfcS*y)/sum(DxfcS)
 ymS       <- sum(rowSums(MalesS)*y)/sum(DxmcS)
 yfS       <- sum(rowSums(FemalesS)*y)/sum(DxfcS)
 
 Males2S   <- t(LDM)[2:112,2:112] * DxmcS
 Females2S <- t(LDF)[2:112,2:112] * DxfcS
 
 WmS       <- sum(rowSums(Males2S)*y)/sum(Males2S)
 WfS       <- sum(rowSums(Females2S)*y)/sum(Females2S)
#
 AmS       <- sum(colSums(Males2S)*y)/sum(Males2S)
 AfS       <- sum(colSums(Females2S)*y)/sum(Females2S)
 
 Stat <- cbind(xmS,xfS,ymS,yfS,WmS,WfS,AmS,AfS)
 list(Obs=Obs,Stat=Stat)
}

# now get the same stuff for all-cause for the last row of the table..
Dxm     <- rowSums(COD[[XYZ]]$Dxmc)
Dxf     <- rowSums(COD[[XYZ]]$Dxfc)
Mxm     <- COD[[XYZ]]$Mxm
Mxf     <- COD[[XYZ]]$Mxf
lxm     <- COD[[XYZ]]$lxm
lxf     <- COD[[XYZ]]$lxf
Lxm     <- (lxm[1:111] + lxm[2:112]) / 2
Lxf     <- (lxf[1:111] + lxf[2:112]) / 2
dxm     <- COD[[XYZ]]$dxm
dxf     <- COD[[XYZ]]$dxf
xm      <- sum(Dxm*y)/sum(Dxm)
xf      <- sum(Dxf*y)/sum(Dxf) 
Males   <- Thano(Dxm, dxm)
Females <- Thano(Dxf, dxf)
ym      <- sum(rowSums(Males)*y)/sum(Dxm)
yf      <- sum(rowSums(Females)*y)/sum(Dxf)
LDM     <- makeLD(lxm)
LDF     <- makeLD(lxf)
Males2   <- t(LDM)[2:112,2:112] * Dxm
Females2 <- t(LDF)[2:112,2:112] * Dxf
Wm      <- sum(rowSums(Males2)*y)/sum(Males2)
Wf      <- sum(rowSums(Females2)*y)/sum(Females2)
Am      <- sum(colSums(Males2)*y)/sum(Males2)
Af      <- sum(colSums(Females2)*y)/sum(Females2)
ObsT    <- cbind(xm,xf,ym,yf,Wm,Wf,Am,Af)

# and the stationary equivalents...
MalesS    <- Thano(dxm, dxm)
FemalesS  <- Thano(dxf, dxf)

xmS       <- sum(dxm*y)
xfS       <- sum(dxf*y)
ymS       <- sum(rowSums(MalesS)*y)
yfS       <- sum(rowSums(FemalesS)*y)

Males2S   <- t(LDM)[2:112,2:112] * dxm
Females2S <- t(LDF)[2:112,2:112] * dxf

sum(rev(cumsum(rev(Lxm)))*dxm*y)/sum(rev(cumsum(rev(Lxm)))*dxm)

sum(rev(cumsum(rev(Lxm))) * dxm)

WmS       <- sum(rowSums(Males2S)*y)/sum(Males2S)
WfS       <- sum(rowSums(Females2S)*y)/sum(Females2S)
#
AmS       <- sum(colSums(Males2S)*y)/sum(Males2S)
AfS       <- sum(colSums(Females2S)*y)/sum(Females2S)

Stat <- cbind(xmS,xfS,ymS,yfS,WmS,WfS,AmS,AfS)

Causes <- c("Infectious","Cancer","Other","Cardio","Inf_Cong","External","Mental")
getMeans("USA","Cardio")

All <- lapply(Causes, getMeans, XYZ = "USA")
names(All) <- Causes

# this is too complex to include in a single table.
# do M/F observed in one table, and stationary in a separate (appendix) table

Obs <- do.call(rbind, lapply(All,"[[",1))
rownames(Obs) <- Causes
Obs <- rbind(Obs, Total = ObsT)

Stat <- do.call(rbind, lapply(All,"[[",2))
rownames(Stat) <- Causes

print(xtable(Obs,digits=1))

