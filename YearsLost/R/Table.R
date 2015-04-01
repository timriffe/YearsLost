

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
 
  Mxm  <- COD[[XYZ]]$Mxm
  Mxf  <- COD[[XYZ]]$Mxf
  lxm  <- COD[[XYZ]]$lxm
  lxf  <- COD[[XYZ]]$lxf
  Lxm <- (lxm[1:111] +  lxm[2:112]) / 2
  Lxf <- (lxf[1:111] +  lxf[2:112]) / 2
  dxm  <- COD[[XYZ]]$dxm
  dxm  <- COD[[XYZ]]$dxm
  Mxfc <- COD[[XYZ]]$Mxfc[,cause]
  Mxmc <- COD[[XYZ]]$Mxmc[,cause]
  dxfc <- COD[[XYZ]]$Kdxf[,cause]
  dxmc <- COD[[XYZ]]$Kdxm[,cause]
  lxmc <- COD[[XYZ]]$Klxm[,cause]
  lxfc <- COD[[XYZ]]$Klxf[,cause]
# Deaths from cause:
  Dxfc <- COD[[XYZ]]$Dxfc[,cause]
  Dxmc <- COD[[XYZ]]$Dxmc[,cause]
  
  Males   <- Thano(Dxmc, dxmc)
  Females <- Thano(Dxfc, dxfc)
  y <- .5:110.5

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

cbind(xm,xf,ym,yf,Wm,Wf,Am,Af)


# stationary pop (of same size?)
 PM <- sum(rowSums(COD[[XYZ]]$Dxmc) / Mxm)
 PF <- sum(rowSums(COD[[XYZ]]$Dxfc) / Mxf)
 DxmcS <- Mxmc * Lxm
 DxfcS <- Mxfc * Lxf
 
 
 
 Lxmc <- (lxmc[1:111]+lxmc[2:112])/2
 Lxfc <- (lxfc[1:111]+lxfc[2:112])/2
 yms <- sum(dxmc * rev(cumsum(rev(Lxmc)))/lxmc[1:111])
 yfs <- sum(dxfc * rev(cumsum(rev(Lxfc)))/lxfc[1:111])
 #sum(Dxmc);sum(Dxfc);
 Dcym <- colSums(Males)
 Dcyf <- colSums(Females)
}



