

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



