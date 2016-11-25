# TR: Based on script by MG
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/YearsLost/YearsLost")
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//YearsLost//YearsLost")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
	}
}
###############################################################################
library(reshape2)

E 		       <- local(get(load("Data/PCmaterials/E.Rdata")))
Saez           <- read.table("Data/SeazFx.csv", header = TRUE , sep = ",")

# --------------------
colnames(Saez) <- gsub("X","",colnames(Saez))
rownames(Saez) <- Saez[, 1]
Saez           <- Saez[, -1]

# looking at table 1 of Saez makes it clear that the years are 1 left
colnames(Saez) <- as.integer(colnames(Saez))
yrs            <- as.integer(colnames(Saez))
yrs1           <- yrs[1]:yrs[length(yrs)]

Saez1          <- t(apply(Saez,1,function(y, yrs, yrs1){
			           approx(x = yrs, y = y, xout = yrs1)$y
		              }, 
				      yrs = yrs, 
				      yrs1 = yrs1)
                    )

colnames(Saez1) <- yrs1
#because these numbers are births per 1000
# -> divide them by 1000 to get the rates
#necessary because we need the birthcounts for Rizzi model
Saez1 <- Saez1 / 1000
#delete first two years because exposures start at 1908
#-----------
Exp   <- acast(E,Age~Year, value.var = "Female")
x     <- Exp[,1]

Exp5  <- apply(Exp,2,function(x, Age = 1:length(x) - 1, N = 5){
			an <- Age - Age %% N
			tapply(x, an, sum)
		})

Cols  <- intersect(colnames(Exp5),colnames(Saez1))
Exp5  <- Exp5[rownames(Saez1),Cols]
Saez1 <- Saez1[, Cols]
Bx5   <- Saez1 * Exp5
Bx5   <- rbind(0, Bx5, 0)

#graduate the data
source("R/pclm.R")
source("R/graduate_births.R")

#just remember... omega is set to a value of 50
#lambda is still not sure...
#it's necessary to extract the births and the exposures from the data.frame
alpha <- 10
beta <- 50
a <- alpha:(beta - 1)
Fx1Saez <- matrix(0, nrow = length(a), ncol = ncol(Bx5), dimnames = list(a, colnames(Bx5)))
for (i in 1:ncol(Bx5)){
	Fx1Saez[,i] <- graduateBirths(
			          BxN = Bx5[,i], 
					  Ex = Exp[,i],
					  lambda = 100, 
					  alpha = alpha, 
					  beta = beta, 
					  width = 5)$Fx
}

save(Fx1Saez, file = "Data/FxSaez_graduated.Rdata")
