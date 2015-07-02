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
getwd()
# get plotting functions
source("R/Functions.R")
library(DemogBerkeley)

B   <- readHFDweb("USA", "asfrRR", password = pw, username = us)
B   <- B[B$Year == 2010, ]

Dx  <- readHMDweb("USA", "Deaths_1x1", username = us, password = pw)
Ex  <- readHMDweb("USA", "Exposures_1x1", username = us, password = pw)
Px  <- readHMDweb("USA", "Population", username = us, password = pw)


Dxf <- Dx$Female[Dx$Year == 2010]
Exf <- Ex$Female[Ex$Year == 2010]
Pxf <- Px$Female1[Px$Year == 2010]

Mxf <- Dxf / Exf

lxf <- c(1, exp(-cumsum(Mxf)))

# xlim is shared

LDF <- makeLD(lxf)
#(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))


Females <- t(LDF)[2:112,2:112] * Dxf

CumExpos <- colSums(FemalesY)
names(CumExpos) <- 0:110
sum(CumExpos[as.character(B$Age)] * B$ASFR) # 54122 hypothetical births foregone
