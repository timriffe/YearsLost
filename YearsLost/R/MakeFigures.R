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


# life line Figure (1)
source("R/LifeLine.R")
source("R/fya.R")
source("R/AllCauseFigures.R")







