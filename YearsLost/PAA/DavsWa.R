# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/YearsLost/YearsLost")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
}

# read in HMD USA 2010:

HMD <- local(get(load("/home/tim/git/YearsLost/YearsLost/Data/HMD.Rdata")))
HMD$USA

##########################################
# Make figure that jumps from Dx to Wx....

# 2) then a diagram showing a lifeline that starts at x and increases to x+y...

# 3) then the W figure where ages are appropriately shifted up: the cumulative years of life foregone.
#










