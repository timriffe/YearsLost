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

lxm <- c(1, exp(-cumsum(Mxm)))
lxf <- c(1, exp(-cumsum(Mxf)))

dxm <- lx2dx(lxm)
dxf <- lx2dx(lxf)
# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.

# xlim is shared
xlim     <- max(pretty(c(Pxm, Pxf))) * c(-1, 1)

# Leaf
Males   <- ThanoAgg(Pxm, dxm, FALSE, N = 10)
Females <- ThanoAgg(Pxf, dxf, FALSE, N = 10)

pdf("Figures/Leafyx10.pdf",height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = FALSE, colRamp = BrewerRamp("BuGn"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Left")
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("BuGn"), label = "Years Lived",revColors = TRUE)
dev.off()




dx2ex <- function(dx){
	a <- 1:length(dx)-.5
	fya <- da2fya(dx)
	colSums(t(fya)*a)
}
# PARYL
sum((Pxm * dx2ex(dxm))) / sum(Pxm)

Pym <- rowSums(Thano(Pxm, dxm,stagger=FALSE))
sum(Pym * .5:110.5) / sum (Pym)

# Equal

years <- unique(Dx$Year)
year <- 2010
Pex15 <- function(year, Dx,Ex,Px){
	Dxm <- Dx$Male[Dx$Year == year]
	Exm <- Ex$Male[Ex$Year == year]
	Dxf <- Dx$Female[Dx$Year == year]
	Exf <- Ex$Female[Ex$Year == year]
	Pxm <- Px$Male1[Px$Year == year]
	Pxf <- Px$Female1[Px$Year == year]
	
	Mxm <- Dxm / Exm
	Mxf <- Dxf / Exf
	
	lxm <- c(1, exp(-cumsum(Mxm)))
	lxf <- c(1, exp(-cumsum(Mxf)))
	
	dxm <- lx2dx(lxm)
	dxf <- lx2dx(lxf)
	
	Pym <- rowSums(Thano(Pxm, dxm))
	Pyf <- rowSums(Thano(Pxf, dxf))
	
	Py15 <- sum(c(Pym[1:15],Pyf[1:15])) / sum(Pym+Pyf)
	
	exm <- dx2ex(dxm)
	exf <- dx2ex(dxf)
	
	threshm <- approx(exm,0:110,xout=15)$y
	threshf <- approx(exf, 0:110, xout=15)$y
	intm <- floor(threshm)
	intf <- floor(threshf)
	
	diffm <- threshm - intm
	difff <- threshf - intf
	
	numm <- sum(Pxm[1:(intm+1)]) + diffm * Pxm[intm+2]
	numf <- sum(Pxf[1:(intf+1)]) + difff * Pxf[intf+2]
	
	PLE15 <- 1 - (numm + numf) / sum(Pxm+Pxf)
	c(Py15=Py15,PLE15=PLE15)
}

Out <- sapply(years, Pex15, Dx=Dx, Ex=Ex, Px=Px)

plot(years, Out[1,], type='l',ylim=c(0,.3))
lines(years, Out[2,],col="red")

plot(Out[1,],Out[2,],asp=1)