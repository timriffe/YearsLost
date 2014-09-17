# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/YearsLost/YearsLost")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/hdir/0/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
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
xlim     <- max(pretty(c(Dxm, Dxf))) * c(-1, 1)

# Leaf
Males   <- ThanoAgg(Dxm, dxm, FALSE, N = 10)
Females <- ThanoAgg(Dxf, dxf, FALSE, N = 10)

# xlim is shared
xlim     <- max(pretty(c(rowSums(Thano(Dxm, dxm)), rowSums(Thano(Dxf, dxf))))) * c(-1, 1)

pdf("Figures/Deathsyx10.pdf",height = 4,width = 4.5)
#dev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = FALSE, colRamp = BrewerRamp("BuGn"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Left",xlab = "(1000s)", xlabs = zapsmall(abs(tics$x / 1e3)))
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("BuGn"), label = "Years Lived",revColors = TRUE)
dev.off()



