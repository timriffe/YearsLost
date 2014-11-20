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

dxm <- -diff(lxm)
dxf <- -diff(lxf)
# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.




graphics.off()

# does thanatological decomposition, groups 10-year thanatological age classes within chronological age
Males   <- ThanoAgg(Pxm, dxm, TRUE)
Females <- ThanoAgg(Pxf, dxf, TRUE)

# xlim is shared
xlim     <- max(pretty(c(colSums(Males), colSums(Females)))) * c(-1, 1)

pdf("Figures/Pyrxy10.pdf",height = 4,width = 4.5)
#ev.new(height = 4,width = 4.5)
par(xaxs = "i",yaxs = "i", mai = c(.5, .5, .5, 1))
tics <- plotSetup(xlim)
PyrLevels(Males, Females, revColors = TRUE, colRamp = BrewerRamp("Blues"))
PyramidOutline(colSums(Males), colSums(Females), col = NA, border = gray(.2), lwd = .5)
DrawLabels(tics, ylab = "Years Lived")
Drawlegend(tics, N = min(dim(Males)), colRamp = BrewerRamp("Blues"))
dev.off()

