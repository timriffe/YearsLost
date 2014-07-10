setwd("/home/triffe/git/YearsLost/YearsLost")
# get plotting functions
source("R/Functions.R")


library(DemogBerkeley)
Dx  <- readHMDweb("USA","Deaths_1x1",username = us, password = pw)
Ex  <- readHMDweb("USA","Exposures_1x1",username = us, password = pw)

Dxm <- Dx$Male[Dx$Year == 2010]
Exm <- Ex$Male[Ex$Year == 2010]

Mxm <- Dxm / Exm
lxm <- c(1, exp(-cumsum(Mxm)))
dxm <- lx2dx(lxm)

fya <- da2fya(dxm)

ages <- seq(0, 80, by = 10)
FYA <- fya[as.character(ages),]

colRamp <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"),space="Lab")

pdf("Figures/fya.pdf",width=5,height=4)
par(mai = c(.65,.5,.65,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
ylim <- c(0,max(pretty(FYA)))
plot(NULL, type = 'n', xlim = c(0,110), ylim = ylim, axes = FALSE, xlab = "", ylab = "",
        panel.first = list(
                rect(0,0,110,ylim[2],col = gray(.95), border = NA),
                segments(0,pretty(ylim,n=10),110,pretty(ylim,n=10),col="white",lwd=.5),
                segments(seq(0,100,by=10),0,seq(0,100,by=10),ylim[2],col="white",lwd=.5),
                text(0,pretty(ylim,n=10),pretty(ylim,n=10),pos=2,cex=.8),
                text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,cex=.8)))
matplot(t(FYA[nrow(FYA):1, ]), type = 'l', lty = 1, add = TRUE,
        col = colRamp(length(ages)+3)[-c(1:3)], 
        lwd = rev(seq(1,2,length=length(ages))))

Inds <- apply(FYA,1,which.max)
y <- diag(FYA[1:nrow(FYA),Inds])
text(Inds+1,y,ages,pos=3)
text(-5,ylim[2]*1.15,"f(y|a)")
text(50,ylim[2]*-.15,"y (years left)")
text(60,ylim[2] * .8, "Given survival to age")
segments(78, 0.053, 85.67, 0.041)
segments(37, 0.056, 10, 0.07)
dev.off()








