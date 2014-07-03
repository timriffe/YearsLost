
Do.This <- FALSE

# this was my first hacky attempt. Things are cleaner now and in separate files...
if (Do.This){
library(DemogBerkeley)
Dx <- readHMD("/home/triffe/DATA/HMD/deaths/Deaths_1x1/USA.Deaths_1x1.txt")
Ex <- readHMD("/home/triffe/DATA/HMD/exposures/Exposures_1x1/USA.Exposures_1x1.txt")

Dxm <- Dx$Male[Dx$Year == 2010]
Exm <- Ex$Male[Ex$Year == 2010]
Dxf <- Dx$Female[Dx$Year == 2010]
Exf <- Ex$Female[Ex$Year == 2010]

Mxm <- Dxm / Exm
Mxf <- Dxf / Exf

lxm <- c(1, exp(-cumsum(Mxm)))
lxf <- c(1, exp(-cumsum(Mxf)))
plot(0:111,lx,type='l')

dxm <- -diff(lxm)
dxf <- -diff(lxf)

# OK, the years lost by deaths in a given age is a function of
# the distribution of remaining life in each age and the deaths
# in those ages.

library(devtools)
load_all("/home/triffe/git/Leaves/PlosOne/R/RiffeetalFunctions")

plot(rowSums(Thano(dxm,dxm)))
plot(rowSums(Thano(Dxm,dxm)))
plot(rowSums(Thano(dxf,dxf)))
plot(rowSums(Thano(Dxf,dxf)))
sum(Dxm) # males persons lost
sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) # total male person-years lost due to death
sum(Dxf) # females lost
sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) # total female person-years lost due to death

sum(rowSums(Thano(Dxm,dxm)) * .5:110.5) / sum(Dxm) # lose 16.3 PYL per male
sum(rowSums(Thano(Dxf,dxf)) * .5:110.5) / sum(Dxf)  # lose 13.7 PYL per female
# OK, but if each is repeated over x:x+y?

LDM <- outer(lxm,lxm,"/")
LDF <- outer(lxf,lxf,"/")
LDM[LDM > 1] <- NA
LDF[LDF > 1] <- NA

image(LDM)
image(LDF)
image(LDF / LDM)
#all(LDM == t(1/LDM), na.rm= TRUE)

# ages go down the matrix
head(LD)[,1:6]
plot(LDM[,1])
LDM[,1] == lxm
# so:
plot(rowSums(t(LDM)[2:112,2:112] * Dxm,na.rm=TRUE))
plot(rowSums(t(LDF)[2:112,2:112] * Dxf,na.rm=TRUE))


Males   <- t(LDM)[2:112,2:112] * Dxm
Females <- t(LDF)[2:112,2:112] * Dxf
library(Pyramid)
par(mfrow = c(1, 2))
MalesY   <- rowSums(Males, na.rm = TRUE)
FemalesY <- rowSums(Females, na.rm = TRUE)
Pyramid(MalesY,FemalesY, prop = FALSE, grid = FALSE, main = "Potental person-years of life lost due to death\nby age and sex in 2010")

MalesA   <- colSums(Males, na.rm = TRUE)
FemalesA <- colSums(Females, na.rm = TRUE)
Pyramid(MalesA,FemalesA, prop = FALSE, grid = FALSE, main = "Potental person-ages of life lost due to death\nby age and sex in 2010")

graphics.off()
plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)))
PyramidOutline(MalesY,FemalesY, col = "#00000050")
PyramidOutline(MalesA,FemalesA, col = "#FF000050")

# -------------------------------------------------
# now for heterogeneity 1:
aggN <- function(x,N){
    age <- 0:(length(x) - 1)
    tapply(x,age - age %% N, sum, na.rm = TRUE)
}

MalesYH   <- apply(Males, 1, aggN, N = 10)
FemalesYH <- apply(Females, 1, aggN, N = 10)

MalesAH   <- apply(Males, 2, aggN, N = 10)
FemalesAH <- apply(Females, 2, aggN, N = 10)

MalesYH   <- MalesYH[nrow(MalesYH):1,]
FemalesYH <- FemalesYH[nrow(MalesYH):1,]
MalesAH   <- MalesAH[nrow(MalesAH):1,]
FemalesAH <- FemalesAH[nrow(MalesAH):1,]




MalesYHC   <- t(apply(MalesYH,2,cumsum))
FemalesYHC <- t(apply(FemalesYH,2,cumsum))
MalesAHC   <- t(apply(MalesAH,2,cumsum))
FemalesAHC <- t(apply(FemalesAH,2,cumsum))

library(RColorBrewer)
display.brewer.all()
Cols1  <- colorRampPalette(brewer.pal(9,"BuGn"),space="Lab")(ncol(MalesYHC))
plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)),
        main = "Potental person-years of life lost due to death\nby age and sex in 2010", sub = 
                "Color bands indicate potential age attained (darker = younger, lighter = older)")
for (i in ncol(MalesYHC):1){
    PyramidOutline(MalesYHC[,i],FemalesYHC[,i], col = Cols1[i],border=NA)
}
text(0,-10,"Millions",xpd=TRUE)

plot(NULL, type = "n", xlim = c(-1.2e6,1.2e6),ylim=c(0,111), axes = FALSE, xlab = "",ylab = "",
        panel.first = list(rect(-1.2e6,0,1.2e6,111,col=gray(.95),border = NA),
                segments(seq(-1.2e6,1.2e6,by=.2e6),0,seq(-1.2e6,1.2e6,by=.2e6),111,col="white",lwd=.5,lty=1),
                segments(-1.2e6,seq(0,111,by=10),1.2e6,seq(0,110,by=10),111,col="white",lwd = .5,lty=1),
                text(seq(-1.2e6,1.2e6,by=.2e6),-5,zapsmall(abs(seq(-1.2,1.2,by=.2))),cex=1,xpd=TRUE),
                text(-1.2e6,seq(0,110,by=10),seq(0,110,by=10),pos=2,cex=1,xpd=TRUE)), main = 
                "Potental person-ages of life lost due to death\nby age (10-year color bands) and sex in 2010")
for (i in ncol(MalesYHC):1){
    PyramidOutline(MalesAHC[,i],FemalesAHC[,i], col = Cols1[i],border=NA)
}
text(0,-10,"Millions",xpd=TRUE)

args(Pyramid)
sum(rowSums(t(LD)[2:112,2:112] * Dx,na.rm=TRUE)) # closer

# COD:

COD <- read.csv("/home/triffe/Desktop/COD_5x1_adjusted_chapters.csv", stringsAsFactors = FALSE)
COD <- COD[COD$Year == 2010, ]
COD$Age <- as.integer(gsub("\\+","",unlist(lapply(strsplit(COD$Age,split = "-"),"[[",1))))

}

