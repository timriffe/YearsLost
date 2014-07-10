setwd("/home/triffe/git/YearsLost/YearsLost")
# get plotting functions
source("R/Functions.R")

a <- 5

graphics.off()
#dev.new(height = 1, width = 5)
pdf("Figures/LifeLine.pdf", height = 1, width = 5)
par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot(NULL, 
        type = 'n', 
        xlim = c(0, 10), 
        ylim = c(0, 2), 
        axes = FALSE, 
        xlab = "", 
        ylab = "",
        asp = 1)
curlyBrace1(1, 1.5, length = a, radius1 = .25, radius2 = .15, col = gray(.5) )
curlyBrace1(a + 1, 1.5, length = 8 - a, radius1 = .25, radius2 = .15, col = gray(.5))
curlyBrace1(1, .5, length = 8, radius1 = .25, radius2 = .15, top = FALSE, col = gray(.5))

segments(1, 1, a + 1, 1, lwd = 2, col = "forestgreen")
segments(a + 1, 1, 9, 1, lwd = 2, col = "blue")
points(1, 1, pch = 1, cex = 1.5)
points(9, 1, pch = 19, cex = 1.5)

segments(a + 1, .9, a + 1, 1.1, lwd = 1)
text(c(1+a/2,1+a + (8-a)/2),1.8,c("a","y"),col = c("forestgreen","blue"))
text(5,.2,"X")
dev.off()



