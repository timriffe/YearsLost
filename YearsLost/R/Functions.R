library(devtools)
if (! "RiffeetalFunctions" %in% installed.packages()[,"Package"]){
    cat("\nInstalling custom package from github
This may fail on Mac or Windows if some preliminary configuring isn't done
See page: https://github.com/hadley/devtools
if you're having problems loading/installing from github\n")
    install_github(repo = "Leaves", subdir = "PlosOne/R/RiffeetalFunctions", username = "timriffe")
}
library(RiffeetalFunctions)


# custom functions used in figure production. Not annotated. All experimental.
lx2dx <- function(lx){
    dx <- -diff(c(lx,0))
    dx[(length(dx) - 1)] <-  sum(dx[(length(dx) - 1):length(dx)])
    dx[-length(dx)]
}
makeLD <- function(lx){
    LD <- outer(lx,lx,"/")
    LD[LD > 1] <- 0
    LD
}

AggM <- function(M, Pyr = TRUE, N = 10){
    apply(M, ifelse(Pyr, 2, 1), aggN, N = N)
}
ThanoAgg <- function(Px, dx, Pyr = TRUE, N = 10){
    Pxy  <- Thano(Px, dx)
    Gxy  <- AggM(Pxy, Pyr = Pyr, N = N)
#    if (Cumsum){
#        Gxy <- apply(Gxy, which.min(dim(Gxy)), cumsum)
#    }
    Gxy
}

PyrLevels <- function(
        Males, 
        Females, 
        colRamp = BrewerRamp("Blues"), 
        revColors = FALSE, 
        revCategories = FALSE,
        scale = sum(c(Males, Females)),
        ...){
    
    LevelMargin <- which.min(dim(Males))
    # make sure oriented in a regular way.
    if (LevelMargin == 1){
        Males   <- t(Males)
        Females <- t(Females)
    }
    
    # rescale
    Tot     <- sum(Males) + sum(Females)
    Males   <- scale * Males / Tot
    Females <- scale * Females / Tot
    
    # for in to out or from out to in?
    N           <- ncol(Males)
    if (revCategories){
        Males   <- Males[, N:1]
        Females <- Females[, N:1]
    }
    
    # Cumusum for plotting only
    Males   <- t(apply(Males, 1, cumsum))
    Females <- t(apply(Females, 1, cumsum))
    
    # determine colors and ordering
    Colors      <- colRamp(N)
    if(revColors){
        Colors <- rev(Colors)
    }
    
    # plot from big to small
    Males   <- Males[, N:1]
    Females <- Females[, N:1]
    
    # plot iterating over additive categories
 
    for (i in 1:N){
        PyramidOutline(Males[, i], Females[, i], col = Colors[i], border = FALSE, ...)
    }
}

plotSetup <- function(
        xlim, 
        ylim = c(0,111), 
        bg = gray(.95),
        grid = TRUE, 
        lwd = .5,
        ...){
    xpretty <- pretty(xlim, n = 10)
    if ((abs(min(xpretty) -  min(xlim)) > 1e-6)){
        xpretty <- xpretty[-1]
    }
    if ((abs(max(xpretty) - max(xlim)) > 1e-6)){
        xpretty <- xpretty[-length(xpretty)]
    }
    # switched with above due to precision issues
    # xpretty <- xpretty[xpretty >= min(xlim) & xpretty <= max(xlim)]
    if (all(ylim == c(0, 111))){
        ypretty <- seq(0, 110, by = 10)
    } else {
        ypretty <- pretty(ylim, n = 10)
        ypretty <- ypretty[ypretty >= min(ylim) & ypretty <= max(ylim)] 
    }

    if (grid){
    plot(
            NULL, 
            type = "n", 
            xlim = xlim, 
            ylim = ylim, 
            axes = FALSE, 
            xlab = "", 
            ylab = "",
            panel.first =  list(
                    rect(xlim[1], ylim[1], xlim[2], ylim[2], col = bg, border = FALSE),
                    segments(xpretty, min(ylim), xpretty, max(ylim), col = "white", lwd = lwd, ...),
                    segments(min(xlim), ypretty, max(xlim), ypretty, col = "white", lwd = lwd, ...))
            )
        } else {
            plot(
                    NULL, 
                    type = "n", 
                    xlim = xlim, 
                    ylim = ylim, 
                    axes = FALSE, 
                    xlab = "", 
                    ylab = "",
                    panel.first =  list(rect(xlim[1], ylim[1], xlim[2], ylim[2], col = bg, border = FALSE))
            )
        }
    invisible(list(x = xpretty, y = ypretty, xl = xlim[1]))
}

DrawLabels <- function(
        tics, 
        xlab = "(Millions)", 
        ylab = "Age", 
        xlabs = zapsmall(abs(tics$x / 1e6)),
        cex = .8){
    xdiff <- diff(tics$x[1:2])
    text(tics$x, 0, xlabs, pos = 1, cex = cex, xpd = TRUE)
    text(tics$xl, tics$y, tics$y, pos = 2, cex = cex, xpd = TRUE)
    text(mean(tics$x), -12, xlab, cex = cex, xpd = TRUE)
    text(tics$xl - xdiff / 2, max(tics$y) + 10, ylab, cex = cex, xpd = TRUE)
}

Drawlegend <- function(
        tics, 
        N,
        colRamp = BrewerRamp("Blues"),
        revColors = FALSE,
        label = "Years Left",
        cex = .8
        ){
            Colors      <- colRamp(N)
            if(revColors){
                Colors <- rev(Colors)
            }
            xdiff <- diff(tics$x[1:2])
            yat   <- seq(min(tics$y), max(tics$y), length = N + 1)
            rect(max(tics$x) + xdiff, yat[1:N], max(tics$x) + 2 * xdiff, yat[2:(N + 1)], 
                    col = Colors, border = gray(.3), lwd = .5, xpd = TRUE)
            text(max(tics$x) + 2 * xdiff, yat[1:N], tics$y, cex = cex, pos = 4, xpd = TRUE)
            text(max(tics$x) + 1.5 * xdiff, max(tics$y) + 10, label, cex = cex, xpd = TRUE)

        }

# help functions for diagram of lifeline
# clockwise quarter arc (90 degrees)
degrees2radians <- function(degrees){
    degrees * (pi / 180)
}

quarterArc <- function(x, y, radius = 1, fromDegrees = 180, ...){
    xx <- degrees2radians(seq(fromDegrees, fromDegrees + 90, by = .5))
    x <- cos(xx) * radius + x
    y <- sin(xx) * radius + y
    lines(x, y, ...)
}

curlyBrace1 <- function(xl, y, length = 5, radius1 = .5, radius2 = .25, top = TRUE, ...){  
    # i.e. the pointy part on top or on bottom?
    if (top){
        quarterArc(xl + radius1, y - radius1, radius = radius1, fromDegrees = 90, ...)
        quarterArc(xl + length - radius1, y - radius1 , radius = radius1, fromDegrees = 0, ...)
        quarterArc(xl + length / 2 - radius2, y + radius2, radius = radius2, fromDegrees = 270, ...)
        quarterArc(xl + length / 2 + radius2, y + radius2, radius = radius2, fromDegrees = 180, ...)
    } else {
        quarterArc(xl + radius1, y + radius1, radius = radius1, fromDegrees = 180, ...)
        quarterArc(xl + length - radius1, y + radius1 , radius = radius1, fromDegrees = 0 - 90, ...)
        quarterArc(xl + length / 2 - radius2, y - radius2, radius = radius2, fromDegrees = 270 + 90, ...)
        quarterArc(xl + length / 2 + radius2, y - radius2, radius = radius2, fromDegrees = 180 - 90, ...)        
    }
    segments(xl + radius1, y, xl + length / 2 - radius2, y, ...)
    segments(xl + length - radius1, y, xl + length / 2 + radius2, y, ...)   
}

da2fya <- function(da, stagger = FALSE){
    N       <- length(da)
    ay      <- 1:N - 1
    
    da      <- Mna0(da)   # remove NAs if any       
    da      <- c(da, da * 0) / sum(da) # pad out with 0s
    fya     <- matrix(da[col(matrix(nrow = N, 
                                    ncol = N)) + ay], 
            nrow = N, 
            ncol = N, 
            dimnames = list(Ex = ay, 
                    Age = ay)
    )
    if (stagger){
        fya <- (fya + cbind(fya[, 2:ncol(fya)], 0)) / 2
    }
    fya <- Minf0(Mna0(fya / rowSums(fya)))
    fya
}