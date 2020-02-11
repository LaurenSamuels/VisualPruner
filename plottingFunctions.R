# Plotting functions used by Visual Pruner

makePSPlot <- function(dat, sortedFactorLevels, grpVarFactorName, 
    xvarName, colorscale, alphaval, logit= TRUE) {
    
    if (logit) {
        pointx <- 0
        xLab <- "Logit PS"
    } else {
        pointx <- 0.5
        xLab <- "PS"
    }
    
    histlist <- vector("list", length(sortedFactorLevels))
    names(histlist) <- sortedFactorLevels
    for (lev in sortedFactorLevels) {
        x <- dat[get(grpVarFactorName) == lev, get(xvarName)]
        if (length(x) > 0) {
            histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
        } else {
            histlist[[lev]] <- NULL
        }
    }
    histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
    plot(pointx, 0, 
        xlim= range(dat[, get(xvarName)]),
        ylim= c(0, max(histcounts)), 
        xlab= xLab,
        ylab= "Count",
        bty= "n",
        type= "n",
        cex.lab= 1,
        cex.axis= 0.8
    )
    # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
    for (lev in sortedFactorLevels) {
        myColor <- colorscale[lev] 
        if (!is.null(histlist[[lev]])) {
            plot(histlist[[lev]], 
                freq   = TRUE, 
                col    = adjustcolor(myColor, alpha.f= alphaval),
                border = NA,
                add    = TRUE
            )
        }
    }
    if (logit) {
        legend(
            "topleft",
            inset  = .05,
            cex    = .8,
            title  = NULL,
            sortedFactorLevels,
            horiz  = FALSE,
            bty    = "n",
            border = NA,
            fill   = do.call(c, lapply(sortedFactorLevels, function(x)
                adjustcolor(colorscale[x], alpha.f= alphaval)))
        )   
    }
}

makeSMDPlot <- function(dat, wantLines) {
    nvars <- nrow(dat)
    allTabTypes <- c("Original", "Pruned", "WeightedATE", 
        "WeightedATT", "WeightedATM", "WeightedATO")
    myTabTypes <- intersect(allTabTypes, names(dat))  
    nTabTypes <- length(myTabTypes)      

    # colors from http://colorbrewer2.org
    allColors <- 
        c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33')
    names(allColors) <- allTabTypes
    myColors <- allColors[myTabTypes]

    allShapes <- c(1, 21:25)
    names(allShapes) <- allTabTypes
    myShapes <- allShapes[myTabTypes]

    allLinetypes <- c("solid", "dashed", "dotted", 
        "dotdash", "longdash", "twodash")
    names(allLinetypes) <- allTabTypes
    myLinetypes <- allLinetypes[myTabTypes]

    maxX <- max(dat[, myTabTypes, with= FALSE], na.rm= TRUE)
    
    def.par <- par(no.readonly = TRUE)
    par(
        #ann      = FALSE,
        mar      = c(5, 10, 4, 2) + 0.1, #bltr
        oma      = c(0,0,0,0),
        cex.axis = 1.1,
        cex.lab  = 1.2
    ) 
    plot(0, 1,
        type = "n",
        bty  = "n",
        xlim = c(0, maxX),
        ylim = c(1, nvars),
        axes = FALSE,
        xlab = 'Absolute standardized mean difference',
        ylab = ''
    )
    axis(1)
    axis(2, at= 1:nvars, labels= dat[, variable],
        las= 1)

    abline(v = 0.1, lty= 'dashed', col= 'gray')
    
    if (wantLines) { 
        for (j in 1:nTabTypes) {
            lines(
                x   = as.numeric(dat[[myTabTypes[j]]]),
                y   = 1:nvars,
                lty = myLinetypes[j],
                lwd = 1.2,
                col = myColors[j]
            )
        }
    }
    
    for (i in 1:nvars) {
        abline(h= i, lty= 'dotted', col= 'gray')
        points(
            x   = as.numeric(dat[i][, myTabTypes, with = FALSE]),
            y   = rep(i, nTabTypes),
            pch = myShapes,
            col = myColors,
            bg  = myColors
        )
    }
    legend(
        "bottomright",
        legend = myTabTypes,
        inset  = c(.1, .05), #x, y
        cex    = 1,
        title  = NULL,
        horiz  = FALSE,
        bty    = "n",
        border = NA,
        col    = myColors,
        pt.bg  = myColors,
        lty    = myLinetypes,
        pch    = myShapes
    )
    # reset the graphics
    par(def.par)
}

# For covariate plots:
makeFig1 <- function(Testing) {
    # fig 1: Y axis label for central plot. 
    plot(x= 1, y= 1, type= "n", ylim= c(-1, 1), xlim= c(-1, 1))
    text(0, 0, paste("Logit PS"), cex= 1.4, srt= 90)
    if (Testing) box("outer", col= "blue")
    if (Testing) box("figure", col= "green") 
}

makeFig2 <- function(Varname, Testing) {
    # fig 2:  X axis label for central plot. 
    plot(x= 1, y= 1, type="n", ylim= c(-1, 1), xlim= c(-1, 1))
    text(0, 0, paste(Varname), cex=1.4)
    if (Testing) box("figure", col= "green")    
}


makeFig3 <- function(dat, yLim, WantShading, brushmin, brushmax, 
    gVarFactorLevelsSorted, gVarFactorName, lpsVarName,
    pSizeVal, colScale, alphVal, 
    MyJitter, MyAtAdds, MyWidth, Testing) {
    # fig 3, right-side central (row2) plot
    
    plot(1, 0,
        xlim = c(0, 2), 
        ylim = yLim, 
        axes = FALSE,
        type = "n"
    )
    if (WantShading) {
        par.usr <- par("usr")
        rect(
            xleft   = par.usr[1], 
            ybottom = brushmin, 
            xright  = par.usr[2], 
            ytop    = brushmax,
            density = NA,
            border  = NA,
            col     = "#DFD7CA"
        )
    }
    if (nrow(dat) >= 1) {
        for (lev in gVarFactorLevelsSorted) {
            y <- dat[get(gVarFactorName) == lev, get(lpsVarName)]
            stripchart(y,
                vertical = TRUE,
                add      = TRUE,
                method   = "jitter",
                jitter   = MyJitter,
                pch      = 20,
                at       = 1 + MyAtAdds[lev],
                cex      = pSizeVal,
                col      = adjustcolor(colScale[lev], 
                            alpha.f= alphVal)
            )
        }
        myMean <- 
            mean(dat[[lpsVarName]])   
        segments(
            1 - MyWidth / 2, myMean,
            1 + MyWidth / 2, myMean
        )
    }
    axis(1, at = 1, labels= "Missing")
    if (Testing) box("figure", col= "green")
    if (Testing) box("plot", col= "black")    
}


makeFig4 <- function(dat, VarIsCont, Varname, myYLimCounts,
    gVarFactorLevelsSorted, colScale, alphVal, Histlist,
    MyAtOrig, MyAtAdds, MyJitter, Xtbl,
    Testing) {
    # fig 4, top central plot. 
    
    if (VarIsCont) {    
        plot(min(dat[[Varname]]), 0, 
            xlim = range(dat[[Varname]]),
            ylim = c(0, myYLimCounts), 
            bty  = if (Testing) "o" else "n",
            type = "n"
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in gVarFactorLevelsSorted) {
            myColor <- colScale[lev] 
            if (!is.null(Histlist[[lev]])) {
                plot(Histlist[[lev]], 
                    freq   = TRUE, 
                    col    = adjustcolor(myColor, alpha.f= alphVal),
                    border = NA,
                    lty    = 0, # this is to help with rendering on server. Not sure it does though.
                    add    = TRUE
                )
            }
        }
    } else { # discrete
        # https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/
        plot(1, 0,
            xlim = c(min(MyAtOrig) - 1, max(MyAtOrig) + 1), 
            ylim = c(0, myYLimCounts), 
            bty  = if (Testing) "o" else "n",
            type = "n")
    
        for (grouplev in gVarFactorLevelsSorted) {
            for (varlev in levels(dat[[Varname]])) {
                rect(
                    xleft   = MyAtOrig[varlev] + MyAtAdds[grouplev] - MyJitter,
                    ybottom = 0,
                    xright  = MyAtOrig[varlev] + MyAtAdds[grouplev] + MyJitter,
                    ytop    = Xtbl[grouplev, varlev],
                    density = NA,
                    border  = NA,
                    col     = colScale[grouplev] 
                )
            }
        }
    }
    if (Testing) box("figure", col= "green")   
}


makeFig5 <- function(dat, VarIsCont, Varname, lpsVarName,
    MyXlim, MyYlimPS, MyAtOrig, MyAtAdds, MyJitter, MyWidth,
    WantShading, brushmin, brushmax, 
    gVarFactorLevelsSorted, gVarFactorName, 
    pSizeVal, colScale, alphVal, 
    Testing) {
    # fig 5, the main plot
    if (VarIsCont) {    
        plot(dat[[Varname]], dat[[lpsVarName]], 
            xlim = MyXlim, 
            ylim = MyYlimPS, 
            bty  = if (Testing) "o" else "n",
            type = "n"
        )
    } else {
        par(las= 2) #TODO: try to angle instead
    
        plot(1, 0,
            xlim = c(min(MyAtOrig) - 1, max(MyAtOrig) + 1), 
            ylim = MyYlimPS, 
            axes = FALSE,
            type = "n")
    }
    if (WantShading) {
        par.usr <- par("usr")
        rect(
            xleft   = par.usr[1], 
            ybottom = brushmin, 
            xright  = par.usr[2], 
            ytop    = brushmax,
            density = NA,
            border  = NA,
            col     = "#DFD7CA"
        )
    }
    if (VarIsCont) {    
        for (lev in gVarFactorLevelsSorted) {
            x <- dat[get(gVarFactorName) == lev, 
                get(Varname)]
            y <- dat[get(gVarFactorName) == lev, 
                get(lpsVarName)]
            points(x, y,
                pch = 20,
                cex = pSizeVal,
                col = adjustcolor(colScale[lev], 
                    alpha.f= alphVal))
        }
        lines(loess.smooth(dat[[Varname]], 
            dat[[lpsVarName]]))
    } else {
        for (lev in gVarFactorLevelsSorted) {
            x <- dat[get(gVarFactorName) == lev, 
                get(Varname)]
            y <- dat[get(gVarFactorName) == lev, 
                get(lpsVarName)]
            stripchart(y ~ x,
                vertical = TRUE,
                add      = TRUE,
                method   = "jitter",
                jitter   = MyJitter,
                pch      = 20,
                at       = MyAtOrig + MyAtAdds[lev],
                cex      = pSizeVal,
                col      = adjustcolor(colScale[lev], 
                            alpha.f= alphVal)
            )
        }
        means <- unlist(lapply(levels(dat[[Varname]]), 
            function(levl) {
                mean(dat[get(Varname) == levl, 
                    get(lpsVarName)])
            }
        ))
        for (i in seq_along(levels(dat[[Varname]]))) {
            levl <- levels(dat[[Varname]])[i]
            myMean <- mean(dat[get(Varname) == levl, 
                get(lpsVarName)])   
            segments(
                MyAtOrig[i] - MyWidth / 2, myMean,
                MyAtOrig[i] + MyWidth / 2, myMean
            )
        }
        axis(1, at = MyAtOrig, labels = names(MyAtOrig))
        axis(2)
    }
    if (Testing) box("figure", col= "green")
    if (Testing) box("plot", col= "black")
}

makeFig6 <- function(dat, MyYlimCounts, gVarFactorLevelsSorted,
    MyAtAdds, MyJitter, colScale, Testing) {
    # fig 6, top right plot. 
    plot(1, 0,
        xlim = c(0, 2), 
        ylim = c(0, MyYlimCounts), 
        bty  = if (Testing) "o" else "n",
        type = "n")

    for (grouplev in gVarFactorLevelsSorted) {
        rect(
            xleft   = 1 + MyAtAdds[grouplev] - MyJitter,
            ybottom = 0,
            xright  = 1 + MyAtAdds[grouplev] + MyJitter,
            ytop    = dat[grouplev],
            density = NA,
            border  = NA,
            col     = colScale[grouplev] 
        )
    }
    if (Testing) box("figure", col= "green")    
}
