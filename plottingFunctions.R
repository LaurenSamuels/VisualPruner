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
        "WeightedATT", "WeightedATM")
    myTabTypes <- intersect(allTabTypes, names(dat))  
    nTabTypes <- length(myTabTypes)      

    # colors from http://colorbrewer2.org
    allColors <- 
        c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
    names(allColors) <- allTabTypes
    myColors <- allColors[myTabTypes]

    allShapes <- (21:25)
    names(allShapes) <- allTabTypes
    myShapes <- allShapes[myTabTypes]

    allLinetypes <- c("solid", "dashed", "dotted", 
        "dotdash", "longdash")
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

