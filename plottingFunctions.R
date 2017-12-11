
makeLogitPSPlot <- function(dat, sortedFactorLevels, grpVarFactorName, 
    xvarName, colorscale, alphaval) {
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
        plot(0, 0, 
            xlim= range(dat[, get(xvarName)]),
            ylim= c(0, max(histcounts)), 
            xlab= "Logit PS",
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