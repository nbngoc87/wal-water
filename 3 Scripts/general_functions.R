cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

loadpackage <- function(x) {
  if (!suppressWarnings(require(x,character.only = TRUE))) list(install.packages(x, dep=TRUE), library(x, character.only = TRUE))
}


plgcol <- function(var, col = NULL, n = 5, pal = 'Blues', rev = F, style = 'quantile', fixedBreaks = NULL) {
  require('RColorBrewer')
  require('classInt')
  if (is.null(col) == T) {
    if (rev) {
      plotclr <- rev(brewer.pal(n, name = pal))  
    } else {
      plotclr <- brewer.pal(n, name = pal)    
    }    
  } else {
    plotclr <- col[1:n]
  }
  if (style %in% 'fixed') {
    class <- classIntervals(var, n, style= 'fixed', fixedBreaks = fixedBreaks)
  } else {
    class <- classIntervals(var, n, style= style)
  }
  colcode <- findColours(class, plotclr)
}


lvls <- function(x) {
  levels(droplevels(as.factor(x)))
}

nlvls <- function(x) {
  nlevels(droplevels(as.factor(x)))
}

