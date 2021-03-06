

cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )



lvls <- function(x) {
  levels(droplevels(as.factor(x)))
}

nlvls <- function(x) {
  nlevels(droplevels(as.factor(x)))
}

summaryf <- function(x) {
  summary(droplevels(as.factor(x)))
}

theme_kat <- function(base_size = 8, legend.position = "bottom") {
  theme_bw(base_size = base_size) +
    theme(
      line = element_line(size = 0.8),
      axis.text.x = element_text(color = 'black'),
      axis.text.y = element_text(color = 'black'),
      legend.position = legend.position,
      legend.box = "vertical",
      legend.box.spacing = unit(1, "lines"),
      legend.margin = margin(-2, 0,-2, 0, unit = "mm"),
      legend.text = element_text(size = rel(1))
    )
}
