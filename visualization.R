
# in case there are a lot of outliers and we do not want to show them
# in ggplot, setting outliers outlier.shape=NA will not automatically reset the plot borders to boxplot only
# but will instead pretend the outliers are still there but visible
# therefore, the plot y-axis margins need to be reset by hand

findminmax <- function(lfc) {
  low.whisker <- quantile(lfc,0.25) - (1.5*IQR(lfc))
  high.whisker <- quantile(lfc,0.75) + (1.5*IQR(lfc))
  return(if (prod(low.whisker, high.whisker)<0) {
    c(low.whisker, high.whisker)
  } else {
    if (low.whisker>0) {
      c(0, high.whisker)
    } else {
      c(low.whisker, 0)
    }
  })
}

# this function will include 0 point!

# coord_cartesian(ylim = findminmax(data$value))

# we can also make the distance to 0 equal on positive and negative side:

# coord_cartesian(ylim = c(-1,1) * max(abs(data$value)))
