library(ggplot2)

### BOXPLOTS

# by default, ggplot2 plots 1.5*IQR as whiskers
# setting new boxplot parameters using stat_summary function

setquantiles <- function(x, lq=0.05, up=0.95) {
  r <- quantile(x, probs=c(lq, 0.25, 0.5, 0.75, up))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return (r)
}

#ggplot(data,aes(x=motif, y=log2FoldChange)) +
#stat_summary(fun.data = setquantiles, geom = "boxplot", width=0.25, fill="white")


# in case there are a lot of outliers and we do not want to show them
# in ggplot, setting outliers outlier.shape=NA will not automatically reset the plot borders to boxplot only
# but will instead pretend the outliers are still there but invisible
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
# ggplot(data,aes(x=motif, y=log2FoldChange)) +
# coord_cartesian(ylim = findminmax(data$value))

# we can also make the distance to 0 equal on positive and negative side:
# coord_cartesian(ylim = c(-1,1) * max(abs(data$value)))
