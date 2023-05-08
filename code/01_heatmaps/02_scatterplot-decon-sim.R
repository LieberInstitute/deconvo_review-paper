#!/usr/bin/env R

# Author: Sean Maden
#
# Get example simulation results for main figure. Make results scatterplots 
#

libv <- c("lute", "reshape2", "ggplot2", "dplyr")
sapply(libv, library, character.only = T)

# load
env.path <- './deconvo_commentary-paper/output/fig4_data.RData'
load(file=env.path)


# get simulation series on variable ptrue
ptrue1 <- seq(0.5, 0.7, 0.01)
ptrue.list <- lapply(ptrue1, function(ptrue1.iter){
  pdiff <- 1 - ptrue1.iter
  ptrue2.iter <- 2*(pdiff/3)
  ptrue3.iter <- 1 - (ptrue1.iter + ptrue2.iter)
  return(c("Neuron" = ptrue1.iter, 
           "Oligodendrocyte" = ptrue2.iter, 
           "Astrocyte" = ptrue3.iter))
})

set.seed(0)
z <- hm.marker.table
z <- z[,order(colnames(z))]
z.list <- lapply(seq(length(ptrue.list)), function(iter){
  z + rnorm(nrow(z)*2, mean = 0, sd = 2e-1)
})

# type.vector <- c("neuron", "astro", "oligo")
# s <- c(10, 3, 2)
s1.vector <- seq(9, 11, 1e-3)
s1.vector <- s1.vector[sample(length(s1.vector), length(ptrue1))]
s.list <- lapply(s1.vector, function(s1.value){
  s2.value <- s1.value/2; s3.value <- s2.value/2
  return(c("Neuron" = s1.value,
           "Oligodendrocyte" = s3.value,
           "Astrocyte" = s2.value))
})
iter.vector <- seq(length(ptrue.list))
lsim <- lapply(iter.vector, function(iter){
  s.iter <- s.list[[iter]]
  ptrue.iter <- ptrue.list[[iter]]
  z.iter <- z.list[[iter]]
  # match orders of s and p to z
  ptrue.iter <- ptrue.iter[
    order(match(names(ptrue.iter), colnames(z.iter)))]
  s.iter <- s.iter[
    order(match(names(s.iter), colnames(z.iter)))]
  identical(names(ptrue.iter), colnames(z.iter))
  identical(names(s.iter), colnames(z.iter))
  # ypb always uses the same z
  zs.iter <- lute:::.zstransform(z, s.iter)
  ypb.iter <- t(t(ptrue.iter) %*% t(zs.iter))
  result1 <- nnlsParam(z = z.iter, y = ypb.iter) %>% deconvolution()
  result2 <- nnlsParam(z = z.iter, y = ypb.iter, s = s.iter) %>% 
    deconvolution()
  result <- rbind(result1, result2, ptrue.iter)
  rownames(result) <- c("p.pred.false", "p.pred.true", "p.true")
  data <- t(result) %>% as.data.frame()
  plot.data <- melt(data[,seq(2)], id = data$p.true)
  plot.data$p.true <- rep(data$p.true, 2)
  plot.data$cell.type <- rep(colnames(result), 2)
  return(plot.data)
})
tsim <- do.call(rbind, lsim) %>% as.data.frame()

tsim$variable <- as.character(tsim$variable)
is.true <- tsim$variable=="p.pred.true"
is.false <- tsim$variable=="p.pred.false"
tsim[is.true,]$variable <- "scaled"
tsim[is.false,]$variable <- "unscaled"
tsim$cell.scale.factor <- tsim$variable

new.plot <- ggplot(tsim, aes(x = p.true, y = value, 
                 shape = cell.scale.factor, color = cell.type)) + 
  geom_point(size = 2, alpha = 0.3) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() + xlim(0, 1) + ylim(0, 1) +
  xlab("Truth") + ylab("Prediction") +
  stat_ellipse() + facet_wrap(~cell.scale.factor) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("#db6b00",
                                "#6db100",
                                "#dbc700"))


plot.name <- "scatterplot_panel_truth-vs-pred.jpg"
plot.path <- file.path("./deconvo_commentary-paper", "output", plot.name)
jpeg(plot.path, width = 5.5, height = 2.5, units = "in", res = 400)
new.plot
dev.off()
