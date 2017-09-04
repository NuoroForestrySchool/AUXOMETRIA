regr.plot <- function(df, fit, xvar = NULL, xpred = NULL, yvar = NULL, xlbl = NULL, ylbl = NULL, tit = NULL) {
#  Plot observations and fitting details  
#  df <- AlberiModello; fit <- ci; xvar <- "d_130"; xpred = Tavola$d_130; yvar = NULL; xlbl = NULL; ylbl = NULL; tit = NULL
  
  l <- list(R2 = signif(summary(fit)$r.squared, 4), coef = signif(coefficients(fit), 4), 
            y.fit = all.vars(formula(fit))[[1]])
  l[["x.fit"]]  <- with(l, names(coef)[[2]])
  l[["label1"]] <- with(l, paste(y.fit, "=", coef[[1]], "+", coef[[2]], "*", x.fit))
  l[["label2"]] <- with(l, paste("R^2 =", R2))
  
  if (is.null(xvar)) xvar <- l$x.fit
  if (is.null(xlbl)) xlbl <- xvar
  if (is.null(yvar)) yvar <- l$y.fit
  if (is.null(ylbl)) ylbl <- yvar
  
  p <- ggplot(data = df) +
    geom_point(mapping = aes(x = get(xvar), y =get(yvar)), colour = "grey40", shape = 1) +
    labs(x = xlbl, y = ylbl) + 
    ggtitle(tit)
  p.range <- list(
    x= ggplot_build(p)$layout$panel_ranges[[1]]$x.range,
    y= ggplot_build(p)$layout$panel_ranges[[1]]$y.range )
  if (is.null(xpred)) xpred <- pretty(p.range$x)
  df.pred <- data.frame(xpred)
  names(df.pred) = xvar
  ci_conf <- cbind(df.pred, predict(fit, df.pred, interval="prediction"))
  p + 
    geom_ribbon(data = ci_conf, aes(x = get(xvar), ymin = lwr, ymax = upr),
                fill="grey", alpha=.4) + 
    geom_line(data = ci_conf, aes(x = get(xvar), y = fit)) +
    geom_text(x=p.range$x[[2]], y=p.range$y[[1]], label = paste(l$label1,"\n", l$label2), fontface = 'italic', hjust = "right", vjust="bottom", size=3)
  # annotate("text", x=p.range$x[[2]], y=p.range$y[[1]], label = paste(l$label1,"\n", l$label2),                          fontface = 'italic', hjust = "right", vjust="bottom", size=3)
  
}
