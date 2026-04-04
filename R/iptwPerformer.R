.createIptwDensities = function(jaspResults, dataw, options) {
  treatment_col <- options$treatment
  p <- ggplot2::ggplot(dataw, aes(x = .data[["weight"]], fill = factor(.data[[treatment_col]]))) +
    ggplot2::geom_density(alpha = options$opacity) +
    ggplot2::scale_fill_manual(values = c(options$untreatedColor, options$treatedColor),
                               labels = c("Control", "Treated")) +
    ggplot2::labs(title = "Weight Distribution by Treatment",
                  x = "IPW Weights", y = "Density") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  weightPlot <- createJaspPlot(title = "Weight Densities", width = 400, height = 300)
  weightPlot$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled", "opacity"))
  weightPlot$plotObject <- p
  jaspResults[["weightDensities"]] <- weightPlot
}

iptw = function(jaspResults, dataset, options) {
  if (length(options$confounders) == 0) return()
  if (length(options$treatment) == 0) return()

  treatment   <- options$treatment
  confounders <- options$confounders

  f         <- as.formula(paste0(options$treatment, "~", paste(options$confounders, collapse = " + ")))
  den_model <- glm(f, data = dataset, family = binomial(link = "logit"))
  ps        <- predict(den_model, type = "response")
  treat_col <- dataset[[treatment]]

  w <- ifelse(treat_col == 1, 1 / ps, 1 / (1 - ps))

  if (isTRUE(options$stabilize)) {
    p_treat <- mean(treat_col)
    w       <- ifelse(treat_col == 1, p_treat / ps, (1 - p_treat) / (1 - ps))
  }

  if (isTRUE(options$truncateEnabled)) {
    lower <- quantile(w, options$truncate)
    upper <- quantile(w, 1 - options$truncate)
    w     <- pmin(pmax(w, lower), upper)
  }

  dataset$weight <- w
  .createIptwDensities(jaspResults, dataset, options)
}
