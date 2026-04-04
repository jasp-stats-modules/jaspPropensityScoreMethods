# density plot
.createIptwDensities = function(jaspResults, dataw, options) {
  treatment_col <- options$treatment
  dataw[[treatment_col]] <- as.character(dataw[[treatment_col]])

  p <- ggplot2::ggplot(dataw, aes(x = .data[["weight"]], fill = .data[[treatment_col]])) +
    ggplot2::geom_density(alpha = options$opacity) +
    ggplot2::scale_fill_manual(values = c(options$untreatedColor, options$treatedColor),
                               labels = c("Untreated", "Treated"),
                               guide  = ggplot2::guide_legend(nrow=1, byrow=T)) +
    ggplot2::guides(alpha = "none") +
    ggplot2::labs(x    = "IPW Weights",
                  y    = "Density",
                  fill = "Treatment") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  weightPlot <- createJaspPlot(title = "Weight Densities", width = 400, height = 300)
  weightPlot$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled", "opacity", "untreatedColor", "treatedColor"))
  weightPlot$plotObject <- p
  jaspResults[["weightDensities"]] <- weightPlot
}
# love plot
.createIptwLovePlot = function(jaspResults, dataset, options) {
  treatment   <- options$treatment
  confounders <- options$confounders
  w           <- dataset$weight

  loveplot <- cobalt::love.plot(
    x       = as.formula(paste0(treatment, "~", paste(confounders, collapse = " + "))),
    data    = dataset,
    weights = w,
    binary  = "std",
    un      = TRUE,
    s.d.denom = "pooled"
  ) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c(options$untreatedColor, options$treatedColor)) +
    ggplot2::geom_vline(xintercept = c(-0.1, 0.1), lty = 2, col = "black") +
    ggplot2::theme(legend.position = "bottom")

  lovePlot <- createJaspPlot(title = gettext("Love Plot"), width = 400, height = 400)
  lovePlot$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled",
                      "truncate", "untreatedColor", "treatedColor"))
  lovePlot$plotObject <- loveplot
  jaspResults[["iptwLovePlot"]] <- lovePlot
}
# propensity score overlap
.createPSOverlapPlot = function(jaspResults, dataset, options) {
  treatment   <- options$treatment
  confounders <- options$confounders

  f         <- as.formula(paste0(treatment, "~", paste(confounders, collapse = " + ")))
  ps_model  <- glm(f, data = dataset, family = binomial(link = "logit"))
  ps        <- predict(ps_model, type = "response")

  plotdf <- data.frame(
    ps        = ps,
    treatment = as.character(dataset[[treatment]])
  )

  p <- ggplot2::ggplot(plotdf, aes(x = ps, fill = treatment)) +
    ggplot2::geom_density(alpha = options$opacity) +
    ggplot2::scale_fill_manual(
      values = c(options$untreatedColor, options$treatedColor),
      labels = c("Untreated", "Treated"),
      guide  = ggplot2::guide_legend(nrow = 1, byrow = T)
    ) +
    ggplot2::guides(alpha = "none") +
    ggplot2::labs(x    = "Propensity Score",
                  y    = "Density",
                  fill = "Treatment") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  psPlot <- createJaspPlot(title = gettext("Propensity Score Overlap"), width = 400, height = 300)
  psPlot$dependOn(c("treatment", "confounders", "opacity", "untreatedColor", "treatedColor"))
  psPlot$plotObject <- p
  jaspResults[["psOverlapPlot"]] <- psPlot
}
# weight summary table
.createWeightSummaryTable = function(jaspResults, dataw, options) {
  treatment <- options$treatment
  treat_col <- as.integer(as.character(dataw[[treatment]]))
  w         <- dataw$weight

  # Compute summary by group
  groups    <- list(Treated = w[treat_col == 1], Control = w[treat_col == 0])

  rows <- lapply(names(groups), function(g) {
    wg <- groups[[g]]
    data.frame(
      Group  = g,
      N      = length(wg),
      Min    = round(min(wg), 3),
      Max    = round(max(wg), 3),
      Mean   = round(mean(wg), 3),
      Median = round(median(wg), 3),
      SD     = round(sd(wg), 3)
    )
  })
  df <- do.call(rbind, rows)

  table <- createJaspTable(title = gettext("Weight Summary"))
  table$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled", "truncate"))
  table$addRows(df)
  jaspResults[["weightSummary"]] <- table
}
# smd summary table
.createSMDTable = function(jaspResults, dataset, options) {
  treatment   <- options$treatment
  confounders <- options$confounders
  w           <- dataset$weight
  treat_num   <- as.integer(as.character(dataset[[treatment]]))

  bal <- cobalt::bal.tab(
    x       = as.formula(paste0(treatment, "~", paste(confounders, collapse = " + "))),
    data    = dataset,
    weights = w,
    un=T,
    #treat   = treat_num,
    binary  = "std"
  )

  # Debug: show actual column names

  df <- as.data.frame(bal$Balance)
  df$Covariate <- rownames(df)
  df <- df[, c("Covariate", "Diff.Un", "Diff.Adj")]
  colnames(df) <- c("Covariate", "SMD Before", "SMD After")
  df[, 2:3] <- round(df[, 2:3], 3)

  table <- createJaspTable(title = gettext("Standardized Mean Differences"))
  table$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled", "truncate"))
  table$addRows(df)
  jaspResults[["smdTable"]] <- table
}
# ess table
.createESSTable = function(jaspResults, dataset, options) {
  treatment <- options$treatment
  treat_num <- as.integer(as.character(dataset[[treatment]]))
  w         <- dataset$weight

  # ESS = (sum(w))^2 / sum(w^2) per group
  ess_treated <- sum(w[treat_num == 1])^2 / sum(w[treat_num == 1]^2)
  ess_control <- sum(w[treat_num == 0])^2 / sum(w[treat_num == 0]^2)
  n_treated   <- sum(treat_num == 1)
  n_control   <- sum(treat_num == 0)

  df <- data.frame(
    Group             = c("Treated", "Control"),
    N                 = c(n_treated, n_control),
    ESS               = c(round(ess_treated, 1), round(ess_control, 1)),
    "ESS/N (%)"       = c(round(100 * ess_treated / n_treated, 1),
                          round(100 * ess_control / n_control, 1)),
    check.names = FALSE
  )

  table <- createJaspTable(title = gettext("Effective Sample Size"))
  table$dependOn(c("treatment", "confounders", "stabilize", "truncateEnabled", "truncate"))
  table$addRows(df)
  jaspResults[["essTable"]] <- table
}
# weighting function
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
    treat_num <- as.integer(as.character(treat_col))
    # jaspResults[["debug"]] <- createJaspHtml(
    #   paste("treat_num sample:", paste(head(treat_num), collapse=", "),
    #         "<br>mean:", mean(treat_num, na.rm=TRUE),
    #         "<br>class:", class(treat_col),
    #         "<br>levels:", paste(levels(treat_col), collapse=", ")),
    #   "p"
    # )
    p_treat <- mean(treat_num, na.rm = TRUE)
    w       <- ifelse(treat_num == 1, p_treat / ps, (1 - p_treat) / (1 - ps))
  }

  if (isTRUE(options$truncateEnabled)) {
    lower <- quantile(w, options$truncate)
    upper <- quantile(w, 1 - options$truncate)
    w     <- pmin(pmax(w, lower), upper)
  }

  dataset$weight <- w
  .createWeightSummaryTable(jaspResults, dataset, options)
  .createSMDTable(jaspResults, dataset, options)
  .createESSTable(jaspResults, dataset, options)
  .createIptwDensities(jaspResults, dataset, options)
  .createIptwLovePlot(jaspResults, dataset, options)
  .createPSOverlapPlot(jaspResults, dataset, options)
}
