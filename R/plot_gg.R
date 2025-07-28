plot_results <- function(
    popvec = NULL,
    standard.A = FALSE,
    facet = FALSE,
    compare = FALSE,
    sort = FALSE,
    palette = NULL
  ) {

  if (isFALSE(standard.A)) {
    ylab <- "Population"
  } else {
    ylab <- "Population size / density"
  }

  multiple <- class(popvec) == "list"

  if (isTRUE(multiple)) {
    n <- length(popvec)
    pops <- unlist(popvec)
    time <- 0:((length(pops)-1) / n)

    dat <- data.frame(
      id = rep(1:n, each = length(time)),
      pop = pops,
      time = time,
      grp = rep(1:n, each = length(time))
    )
  } else {
    dat <- data.frame(
      pop = popvec,
      time = 0:(length(popvec)-1)
    )
  }

  drop <- c()

  if (isFALSE(multiple)) {
    if (isTRUE(facet)) {
      drop <- c(drop, "facet")
    }
    if (isTRUE(compare)) {
      drop <- c(drop, "compare")
    }
    if (isTRUE(compare)) {
      drop <- c(drop, "sort")
    }

    if (length(drop) == 1) {
      message(
        paste0("`", drop[1], " = TRUE` is ignored as popvec only contains one population projection.")
      )
    }
    if (length(drop) > 1) {
      message(
        paste0(
          paste(head(paste0("`", drop, " = TRUE`"), -1), collapse = ", "),
          " and ",
          tail(paste0("`", drop, " = TRUE`"), 1),
          " are ignored as popvec only contains one population projection."
        )
      )
    }
  }

  if (isTRUE(sort) & isTRUE(multiple)) {
    dat <- dat[order(dat$pop, decreasing = TRUE), ]
    dat$id <- factor(dat$id, levels = unique(dat$id))
  }

  if (isFALSE(multiple) & !is.null(palette)) {
    color <- palette[1]
  } else {
    color <- "black"
  }

  plot <- ggplot2::ggplot(data = dat) +
    ggplot2::aes(x = time, y = pop) +
    {
      if((isTRUE(multiple) & isFALSE(facet)) | !is.null(palette))
        ggplot2::aes(x = time, y = pop, color = factor(id))
    } +
    {
      if(isTRUE(multiple) & isTRUE(compare))
        ggplot2::geom_line(
          data = dat[, 2:4],
          mapping = aes(group = grp),
          color = "grey", alpha = (1 / (n / 8)), linewidth = .6
        )
    } +
    {
      if(isFALSE(multiple))
        ggplot2::geom_line(linewidth = .9, color = color)
    } +
    {
      if(isTRUE(multiple))
        ggplot2::geom_line(aes(group = grp), linewidth = .9)
    } +
    {
      if(isTRUE(multiple) & isTRUE(facet))
        ggplot2::facet_wrap(vars(id))
    } +
    ggplot2::scale_y_continuous(
      #limits = c(ylim, NA),
      expand = ggplot2::expansion(mult = c(.05, .03))
    ) +
    {
      if(isTRUE(multiple) & !is.null(palette))
        ggplot2::scale_color_manual(values = palette)
    } +
    {
      if(!is.null(palette) & isTRUE(facet))
        ggplot2::guides(color = ggplot2::guide_none())
    } +
    ggplot2::labs(x = "Time intervals", y = ylab, color = "Projection ID:") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  plot
}
