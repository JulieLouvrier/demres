#' Plot Population Projection Results
#'
#' Creates line plots for single or multiple population projections over time.
#' Supports various visualization options including faceting, comparison overlays,
#' sorting by final population size, and custom color palettes.
#'
#' @param popvec Numeric vector or list of numeric vectors containing population
#'   values over time. If a single vector, creates a simple time series plot.
#'   If a list, creates multiple trajectories, displayed as small multiples by
#'   default.
#' @param standard.A Logical. Set to \code{TRUE} if projection matrices contain
#'   standardised asymptotic dynamics (scaled by the dominant eigenvalue).
#' @param facet Logical. If \code{TRUE} (default for lists), creates separate
#'   panels for each trajectory. If \code{FALSE}, plots all trajectories on the
#'   same panel. Ignored for single trajectories.
#' @param compare Logical. If \code{TRUE} (default for lists), adds grey
#'   background lines showing all populations in each facet for comparison.
#'   Only applies when \code{facet = TRUE}. Ignored for single trajectories.
#' @param sort Logical. If \code{TRUE}, orders trajectories by their final
#'   (most recent) population value in descending order. Changes legend title
#'   to "Time step (ranked)". Only applies when \code{popvec} is a list.
#'   Ignored for single trajectories.
#' @param palette Character vector of colors to use for the plot. Should contain
#'   at least as many colors as trajectories. For multiple trajectories, colors
#'   are applied in order (affected by \code{sort} option) in case of an unnamed
#'   vector. To set a single line color, pass only one color.
#' @param ... Other arguments passed on to \code{ggplot2::geom_line()}'s params
#'   argument. Might throw a warning about duplicated aesthetics which can be
#'   ignored.
#'
#' @return A ggplot2 object containing the population projection plot(s).
#'
#' @details
#' The function automatically detects whether \code{popvec} contains single or
#' multiple population projections. For multiple projections:
#' \itemize{
#'   \item Time steps are calculated as 0 to (total_length - 1) / n_populations
#'   \item Each population gets a unique ID and group identifier
#'   \item Various visualization options become available
#' }
#'
#' When arguments are not applicable (e.g., \code{facet = TRUE} for single
#' populations), informative messages are displayed and the arguments are ignored.
#'
#' @examples
#' # Single trajectory
#' single_pop <- c(100, 105, 120, 160, 200, 270)
#' plot_proj(single_pop)
#' plot_proj(single_pop, palette = "blue") +
#'   coord_cartesian(ylim = c(0, 300))
#'
#' # Multiple trajectories
#' multi_pop <- list(
#'   `1` = c(100, 110, 130, 160),
#'   `2` = c(100, 90, 80, 75),
#'   `3` = c(100, 105, 120, 130),
#'   `4` = c(100, 95, 90, 80),
#'   `5` = c(100, 105, 115, 150),
#'   `6` = c(100, 105, 110, 125)
#' )
#' plot_proj(multi_pop)
#'
#' # rank trajectories and remove shaded lines
#' plot_proj(multi_pop, sort = TRUE, compare = TRUE)
#' # apply custom color
#' plot_proj(multi_pop, palette = "red")
#' # plot all trajectories in a single panel
#' plot_proj(multi_pop, facet = FALSE)
#' # use additional parameters from geom_line()
#' plot_proj(multi_pop, linewidth = 1.5, linetype = "31")
#' plot_proj(multi_pop, facet = FALSE, palette = "blue", alpha = .3)
#'
#' @export

plot_proj <- function(
    popvec = NULL,
    standard.A = FALSE,
    facet = NULL,
    compare = NULL,
    sort = FALSE,
    palette = NULL,
    ...
  ) {

  if (isFALSE(standard.A)) {
    ylab <- "Population"
  } else {
    ylab <- "Population size / density"
  }

  legend_title <- "Time step"

  multiple <- class(popvec) == "list"

  if (isTRUE(multiple)) {
    n <- length(popvec)
    pops <- unlist(popvec)
    time <- 0:((length(pops)-1) / n)

    if (is.null(facet)) facet <- TRUE
    if (is.null(compare)) compare <- TRUE

    dat <- data.frame(
      id = rep(1:n, each = length(time)),
      pop = pops,
      time = time,
      grp = rep(1:n, each = length(time))
    )
  } else {
    dat <- data.frame(
      pop = popvec,
      time = 0:(length(popvec)-1),
      grp = 1
    )
  }

  # display message that arguments are ignored
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

  # rank years by most recent population value
  if (isTRUE(sort) & isTRUE(multiple)) {
    dat_last <- dat[dat$time == max(dat$time), ]
    order <- dat_last$id[order(dat_last$pop, decreasing = TRUE)]

    dat$id <- factor(dat$id, levels = order)

    legend_title <- "Time step (ranked)"
  }

  # color handling for single projections or unique line color
  if (isFALSE(multiple)) {
    if (!is.null(palette)) { color <- palette[1] } else { color <- "black" }
  } else {
    if (length(palette) == 1) { color <- palette } else { color <- NULL }
  }


  # if (isFALSE(multiple) & !is.null(palette)) {
  #   color <- palette[1]
  # } else if (isTRUE(multiple) & length(palette) == 1) {
  #   color <- palette
  # } else {
  #   color <- "black"
  # }

  # visualization
  plot <- ggplot2::ggplot(data = dat) +
    ggplot2::aes(x = time, y = pop) +
    # add color encoding if required or specified
    {
      if((isTRUE(multiple) & isFALSE(facet)) | length(palette) > 1)
        ggplot2::aes(x = time, y = pop, color = factor(id))
    } +
    # add shaded lines to facets for comparison
    {
      if(isTRUE(multiple) & isTRUE(facet) & isTRUE(compare))
        ggplot2::geom_line(
          data = dat[, 2:4],
          mapping = aes(group = grp),
          color = "grey",
          ...,
          alpha = (1 / (n / 8)),
          linewidth = .5
        )
    } +
    # draw line(s)
    {
      if(is.null(color))
        ggplot2::geom_line(aes(group = grp), ..., linewidth = 1)
    } +
    {
      if(!is.null(color))
        ggplot2::geom_line(aes(group = grp), ..., color = color, linewidth = 1)
    } +
    # create small multiples
    {
      if(isTRUE(multiple) & isTRUE(facet))
        ggplot2::facet_wrap(vars(id))
    } +
    # add padding to top and bottom
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(.05, .03))
    ) +
    {
      if(isTRUE(multiple) & !is.null(palette))
        ggplot2::scale_color_manual(values = palette)
    } +
    {
      if(isTRUE(multiple) & is.null(palette))
        ggplot2::scale_color_viridis_d(option = "mako", begin = .1, end = .8)
    } +
    {
      if(!is.null(palette) & isTRUE(facet))
        ggplot2::guides(color = ggplot2::guide_none())
    } +
    ggplot2::labs(x = "Time intervals", y = ylab, color = legend_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  plot
}
