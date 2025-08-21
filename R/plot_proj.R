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
#' @param refline Specification for the undisturbed population. Defaults to
#'   `NULL` (no reference line). Provide a vector of population abundance over
#'   time (length must match the popvec) to draw a reference line showing
#'   the undisturbed population projection.
#' @param baseline Baseline line specification. Defaults to `NULL` (no baseline).
#'   - `NULL`: no baseline is drawn (default).
#'   - `TRUE`: draws a baseline with default styling.
#'   - A character string: allows custom styling. Can include:
#'       * **color** — a single word (e.g. `"red"`) or a hex code (e.g. `"#FF0000"`).
#'       * **linetype** — one of `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, `"twodash"`.
#'       * **linewidth** — a numeric value.
#'   Elements can appear in any order. Missing elements fall back to defaults (black, dashed, 0.8).
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
#'   argument. Will throw warnings about duplicated aesthetics when using
#'   \code{linewidth} argument which can be ignored.
#'
#' @return A ggplot2 object containing the population projection plot(s).
#'
#' @details
#' The function automatically detects whether \code{popvec} contains single or
#' multiple projections. For multiple trajectories, various additional
#' visualization options become available.
#' When arguments are not applicable for single trajectories (applies to
#' \code{facet}, \code{compare}, and \code{sort}), the arguments are ignored.
#'
#' @examples
#' # Single trajectory
#' single_pop <- c(100, 105, 120, 160, 200, 270)
#' plot_proj(single_pop)
#' plot_proj(single_pop, palette = "blue") + coord_cartesian(ylim = c(0, 300))
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
#'
#' # plot baseline
#' plot_proj(multi_pop, baseline = TRUE)
#' plot_proj(multi_pop, baseline = "red solid 2")
#'
#' # apply custom color
#' plot_proj(multi_pop, palette = "red")
#'
#' # plot all trajectories in a single panel
#' plot_proj(multi_pop, facet = FALSE)
#'
#' # use additional parameters from geom_line()
#' plot_proj(multi_pop, linewidth = 1.5, linetype = "31")
#' plot_proj(multi_pop, facet = FALSE, palette = "blue", alpha = .3)
#'
#' @export

plot_proj <- function(
    popvec = NULL,
    standard.A = FALSE,
    facet = NULL,
    baseline = NULL,
    refline = NULL,
    unpopvec = NULL,
    compare = NULL,
    sort = FALSE,
    palette = NULL,
    ...
  ) {

  # check inputs
  stopifnot('"popvec must be of class "Projection"'= is.null(popvec) == FALSE)
  multiple <- class(popvec) == "list"
  if (isFALSE(multiple)) vc <- class(popvec) else vc <- class(popvec[[1]])
  stopifnot('popvec must be a vector of an object returned from popdemo::project() or a list of the same.'= "Projection" %in% vc)
  stopifnot('standard.A must be either TRUE or FALSE.'= is.logical(standard.A))
  stopifnot('facet must be either NULL, TRUE or FALSE.'= is.logical(facet) | is.null(facet))
  stopifnot('baseline must be either NULL, boolean or a string specifying the styling.'= is.logical(baseline) | is.character(baseline) | is.null(baseline))
  stopifnot('compare must be either NULL, TRUE or FALSE.'= is.logical(compare) | is.null(compare))
  stopifnot('sort must be either TRUE or FALSE.'= is.logical(sort))

  # display message that arguments are ignored
  drop <- c()

  if (isFALSE(multiple)) {
    if (isTRUE(facet))   { drop <- c(drop, "facet") }
    if (isTRUE(compare)) { drop <- c(drop, "compare") }
    if (isTRUE(sort))    { drop <- c(drop, "sort") }

    if (length(drop) == 1) {
      message(
        paste0("`", drop[1], " = TRUE` is ignored as popvec contains a single trajectory.")
      )
    }
    if (length(drop) > 1) {
      message(
        paste0(
          paste(head(paste0("`", drop, " = TRUE`"), -1), collapse = ", "),
          " and ",
          tail(paste0("`", drop, " = TRUE`"), 1),
          " are ignored as popvec contains a single trajectory."
        )
      )
    }
  }

  # set titles for y-axis and legend
  if (isFALSE(standard.A)) {
    ylab <- "Population size"
  } else {
    ylab <- "Population size" ## Vik: we should have also here an argument standard.vec (as for the demres_plot function) and if
    ## it is TRUE we should display "Population density" as Y axis title
  }

  legend_title <- "Time step"

  # define single / multi projection settings
  multiple <- class(popvec) == "list"

  if (isTRUE(multiple)) {
    n <- length(popvec)
    pops <- unlist(popvec)
    time <- 0:((length(pops)-1) / n)

    if (is.null(names(popvec))) {
      grps <- factor(1:n)
    } else {
      grps <- names(popvec)
    }

    if (is.null(facet)) facet <- TRUE
    if (is.null(compare)) compare <- TRUE

    dat <- data.frame(
      id = rep(grps, each = length(time)),
      pop = pops,
      time = time,
      grp = rep(factor(1:n), each = length(time))
    )
  } else {
    dat <- data.frame(
      pop = popvec,
      time = 0:(length(popvec)-1),
      grp = 1
    )
  }

  # rank years by most recent population value
  if (isTRUE(sort) & isTRUE(multiple)) {
    dat_last <- dat[dat$time == max(dat$time), ]
    order <- dat_last$id[order(dat_last$pop, decreasing = TRUE)]
    dat$id <- factor(dat$id, levels = order)
    legend_title <- "Time step (ranked)"
  }

  # color handling for single trajectories or unique line color
  if (isFALSE(multiple)) {
    if (!is.null(palette)) { color <- palette[1] } else { color <- "black" }
  } else {
    if (length(palette) == 1) { color <- palette } else { color <- NULL }
  }

  # baseline settings
  # -> default baseline styling (if specified as TRUE or string)
  bl <- !is.null(baseline) & !isFALSE(baseline)
  if (bl) {
    bl <- list(
      y = min(dat$pop[which(dat$time == 0)]),
      color = "firebrick",
      type = "dashed",
      width = 0.8
    )
    # -> custom baselne settings (if specified as string)
    if (is.character(baseline)) {
      parts <- strsplit(trimws(baseline), "\\s+")[[1]]
      parts <- tolower(trimws(parts))

      # allowed linetypes
      allowed_linetypes <- c("solid", "dashed", "dotted",
                             "dotdash", "longdash", "twodash")

      # helper for hex colors
      is_hex_color <- function(x) grepl("^#([A-Fa-f0-9]{3}|[A-Fa-f0-9]{6})$", x)

      # -> numeric linewidth (take *first* valid one only)
      num_val <- suppressWarnings(as.numeric(parts))
      if (any(!is.na(num_val))) {
        idx <- which(!is.na(num_val))[1]
        bl$width <- num_val[idx]
        parts <- parts[-idx]
      }

      # -> linetype if allowed
      lt_idx <- which(parts %in% allowed_linetypes)
      if (length(lt_idx) > 0) {
        bl$type <- parts[lt_idx[1]]
        parts <- parts[-lt_idx[1]]
      }

      is_hex_color <- function(x) grepl("^#([A-Fa-f0-9]{3}|[A-Fa-f0-9]{6})$", x)
      valid_colors <- tolower(colors())
      is_named_color <- function(x) x %in% valid_colors

      # -> color: first valid hex or named color
      if (length(parts) > 0) {
        col_idx <- which(sapply(parts, function(x) is_hex_color(x) || is_named_color(x)))
        if (length(col_idx) > 0) {
          bl$color <- parts[col_idx[1]]
        }
      }
    }
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
        ggplot2::aes(x = time, y = pop, color = id)
    } +
    # add shaded lines to facets for comparison
    {
      if(isTRUE(multiple) & isTRUE(facet) & isTRUE(compare))
        ggplot2::geom_line(
          data = dat[, 2:4],
          mapping = ggplot2::aes(group = grp),
          color = "grey",
          ...,
          alpha = (1 / (n / 8)),
          linewidth = .5
        )
    } +
    # draw baseline
    {
      if(!isFALSE(bl))
        ggplot2::geom_hline(
          yintercept = bl$y,
          color = bl$color,
          linetype = bl$type,
          linewidth = bl$width
        )
    } +
    # draw trajectory / trajectories
    {
      if(is.null(color))
        ggplot2::geom_line(
          ggplot2::aes(group = grp),
          ...,
          linewidth = 1
        )
    } +
    {
      if(!is.null(color))
        ggplot2::geom_line(
          ggplot2::aes(group = grp),
          ...,
          color = color,
          linewidth = 1
        )
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
    # use custom color palette
    {
      if(isTRUE(multiple) & !is.null(palette))
        ggplot2::scale_color_manual(values = palette)
    } +
    {
      if(isTRUE(multiple) & is.null(palette))
        ggplot2::scale_color_viridis_d(option = "mako", begin = .1, end = .8)
    } +
    # style visualization
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
