#' Plot test
#' @param ask logical; if TRUE, the user is asked before each plot, see [`par(ask=.)`][graphics::par].
#'
#' @export
#'
plot_test <- function(ask = interactive()) {
  if (ask) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
  }
  plot(1)
  grDevices::dev.flush()
  plot(1:10)
  grDevices::dev.flush()
  return(invisible(NULL))
}
