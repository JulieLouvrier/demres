#' Plot test
#'
#' @export
#'
plot_test <- function(ask = interactive()) {
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  plot(1)
  dev.flush()
  plot(1:10)
  dev.flush()
}
