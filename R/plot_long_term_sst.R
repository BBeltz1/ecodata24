#' plot long term sea surface temperature
#'
#' plots long_term_sst data set. This is a shelf wide product
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_long_term_sst <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata24::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata24 object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata24::long_term_sst |>
     dplyr::mutate(hline = mean(Value, na.rm = TRUE))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle("Long-term SST")+
    ggplot2::ylab(expression("Temperature (\u00B0C)"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata24::geom_gls()+
    ecodata24::theme_ts()+
    ecodata24::theme_title()



    return(p)

}

attr(plot_long_term_sst,"report") <- c("MidAtlantic","NewEngland")
