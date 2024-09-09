#' plot thermal habitat persistence
#'
#' plots thermal_habitat_persistence data set. Current SOE Year only
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#' @export

#'

plot_thermal_habitat_persistence <- function(shadedRegion = NULL,
                                             report="MidAtlantic",
                                             year = NULL) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata24::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB","GOM")
  }

  if (is.null(year)) {
    # current SOE report year
    Yr <- setup$shadedRegion[2]
  } else {
    Yr <- year
  }
  # optional code to wrangle ecodata24 object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata24::thermal_habitat_persistence |>
    dplyr::mutate(Var = paste0(Var,"\u00B0C")) |>
    dplyr::filter(Time == Yr)

  # neus_map  <- ggplot2::map_data('worldHires',region = 'USA')
  legendTitle <- unique(fix$Units)

  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_tile(ggplot2::aes(x=Longitude,y = Latitude, color = Value, width = 1/12, height = 1/12),
                       linewidth = setup$line.size) +
    ggplot2::geom_sf(data=ecodata24::coast, size = setup$map.lwd) +
    ggplot2::facet_wrap(~Var )+
    ggplot2::scale_color_viridis_c(legendTitle)+
    ggplot2::coord_sf(xlim = c(setup$xmin,setup$xmax), ylim = c(setup$ymin,setup$ymax)) +
    #ggplot2::annotation_map(neus.map,fill = "grey70")+

    ggplot2::xlab('')+
    ggplot2::ylab('')+
    ecodata24::theme_ts()+
    ecodata24::theme_facet()+
    ecodata24::theme_title() +
    ggplot2::theme(legend.position = "bottom")



    return(p)

}
attr(plot_thermal_habitat_persistence,"report") <- c("MidAtlantic","NewEngland")
