
lt_sst <- ecodata24::long_term_sst %>% 
  dplyr::mutate(hline = mean(Value, na.rm = TRUE))

hline <- mean(lt_sst$Value)

lt_sst %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata24::geom_gls() +
  #ecodata24::geom_lm(aes(x = Time, y = Value, group = Var))+
  #ecodata24::geom_regime()+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_hline(aes(yintercept = hline),
             size = hline.size,
             alpha = hline.alpha,
           linetype = hline.lty)+
  ggplot2::ylab("Temperature (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Long-term SST") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1840,2020,10))+
  ecodata24::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata24::theme_title()
