# Data source
# Bormann P., Aursand P., Dilib F., Dischington P., Manral S. 2020. FORCE Machine Learning Competition. https://github.com/bolgebrygg/Force-2020-Machine-Learning-competition, doi: 10.5281/zenodo.4351156



# LOAD MAGRITTR -----------------------------------------------------------

library(magrittr)



# PULL FONT FROM GOOGLE FONTS ---------------------------------------------

sysfonts::font_add_google(name = "Ubuntu", family = "ubuntu")
showtext::showtext_auto()



# PULL DATA FROM FORCE 2020 GITHUB REPO AND SAVE --------------------------

download.file(url = "https://github.com/bolgebrygg/Force-2020-Machine-Learning-competition/raw/master/lithology_competition/data/train.zip",
              destfile = "data_wells.zip")



# READ IN CSV FROM ZIP ----------------------------------------------------

data_wells <-
  vroom::vroom(file = "data_wells.zip",
               delim = ";",
               guess_max = 1000)



# READ IN LITHOLOGY CODES FROM CSV ----------------------------------------

metadata_lithology <-
  vroom::vroom(file = "lith_codes.csv",
               delim = ",")



# JOIN IN LITHOLOGY NAMES -------------------------------------------------

data_wells <-
  data_wells %>%
  dplyr::left_join(metadata_lithology,
                   by = c("FORCE_2020_LITHOFACIES_LITHOLOGY" = "lith_code"))



# REMOVE ROWS WHERE NA ----------------------------------------------------

data_wells <-
  data_wells %>%
  dplyr::filter(!is.na(NPHI) & !is.na(RHOB))



# CREATE FACTOR TO CONTROL PLOT FACET ORDER -------------------------------

data_wells <-
  data_wells %>%
  dplyr::mutate(lith_name = forcats::as_factor(lith_name)) %>%
  dplyr::mutate(lith_name = forcats::fct_reorder(lith_name, .x = order))



# CALCULATE PLOT DIMENSIONS FOR MAKING HEXBINS 1:1 ASPECT -----------------

plot_temp <-
  data_wells %>%
  ggplot2::ggplot() +
  ggplot2::geom_hex(data = data_wells %>%
                      dplyr::select(NPHI, RHOB),
                    mapping = ggplot2::aes(x = NPHI,
                                           y = RHOB))

xrange <- diff(ggplot2::ggplot_build(plot_temp)$layout$panel_params[[1]]$x.range)
yrange <- diff(ggplot2::ggplot_build(plot_temp)$layout$panel_params[[1]]$y.range)
aspect_ratio <- xrange/yrange
xbins <- 20
xwidth <- xrange/xbins
ywidth <- xwidth/aspect_ratio



# MAKE PLOT ---------------------------------------------------------------

plot_final <-
  data_wells %>%
  ggplot2::ggplot() +
  ggplot2::geom_hex(data = data_wells %>% dplyr::select(NPHI, RHOB),
                    mapping = ggplot2::aes(x = NPHI,
                                           y = RHOB),
                    binwidth = c(xwidth, ywidth),
                    fill = "#E5E5E5",
                    colour = "#E5E5E5",
                    size = 0.1) +
  ggplot2::geom_hex(mapping = ggplot2::aes(x = NPHI,
                                           y = RHOB),
                    binwidth = c(xwidth, ywidth),
                    colour = "#4F4159",
                    size = 0.1) +

  ggplot2::scale_y_reverse(labels = scales::number_format(accuracy = 0.01)) +
  
  ggplot2::scale_fill_viridis_c(option = "rocket",
                                name = "Count (log transformed)",
                                breaks = c(70, 28000),
                                labels = c("70" = "Low", "28000" = "High"),
                                alpha = 0.8,
                                begin = 0.1,
                                end = 0.9,
                                trans = scales::pseudo_log_trans(sigma = 80),
                                direction = 1,
                                guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                                barheight = ggplot2::unit(2, units = "mm"),
                                                                barwidth = ggplot2::unit(50, units = "mm"),
                                                                draw.ulim = FALSE,
                                                                nrow = 1,
                                                                title.position = "top",
                                                                title.hjust = 0.5,
                                                                label.hjust = 0.5,
                                                                ticks.colour = NA)) +
  
  ggplot2::coord_fixed(ratio = aspect_ratio) +
  
  ggplot2::facet_wrap(facets = "lith_name",
                      ncol = 3,
                      strip.position = "top") +
  ggplot2::labs(title = "Lithology distributions in neutron-density log space",
                subtitle = paste0("98 wells from the Norwegian North Sea, ", format(nrow(data_wells), big.mark = ","), " observations from the Rotliegend to the Nordland"),
                x = "NPHI (frac.)",
                y = bquote("RHOB (g c"*m^-3*")"),
                caption = "Data: FORCE Machine Learning Competition training set (2020, doi: 10.5281/zenodo.4351156), Map author: Sam Fielding, Map licence: CC BY-SA 4.0") +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "ubuntu",
                                              color = "#4e4d47",
                                              size = 20 * 4.5),
                 
                 plot.margin = ggplot2::margin(t = 3, r = 13, b = 2, l = 5, # 3
                                               unit = "mm"),
                 plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 plot.title = ggplot2::element_text(hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                       margin = ggplot2::margin(t = 0, r = 0, b = 2, l = 0, # 4
                                                                                unit = "mm"),
                                                       size = 15 * 4.5),
                 plot.title.position = "plot",
                 
                 plot.caption = ggplot2::element_text(hjust = 0.5,
                                                      margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0,
                                                                               unit = "mm"),
                                                      colour = "#939184",
                                                      size = 10 * 4.5),
                 plot.caption.position = "plot",
                 
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0,
                                                                               unit = "mm")),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0,
                                                                               unit = "mm")),
                 
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2, r = 0, b = 0, l = 0,
                                                                              unit = "mm")),
                 axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 2, b = 0, l = 0,
                                                                              unit = "mm")),
                 
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 
                 strip.text.x = ggplot2::element_text(hjust = 1,
                                                      color = "#4e4d47",
                                                      size = 12 * 4.5),
                 strip.background = ggplot2::element_rect(fill = NA,
                                                          colour = NA),
                 
                 legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 1,
                                                                               b = -7,
                                                                               unit = "mm"),
                                                      size = 12 * 4.5),
                 legend.position = "top",
                 legend.justification = "centre",
                 legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.key = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -1,
                                                                              r = 1,
                                                                              t = -7,
                                                                              b = -4,
                                                                              unit = "mm"),
                                                     size = 12 * 4.5))



# SAVE PLOT TO PNG --------------------------------------------------------

ggplot2::ggsave(filename = "neutron-density_by_lithology.png",
                plot = plot_final,
                width = 235,
                height = 342,
                units = "mm",
                dpi = 600)


