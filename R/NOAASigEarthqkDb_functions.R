
#' eq_location_clean
#'
#' This function cleans the LOCATION_NAME column in the raw NOAA data frame.
#'
#' @param filename The name of the file that the function will read into R.
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_to_title
#' @importFrom readr write_csv
#'
#' @return This function strips out the country names (including the colon) from the `Location Name' variable
#'    and converts the resulting string(s) to title case (as opposed to all caps).
#'
#' @note Example: eq_location_clean("./inst/extdata/earthquakes_-2150to2024.csv")
#'
#' @note If the file  does not exist (e.g. earthquakes_-2150to2050.csv), attempting to read this file
#'     will generate an error.
#' try(eq_location_clean("./inst/extdata/earthquakes_-2150to2050.csv"))
#'
#' @note Attempting to read a non-CSV file will also generate an error.
#' try(eq_location_clean("./inst/extdata/earthquakes_-2150to2024.xlsx"))
#'
#' @export
eq_location_clean = function(filename){
  if (!dir.exists("./inst/extdata")){
    TAF::mkdir("./inst/extdata")
  }
  data = readr::read_csv(filename, col_names = T, progress = FALSE)
  data$Country = gsub(":.*$", "", data$`Location Name` )
  data$`Location Name` = gsub(".*:\\s+", "", data$`Location Name`)
  data$`Location Name` = stringr::str_to_title(data$`Location Name`)
  readr::write_csv(data, "./inst/extdata/earthquakes_-2150to2024_clean1.csv")
}


#####################


#' eq_clean_data
#'
#' This function further cleans the NOAA data frame produced by the eq_location_clean function.
#'
#' @param filename The name of the file that the function will read into R.
#'
#' @importFrom readr read_csv
#' @importFrom tidyr unite
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#'
#' @return This function removes events in the BCE era (49 out of the 6416 events) and returns a cleaned NOAA data
#'    frame, with the following additional changes:
#'    1. A Date column created by uniting the year, month, and day variables, and converting it to the Date class
#'    2. Latitude and Longitude variables converted to numeric class
#'
#' @note Example: eq_clean_data("./inst/extdata/earthquakes_-2150to2024_clean1.csv")
#'
#' @note If the file  does not exist (e.g. earthquakes_-2150to2050.csv), attempting to read this file
#'     will generate an error.
#' try(eq_clean_data("./inst/extdata/earthquakes_-2150to2050_clean1.csv"))
#'
#' @note Attempting to read a non-CSV file will also generate an error.
#' try(eq_clean_data("./inst/extdata/earthquakes_-2150to2024_clean1.xlsx"))
#'
#' @export
eq_clean_data = function(filename){
  Date = Year = Mo = Dy = Longitude = Latitude = NULL
  data = readr::read_csv(filename, col_names = T, progress = FALSE)
  data = data[50:6416,]
  data$Year = sprintf("%04d", data$Year)
  data = tidyr::unite(data, col = "Date", Year, Mo, Dy, sep = "-")
  data$Date = lubridate::parse_date_time(data$Date, "Ymd", truncated = 5)
  data$Date = format(as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
  data = dplyr::mutate(data, Longitude = as.numeric(Longitude))
  data = dplyr::mutate(data, Latitude = as.numeric(Latitude))
  readr::write_csv(data, "./inst/extdata/earthquakes_-2150to2024_clean2.csv")
}


#####################


#' eq_filter_data
#'
#' This function filters the NOAA data frame produced by the eq_clean_data function.
#'
#' @param filename The name of the file that the function will read into R.
#' @param xmin Minimum date for filtering
#' @param xmax Maximum date for filtering
#' @param columns Columns of interest to select
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @importFrom dplyr between
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom stats complete.cases
#' @importFrom readr write_csv
#'
#' @return This function returns the final NOAA data frame for analysis, with the following additional changes:
#'    1. Filtering a subset of NOAA data between a minimum and maximum date
#'    2. Additional of columns of interest
#'    3. Selection of all columns of interest
#'    4. Filtering of NOAA data for complete cases
#'
#' @note Example: eq_filter_data("./inst/extdata/earthquakes_-2150to2024_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
#'  xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country"))
#'
#' @note If the file  does not exist (e.g. earthquakes_-2150to2050.csv), attempting to read this file
#'     will generate an error.
#' try(eq_filter_data("./inst/extdata/earthquakes_-2150to2050_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
#'  xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country")))
#'
#' @note Attempting to read a non-CSV file will also generate an error.
#' try(eq_filter_data("./inst/extdata/earthquakes_-2150to2024_clean2.xlsx", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
#'  xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country")))
#'
#' @export
eq_filter_data = function(filename, xmin, xmax, columns){
  Date = NULL
  data = readr::read_csv(filename, col_names = T, progress = FALSE)
  data = dplyr::filter(data, dplyr::between(Date, xmin, xmax))
  data = cbind(data, xend=data$Date, y=rep(0,length(data$Date)))
  data = cbind(data, yend=rep(0.4,length(data$Date)))
  data = dplyr::select(data, tidyselect::all_of(columns))
  data = data[complete.cases(data), ]
  readr::write_csv(data, "./inst/extdata/earthquakes_-2150to2024_final.csv")
}


#####################


#' GeomTimeline class
#'
#' A new class corresponding to the geom_timeline geom.
#'
#' @param data The input database.
#' @param panel_params set of per-panel parameters for the coord.
#' @param coord Object that describes coordinate system for plot.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom grid gList
#'
#' @return The ggproto function returns an object of class GeomTimeline for input as the default geom for
#' the user facing geom_timeline() function.
#'
#' @export
GeomTimeline = ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                required_aes = c("x"),
                                default_aes = ggplot2::aes(y=NA, colour = NA, size = NA, alpha = NA, linewidth = .5,
                                                           linetype = 1, shape = 16, fill = NA, stroke = 1),
                                draw_panel = function(data, panel_params, coord, ...) {
                                  # Transform data for point2
                                  point = transform(data)
                                  # Return all components
                                  grid::gList(
                                    GeomPoint$draw_panel(point, panel_params, coord, ...)
                                  )
                                  }

                                )



#' geom_timeline
#'
#' This function uses GeomTimeline as the default geom class and plots a time line of selected earthquakes.
#'
#' @param data The input database
#' @param mapping The mapping of variables
#' @param geom The geom to use
#' @param stat The stat to use
#' @param position The position to use
#' @param show.legend Whether to show the legend
#' @param inherit.aes Whether to inherit aesthetics
#' @param params Specified parameters
#'
#' @importFrom ggplot2 layer
#'
#' @return This function returns a layer, that consists of the time line of selected earthquakes ranging from
#'    min to max dates (with a point for each earthquake), using GeomTimeline as default geom class.
#'
#' @note Example: sigEarthqks_final = readr::read_csv("./inst/extdata/earthquakes_-2150to2024_final.csv")
#'                library(ggplot2)
#'                ggplot() + geom_timeline(data=sigEarthqks_final, aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3)
#'
#' @note If the file  does not exist (e.g. sigEarthqks), attempting to read this file will generate an error.
#' try(ggplot() + geom_timeline(data=sigEarthqks, aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3))
#'
#' @export
geom_timeline = function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = NA,inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, geom = GeomTimeline, stat = stat, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
}


#####################


#' GeomTimelineLabel class
#'
#' A new class corresponding to the geom_timeline geom.
#'
#' @param data The input database.
#' @param panel_params set of per-panel parameters for the coord.
#' @param coord Object that describes coordinate system for plot.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom grid gList
#'
#' @return The ggproto function returns an object of class GeomTimelineLabel for input as the default geom for
#' the user facing geom_timeline_label() function.
#'
#' @export
GeomTimelineLabel = ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                required_aes = c("x", "y", "xend", "yend"),
                                default_aes = ggplot2::aes(colour = NA, size = NA, alpha = NA, label = NA, linewidth = .5,
                                                           linetype = 1, shape = 16, fill = NA, stroke = 1),
                                draw_panel = function(data, panel_params, coord, ...) {
                                  # Transform data for point2
                                  point1 = transform(data)
                                  point2 = transform(data, x=xend, y=yend)
                                  # Return all components
                                  grid::gList(
                                    GeomPoint$draw_panel(point1, panel_params, coord, ...),
                                    GeomPoint$draw_panel(data, panel_params, coord, ...),
                                    GeomSegment$draw_panel(data, panel_params, coord, ...)
                                  )
                                }

)



#' geom_timeline_label
#'
#' This function uses GeomTimelineLabel as the default geom class and plots a time line of selected earthquakes.
#'
#' @param data The input database
#' @param mapping The mapping of variables
#' @param geom The geom to use
#' @param stat The stat to use
#' @param position The position to use
#' @param show.legend Whether to show the legend
#' @param inherit.aes Whether to inherit aesthetics
#' @param params Specified parameters
#'
#' @importFrom ggplot2 layer
#'
#' @return This function returns a layer, that consists of the time line of selected earthquakes ranging from
#'    min to max dates (with a point for each earthquake), and a vertical line to selected data points with a
#'    text annotation, using GeomTimeline as default geom class.
#'
#' @note Example: sigEarthqks_final = readr::read_csv("./inst/extdata/earthquakes_-2150to2024_final.csv")
#'                library(ggplot2)
#'                ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag,
#'                colour=Deaths,label=Country), alpha=0.3)
#'
#' @note If the file  does not exist (e.g. sigEarthqks), attempting to read this file will generate an error.
#' try(ggplot() + geom_timeline_label(data=sigEarthqks, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag,
#'    colour=Deaths,label=Country), alpha=0.3))
#'
#' @export
geom_timeline_label = function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", ..., na.rm = FALSE,
                         show.legend = NA,inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, geom = GeomTimelineLabel, stat = stat, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
}


#####################


#' eq_map
#'
#' This function uses NOAA data to create interactive maps of select NOAA earthquake epicenters.
#'
#' @param eq_data The name of the file that the function will read into R.
#' @param annot_col The column to use for the annotation in the pop-up.
#' @param x1 Western longitude limit for plot by 5 degree increments.
#' @param x2 Eastern longitude limit for plot by 5 degree increments.
#' @param y1 Southern latitude limit for plot by 5 degree increments.
#' @param y2 Northern latitude limit for plot by 5 degree increments.
#'
#' @importFrom ggplot2 map_data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 coord_map
#' @importFrom ggplot2 geom_text
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggiraph geom_point_interactive
#' @importFrom ggiraph girafe_options
#' @importFrom ggiraph girafe
#' @importFrom ggiraph opts_tooltip
#'
#' @return This function maps earthquake epicenters (LATITUDE/LONGITUDE) and annotates each point within a pop up window
#'    containing annotation data stored in a column of the data frame. The user can also choose which column is used for
#'    the annotation in the pop-up.
#'
#' @note Example: library(magrittr)
#'      dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
#'      eq_map(annot_col = "Date",0,180,30,90)
#'
#' @note If the selected annotation column  does not exist, attempting to use this column will
#'      generate an error.
#' try(eq_map(annot_col = "Column",0,180,30,90))
#'
#' @export
eq_map = function(eq_data, annot_col, x1, x2, y1, y2){
  eqData = eq_data
  eqData = as.data.frame(eqData)
  tooltip = eqData[[annot_col]]
  data_id = eqData[[annot_col]]
  mapData = ggplot2::map_data("world", region = ".")
  colnames(mapData)[1] = "Longitude"
  colnames(mapData)[2] = "Latitude"
  gg_map = ggplot2::ggplot(mapData, ggplot2::aes(x=Longitude, y=Latitude, group=group)) +
    ggplot2::geom_polygon(color="pink", fill="beige") +
    ggplot2::scale_x_continuous(limits = c(x1,x2), breaks=seq(x1,x2,5)) +
    ggplot2::scale_y_continuous(limits = c(y1,y2), breaks=seq(y1,y2,5)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "lightblue"),
          panel.background = ggplot2::element_rect(fill = "lightblue", colour = "lightblue", linewidth = 0.5, linetype = "solid"),
          panel.grid.major = ggplot2::element_line(linewidth = 0.5, linetype = 'solid', colour = "lightblue"),
          panel.grid.minor = ggplot2::element_line(linewidth = 0.25, linetype = 'solid', colour = "lightblue"),
          axis.title.x = ggplot2::element_text(colour = "lightblue"),
          axis.title.y = ggplot2::element_text(colour = "lightblue"),
          axis.text.x = ggplot2::element_text(colour="lightblue"),
          axis.text.y = ggplot2::element_text(colour="lightblue"),
          axis.ticks.x = ggplot2::element_line(colour = "lightblue"),
          axis.ticks.y = ggplot2::element_line(colour = "lightblue"),
          plot.title = ggplot2::element_text(colour = "black", hjust = 0.5)) +
    ggplot2::ggtitle("NOAA Earthquakes") +
    ggplot2::coord_map() +
    ggplot2::geom_text(data=mapData %>% dplyr::group_by(region) %>% dplyr::arrange(Longitude, Latitude) %>%
                         dplyr::filter(dplyr::row_number()==ceiling(dplyr::n()/2)),
                       ggplot2::aes(x=Longitude, y=Latitude, label=region, group=group), size=1,
                       color = "black", na.rm = T) +
    ggplot2::guides(size = ggplot2::guide_legend(order = 1), alpha="none") +
    ggiraph::geom_point_interactive(data = eqData,
                                    ggplot2::aes(x=Longitude, y=Latitude, group=NULL, size=Mag,
                                                 tooltip=tooltip,data_id=data_id), alpha=0.1, color="blue")
  ggiraph::girafe_options(ggiraph::girafe(ggobj = gg_map), ggiraph::opts_tooltip(opacity=0.7, use_fill = T, use_stroke = T))
}


#####################


#' eq_create_label
#'
#' This function creates an HTML label that can be used as annotation text in interactive maps of NOAA earthquake epicenters.
#'
#' @param data The input database.
#'
#' @importFrom magrittr %>%
#'
#' @return This function constructs a character string for each earthquake that includes location, magnitude, and total
#'    number of deaths (all boldface labels). If an earthquake is missing values for any of these, both label and value
#'    will be skipped for that element of tag.
#'
#' @note Example: library(magrittr)
#'      dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
#'      dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'      eq_create_label(annot_col = "popup_text")
#'
#' @note If the selected annotation columns do not exist, attempting to use these columns will
#'      generate an error.
#'  try(eq_create_label(annot_col = "Columns"))
#'
#' @export
eq_create_label = function(data){
  eqData = data
  eqData %>%
    {paste0(ifelse(is.na(.$`Location Name`)==F, {paste("<B>Location: </B>", .$`Location Name`)}, {""}), "\n",
            ifelse(is.na(.$Mag)==F, {paste("<B>Magnitude: </B>", .$Mag)}, {""}), "\n",
            ifelse(is.na(.$Deaths)==F, {paste("<B>Total Deaths: </B>", .$Deaths)}, {""}), "\n")}
}



#####################



###################################################################################################################
# Testing

## Data
eq_location_clean("./inst/extdata/earthquakes_-2150to2024.csv")
eq_clean_data("./inst/extdata/earthquakes_-2150to2024_clean1.csv")
eq_filter_data("./inst/extdata/earthquakes_-2150to2024_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
               xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country"))
sigEarthqks_clean1 = readr::read_csv("./inst/extdata/earthquakes_-2150to2024_clean1.csv")
sigEarthqks_clean2 = readr::read_csv("./inst/extdata/earthquakes_-2150to2024_clean2.csv")
sigEarthqks_final = readr::read_csv("./inst/extdata/earthquakes_-2150to2024_final.csv")
sigEarthqks_final[1:10,]


## Plots
### geom_timeline()
library(ggplot2)
library(grid)
ggplot() + geom_timeline(data=sigEarthqks_final, aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  ylim(c(0,1)) + geom_hline(aes(yintercept = 0.2)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

ggplot() + geom_timeline(data=sigEarthqks_final, aes(Date, Country, size=Mag, colour=Deaths), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  geom_hline(aes(yintercept = sigEarthqks_final$Country)) +
  annotate("text", x=sigEarthqks_final$Date, y=sigEarthqks_final$Country, label=sigEarthqks_final$Country, vjust=0, hjust=-0.2, size=2) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

### geom_timeline_label()
ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag, colour=Deaths,
                                      label=Country), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  ylim(c(0,1)) + geom_hline(aes(yintercept = 0.2)) +
  annotate("text", x=sigEarthqks_final$Date, y=0.42, label=sigEarthqks_final$Country, vjust=0,
           hjust=-0, size=2, angle=45) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"), axis.ticks.y = element_line(colour = "white"),
        axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag, colour=Deaths,
                                   label=Country), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  ylim(c(0,1)) + geom_hline(aes(yintercept = 0.2)) +
  geom_text(aes(sigEarthqks_final$Date, y=0.42, label=ifelse(sigEarthqks_final$Mag>=6,as.character(sigEarthqks_final$Country),'')), vjust=0, hjust=-0.0, size=2, angle=45) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, Country, xend=xend, yend=yend, size=Mag, colour=Deaths,
                                   label=Country), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  geom_hline(aes(yintercept = sigEarthqks_final$Country)) +
  annotate("text", x=sigEarthqks_final$Date, y=sigEarthqks_final$Country, label=sigEarthqks_final$Country, vjust=0, hjust=-0.2, size=2, angle=45) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, Country, xend=xend, yend=yend, size=Mag, colour=Deaths,
                                      label=Country), alpha=0.3) +
  labs(size = "Richter Scale", colour = ("Deaths")) +
  geom_hline(aes(yintercept = sigEarthqks_final$Country)) +
  geom_text(aes(sigEarthqks_final$Date, y=sigEarthqks_final$Country, label=ifelse(sigEarthqks_final$Mag>=6,as.character(sigEarthqks_final$Country),'')), vjust=0, hjust=-0.2, size=2, angle=45) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "white"),
        axis.title.y = element_text(colour = "white"), axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_line(colour = "white"), axis.line.x = element_line(colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)) +
  theme(legend.position="bottom") +
  guides(size = guide_legend(order = 1), colour = guide_colorbar(order = 2), alpha="none")

### eq_map()
library(magrittr)
dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col="Date", -120, -60, 0, 40)

dplyr::filter(sigEarthqks_clean2, Country == "INDIA" & lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col="Date", 30, 150, -30, 60)

### eq_create_label()
library(magrittr)
dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col="popup_text", -120, -60, 0, 40)

dplyr::filter(sigEarthqks_clean2, Country == "INDIA" & lubridate::year(Date) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col="popup_text", 30, 150, -30, 60)


##################################################################################################################

