





#Lollipop chart!

lollipop <- function(data) {
  wessy_pal <- c("high"="#C93312","low"="#899DA4")
                          #in our shiny app format, we should be able to selct which sensor we want to see this for in a dropdown menu
  print(tidyverse::ggplot(data=data[data$longitude == sensors[7,1],], tidyverse::aes(x=day, y=PM2.5, group = day)) +
          tidyverse::geom_line(lwd=1) +
          tidyverse::geom_point(data=data[data$type == "high" & data$PM2.5<=300 & data$longitude == sensors[7,1], ],
                                tidyverse::aes(x=day, y=PM2.5, group = type, col="high"), size=5)+
          tidyverse::geom_point(data=data[data$type == "low" & data$longitude == sensors[7,1],],
                                tidyverse::aes(x=day, y=PM2.5, group = type, col="low"), size=5)+
          tidyverse::scale_color_brewer(palette="Dark2") +
          tidyverse::labs(x = "Day", y = "PM2.5 (µg/m³)") +
          tidyverse::scale_colour_manual(name="Type",values=wessy_pal, guide = guide_legend(override.aes=aes(fill=NA)) ) + scale_fill_manual(name="Type",values=wessy_pal) +
          tidyverse::ggtitle(sensors$names[7]) +
          tidyverse::theme_minimal())
}


