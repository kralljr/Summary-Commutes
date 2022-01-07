# adapted from https://rpubs.com/mariner610/windrose

plot.windrose <- function(data,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){


  # only need direction (col data)

  # Tidy up input data ----
  n.in <- NROW(data)


  # clean up the data
  data. <- na.omit(data)

  # figure out the wind direction bins
  # dirres is input?
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[["dir"]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned


  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          #panel.border = element_rect(colour = "blank"),
          panel.grid.major = element_line(colour="grey65"))

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }


  # return the handle to the wind rose
  return(p.windrose)
}
