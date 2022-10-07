# Old plot code

# old
dpm <- ggplot(va24, aes(y = daily)) +
  geom_boxplot() +
  ylab((expression(paste("ambient daily PM"[2.5], " (", mu, "g/m"^3, ")")))) +
  #xlab("Date") +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



fighourly <- ggplot() +
  geom_boxplot(data = obsdiff, aes(x = factor(time_local), y = obsdiff)) +
  ylab(expression(paste("Change in PM"[2.5], " (", mu, "g/m"^3, ")"))) +#, #"\nrelative to daily average"))) +
  xlab("Hour of day") +
  theme_bw() +
  theme(text = element_text(size = 12))

dpm + fighourly + plot_layout(widths = c(1, 5))


#Trends in ambient hourly PM$_{2.5}$ (difference in $\mu$g/m$^3$, change relative to daily mean) by hour of the day and by season for each unique day of observation (N=38)**
fighourly <- ggplot() +
  geom_line(data = obsdiff, aes(x = time_local, y = obsdiff, group = date_local), linetype = 1, alpha = 0.3) +
  geom_smooth(data = obsdiff, aes(x = time_local, y = obsdiff), linetype = 2,
              se = T, colour = cols[2], fill = cols[2]) +
  # scale_color_manual(name = "Season", values = cols) +
  # scale_alpha_manual(name = "Season", values = alps) +
  # scale_color_gradient2(name = "Month", labels = lab1, breaks = c(-4,0, 4 ),
  #                       low = "orange", mid = "blue", high = "green")+
  ylab(expression(paste("Change in PM"[2.5], " (", mu, "g/m"^3, ")"))) +#, #"\nrelative to daily average"))) +
  xlab("Hour of day") +
  theme_bw() +
  theme(text = element_text(size = 12))
fighourly

