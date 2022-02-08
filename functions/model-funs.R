

get_lme <- function(data1, iqrs, eqn1, re = "~1 | ID / id3", intervals = T) {

  # get correlation
  cs1 <- corAR1(form = formula(re))
  cs1 <- Initialize(cs1, data = data1)

  # Fit model
  lme1 <- lme(formula(eqn1),
              random= formula(re),
              data=data1, correlation= cs1)

  # Get residuals
  lme1b <- augment(lme1)

  # format
  t1 <- tidy(lme1, conf.int = T) %>%
    filter(effect == "fixed", term != "(Intercept)") %>%
    full_join(iqrs) %>%
    mutate(IQR = ifelse(is.na(IQR), 1, IQR),
           estimateIQR = estimate * IQR,
           conf.lowIQR = conf.low * IQR,
           conf.highIQR = conf.high * IQR,
           term1 = factor(term, levels = c("daily", "obsdiff", "srness", "timemin",
                                           "rtypeLocalConn",  "rtypeLocal",    "rtypeOther",
                                           "tmax" ,"tmin",  "prcpbin", "snowbin" , "awnd", "cat5smSE", "cat5smOther"),
                          labels = c("Daily PM2.5", "Hourly PM2.5", "Roadiness", "Time since start",
                                     "Local conn", "Local", "Other",
                                     "Max Temp",  "Min Temp", "Precipitation","Snow",
                                     "Wind", "WindDir SE", "WindDir Other")),
           type = ifelse(term %in% c("daily", "obsdiff", "srness",   "rtypeLocalConn",  "rtypeLocal",    "rtypeOther", "timemin"),
                         "Pollution", "Meteorology"),
           type2 = ifelse(term %in% c("prcpbin", "snowbin",  "rtypeLocalConn",  "rtypeLocal",    "rtypeOther", "cat5smSE",
                                      "cat5smOther"), "cat", "num"))


  # get intervals for variance components
  if(intervals) {
    print(intervals)
    int1 <- intervals(lme1)
    reint <- int1$reStruct


    # bind together intervals
    re <- bind_rows(int1$sigma, reint[[1]])
    for(i in 2 : length(reint)) {
      re <- bind_rows(re, reint[[i]])
    }
    re <- mutate(re, name = c("sigma", names(reint))) %>%
      rename(est= `est.`)

  } else {
    sigs <- lme1$sigma

    re1 <- coef(lme1$modelStruct$reStruct, unconstrained = F) %>% sqrt()
    re1 <- re1 * sigs
    names(re1) <- strsplit(names(re1), "\\.") %>% sapply(., function(x) x[[1]])

    re1b <- c(sigs, re1)
    re <- data.frame(est = re1b, name = c("sigma", names(re1)))
    re <- mutate(re, est = as.numeric(est))
  }


  # return
  if("corStruct" %in% names(lme1$modelStruct)) {
    cor <- lme1$modelStruct$corStruct
  } else {
    cor <- NA
  }

  list(t1 = t1, re = re, cor = cor, lme1 = lme1, aug = lme1b)
}

plot_re <- function(re) {


  ggplot(re, aes(x = `est.`, y = name)) +
    geom_pointrange(aes(xmin = lower, xmax = upper )) +
    xlab("Estimate") + ylab("")
}



plot_cat <- function(res) {

  cat<- filter(res$t1, type2 == "cat") %>%
    mutate(type2 = ifelse(term %in% c("cat5smSE", "cat5smOther"), "Wind direction",
                          ifelse(term %in% c("rtypeOther", "rtypeLocal", "rtypeLocalConn"), "Road type",
                                 as.character(term1))),
           term2 = ifelse(term %in% c("cat5smSE","rtypeLocal" ), 2,
                          ifelse(term %in% c("cat5smOther", "rtypeLocalConn"), 3,
                                 ifelse(term == "rtypeOther", 4, 1))), term2 = factor(term2),
           type2 = factor(type2, levels = c("Precipitation", "Snow", "Wind direction", "Road type")))

  cols <- brewer.pal(8, "Dark2")
  g1 <- ggplot(cat, aes(y = term1, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, colour = term2),
                    position = position_dodge(0.2)) +
    scale_colour_manual(values = cols,  guide = "none") +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ") compared to reference"))) +
    theme_bw() +
    theme(text = element_text(size = 12)) + facet_wrap(~type2, scales = "free_y", ncol = 1)

  g1


}


plot_nocat <- function(res) {

  g2 <- ggplot(filter(res$t1, type2 != "cat"), aes(y = term1, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 24))

  g2

}





plot_cat <- function(res) {

  cat<- filter(res$t1, type2 == "cat") %>%
    mutate(type2 = ifelse(term %in% c("cat5smSE", "cat5smOther"), "Wind direction",
                          ifelse(term %in% c("rtypeOther", "rtypeLocal", "rtypeLocalConn"), "Road type",
                                 as.character(term1))),
           term2 = ifelse(term %in% c("cat5smSE","rtypeLocal" ), 2,
                          ifelse(term %in% c("cat5smOther", "rtypeLocalConn"), 3,
                                 ifelse(term == "rtypeOther", 4, 1))), term2 = factor(term2),
           type2 = factor(type2, levels = c("Precipitation", "Snow", "Wind direction", "Road type")))

  cols <- brewer.pal(8, "Dark2")
  g1 <- ggplot(cat, aes(y = term1, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, colour = term2),
                    position = position_dodge(0.2)) +
    scale_colour_manual(values = cols,  guide = "none") +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ") compared to reference"))) +
    theme_bw() +
    theme(text = element_text(size = 12)) + facet_wrap(~type2, scales = "free_y", ncol = 1)

  g1


}


plot_catALL <- function(t1) {

  cat<- filter(t1, type2 == "cat") %>%
    mutate(type2 = ifelse(term %in% c("cat5smSE", "cat5smOther"), "Wind direction",
                          ifelse(term %in% c("rtypeOther", "rtypeLocal", "rtypeLocalConn"), "Road type",
                                 as.character(term1))),
           term2 = ifelse(term %in% c("cat5smSE","rtypeLocal" ), 2,
                          ifelse(term %in% c("cat5smOther", "rtypeLocalConn"), 3,
                                 ifelse(term == "rtypeOther", 4, 1))), term2 = factor(term2),
           type2 = factor(type2, levels = c("Precipitation", "Snow", "Wind direction", "Road type")))

  cols <- brewer.pal(8, "Dark2")
  g1 <- ggplot(cat, aes(y = term1, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, colour= model), position = position_dodge(0.1)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ") compared to reference"))) +
    theme_bw() +
    theme(text = element_text(size = 12)) + facet_wrap(~type2, scales = "free_y", ncol = 1)

  g1


}


plot_nocat <- function(res) {

  g2 <- ggplot(filter(res$t1, type2 != "cat"), aes(y = term1, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 24))

  g2

}





plot_nocatALL <- function(t1) {

  g2 <- ggplot(filter(t1, type2 != "cat"), aes(y = term1, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR, colour= model), position = position_dodge(0.1)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 24))

  g2

}
