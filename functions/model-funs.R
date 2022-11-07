

get_lme <- function(data1, iqrs, eqn1, rer = "~timemin | ID / id3",
                    re = "~1 | ID / id3", intervals = T, corr = T) {

  # get correlation
  if(corr) {
    #
    cs1 <- corARMA(form = formula(rer), p = 1, q = 1)
    cs1 <- corAR1(form = formula(rer))
    cs1 <- Initialize(cs1, data = data1)
  } else {
    cs1 <- NULL
  }
  # Fit model
  lme1 <- lme(formula(eqn1),
              random= formula(re),
              data=data1, correlation= cs1)

  # Get residuals
  lme1b <- augment(lme1)

  # format
  t1 <- tidy(lme1, conf.int = T) %>%
    filter(effect == "fixed", term != "(Intercept)") %>%
    mutate(term = case_when(term == "snowbinL1m" ~ "snowbin",
                            term == "awndL1" ~ "awnd",
                            term == "tmaxL1" ~ "tmax",
                            term == "tminL1" ~ "tmin",
                            term == "prcpbinL1" ~ "prcpbin",
                            TRUE ~ term)) %>%
    full_join(iqrs) %>%
    mutate(IQR = ifelse(is.na(IQR), 1, IQR),
           estimateIQR = estimate * IQR,
           conf.lowIQR = conf.low * IQR,
           conf.highIQR = conf.high * IQR,
           term1 = factor(term, levels = c("daily", "obsdiff", "srness", "timemin",
                                           "rtypeLocalConn",  "rtypeLocal",    "rtypeOther",
                                           "tmax" ,"tmin",  "prcpbin", "snowbin" , "awnd", "cat5smSE", "cat5smOther",
                                           "hourly", "hourlymonth"),
                          labels = c("Daily PM2.5", "Hourly PM2.5", "Roadiness", "Time since start",
                                     "Local conn", "Local", "Other",
                                     "Max Temp",  "Min Temp", "Precipitation","Snow",
                                     "Wind", "WindDir SE", "WindDir Other", "Hour Mean", "Hour Mon Mean")),
           type = ifelse(term %in% c("daily", "obsdiff", "srness",
                                     "rtypeLocalConn",  "rtypeLocal",    "rtypeOther", "timemin"),
                         "Pollution", "Meteorology"),
           type2 = ifelse(term %in% c("prcpbin", "snowbin",
                                      "rtypeLocalConn",  "rtypeLocal",
                                      "rtypeOther", "cat5smSE",
                                      "cat5smOther"), "cat", "num"))


  # get intervals for variance components
  if(intervals) {
    print(intervals)
    int1 <- intervals(lme1)
    reint <- int1$reStruct


    # bind together intervals
    re <- bind_rows(int1$sigma, reint[[1]][1, ])
    for(i in 2 : length(reint)) {
      re <- bind_rows(re, reint[[i]][1, ])
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



runlag <- function(data1, iqrs, eqn1, rer = "~timemin | ID / id3",
                   re = "~1 | ID / id3", intervals = T, corr = T, lags = c(0)) {

  k <- 1
  resall <- list()
  for(i in seq_along(lags)) {
    print(i)
    datlag <- group_by(data1, ID, id3) %>%
      arrange(ID, id3, rdatetime) %>%
      mutate(var = dplyr::lag(var, lags[i])) %>%
      dplyr::select(ID, id3, var, lPM) %>%
      na.omit() %>%
      mutate(keeps = 1, timemin = cumsum(keeps))
    resall[[k]] <- get_lme(datlag, iqrs, eqn1, re = "~ 1| ID / id3",
                           intervals = intervals, corr = corr)
    k <- k + 1

  }
  names(resall) <- paste0("lag", lags)
  resall
}


plot_re <- function(re) {


  ggplot(re, aes(x = `est.`, y = name)) +
    geom_pointrange(aes(xmin = lower, xmax = upper )) +
    xlab("Estimate") + ylab("")
}







plot_cat_sens <- function(reslist, myterm) {
  nc <- nchar(myterm)
  list1 <- lapply(reslist, function(x) filter(x$t1, substr(term, 1, nc) == myterm))
  for(i in 1 : length(list1)) {
    list1[[i]] <- mutate(list1[[i]], name = names(reslist)[i])
    if(i == 1) {
      lista <- list1[[i]]
    } else {
      lista <- bind_rows(lista, list1[[i]])
    }
  }
  lista <- mutate(lista, name = factor(name, levels = names(reslist)),
                  term = substring(term, nc + 1))

  cols <- rep(brewer.pal(8, "Dark2"), 2)
  g1 <- ggplot(lista, aes(y = term, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, colour = name),
                    position = position_dodge(0.2)) +
    scale_colour_manual(values = cols) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ") compared to reference"))) +
    theme_bw() +
    theme(text = element_text(size = 12))
  g1


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

  cols <- rep(brewer.pal(8, "Dark2"), 2)
  g1 <- ggplot(cat, aes(y = term1, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high,
                        colour= model), position = position_dodge(0.4)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    scale_color_manual(values = cols, name = "Model") +
    xlab("") +
    ylab(expression(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ") compared to reference"))) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "top") +
    facet_wrap(~type2, scales = "free", ncol = 2, dir = "h")

  g1


}


plot_nocat_sens <- function(reslist, myterm) {
  list1 <- lapply(reslist, function(x) filter(x$t1, term == myterm))

  for(i in 1 : length(list1)) {

    list1[[i]] <- mutate(list1[[i]], name = names(reslist)[i])
    if(i == 1) {
      lista <- list1[[i]]
    } else {
      lista <- bind_rows(lista, list1[[i]])
    }

  }
  lista <- mutate(lista, name = factor(name, levels = names(reslist)))

  g2 <- ggplot(lista, aes(y = name, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 12))

  g2

}

plot_nocat <- function(res) {

  res1 <- filter(res$t1, type2 != "cat", !is.na(estimateIQR))
  g2 <- ggplot(res1, aes(y = term1, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 12))

  g2

}





plot_nocatALL <- function(t1) {
  cols <- rep(brewer.pal(8, "Dark2"), 2)

  g2 <- ggplot(filter(t1, type2 != "cat"), aes(y = term1, x = estimateIQR)) +
    geom_pointrange(aes(xmin = conf.lowIQR, xmax = conf.highIQR, colour= model),
                    position = position_dodge(0.4)) +
    scale_color_manual(values = cols, name = "Model") +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    xlab("") +
    ylab(expression(atop(paste("Change in PM"[2.5]," (",mu,"g/m"^3, ")" ), " per IQR increase"))) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "top") +
    facet_wrap(~term1, scales = "free",ncol = 2)

  g2

}
