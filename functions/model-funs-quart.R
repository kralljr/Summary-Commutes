

get_lmeQ <- function(data1, iqrs, eqn1, rer = "~timemin | ID / id3",
                     re = "~1 | ID / id3", intervals = T, corr = T) {

  # get correlation
  if(corr) {
    #
    #this is not correct
    # cs1 <- corAR1(form = formula(rer))
    # cs1 <- Initialize(cs1, data = data1)
    #
    # Fit model
    lme1 <- lme(formula(eqn1),
                random= formula(re),
                data=data1, correlation= corAR1(form = formula(rer)))

    # lme1 <- lme(lPM ~ qsrness1hw,
    #             random= ~1 | ID / id3,
    #             data=data1, correlation= corAR1(form = ~timemin | ID / id3))
  } else {
    lme1 <- lme(formula(eqn1),
                random= formula(re),
                data=data1)
  }


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
                                     "rtypeLocalConn",  "rtypeLocal",    "rtypeOther", "timemin",
                                     "Q2", "Q3", "Q4"),
                         "Pollution", "Meteorology"),
           type2 = ifelse(term %in% c("prcpbin", "snowbin",
                                      "rtypeLocalConn",  "rtypeLocal",
                                      "rtypeOther", "cat5smSE",
                                      "cat5smOther"), "cat", "num"))


  # get intervals for variance components
  if(intervals & corr) {
    #print(intervals)
    int1 <- intervals(lme1)
    reint <- int1$reStruct


    # bind together intervals
    re <- bind_rows(int1$sigma, reint[[1]][1, ])
    for(i in 2 : length(reint)) {
      re <- bind_rows(re, reint[[i]][1, ])
    }
    re <- mutate(re, name = c("sigma", names(reint))) %>%
      rename(est= `est.`)

  } else if(corr != F){
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


lmequart <- function(rcommsr, name, name2) {
  eqn1 <- paste0("lPM ~ ", name)
  res1 <- get_lmeQ(rcommsr, iqrs, eqn1, re = "~ 1| ID / id3")

  eqn2b <- paste0("lPM ~ ", name, " + awnd + prcpbin +
               tmax  + tmin +  RH +
               cat5sm + snowbin")
  res2b <- get_lmeQ(rcommsr, iqrs, eqn2b, re = "~ 1| ID / id3")

  eqn2 <- paste0("lPM ~ ", name, " + awnd + prcpbin +
               tavg +  RH +
               cat5sm + snowbin")
  res2 <- get_lmeQ(rcommsr, iqrs, eqn2, re = "~ 1| ID / id3")

  eqn3 <- paste0("lPM ~ ", name, " + daily  + obsdiff")
  res3 <- get_lmeQ(rcommsr, iqrs, eqn3, re = "~ 1| ID / id3")

  eqn4 <- paste0("lPM ~ ", name, " + rushmorn + rusheven")
  res4 <- get_lmeQ(rcommsr, iqrs, eqn4, re = "~ 1| ID / id3")

  eqn5 <- paste0("lPM ~ ", name, " + rtype +", name2)
  res5 <- get_lmeQ(rcommsr, iqrs, eqn5, re = "~ 1| ID / id3")

  resl <- list(Main = res1,  Meteorology = res2, `Ambient PM2.5` = res3,
               Rush = res4, `All road features` = res5)



  resl
}





lmequartdow <- function(rcommsr, name, name2) {
  eqn1 <- paste0("lPM ~ ", name)
  res1 <- get_lmeQ(rcommsr, iqrs, eqn1, re = "~ 1| ID / id3")

  eqn2b <- paste0("lPM ~ ", name, " + awnd + prcpbin +
               tmax  + tmin +  RH +
               cat5sm + snowbin")
  res2b <- get_lmeQ(rcommsr, iqrs, eqn2b, re = "~ 1| ID / id3")

  eqn2 <- paste0("lPM ~ ", name, " + awnd + prcpbin +
               tavg +  RH +
               cat5sm + snowbin")
  res2 <- get_lmeQ(rcommsr, iqrs, eqn2, re = "~ 1| ID / id3")

  eqn3 <- paste0("lPM ~ ", name, " + daily  + obsdiff")
  res3 <- get_lmeQ(rcommsr, iqrs, eqn3, re = "~ 1| ID / id3")

  eqn4 <- paste0("lPM ~ ", name, " + dow")
  rcommsr <- mutate(rcommsr, dow = wday(date_local, label = T))
  res4 <- get_lmeQ(rcommsr, iqrs, eqn4, re = "~ 1| ID / id3")

  eqn5 <- paste0("lPM ~ ", name, " + rtype +", name2)
  res5 <- get_lmeQ(rcommsr, iqrs, eqn5, re = "~ 1| ID / id3")

  resl <- list(Main = res1,  Meteorology = res2, `Ambient PM2.5` = res3,
               Rush = res4, `All road features` = res5)



  resl
}




plot_cat_sensQ <- function(reslist, myterm) {
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
                  term = substring(term, nc + 1),
                  term = factor(term, levels = c("Q2", "Q3", "Q4")))

  cols <- rep(brewer.pal(8, "Dark2"), 2)
  cols <- c("grey70", "grey40", "black")

  lt <- rep(c(1, 1, 1, 1, 1), 4)
  sh <- rep(c(15 : 18, 8), 4)
  g1 <- ggplot(lista, aes(y = term, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, shape = name, linetype = name, colour = term),
                    position = position_dodge(0.6)) +
    scale_color_manual(values = cols, guide = "none") +

    # scale_colour_manual(values = cols, name = "Model", breaks = c("All road features", "Rush", "Ambient PM2.5",
    #                                                               "Meteorology", "Main")) +
    scale_shape_manual(name = "Model", values= sh, breaks = c("All road features", "Rush", "Ambient PM2.5",
                                                              "Meteorology", "Main")) +
    scale_linetype_manual(name = "Model", values = lt,breaks = c("All road features", "Rush", "Ambient PM2.5",
                                                                 "Meteorology", "Main")) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    ylab("") +
    xlab(expression(atop(paste("Change in log PM"[2.5]," (log ",mu,"g/m"^3, ")"), "compared to Q1"))) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "right")
  list(fig = g1, tab = lista)


}


plot2 <- function(lista) {
  cols <- rep(brewer.pal(8, "Dark2"), 2)
  cols <- brewer.pal(6, "Reds")[c(2, 4, 6)]
  cols <- c("grey70", "grey40", "black")
  lt <- rep(c(1, 1, 1, 1, 1), 4)
  sh <- rep(c(15 : 18, 8), 4)

  g1 <- ggplot(lista, aes(y = term, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, shape = name, linetype = name, colour = term),
                    position = position_dodge(0.6)) +
    # scale_colour_manual(values = cols, name = "Model", breaks = c("All road features", "Rush", "Ambient PM2.5",
    #                                                               "Meteorology", "Main")) +
    scale_color_manual(values = cols, guide = "none") +
    scale_shape_manual(name = "Model", values= sh, breaks = c("All road features", "Rush", "Ambient PM2.5",
                                                              "Meteorology", "Main")) +
    scale_linetype_manual(name = "Model", values = lt,breaks = c("All road features", "Rush", "Ambient PM2.5",
                                                                 "Meteorology", "Main")) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    ylab("") +
    xlab(expression(atop(paste("Change in log PM"[2.5]," (log ",mu,"g/m"^3, ")"), "compared to Q1"))) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "right") +
    facet_wrap(~type)
  g1
}



plot_nocat_senslagQ <- function(reslist, myterm, reslist2 = NULL, myterm2 = NULL) {
  list1 <- lapply(reslist, function(x) filter(x$t1, substr(term, 1, 3) == "var"))
  cols <- brewer.pal(8, "Dark2")

  for(i in 1 : length(list1)) {

    list1[[i]] <- mutate(list1[[i]], name = names(reslist)[i])
    if(i == 1) {
      lista <- list1[[i]]
    } else {
      lista <- bind_rows(lista, list1[[i]])
    }

  }
  lista2 <- mutate(lista, name = factor(name, levels = names(reslist)),
                   name = as.numeric(substring(name, 4)),
                   term = substring(term, 4),
                   Road = myterm)


  if(!is.null(reslist2)) {
    list1 <- lapply(reslist2, function(x) filter(x$t1, substr(term, 1, 3) == "var"))

    for(i in 1 : length(list1)) {

      list1[[i]] <- mutate(list1[[i]], name = names(reslist)[i])
      if(i == 1) {
        lista <- list1[[i]]
      } else {
        lista <- bind_rows(lista, list1[[i]])
      }

    }
    lista1 <- mutate(lista, name = factor(name, levels = names(reslist)),
                     name = as.numeric(substring(name, 4)),
                     term = substring(term, 4),
                     Road = myterm2)
    lista2 <- full_join(lista1, lista2)
  }

  #lista2 <- dplyr::filter(lista2, name %in% c(0: 5))

  g2 <- ggplot(lista2, aes(y = term, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high,
                        colour = name),
                    position = position_dodge2(0.6)) + #shape = name, linetype = name)
    scale_color_gradient(name = "Lag", high = "#132B43",  low = "#56B1F7") +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    ylab("") +
    xlab(expression(atop(paste("Change in log PM"[2.5]," (log ",mu,"g/m"^3, ")" ), " compared to Q1"))) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "right")

  if(!is.null(reslist2)) {
    g2 <- g2 + facet_wrap(~Road)
  }

  g2

}

