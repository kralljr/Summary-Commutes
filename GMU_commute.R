#' Return usable GPS commute data
#'
#' @title GMU_commute
#' @param file .csv filename containing commute data
#' @param output string value of the desired function output; accepted values are: "trip summary", "overall summary", "df", or "all"
#' @return defined by the user by the output parameter.
#' @author Gabi Armada and Jenna Krall
#' @examples
#' file <- here("GPS data/100100.csv")
#' # obtains ??
#' GMU_commute(file, output = "df")
#' GMU_commute(file, output = "trip summary")
#' @export
GMU_commute <- function(file, output = "df"){
  if(class(file)[1] == "tbl_df") {
    file <- file$id2
  }
  print(file)
  # find headers
  headers <- read.csv(file, header = F, nrows = 1, as.is = T)
  headers <- as.character(headers[, c(2:18)])
  # read in data, assign headers
  df1 <- data.table::fread(file, skip = 3, select = c(2 : 18))
  colnames(df1) = headers

  # number of total trips
  total_trips <- n_distinct(df1$Trip)

  # select columns
  df2 <- df1 %>%
          select("Date & Time", "Latitude", "Longitude", "Trip", "Trip duration", "Trip distance")

  # maximum time
  df2 <- df2 %>%
          group_by(Trip) %>%
          mutate(trip_total_time = max(`Trip duration`))
  # find all trip times over 5 minutes
  df2 <- df2[!(df2$trip_total_time < "00:05:00"),]

  # create variable for missingness (for %)
    # note: 0 if missing, 1 if observed
  df2 <- mutate(df2, missing = ifelse(Latitude == 0 | Longitude == 0, 0, 1))
  # trips with at least some non-missing lat/lon
  actual_trips <- filter(df2, missing == 1) %>%
                  select(Trip) %>%
                  n_distinct()

  # trip summary (% missing)
  trip_summaries <- summarize(df2, nrows = n(), sum = sum(missing), mean = mean(missing))

  # overall summary
  overall_summary<- ungroup(df2) %>%
                    summarize(nrows = n(),
                              sum = sum(missing),
                              mean = round(mean(missing), digits = 3)) %>%
                    mutate(total_trips = total_trips, actual_trips = actual_trips)

  # filter df2 (only usable rows)
  df2b <- filter(df2, missing == 1)

  if (output == "trip summary"){
    return (trip_summaries)}
    else if (output == "overall summary"){
      return(overall_summary)}
    else if(output == "df"){
      return (df2b)}
    else if(output == "rawdf") {
      return(df2)
    }

}


sums_only <- function(df2) {

  # number of total trips
  # excluding <5 mins
  total_trips <- n_distinct(df2$Trip)

  # trips with non missing lat/lon
  actual_trips <- filter(df2, missing == 1) %>%
    select(Trip) %>%
    n_distinct()

  # trip summary (% missing)
  trip_summaries <- summarize(df2, nrows = n(), sum = sum(missing), mean = mean(missing))

  # overall summary
  overall_summary<- ungroup(df2) %>%
    summarize(nrows = n(),
              sum = sum(missing),
              mean = round(mean(missing), digits = 3)) %>%
    mutate(total_trips = total_trips, actual_trips = actual_trips)

  full_join(overall_summary, trip_summaries)
}

# outer function
oGMU_commute <- function(fp){
  lf <- list.files(fp)
  lf2 <- sapply(strsplit(lf, "\\."), function(x) paste0("GMU", x[[1]]))
  lf <- paste0(fp, "/", lf)

  dat <- data.frame(id1 = lf2, ID = substr(lf2, 1, 7), id2 = lf)
  dat <- tidyr::nest(dat, data = id2) %>%
    dplyr::mutate(gps= purrr::map(data, ~GMU_commute(file = ., output= "rawdf"))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(gps) %>%
    dplyr::mutate(Trip = paste0(substring(id1, 9), "-", Trip)) %>%
    dplyr::select(-id1)

  trips <- tidyr::nest(dat, data = !ID) %>%
    dplyr::mutate(sum= purrr::map(data, ~sums_only(df2 = .))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(sum)

  list(dat = dat, trips = trips)
}


