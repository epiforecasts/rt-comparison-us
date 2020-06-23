require(data.table)

report_rt <- function(results_dir = "nowcast/national", creation_date = Sys.Date(),
                      map_to_growth = FALSE, model = "EpiNow", version = "1.0") {
  
  
  rt <- EpiNow::get_timeseries(results_dir, summarised = FALSE)$rt
  
  rt <- rt[type == "nowcast"][rt_type == "nowcast"][, .(region, date, R, rt_type, sample)]
  
  if (map_to_growth) {
    gi <- data.table::setDT(readRDS(
      url(
        "https://github.com/epiforecasts/EpiNow/blob/master/data-raw/gi.rds?raw=true"
      )))
    
    R_to_r <- function(R) {
      gi_sample <- gi[sample(1:nrow(gi), 1)]
      gamma_mean <-  gi_sample$mean
      gamma_sd <-  gi_sample$sd
      
      k <- (gamma_sd / gamma_mean)^2
      r <- (R^k - 1) / (k * gamma_mean)
      
      return(r)
    }  
    
    rt <- rt[, R := R_to_r(R), by = "sample"] 
  }else{
    rt[, sample := NULL]
  }
  
  rt <- rt[,
             .(Value = median(R, na.rm = TRUE), 
               `Quantile 0.05` = quantile(R, 0.05, na.rm = TRUE),
               `Quantile 0.1` = quantile(R, 0.1, na.rm = TRUE),
              `Quantile 0.15` = quantile(R, 0.15, na.rm = TRUE),
              `Quantile 0.2` = quantile(R, 0.2, na.rm = TRUE),
              `Quantile 0.25` = quantile(R, 0.25, na.rm = TRUE),
              `Quantile 0.3` = quantile(R, 0.3, na.rm = TRUE),
              `Quantile 0.35` = quantile(R, 0.35, na.rm = TRUE),
              `Quantile 0.4` = quantile(R, 0.4, na.rm = TRUE),
              `Quantile 0.45` = quantile(R, 0.45, na.rm = TRUE),
              `Quantile 0.5` = quantile(R, 0.5, na.rm = TRUE),
              `Quantile 0.55` = quantile(R, 0.55, na.rm = TRUE),
              `Quantile 0.6` = quantile(R, 0.6, na.rm = TRUE),
              `Quantile 0.65` = quantile(R, 0.65, na.rm = TRUE),
              `Quantile 0.7` = quantile(R, 0.7, na.rm = TRUE),
              `Quantile 0.75` = quantile(R, 0.75, na.rm = TRUE),
              `Quantile 0.8` = quantile(R, 0.8, na.rm = TRUE),
              `Quantile 0.85` = quantile(R, 0.85, na.rm = TRUE),
              `Quantile 0.9` = quantile(R, 0.9, na.rm = TRUE),
              `Quantile 0.95` = quantile(R, 0.95, na.rm = TRUE)), 
             by = c("region", "date")][,
           `:=`(
             Group = "LSHTM",
             `Creation Day` = lubridate::day(creation_date),
             `Creation Month` = lubridate::month(creation_date),
             `Creation Year` = lubridate::year(creation_date),
             `Day of Value` = lubridate::day(date),
             `Month of Value` = lubridate::month(date),
             `Year of Value` = lubridate::year(date),
             Geography = region,
             ValueType = ifelse(map_to_growth, "growth_rate", "R"),
             Model = model,
             Version = version
           )
           ][, date := NULL][, region := NULL]
  
  data.table::setcolorder(rt, c("Group", "Creation Day", "Creation Month", "Creation Year",
                                "Day of Value", "Month of Value", "Year of Value",
                                "Geography", "ValueType", "Model", "Version"))
  
  return(rt)
  
}



