check_map <- function(region, value){
  mapdat <- as.data.frame(cbind(region, value))
  # mapdat <- select(x, fips, value)
  names(mapdat) <- c("region", "value")
  mod_map <- county_choropleth(mapdat,
                   title      = NULL)
  mod_map
}
