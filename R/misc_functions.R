check_map <- function(x){
  mapdat <- select(x, fips, value)
  names(mapdat) <- c("region", "value")
  mod_map <- county_choropleth(mapdat,
                   title      = NULL)
  mod_map
}
