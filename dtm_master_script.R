## Script to make my dtm_master variable
## calculate all of the stuff here, save it as an object, and just load the object for my writeups

## required libraries
library(tidyverse)
library(openxlsx)
library(rgdal)
libray(plyr)
library(sp)



## reading in afghanistan data
afg <- read.xlsx("Data/Displacement_Data/DTM_Afghanistan/Dataset_by_Settlements_DTM_AFG_9_provinces_phase_1&2_June_30_2017.xlsx", sheet = 3)
afg <- tbl_df(afg)
## making afg.cor without NA for lat/lon
afg.cor = afg %>% filter(!is.na(Latitude))
## creating dtm_master
dtm_master <- afg.cor
# mutate rownames on dtm_master
dtm_master <- dtm_master %>% rownames_to_column()
## mutate displacement col thats refugees + IDPs
dtm_master <- dtm_master %>% mutate(civ_dis = OutMigrants2016 + FledIDPs2016)

#### Adding in priogrid information

## read in grid data
prio_grid_static_original <- read.csv("Data/PRIO-GRID Static Variables - 2018-02-09.csv", 
                                      stringsAsFactors = FALSE)
prio_grid_yearly_original <- read.csv("Data/PRIO-GRID Yearly Variables for 2010-2014 - 2018-02-09.csv", 
                                      stringsAsFactors = FALSE)
## format
prio_grid_static <- as.tibble(prio_grid_static_original)
prio_grid_yearly <- as.tibble(prio_grid_yearly_original)

## trim Afghanistan - gwcode 700
prio_grid_afg <- prio_grid_yearly %>% filter(gwno == 700)
## only have codes for yearly - so left_join
prio_grid_afg <- left_join(prio_grid_afg, prio_grid_static, by = "gid")

## read in prio_grid shapefiles to annotate
prio_grid_shapes<- readOGR(dsn = "Data/priogrid_cellshp", layer = "priogrid_cell", stringsAsFactors = FALSE)
## establish which grid each DTM settlement is in
## make dtm shapes (repeating code from earlier)
afg_points <- SpatialPoints(dtm_master %>% select(Longitude, Latitude), 
                            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
## establish prio_grid for each - tested to make sure no overlap
dtm_grid_over <- over(afg_points, prio_grid_shapes)
dtm_grid_over <- as.tibble(dtm_grid_over)
## merge back into dtm master
dtm_master <- full_join(dtm_master, dtm_grid_over %>% rownames_to_column(), by = "rowname")
## annotate dtm_master with priogrid attributes
dtm_master$gid <- as.numeric(dtm_master$gid)
dtm_master <- left_join(dtm_master, prio_grid_afg %>% filter(year == 2013), by = "gid")



#### Distance

##here I define my master looping function for distance-bands aggregation
## it takes the band parameters (inner, outer, interval, max) and a given function to apply
## it alters the dtm_master dataframe in the global environment
bands_calc_master <- function(max.radius = 10, interval = 10, inner = 0, outer = 10, short_name, base_calc){
  # set function based on name
  fun <- match.fun(base_calc)
  
  
  # main while loop = each one creates a new col on the specificed df
  while(outer <= max.radius){
    var.name <- paste(short_name, outer , sep="_")
    dtm_master <<- dtm_master %>% mutate(!!var.name := as.numeric(mapply
                                                                  (fun, inner, 
                                                                    outer, dtm_master$Longitude,
                                                                    dtm_master$Latitude)))
    
    inner <- inner + interval
    outer <- outer + interval
  }
}

## this is my most basic calculator: just returns the sum of fatalities in the given band for a given longlat
## all of the calcs are set up to take inner, out, lon lat - these are passed by the master loop
total_fatalities_band_calc <- function(inner, outer, lon, lat){
  ucdp_master %>% mutate(dist = spDistsN1(as.matrix(ucdp_master %>% select(longitude, latitude)), 
                                          c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner & dist < outer) %>% summarise(sum(best))
}

## this calls the basic band calc from 0 to 200 on the full dtm dataset
bands_calc_master(200, 10, 0, 10, "d", "total_fatalities_band_calc")

## band calc to figure out number of conflicts
num_conflicts_band_calc <- function(inner, outer, lon, lat){
  ucdp_master %>% mutate(dist = spDistsN1(as.matrix(ucdp_master %>% select(longitude, latitude)), 
                                          c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner & dist < outer) %>% summarise(n())
}

## master calc call with nconflicts
bands_calc_master(50, 10, 0, 10, "d_numc", "num_conflicts_band_calc")



#### Identity/actor stuff

## import EPR shapefile data
epr_shapes <- readOGR(dsn = "Data/GeoEPR-2014", layer = "GeoEPR-2014", stringsAsFactors = FALSE)
## make afghanistan subset
epr_afg <- epr_shapes[epr_shapes$statename == "Afghanistan",]
## limit to contemporary shapes
epr_main <- epr_afg[epr_afg$to == 2013,]

# make spatial points data for DTM
dtm_points <- SpatialPoints(dtm_master %>% select(Longitude, Latitude), 
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
# use over with returnList to get the matching polygons
dtm_overlap <- over(x = dtm_points, y = epr_main, returnList = TRUE)
# make formatted dataframe with duplicate rows via plyr
dtm_pf <- plyr::ldply(dtm_overlap, data.frame)
# rename .id to rowname
colnames(dtm_pf)[1] <- "rowname"
# pull out the duplicate rows
dtm_dups <- dtm_pf %>% filter(duplicated(.[["rowname"]]))
# change the name of their ethnic_group
colnames(dtm_dups)[6] <- "ethnic_group_2"
# just select the rowname and ethnic_group (if i want other variables, have to also rename them here)
dtm_dups <- dtm_dups %>% select(rowname, ethnic_group_2)
## remove the dups from afg
dtm_pf <- dtm_pf %>% filter(!duplicated(.[["rowname"]]))
# join with dups
dtm_pf <- left_join(dtm_pf, dtm_dups, by = "rowname")
# can left_join to afg data
dtm_master <- left_join(dtm_master, dtm_pf, by = "rowname")
# rename ethnic group
colnames(dtm_master) <- replace(colnames(dtm_master), colnames(dtm_master) == "group", "ethnic_group_1")

## here is the function that selects all conflicts within band of settlent
## rewrite this one to only calculate same ethnic region fatalities
band_calc_same_ethnic <- function(inner, outer, lon, lat, ethnic_group){
  ucdp_master %>% mutate(dist = spDistsN1(as.matrix(ucdp_master %>% select(longitude, latitude)), 
                                          c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner & dist < outer) %>% 
    filter(ethnic_group_1 == ethnic_group) %>% summarise(sum(best))
}
## and another one for different region
band_calc_other_ethnic <- function(inner, outer, lon, lat, ethnic_group){
  ucdp_master %>% mutate(dist = spDistsN1(as.matrix(ucdp_master %>% select(longitude, latitude)), 
                                          c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner & dist < outer) %>% 
    filter(ethnic_group_1 != ethnic_group) %>% summarise(sum(best))
}

## becasue these need an "ethnic group" argument passed, I can't use the master looping function from earlier
## here is a new one that passes ethnic group
bands_calc_ethnic <- function(max.radius = 10, interval = 10, inner = 0, outer = 10, short_name, base_calc){
  # set function based on name
  fun <- match.fun(base_calc)
  
  
  # main while loop = each one creates a new col on the specificed df
  while(outer <= max.radius){
    var.name <- paste(short_name, outer , sep="_")
    dtm_master <<- dtm_master %>% mutate(!!var.name := as.numeric(mapply
                                                                  (fun, inner, 
                                                                    outer, dtm_master$Longitude,
                                                                    dtm_master$Latitude, dtm_master$ethnic_group_1)))
    
    inner <- inner + interval
    outer <- outer + interval
  }
}

## call the loop with same and different ethnic calcs
## same - 0-50
bands_calc_ethnic(50, 10, 0, 10, "d_se", "band_calc_same_ethnic")
## different 0-50
bands_calc_ethnic(50, 10, 0, 10, "d_de", "band_calc_other_ethnic")

## mutating 0-50 band to select border settlements
dtm_master <- dtm_master %>% mutate(d_de_fifty = d_de_10 + d_de_20, d_de_30, d_de_40, d_de_50)
dtm_master <- dtm_master %>% mutate(d_se_fifty = d_se_10 + d_se_20, d_se_30, d_se_40, d_se_50)


##### Pashtun/Taleban analysis

## add Taleban casualty column to ucdp data
ucdp_master <- ucdp_master %>% mutate(deaths_taleban_a = ifelse(side_a == "Taleban", deaths_a, 0)) %>% 
  mutate(deaths_taleban_b = ifelse(side_b == "Taleban", deaths_b, 0)) %>% 
  mutate(deaths_taleban = deaths_taleban_a + deaths_taleban_b) %>% 
  select(-deaths_taleban_a, -deaths_taleban_b)
## add pashtun civilian column
ucdp_master <- ucdp_master %>% mutate(deaths_pashtun_civ = ifelse(ethnic_group_1 == "Pashtuns" & !is.na(ethnic_group_1), deaths_civilians, 0))
## add non-Taleban casualties
ucdp_master <- ucdp_master %>% mutate(deaths_non_taleban_a = ifelse(side_a != "Taleban", deaths_a, 0)) %>% 
  mutate(deaths_non_taleban_b = ifelse(side_b != "Taleban", deaths_b, 0)) %>% 
  mutate(deaths_non_taleban = deaths_non_taleban_a + deaths_non_taleban_b) %>% 
  select(-deaths_non_taleban_a, -deaths_non_taleban_b)
## add non_pashtun civilians
ucdp_master <- ucdp_master %>% mutate(deaths_non_pashtun_civ = ifelse(ethnic_group_1 != "Pashtuns" & !is.na(ethnic_group_1), deaths_civilians, 0))

# now I need a new calculator for each type of casualty
## base function for taleban deaths by band
h2b_taleban_donut_calc <- function(inner.radius, outer.radius, lon, lat){
  ucdp_master %>% filter(deaths_taleban > 0) %>% 
    mutate(dist = spDistsN1(as.matrix(ucdp_master %>% filter(deaths_taleban > 0) %>% select(longitude, latitude)), 
                            c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner.radius & dist < outer.radius) %>% summarise(sum(deaths_taleban))
}
## base function for pashtun civilian deaths by band
h2b_pashciv_donut_calc <- function(inner.radius, outer.radius, lon, lat){
  ucdp_master %>% filter(deaths_pashtun_civ > 0) %>% 
    mutate(dist = spDistsN1(as.matrix(ucdp_master %>% 
                                        filter(deaths_pashtun_civ > 0) %>% select(longitude, latitude)), 
                            c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner.radius & dist < outer.radius) %>% summarise(sum(deaths_pashtun_civ))
}
## base function for non_taleban deaths
h2b_nont_donut_calc <- function(inner.radius, outer.radius, lon, lat){
  ucdp_master %>% filter(deaths_non_taleban > 0) %>% 
    mutate(dist = spDistsN1(as.matrix(ucdp_master %>% 
                                        filter(deaths_non_taleban > 0) %>% select(longitude, latitude)), 
                            c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner.radius & dist < outer.radius) %>% summarise(sum(deaths_non_taleban))
}
## base function for non-pashtun civilian deaths
h2b_nonpashciv_donut_calc <- function(inner.radius, outer.radius, lon, lat){
  ucdp_master %>% filter(deaths_non_pashtun_civ > 0) %>% 
    mutate(dist = spDistsN1(as.matrix(ucdp_master %>% filter(deaths_non_pashtun_civ > 0) %>% select(longitude, latitude)), 
                            c(lon, lat), longlat = TRUE)) %>%
    filter(dist >= inner.radius & dist < outer.radius) %>% summarise(sum(deaths_non_pashtun_civ))
}

# run the master functions all 0-50
## call the master calc with the taleban subfunction
bands_calc_master(max.radius = 50, short_name = "d_tal", base_calc = "h2b_taleban_donut_calc")
## pashtun civ
bands_calc_master(max.radius = 50, short_name = "d_pashc", base_calc = "h2b_pashciv_donut_calc")
## non-taleban
bands_calc_master(max.radius = 50, short_name = "d_ntal", base_calc = "h2b_pashciv_donut_calc")
## non-pashtun civ
bands_calc_master(max.radius = 50, short_name = "d_npashc", base_calc = "h2b_nonpashciv_donut_calc")

## now that I have these 4 subcategories 1-50, I need a few subcategories
## pashtun civ + taleban
dtm_master <- dtm_master %>% mutate(d_p_10 = d_tal_10 + d_pashc_10) %>%
  mutate(d_p_20 = d_tal_20 + d_pashc_20) %>%
  mutate(d_p_30 = d_tal_30 + d_pashc_30) %>%
  mutate(d_p_40 = d_tal_40 + d_pashc_40) %>%
  mutate(d_p_50 = d_tal_50 + d_pashc_50)
## non-taleban + nonpashtun vic
dtm_master <- dtm_master %>% mutate(d_np_10 = d_tal_10 + d_pashc_10 + d_ntal_10 + d_npashc_10) %>%
  mutate(d_np_20 = d_ntal_20 + d_npashc_20) %>%
  mutate(d_np_30 = d_ntal_30 + d_npashc_30) %>%
  mutate(d_np_40 = d_ntal_40 + d_npashc_40) %>%
  mutate(d_np_50 = d_ntal_50 + d_npashc_50)

## closest actor analysis

# simple calc to find distance to nearest conflict with given armed actor
armed_actor_nearest_calc <- function(armed_actor, lon, lat){
  ucdp_master %>% filter(side_a == armed_actor | side_b == armed_actor) %>%
    mutate(dist = spDistsN1(as.matrix(ucdp_master %>% filter(side_a == armed_actor | side_b == armed_actor)
                                      %>% select(longitude, latitude)), 
                            c(lon, lat), longlat = TRUE)) %>% summarize(min(dist))
}

# call it with mapply for each armed group I care about
#afggov
dtm_master <- dtm_master %>% mutate(dist_afgov = mapply(armed_actor_nearest_calc, 
                                                        "Government of Afghanistan", Longitude, Latitude))
#taleban
dtm_master <- dtm_master %>% mutate(dist_tal = mapply(armed_actor_nearest_calc, 
                                                        "Taleban", Longitude, Latitude))
#IS
dtm_master <- dtm_master %>% mutate(dist_is = mapply(armed_actor_nearest_calc, 
                                                      "IS", Longitude, Latitude))
## format
dtm_master$dist_afgov <- as.numeric(dtm_master$dist_afgov)
dtm_master$dist_tal <- as.numeric(dtm_master$dist_tal)
dtm_master$dist_is <- as.numeric(dtm_master$dist_is)

## fixed-decay actor analysis

# fixed decay basic calc
armed_actor_fixed_decay_calc <- function(armed_actor, lon, lat){
  sum(
    (ucdp_master %>% filter(side_a == armed_actor | side_b == armed_actor) %>% 
       select(best)) / 
        log(spDistsN1(as.matrix(ucdp_master %>% filter(side_a == armed_actor | side_b == armed_actor) %>%
                                  select(longitude, latitude)), 
                      c(lon, lat), longlat = TRUE))
    )
  
}

# mapply calls
#afggov
dtm_master <- dtm_master %>% mutate(dscore_afgov = mapply(armed_actor_fixed_decay_calc, 
                                                        "Government of Afghanistan", Longitude, Latitude))
#taleban
dtm_master <- dtm_master %>% mutate(dscore_tal = mapply(armed_actor_fixed_decay_calc, 
                                                      "Taleban", Longitude, Latitude))
#IS
dtm_master <- dtm_master %>% mutate(dscore_is = mapply(armed_actor_fixed_decay_calc, 
                                                     "IS", Longitude, Latitude))

## Actor-inflicted casualties
## calc
armed_actor_kill_calc <- function(armed_actor, lon, lat){
  
 sum(
  ## check if actor a is actor, calculate distance score using side_b casualties 
  ifelse(ucdp_master %>% filter(side_a == armed_actor) %>% nrow() > 0, sum(
    (ucdp_master %>% filter(side_a == armed_actor) %>% 
       select(deaths_b))/
      log(spDistsN1(as.matrix(ucdp_master %>% filter(side_a == armed_actor) %>%
                                select(longitude, latitude)),
                    c(lon, lat), longlat = TRUE))
  ), 0), 
  
  # check if actor b is actor, calculate distance score using side_a casualties
  ifelse(ucdp_master %>% filter(side_b == armed_actor) %>% nrow() > 0, sum(
    (ucdp_master %>% filter(side_b == armed_actor) %>% 
       select(deaths_a)) / 
      log(spDistsN1(as.matrix(ucdp_master %>% filter(side_b == armed_actor) %>%
                                select(longitude, latitude)), 
                    c(lon, lat), longlat = TRUE))
  ), 0)
 )
}

## mapply calls
#afggov
dtm_master <- dtm_master %>% mutate(kscore_afgov = mapply(armed_actor_kill_calc, 
                                                          "Government of Afghanistan", Longitude, Latitude))
## taleban
dtm_master <- dtm_master %>% mutate(kscore_tal = mapply(armed_actor_kill_calc, 
                                                          "Taleban", Longitude, Latitude))
## islamic state
dtm_master <- dtm_master %>% mutate(kscore_is = mapply(armed_actor_kill_calc, 
                                                          "IS", Longitude, Latitude))




################################## save it all
save(dtm_master, file = "dtm_data.Rda")
