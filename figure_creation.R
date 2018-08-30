## figure creation

## load libraries
library(tidyverse)
library(reshape2)
library(broom)
library(ggrepel)
library(dotwhisker)
library(RColorBrewer)
library(scales)
library(grid)
library(wesanderson)
library(sp)
library(rgdal)

##### Data

## load master data with pre-calculated values, including band distances, etc.
load("dtm_data.Rda")
## reading in and trimming UCDP.GED data
ucdp2 <- read.csv("Data/Violence_Data/UCDP_GED/ged171.csv", 
                  stringsAsFactors = FALSE)
ucdp <- tbl_df(ucdp2)
ucdp.afg <- ucdp %>% filter(country == "Afghanistan")
ucdp.afg.2016 <- ucdp.afg %>% filter(year == 2016)
ucdp_master <- ucdp.afg.2016
ucdp_master <- ucdp_master %>% rownames_to_column()

# read in grid data
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
prio_grid_shapes<- readOGR(dsn = "Data/priogrid_cellshp", layer = "priogrid_cell", stringsAsFactors = FALSE, 
                           verbose = FALSE)
## establish which grid each DTM settlement is in
## make dtm shapes (repeating code from earlier)
afg_points <- SpatialPoints(dtm_master %>% select(Longitude, Latitude), 
                            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## geonames
## load geonames af .csv
geonames_original <- read.csv(file = "Data/Displacement_Data/DTM_Afghanistan/AF.csv", header = FALSE, stringsAsFactors = FALSE)
## tibblify
geonames_afg <- as.tibble(geonames_original)
## name some cols
colnames(geonames_afg) <- c("geonameid", "name_original", "SettNameEnglish", "alternatenames", "Latitude", "Longitude",
                            "feature_class", "feature_code", "country_code", "cc2", "admin1_code",
                            "admin2_code", "admin3_code", "admin4_code", "population", "elevation",
                            "dem", "timezone", "modification_date")
## make points
geonames_points <- SpatialPoints(geonames_afg %>% select(Longitude, Latitude), 
                                 CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# over()
geonames_grid_over <- over(geonames_points, prio_grid_shapes)
# join
geonames_grid <- left_join( geonames_afg %>% rownames_to_column(), geonames_grid_over %>% rownames_to_column())


#### creating figures

## set custom ggtheme
theme_andy <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=11)  + 
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background))  + 
    theme(plot.background=element_rect(fill=color.background, color=color.background))  + 
    theme(panel.border=element_rect(color=color.background))  + 
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25))  + 
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.15))  + 
    theme(panel.grid.minor.y=element_blank())  + 
    theme(axis.ticks=element_blank())  + 
    
    # Format the legend
    theme(legend.background = element_rect(fill=color.background))  + 
    theme(legend.key = element_rect(fill=color.background))  + 
    theme(legend.text = element_text(size=10,color=color.axis.title))  + 
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=14, vjust=1.25))  + 
    theme(axis.text.x=element_text(size=10,color=color.axis.text))  + 
    theme(axis.text.y=element_text(size=10,color=color.axis.text))  + 
    theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=0))  + 
    theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.25))  + 
    
    theme(strip.background = element_rect(colour = color.background, 
                                          fill = palette[3], size = 0.5)) +
    # Title fonts
    #theme(plot.title=element_text(family="OpenSans-CondensedBold", margin=margin(b=15)))  + 
    #theme(plot.subtitle=element_text(family="OpenSans-CondensedLightItalic"))  + 
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
theme_set(theme_andy())

## create figure 2
lm(data = dtm_master, civ_dis ~ d_10 + d_20 + d_30 + d_40 + d_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean) %>% 
  tidy() %>% 
  filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean") %>% 
  relabel_predictors(c(d_10 = "10km", d_20 = "20km", d_30 = "30km", d_40 = "40km", d_50 = "50km")) %>%
  dwplot() + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Predictive Power Decays With Distance", 
       subtitle = "Effect of conflict deaths on civilian displacement by 10km bands", caption = "Figure 2") + 
  theme(legend.position="none") + scale_color_grey(start = 0, end = .6)
## ggsave(filename = "Combined Distance Decay Graph", device = "jpeg", path = c("../Writing/Figures"), 
##      height = 4, width = 6)

## create figure 3
er_lm_full <- lm(data = dtm_master, civ_dis ~ d_se_10 + d_se_20 + d_se_30 + d_se_40 + d_se_50 + d_de_10 + d_de_20 + d_de_30 + d_de_40 + d_de_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean) %>% tidy() %>%
  filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean")
er_lm_full$model = c("NA", "Same Ethnic Region", "Same Ethnic Region","Same Ethnic Region","Same Ethnic Region","Same Ethnic Region",
                     "Different Ethnic Region", "Different Ethnic Region","Different Ethnic Region","Different Ethnic Region","Different Ethnic Region")
er_lm_full$term = c("(Intercept)", "10km", "20km", "30km", "40km", "50km", "10km", "20km", "30km", "40km", "50km")
er_lm_full %>% dwplot() + theme_bw() + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Effect of Violence Across Ethnic Region Boundaries", 
       subtitle = "Predictive power by 10km band, seperated by shared or different ethnic region",
       caption = "Figure 3") + scale_color_grey(start = 0, end = .6)
##ggsave(filename = "Combined Ethnic Regions Graph", device = "jpeg", path = c("../Writing/Figures"), 
##       height = 4, width = 6)

## create figure 4
pashtun_model <- lm(data = dtm_master, civ_dis ~ d_p_10 + d_p_20 + d_p_30 + d_p_40 + d_p_50 + 
                      d_np_10 + d_np_20 + d_np_30 + d_np_40 + d_np_50 + 
                      bdist1 + nlights_mean + ttime_mean + mountains_mean) %>% tidy()
pashtun_model <- pashtun_model %>% filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean")
pashtun_model$model <- c("NA", "Pashtun", "Pashtun", "Pashtun", "Pashtun", "Pashtun", 
                         "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun")
pashtun_model$term <- c("(Intercept)", "10km", "20km","30km","40km","50km",
                        "10km", "20km","30km","40km","50km")
pashtun_model %>% dwplot() + theme_bw() + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Pashtun-Specific Displacement Effects", 
       subtitle = "Predictive power by 10km band of pashtun/non-pashtun casualties",
       caption = "Figure 4")  + scale_color_grey(start = 0, end = .6)
##ggsave(filename = "Combined Pashtun-Specific Graph", device = "jpeg", path = c("../Writing/Figures"), 
##       height = 4, width = 6)

## create figure 5

idp_dist_lm <- lm(data = dtm_master, FledIDPs2016 ~ d_10 + d_20 + d_30 + d_40 + d_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean)
idp_dist_lm <- tidy(idp_dist_lm) %>% mutate(model = "IDP")
refugee_dist_lm <- lm(data = dtm_master, OutMigrants2016 ~ d_10 + d_20 + d_30 + d_40 + d_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean)
refugee_dist_lm <- tidy(refugee_dist_lm) %>% mutate(model = "Refugee")
full_join(idp_dist_lm, refugee_dist_lm) %>% 
  filter(term == "d_10"|term =="d_20"|term =="d_30"|term =="d_40"|term =="d_50") %>% 
  relabel_predictors(c(d_10 = "10km", d_20 = "20km", d_30 = "30km", d_40 = "40km", d_50 = "50km")) %>%
  dwplot() + theme_bw() + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Population-Specific Distance Decay", 
       subtitle = "Predictive power by 10km band, seperated by population type",
       caption = "Figure 5") + scale_color_grey(start = 0, end = .6)
##ggsave(filename = "Split Distance Decay Graph", device = "jpeg", path = c("../Writing/Figures"), 
##       height = 4, width = 6)

## create figure 6
er_lm_idp <- lm(data = dtm_master, FledIDPs2016 ~ d_se_10 + d_se_20 + d_se_30 + d_se_40 + d_se_50 + d_de_10 + d_de_20 + d_de_30 + d_de_40 + d_de_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean) 
er_lm_idp <- er_lm_idp %>% tidy()
er_lm_idp <- er_lm_idp %>% filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean")
er_lm_idp$model = c("NA", "Same Ethnic Region", "Same Ethnic Region","Same Ethnic Region","Same Ethnic Region","Same Ethnic Region",
                    "Different Ethnic Region", "Different Ethnic Region","Different Ethnic Region","Different Ethnic Region","Different Ethnic Region")
er_lm_idp$term = c("(Intercept)", "10km", "20km", "30km", "40km", "50km", "10km", "20km", "30km", "40km", "50km")
er_lm_idp <- er_lm_idp %>% mutate(pop = "IDPs")

er_lm_refugee <- lm(data = dtm_master, OutMigrants2016 ~ d_se_10 + d_se_20 + d_se_30 + d_se_40 + d_se_50 + d_de_10 + d_de_20 + d_de_30 + d_de_40 + d_de_50 + bdist1 + nlights_mean + ttime_mean + mountains_mean) 
er_lm_refugee <- er_lm_refugee %>% tidy()
er_lm_refugee <- er_lm_refugee %>% filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean")
er_lm_refugee$model = c("NA", "Same Ethnic Region", "Same Ethnic Region","Same Ethnic Region","Same Ethnic Region","Same Ethnic Region",
                        "Different Ethnic Region", "Different Ethnic Region","Different Ethnic Region","Different Ethnic Region","Different Ethnic Region")
er_lm_refugee$term = c("(Intercept)", "10km", "20km", "30km", "40km", "50km", "10km", "20km", "30km", "40km", "50km")
er_lm_refugee <- er_lm_refugee %>% mutate(pop = "Refugees")

full_join(er_lm_idp, er_lm_refugee) %>% 
  dwplot() + facet_grid(~pop) + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Ethnic Region Analysis by Population", 
       subtitle = "Predictive power by 10km band by shared or different ethnic region, seperated by population",
       caption = "Figure 6") + scale_color_grey(start = 0, end = .6)
##ggsave(filename = "Split Ethnic Regions Graph", device = "jpeg", path = c("../Writing/Figures"), 
##       height = 4, width = 6)

## create figure 7
pop_pashtun_model <- full_join(lm(data = dtm_master, FledIDPs2016 ~ d_p_10 + d_p_20 + d_p_30 + d_p_40 + d_p_50 + 
                                    d_np_10 + d_np_20 + d_np_30 + d_np_40 + d_np_50 + 
                                    bdist1 + nlights_mean + ttime_mean + mountains_mean) %>% tidy(), lm(data = dtm_master, OutMigrants2016 ~ d_p_10 + d_p_20 + d_p_30 + d_p_40 + d_p_50 + 
                                                                                                          d_np_10 + d_np_20 + d_np_30 + d_np_40 + d_np_50 + 
                                                                                                          bdist1 + nlights_mean + ttime_mean + mountains_mean) %>% tidy())
pop_pashtun_model <- pop_pashtun_model %>% filter(term != "mountains_mean" & term != "bdist1" & term != "nlights_mean" & term != "ttime_mean")
pop_pashtun_model$pop = c("NA", "IDP", "IDP", "IDP", "IDP", "IDP", "IDP", "IDP", "IDP", "IDP", "IDP",
                          "NA", "Refugee", "Refugee", "Refugee", "Refugee", "Refugee",
                          "Refugee", "Refugee", "Refugee", "Refugee", "Refugee")
pop_pashtun_model$model <- c("NA", "Pashtun", "Pashtun", "Pashtun", "Pashtun", "Pashtun", 
                             "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun",
                             "NA", "Pashtun", "Pashtun", "Pashtun", "Pashtun", "Pashtun", 
                             "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun", "Non_Pashtun")
pop_pashtun_model$term <- c("(Intercept)", "10km", "20km","30km","40km","50km",
                            "10km", "20km","30km","40km","50km",
                            "(Intercept)", "10km", "20km","30km","40km","50km",
                            "10km", "20km","30km","40km","50km")
pop_pashtun_model %>% dwplot() + facet_grid(~pop, scales = "free_y", "free_x") + geom_vline(xintercept = 0, color = "Grey60", linetype = 2) + 
  labs(title = "Pashtun-Specific Displacement Effects By Population", 
       subtitle = "Predictive power by 10km band by pashtun/non-pashtun casualties by population",
       caption = "Figure 7")  + scale_color_grey(start = 0, end = .6)
##ggsave(filename = "Split Pashtun-Specific Graph", device = "jpeg", path = c("../Writing/Figures"), 
##       height = 4, width = 6)

