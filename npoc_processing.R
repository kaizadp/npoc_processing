# R script for processing NPOC/TOC data from Shimadzu TOC analyzers
# Kaizad F. Patel
# February 2020
# ----------------------------------------------------------------------------------- -
# ----------------------------------------------------------------------------------- -

# STEP 1. load packages ----

library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(googlesheets)
library(readxl)
library(readr)  
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)       # 1.1.0

#

# STEP 2. set input and output files ----

WSOC_DATA = "input/wsoc" # this is the directory for input files. 
WSOC_KEY = "input/key.csv"
WSOC_WEIGHTS = "input/weights.csv"

WSOC_CALIB = "processed/calibration.csv"
CALIB_PLOT = "processed/calibration.tiff"
WSOC_RESULTS = "processed/wsoc_results.csv"

WATER_VOL = 40 # volume of water added for extraction, mL


### save all raw data files as .csv files in the directory "data/wsoc_data"
### make sure all the .csv files have the same column names

#

# STEP 3. import data files ---------------------------- ####

wsoc_raw = sapply(list.files(path = WSOC_DATA,pattern = "*.csv", full.names = TRUE),
                  read_csv, simplify = FALSE) %>% bind_rows()

# STEP 4. begin processing qith drake ----

wsoc_plan = drake_plan(
  
## i. import key ----
wsoc_key = read_csv(WSOC_KEY), # wsoc key
    
    #core_key = read.csv(COREKEY) %>% mutate(Core = as.character(Core)), # core key

## ii. process raw data
wsoc_processed = 
  wsoc_raw %>% 
# remove the appropriate rows
# in this case, we want Remove==NA and Excluded==1
  filter(is.na(Remove)) %>% 
  
# subset only the relevant columns, Sample Name and Area
  dplyr::select(`Sample Name`, Area) %>% 
  group_by(`Sample Name`) %>% 
  dplyr::summarise(Area = mean(Area)) %>% 
# the `Sample Name`` column is actually vial numbers, recorded as "Vial 1", "Vial 2", etc. 
# Create a new column Vial_no by removing the "Vial " string at the start of the name.
  mutate(Vial_no = str_remove(`Sample Name`, "Vial ")) %>%
  
# now combine with the wsoc_key
  left_join(select(wsoc_key, Run,Vial_no, Sample_name, Type,Calib_ppm,Dilution), by = "Vial_no", all.x=T),
  
# create a new file for just the calibration standards
wsoc_calib = 
  wsoc_processed %>% 
  filter(Type=="calibration"),

## ii.  calibration curves ---------------------------- ####
# plot calibration curve for each run 
gg_calib = 
  ggplot(wsoc_calib, aes(x = Area, y = Calib_ppm))+
  geom_smooth(method = "lm", color = "gray", alpha = 0.5, se = F)+
  geom_point()+
#  facet_wrap(~Run)+
  ggtitle("Calibration curve")+
  ylab("NPOC, mg/L")+
  theme_bw(),

ggsave(CALIB_PLOT, gg_calib, height = 7, width = 7),

# create new columns with the slope and intercept for each calibration curve
wsoc_calib_slope = 
  wsoc_calib %>% 
 # dplyr::group_by(Run) %>% 
  dplyr::summarize(slope = lm(Calib_ppm~Area)$coefficients["Area"], 
                   intercept = lm(Calib_ppm~Area)$coefficients["(Intercept)"]),

### if we are using a single calibration curve across all samples, then do this 
SLOPE = mean(wsoc_calib_slope$slope),
INTERCEPT = mean(wsoc_calib_slope$intercept),

#


## iii. calculate DOC concentrations ---------------------------- ####
# first, create a file of only the sample data. no calibration or qaqc data
wsoc_samples = 
  wsoc_processed %>% 
  filter(Type=="sample") %>% 
# next calculate DOC as y = mx + c
  mutate(npoc = Area*SLOPE + INTERCEPT) %>% 
# dilution conversions
  mutate(npoc_mg_l = round(npoc*Dilution,2)) %>% 
# subset only the relevant columns now
  dplyr::select(`Sample_name`,npoc_mg_l),

#


## iv. calculate as mg/g ---------------------------- ####

# first, we need to retrieve weights of soil used for the extraction
# use moist weight and moisture to calculate dry soil weight used
wsoc_weights = 
  read_csv(WSOC_WEIGHTS) %>% 
  dplyr::mutate(dry_weight_g  = round(moist_weight_g/((moisture_perc/100)+1),2),
                soil_water_g = moist_weight_g - dry_weight_g),

wsoc_results = wsoc_samples %>% 
  left_join(wsoc_weights, by = "Sample_name") %>% 
  dplyr::mutate(wsoc_mg_g = round(npoc_mg_l * (WATER_VOL+soil_water_g)/(dry_weight_g*1000),3)) %>% 
  dplyr::select(Sample_name, npoc_mg_l,wsoc_mg_g),

write.csv(wsoc_results, WSOC_RESULTS, row.names = F, na=""))

message("Now type:make(wsoc_plan)
GRRR drake")


#

