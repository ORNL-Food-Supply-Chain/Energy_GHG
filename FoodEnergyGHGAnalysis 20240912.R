# Library ----
{
  library(plyr)
  library(tidyverse)
  library(ggplot2)
  library(readxl) 
  library(scales)
  library(tidyr)
  library(maps)
  library(mapdata)
  library(cowplot)
  library(RColorBrewer)
  library(clipr)
  library(ggrepel)

  setwd("C:/Users/ak1/ORNL Dropbox/Kristina Armstrong/Armstrong Work/Food/Energy GHG Plots & Data/Analysis/GitHub")
  
  options(scipen=999) #turn off sci note
  
  options(dplyr.summarise.inform = FALSE) # turn off the summarise messages
}

# Theme, common functions ----
{
  THEME_simple <- theme_bw() + 
    theme(  plot.title = element_text(face = "bold", size = (12), hjust = 0.5)
            , text  = element_text(family = "serif")
            , legend.title = element_text(face = "bold.italic")
            , axis.title = element_text(size = (11))
            , axis.text = element_text(size = (10))
            , legend.text = element_text(size = (10))
            , legend.position = "bottom"
            , strip.background = element_blank()
            , panel.grid.major.y = element_blank()
            , panel.grid.major.x = element_blank()
            # , legend.box.margin=margin(-10,-10,-10,-10)
    )
  
  addline_format <- function(x,...){
    gsub('\\s','\n',x)
  }
  
}
# Colors ----
{
  COLOR_fuel_alt <- tibble( "Natural Gas"  = "#df8610" #orange
                            , "Petroleum"=	"#ffcd00" #yellow
                            , "Coal"= "#a03123" # red
                            , "Renewables"= "#84b641" # l.green
                            , "Other Fuel"= "#bf541b" # d. orange
                            , "Chemical Manuf."= "#306dbe" # d. blue
                            , "Animal Feed"= "#7030a0" # purple
                            , "Grid Electricity"	= "#2abdda" #l. blue
                            , "Crop Emissions" = "#1e7640" #d. green
                            , "Animal Emissions" = "#964B00" # brown
                            , "Process/Refrig." = "black"
  )
  
  
  
}
# Factor Levels ----
{
  
  LEVELS_fuel <- c( "Natural Gas", "Grid Electricity", "Petroleum", "Coal", "Renewables", "F&P Electricity"  , "F&P Fuel", "F&P Process", "Process",
                    "Other", "AF Manuf.","Crop Emissions", "Animal Emissions", "Refrigerant")
  LEVELS_fuel_red <- c( "Natural Gas", "Grid Electricity", "Petroleum", "Coal", "Renewables","Other Fuel",
                        "Chemical Manuf."  , "Animal Feed",
                        "Crop Emissions", "Animal Emissions", "Process/Refrig.")
  
  LEVELS_source <- c("Grid Electricity", "Fuel", "On-site Renewables", 
                     "AF Electricity", "AF Fuel", "AF Renewables",
                     "Fertilizer Manuf." , "Pesticide Manuf.",
                     "Animal Emissions", "Crop Emissions", "Rail", "Truck" , "Rail Refrig.", "Truck Refrig." 
                     , "Refrigerant", "AF Refrigerant")
  
  LEVELS_naics <- 
    c(        "Animal Feed"           ,     "Meat, Poultry, Eggs"    ,       "Dairy"       
              ,      "Animal Feed Processing"         ,  "Animal Product Processing"     , "Dairy Products"                  
              ,      "Fruit, Vegetables, Nuts"    ,      "Fruit, Vegetables, Nuts Products" 
              ,   "Grain, Oil"                    ,     "Grain, Oil Milling"          ,      "Bakeries, Tortillas"         
              ,   "Seafood"                       ,   "Seafood Processing"                                    
              ,  "Sugar"                        ,  "Sugar, Confectionary"   ,   "Other Fuel"   ,    "Other Products"        
              ,"Warehouse - Dry", "Warehouse - Cold",  "Retail" ,    "Residential"   , "Services"  , "Other"     )
  LEVELS_naicsLine <- addline_format(LEVELS_naics)
}
# Data Files ----
# This sets up a few data files that will be used in multiple parts of the analysis
{
  
  # State/region data file prep ----
  # Lookup files for connecting 
  # States with regions used in various data sources
  BD_stateRegion <- read_excel("Data/FSC - Background Data.xlsx", sheet = "censusRegions") %>% dplyr::rename(State_Abb = STATE)
  # NAICS codes with names
  BD_naicsLookup <- read_excel("Data/FSC - Background Data.xlsx",  sheet = "NAICS")
  
  # Primary Energy & GHGs state level data prep ----
  
  # Uses data on state electricity generation fuel use, filters for only 2016 and power industry (Excluding generation from industrial/comercial cogen etc.)
  # Coal includes anthracite, bituminous coal, subbituminous coal, lignite, waste coal, and synthetic coal.
  # Other includes non-biogenic municipal solid waste, batteries, chemicals, hydrogen, pitch, purchased steam, sulfur, tire-derived fuels, and miscellaneous technologies.
  # Other Biomass includes biogenic municipal solid waste, landfill gas, sludge waste, agricultural byproducts, other biomass solids, other biomass liquids, and other biomass gases (including digester gases and methane). 
  # Other Gases includes blast furnace gas, propane gas, and other manufactured and waste gases derived from fossil fuels.
  # Petroleum includes distillate fuel oil (all diesel and No. 1, No. 2, and No. 4 fuel oils), residual fuel oil (No. 5 and No. 6 fuel oils and bunker C fuel oil), jet fuel, kerosene, petroleum coke, and waste oil.
  # Wood and Wood Derived Fuels includes paper pellets, railroad ties, utility poles, wood chips, bark, red liquor, sludge wood, spent sulfite liquor, and black liquor, with other wood waste solids and wood-based liquids.
  # This data is available here: https://www.eia.gov/electricity/data/state/  "Net Generation by State by Type of Producer by Energy source (EIA-906, EIA-920, and EIA-923)
  
  GEN_stateALL<- read_excel("Data/US - State Electricity Generation and Emissions.xlsx", sheet = "Net_Generation_1990-2019 Final") %>% 
    dplyr::rename(
      "Year" = YEAR, "State_Abb" = STATE, "Producer" ="TYPE OF PRODUCER", "Source" = "ENERGY SOURCE", "Generation_MWh" =  "GENERATION (Megawatthours)" )
  
  GEN_state2016 <- GEN_stateALL %>% filter(Producer == "Total Electric Power Industry") %>% 
    filter(Year == 2016) %>% filter(Source != "Total")
  
  #create heat rate for each fuel type - based on values from EIA: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_7.pdf
  GEN_heatRate <- data.frame(
    Source = c("Coal" , "Geothermal"	, "Hydroelectric Conventional" , "Natural Gas"	, "Nuclear"
               , "Other"	, "Other Biomass"	, "Other Gases" , "Petroleum"	, "Pumped Storage" 
               , "Solar Thermal and Photovoltaic" , "Wind"	, "Wood and Wood Derived Fuels")
    , heatRate = c(10493 , 3412 , 3412 ,  7870 ,  10459
                   ,  9232 , 9232 , 9232   , 10811   ,	9232
                   , 	3412 , 3412 , 9232)) 
  
  # Join and calculate generation fuel use in TBTU based on MWH generated and heat rate (with unit conversions)
  GEN_state2016 <- left_join(GEN_state2016, GEN_heatRate, by = "Source") %>% dplyr::mutate(
    Generation_TBTU = Generation_MWh*1000*heatRate/1000000000000
  )
  # Calculate the % GENENERATED from each fuel type, for each state and fuel type
  GEN_state2016 <- GEN_state2016 %>% dplyr::group_by(State_Abb) %>% dplyr::mutate(percent = (Generation_MWh/sum(Generation_MWh)))
  
  # Pull in data on GHG's released from state level electricity generation by fuel type, filter 2016 & power industry, pull in generation fractions from GEN_state2016
  # This data is available here: https://www.eia.gov/electricity/data/state/  "U.S. Electric Power Industry Estimated Emissions by State (EIA-767, EIA-906, EIA-920, and EIA-923)"
  
  GEN_stateALLGHG<- read_excel("Data/US - State Electricity Generation and Emissions.xlsx",  sheet = "State Emissions")
  
  GEN_stateALLGHG <- GEN_stateALLGHG %>% dplyr::rename(
    "State_Abb" = State, "Producer" ="Producer Type", "Source" = "Energy Source", "CO2_tonnes" =  "CO2\r\n(Metric Tons)", "NOx_tonnes" = "NOx\r\n(Metric Tons)") %>%
    select(-"SO2\r\n(Metric Tons)", -NOx_tonnes)
  
  GEN_state2016GHG <- GEN_stateALLGHG %>% filter(Producer == "Total Electric Power Industry") %>% 
    filter(Year == 2016) %>% filter(Source != "All Sources") %>% left_join(select(GEN_state2016,State_Abb, Source, percent, Generation_MWh), by = c("State_Abb", "Source"))
  
  #calculate CO2 equivalent for NOx with unit conversions
  GEN_state2016GHG <- GEN_state2016GHG %>% dplyr::mutate(
    CO2e_MMT = (CO2_tonnes)/1000000  
    ,CO2e_MMT_TBTU = ((CO2_tonnes)/Generation_MWh) * (1000/3412) # (MWh/1000kWh)*(kWh/3412 Btu)*(1,000,000,000,000 Btu/TBTU)*(MMT/1,000,000 t)
  )
  
  # Pull in data about Generation losses by state
  # This data is available at https://www.eia.gov/electricity/state/archive/2016/ but you need to pull files for all states
  # The authors emailed the EIA to receive the consolidated data file
  GEN_tdlosses <- read_excel("Data/US - State Level Generation, Use, Losses.xlsx", sheet = "2016", 
                             col_types = c("numeric", "text", "skip", "skip", "skip", "numeric", "skip", "skip", "skip", "skip", "numeric", 
                                           "skip", "skip", "skip", "skip", "skip", "numeric", "numeric", "numeric", 
                                           "skip", "skip", "skip", "skip")) %>% 
    rowwise() %>% dplyr::mutate(      #All units in MWh
      negDIRECT_USE = -1 * DIRECT_USE 
      , negEXPORTS = -1 * EXPORTS
      , denom = sum(c_across(c(GENERATION_EPS, IMPORTS , negDIRECT_USE, negEXPORTS)),na.rm = TRUE)
      , tdLossFactor = 1 + ESTIMATED_LOSSES/denom
    ) %>% dplyr::rename("State_Abb" = "STATE")
  
  #Add tdLossFactor to both generation & GHG tables
  # These are the tables that will get combined with electricity use to figure out source energy
  
  GEN_state2016 <- GEN_state2016 %>% left_join(select(GEN_tdlosses, State_Abb, tdLossFactor), by = "State_Abb")
  GEN_state2016GHG <- GEN_state2016GHG %>% left_join(select(GEN_tdlosses, State_Abb, tdLossFactor), by = "State_Abb")
  
  GEN_state2016 %>% select(State_Abb, percent, tdLossFactor, Source) %>% 
    filter(State_Abb != "DC") %>%  filter(State_Abb != "US-Total") %>%
    mutate(    percent = percent(percent, accuracy = 0.1),
               tdLossFactor = round(tdLossFactor, digits = 3)) %>%
    pivot_wider(names_from = Source, values_from = percent)%>%
    mutate( across(everything(), ~replace_na(.x, "0.0%"))) 
  
  # When state level use is not available - use U.S. averages
  GEN_US2016 <- aggregate(Generation_MWh ~ Source,GEN_state2016, FUN = sum) %>% left_join(GEN_heatRate, by = "Source") %>% mutate(
    Generation_TBTU = Generation_MWh*1000*heatRate/1000000000000 # MWh *(1000kWh/MWh)* Btu/kWh * (TBTU/1000,000,000,000 Btu)
    , percent = (Generation_MWh/sum(Generation_MWh))
  ) %>% dplyr::rename("Fuel" = Source)
  
  GEN_US2016GHG <- aggregate(CO2e_MMT ~ Source,GEN_state2016GHG, FUN = sum) %>% dplyr::rename("Fuel" = Source) %>% 
    left_join(select(GEN_US2016, Fuel, percent, Generation_MWh), by = c("Fuel"))%>% 
    mutate(CO2e_MMT_TBTU = (CO2e_MMT/Generation_MWh) *(1000000000/3412) # (MWh/1000kWh)*(kWh/3412 Btu)*(1,000,000,000,000 Btu/TBTU)
    )
  
  GEN_tdLossFactorUS <- 1 + sum(GEN_tdlosses$ESTIMATED_LOSSES)/sum(GEN_tdlosses$denom)
  
  # Animal feed breakdown ----
  # Several portions of animal feed calculations in "Farm" and "Manufacturing" require separating animal feed values into 
  # "animals for human consumption" and "pets/others" 
  # Data from : http://ifeeder.org/wp-content/uploads/210301-FINAL-REPORT-IFEEDER-Animal-Feed-Food-Consumption-COVID-19.pdf
  
  BD_animalFeedbyEater <- data.frame(
    animal = c( 'Beef Cattle','Hogs','Broilers','Dairy Cattle','Egg-Layers','Turkeys','Pets (cats and dogs)','Horses','Aquaculture','Sheep and Meat Goats')
    ,   animalx = c( 'Beef Cattle','Hogs','Broilers','Dairy Cattle','Egg-Layers','Turkeys','Pets','Horses','Aquaculture','Sheep/Goats')
    
    , feedTons = c( 64.5,61.8,60.8,49.8,19.2,10.6,8.6,7.7,0.613,0.138783)
  ) %>% mutate(percent = feedTons/sum(feedTons))
  
  BD_percentNotPet  <- sum(filter(BD_animalFeedbyEater, animal != "Pets (cats and dogs)")$percent)
  BD_percentFeedDairyCowinNotPet <- unlist(filter(BD_animalFeedbyEater, animal == "Dairy Cattle")$percent)/BD_percentNotPet
  BD_percentFeednotDairyinNotPet <- sum(filter(BD_animalFeedbyEater, animal != "Dairy Cattle" & animal != "Pets (cats and dogs)")$percent)/BD_percentNotPet
  
}

# Farm  ----
{
  # Farm data prep ----
  #This sets up data files that will be used through out the farm-level analysis
  # Sugar, Peanuts, Hay breakdown ----
  # NASS data broken by NAICS code combines sugar beats, sugar cane, peanuts and hay.
  # This is a way to estimate each of those commodities' contributions by the relative amount produced per state
  #USDA-NASS: https://quickstats.nass.usda.gov/results/EB602DDB-76B9-39B7-A80A-C72180CC9EF6
  FARM_sugarsPeanutsHayraw <- suppressWarnings(read_excel("Data/Farm - USDA-NASS Other Farm Data.xlsx", sheet = "sugarsPeanutsHay", 
                                         col_types = c("text", "numeric", "text", "numeric", "text",  "text", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric",  "numeric", "numeric", "numeric",   "text", 
                                                       "text", "text", "text", "numeric",  "text")))
  
  FARM_sugarsPeanutsHay <- FARM_sugarsPeanutsHayraw %>% select(State, Commodity, Value) %>% pivot_wider(names_from = Commodity, values_from = Value)
  FARM_sugarsPeanutsHayTotal <- FARM_sugarsPeanutsHay %>% filter(State == "US TOTAL")
  FARM_fractionHAYremainder <- (unlist(as.double(FARM_sugarsPeanutsHayTotal$HAY)) - sum(as.double(filter(FARM_sugarsPeanutsHay, State != "US TOTAL")$HAY), na.rm = TRUE))/dplyr::count(filter(FARM_sugarsPeanutsHay, is.na(HAY)))
  
  FARM_sugarsPeanutsHay <- FARM_sugarsPeanutsHay %>% filter(State != "US TOTAL") %>% rowwise() %>% dplyr::mutate(
    HAY_noD = ifelse(is.na(HAY), unlist(FARM_fractionHAYremainder), HAY)
    , PEANUTS_tons = PEANUTS/2000
    , sugarsPeanutsHay = sum(c_across(c( HAY_noD, SUGARBEETS, SUGARCANE, PEANUTS_tons)),na.rm = TRUE)
    , stateName = str_to_title(State)
  ) %>% select(-HAY, -State, -PEANUTS)
  
  BD_peanutsTons <- select(FARM_sugarsPeanutsHay, stateName, PEANUTS_tons)
  
  FARM_sugarsPeanutsHay <- FARM_sugarsPeanutsHay %>% pivot_longer(!c(stateName, sugarsPeanutsHay), names_to="commodity_naics", values_to="Tons") %>% rowwise() %>% dplyr::mutate(
    fraction = Tons / sugarsPeanutsHay) %>% select(stateName, fraction, commodity_naics) 
  
  # Cows ----
  # There is data where it is necessary to separate beef cattle from dairy cows. It is split using the relative number of cows
  FARM_cows <- suppressWarnings(read_excel("Data/Farm - USDA-NASS Other Farm Data.xlsx",  sheet = "DairyVSBeef_raw", 
                                           col_types = c("skip",  "skip", "text", "skip", "skip", "text",  "skip", "skip", "skip", "skip", "skip",   "skip", 
                                                         "skip", "skip", "skip", "skip", "text", "skip", "skip", "numeric",  "skip")))
  # Warnings were suppressed - from forcing "(D)" as a number
  # USDA-NASS: https://quickstats.nass.usda.gov/results/136587FC-79DB-3F71-9818-2F1D1267A5B1
  
  FARM_cows <-  filter(FARM_cows, `Data Item` == "CATTLE, COWS, MILK - INVENTORY") %>% group_by(State) %>% summarise_at(vars(Value), list(Cows = mean), na.rm = TRUE)
  FARM_cows <- FARM_cows %>% dplyr::mutate(stateName = str_to_title(State)) %>% select(-State)
  
  # Fuel & Electricity
  # Fuel & Electricity price data prep ----
  # USDA only provides fuel *expenditure* data, so fuel prices for the year of data were necessary
  #prices for electricity, LP gas, natural gas
  FARM_Price2017_state <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_Price2017_state")
  # Electricity Prices: https://www.eia.gov/electricity/state/archive/2017/
  # NG prices: http://www.eia.gov/dnav/ng/ng_pri_sum_a_epg0_pin_dmcf_a.htm
  # prices for gasoline & diesel
  FARM_gas_Diesel_Price2017_region <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_gas_Diesel_Price2017_region")
  # Gas & Diesel prices: http://www.eia.gov/oil_gas/petroleum/data_publications/wrgp/mogas_history.html
  # West coast prices: http://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_r5xca_w.htm
  
  #HHV from https://www.eia.gov/energyexplained/units-and-calculators/british-thermal-units.php
  
  FARM_stateData <-
    BD_stateRegion %>% left_join(FARM_Price2017_state, by = "stateName")  %>% 
    select(-Region) %>% 
    rbind(BD_stateRegion %>% 
            left_join(FARM_gas_Diesel_Price2017_region, by = c("Region_PADD"), relationship = "many-to-many") %>% 
            dplyr::rename(Fuel_HHV_MMBTU_x = Fuel_HHV_MMBTU_gal, Price_USD2017_x = FuelPrice_USD2017_gal) %>% 
            dplyr::mutate(x = "gal") %>% select(-region, -Region))
  
  # AK & HI do not have fuel NAICS breakdowns.  This sets up a table with prices & HHVs. 
  # The "Other" in HI and AK seems to mostly be jet fuel. see link below
  FARM_AK_HI_Data <- FARM_stateData %>% filter(State_Abb == "AK" | State_Abb == "HI") %>% filter(Fuel != "Electricity")
  FARM_AK_HI_Data <- FARM_AK_HI_Data %>% 
    rbind(data.frame(
      stateName = c("Alaska", "Hawaii")
      , State_Abb = c("AK", "HI")
      , Region_small = c("West", "West")
      , Region_ARMS = c("AK", "HI")
      , Region_PADD = c("PADD5B","PADD5B")
      , Price_USD2017_x = c(1.633, 1.633)
      , Fuel = c("Other", "Other")
      , x = c("gal", "gal")
      , Fuel_HHV_MMBTU_x = c(.170	, .170) 
    ) # 5.355 MMBtu/ barrel = .17 MMBTU/gal HHV from: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf; 
    # Heating oil price from: https://www.eia.gov/dnav/pet/pet_sum_mkt_a_EPJK_PTG_dpgal_a.htm)
    )
  
  FARM_stateData <- FARM_stateData %>% rbind(filter(FARM_AK_HI_Data, Fuel == "Other"))
  
  # Fuel ----
  # Data is from USDA NASS. It is $ spent on farm for utilities and fuel by state/region and NAICS/commodity.
  {
    #Fuel expense by NAICS and State https://quickstats.nass.usda.gov/results/687F1CA3-1B12-3463-88A6-49C8D2E8170D
    #State + NAICS level fuel expenditures
    FARM_Fuel_NAICS_State_raw <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_Fuel_NAICS_State")
    #State level fuel expenditures
    FARM_Fuel_US <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_fuel_US")
    
    # Separate out non-specified other and specified other from other category, determine amount of fuel not specified in NAICS level data (remainder)  
    FARM_Fuel_NAICS_State <- FARM_Fuel_NAICS_State_raw %>% left_join(BD_naicsLookup, by = "NAICS") %>% select(!NAICS) %>% pivot_wider(names_from = commodity_naics, values_from = Fuel_w_lube_USD2017) 
    FARM_Fuel_NAICS_State <- FARM_Fuel_NAICS_State %>% rowwise()%>%
      dplyr::mutate(specificOther =  sum(c_across(c(Tobacco, Cotton , sugarcane_hay_sugarBeet_peanut)),na.rm = TRUE)) 
    
    FARM_Fuel_NAICS_State <- FARM_Fuel_NAICS_State %>% rowwise() %>%
      dplyr::mutate( otherOther = Other - specificOther)
    
    FARM_Fuel_NAICS_State <- FARM_Fuel_NAICS_State %>% select(-Other, -specificOther)  %>% 
      dplyr::mutate(sumFuel_USD2017 =  sum(c_across(!stateName),na.rm = TRUE)) 
    
    FARM_FuelStateRemainders <- FARM_Fuel_NAICS_State %>% select(stateName, sumFuel_USD2017, otherOther) %>% left_join(select(FARM_Fuel_US, stateName, Fuel_w_lube_USD2017), by = "stateName") %>%
      dplyr::mutate(remainder = Fuel_w_lube_USD2017 - sumFuel_USD2017)
    
    FARM_Fuel_NAICS_State <- FARM_Fuel_NAICS_State %>% left_join(select(FARM_FuelStateRemainders, stateName, remainder), by = "stateName")
    
    # DATA ADJUSTMENTS allocate remainders and separate/aggregate commodities- 
    # states with large remainders: AK, CT, DE, GA, HI, LA, MA, MS == states with (D) entries
    # if dairy was one with (D) --> dairy fuel = # dairy cows * avg fuel/cow,
    #if MPE = (D) meat/poultry/eggs = sum(other meats) + remainder - dairy, if OG = remainder - dairy
    # if feed lots & grain/oil had (D) --> each gets half of remainder
    
    # separate sugar, hay, peanuts 
    # other = sum(otherOther, remainder) except for states with (D)s
    # separate grain/oil food vs feed
    # reduce cotton to 17% (Dong et al 2021)
    # combine animal products, combine veg/fruit, combine cotton & grain/oil, combine feeds, combine peanuts & other
    
    FARM_sugarsPeanutsHayFuel <- FARM_sugarsPeanutsHay %>% left_join(select(FARM_Fuel_NAICS_State, stateName, sugarcane_hay_sugarBeet_peanut), by = "stateName") %>% rowwise() %>% dplyr::mutate(
      fuelSSHP = fraction *sugarcane_hay_sugarBeet_peanut)
    FARM_sugarsPeanutsHayFuel <- FARM_sugarsPeanutsHayFuel %>% select(-fraction, -sugarcane_hay_sugarBeet_peanut ) %>% 
      pivot_wider(names_from = commodity_naics, values_from = fuelSSHP) %>% dplyr::rename(Other1 = PEANUTS_tons, Feed1 = HAY_noD) 
    FARM_sugarsPeanutsHayFuel <- FARM_sugarsPeanutsHayFuel %>% rowwise() %>% dplyr::mutate(Sugar = sum(c_across(c(SUGARBEETS, SUGARCANE)),na.rm = TRUE)) %>% select(-SUGARBEETS, -SUGARCANE)
    
    # Cow fuel intensity
    FARM_cowsFuel <-FARM_Fuel_NAICS_State %>% select(stateName, dairyCattle) %>% left_join(FARM_cows, by = "stateName") %>% 
      dplyr::mutate(EI_cow = dairyCattle/Cows)
    # FARM_cowsFuel <- na_if(FARM_cowsFuel, 0)
    FARM_cowsFuelEI <- mean(FARM_cowsFuel$EI_cow, na.rm = TRUE)
    
    # Final adjustments to each state/NAICS combo
    FARM_Fuel_NAICS_State_adj <- FARM_Fuel_NAICS_State %>% select(stateName, veg_melon, fruit_treeNut,oilSeed_Grain, Cotton, dairyCattle, beefCattleRanch, beefFeedLots, hog_pig, poultry_egg, sheep_goat, aquaculture_other, otherOther, remainder) %>%
      left_join(select(FARM_cows, stateName, Cows), by = "stateName") %>% left_join(FARM_sugarsPeanutsHayFuel, by = "stateName")
    
    FARM_Fuel_NAICS_State_adj <- FARM_Fuel_NAICS_State_adj %>% rowwise() %>% dplyr::mutate(
      Dairy = ifelse(dairyCattle == 0, Cows * FARM_cowsFuelEI, dairyCattle) # if dairy == (D), dairy = cows * cowsEI
      , halfremainder = 0.5 * remainder
      , negDairy = -1 * Dairy
      , meat_poultry1 = sum(c_across(c(beefFeedLots, beefCattleRanch, hog_pig, poultry_egg, sheep_goat)), na.rm = TRUE) # feedlots != (D), meat = sum(all meat)
      , meat_poultry_D = sum(c_across(c(             beefCattleRanch, hog_pig, poultry_egg, sheep_goat, remainder, negDairy)),na.rm = TRUE) # Feedlots & Dairy == (D), meat = sum(other meat) + remainder - dairy
      , meat_poultry_O = sum(c_across(c(             beefCattleRanch, hog_pig, poultry_egg, sheep_goat, halfremainder)),na.rm = TRUE)  # Feedlots & oilGrain == (D), meat = sum(other meat) + 1/2 remainder
      ,meat_poultry = 
        ifelse(is.na(beefFeedLots),meat_poultry1 , 
               ifelse(beefFeedLots !=0, meat_poultry1, 
                      ifelse(dairyCattle==0, meat_poultry_D,  
                             meat_poultry_O) ))
      , Cotton2 = 0.17 * Cotton
      , oilGrainAll = ifelse(oilSeed_Grain == 0, ifelse(dairyCattle ==0, remainder, halfremainder)# if oilGrain & Dairy == (D), full remainder, #if oilGrain & feedlots == (D), half remainder,
                             , oilSeed_Grain)# else, oilSeed_Grain; DO NOT YET add 17% Cotton
      , Other2 = ifelse(remainder < 5000, sum(c_across(c(remainder,otherOther)), na.rm = TRUE), otherOther) #honestly cheating a bit, but the only way remainder > 5000 is if it is used up above
      , Other = ifelse(sum(c_across(c(Other1,Other2)), na.rm = TRUE) < 0, Other1, sum(c_across(c(Other1,Other2)), na.rm = TRUE))
      , oilGrain_Food = oilGrainAll * 0.186931
      , oilGrain = sum(c_across(c(oilGrain_Food, Cotton2)), na.rm = TRUE)
      , oilGrain_feed = oilGrainAll * 0.432122
      , Feed = sum(c_across(c(oilGrain_feed,Feed1)), na.rm = TRUE) 
      , Fruit_veg = sum(c_across(c(veg_melon,fruit_treeNut)), na.rm = TRUE) 
    )
    
    #Final allocation of state + commodity fuel expenditures (don't know breakdown by fuel type)
    FARM_Fuel_NAICS_State_adj <- FARM_Fuel_NAICS_State_adj %>% select(stateName, oilGrain, meat_poultry,Dairy, Fruit_veg, Sugar, aquaculture_other, Other, Feed) %>%
      pivot_longer(!stateName, names_to = "commodity_naics", values_to = "Fuel_w_lube_USD2017")
    
    #Allocate fuel types
    # Fuel expenditures by fuel type + region. Assume each state & commodity in a region has the same fuel use breakdown. 
    # Fuel expense by type and Region https://quickstats.nass.usda.gov/results/0FB579E9-F953-371A-95C8-906B1D54CC7D
    FARM_Fuel_Region_Type <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx",  sheet = "R_Fuel_Region_Type")
    
    
    FARM_Fuel_Region_Type <- FARM_Fuel_Region_Type %>% pivot_wider(names_from = Fuel, values_from = Fuel_USD2017) 
    FARM_Fuel_Region_Type <- FARM_Fuel_Region_Type %>% rowwise() %>%
      dplyr::mutate(Fuel_use = sum(c_across(!Region_ARMS), na.rm = TRUE))
    FARM_Fuel_Region_Type <- FARM_Fuel_Region_Type %>% pivot_longer(!c(Region_ARMS, Fuel_use), names_to = "Fuel", values_to = "Fuel_USD2017") %>% 
      rowwise() %>% dplyr::mutate(fraction = Fuel_USD2017/Fuel_use)
    
    # AK & HI do not have agricultural fuel use breakdown (ARMS), subbing in overall industrial fractions, 
    # also this is TBTU of fuel, not fuel spend, so have to "convert to USD"
    # Data from
    # https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/ind/use_ind_HI.html&sid=HI
    # https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/ind/use_ind_AK.html&sid=AK
    # assume other = jet fuel - https://www.eia.gov/state/analysis.php?sid=HI
    FARM_FRT_AK_HI <- data.frame(
      Region_ARMS = c(rep("AK", 5), rep("HI", 5))
      , Fuel = rep(c("Natural Gas", "Diesel", "Gasoline", "LP Gas", "Other"), 2) # Other = Residual fuel oil,"Other" which is mostly more petro based stuff; "propane" is actually HGL
      ,Fuel_use_TBTUi = c(279.2, 11.4, 0.5, 0.1 , 0.0 + 29.1, 0.1, 1.5, 1.5, 0.4 , 2.8 + 15.2) # from EIA Table CT6, see below - Ind energy use #"LP Gas" = "HGL", Other = Residual & "Other
    ) 
    
    FARM_FRT_AK_HI <-FARM_FRT_AK_HI %>% left_join(FARM_AK_HI_Data, by = c("Region_ARMS", "Fuel"))%>% 
      rowwise() %>%
      dplyr::mutate(Fuel_USD2017 = Price_USD2017_x *1000000/Fuel_HHV_MMBTU_x) %>% 
      select(Region_ARMS, Fuel, Fuel_USD2017) %>% 
      pivot_wider(names_from = Fuel, values_from= Fuel_USD2017) %>%   
      dplyr::mutate(Fuel_use = sum(c_across(!Region_ARMS), na.rm = TRUE))%>% 
      pivot_longer(!c(Region_ARMS, Fuel_use), names_to = "Fuel", values_to = "Fuel_USD2017") %>% 
      rowwise() %>% dplyr::mutate(fraction = Fuel_USD2017/Fuel_use)
    
    FARM_Fuel_Region_Type <- FARM_Fuel_Region_Type %>% rbind(FARM_FRT_AK_HI) %>% 
      select(-Fuel_use, -Fuel_USD2017) %>% 
      left_join(select(BD_stateRegion, -Region, -Region_small), by = "Region_ARMS", relationship = "many-to-many")
    
    FARM_Fuel_NAICS_State_adj <- FARM_Fuel_NAICS_State_adj %>% left_join(FARM_Fuel_Region_Type, by = "stateName", relationship = "many-to-many")
    
    # math is a bit different between Diesel/Gasoline and other fuels. splitting data frame and rejoining to make it easier
    FARM_Fuel_NAICS_State_DG <- FARM_Fuel_NAICS_State_adj %>% filter(Fuel == "Diesel" | Fuel == "Gasoline") %>% left_join(select(FARM_gas_Diesel_Price2017_region, -region), by = c("Fuel", "Region_PADD")) %>%
      dplyr::mutate(Energy_TBTU = (Fuel_w_lube_USD2017 * fraction / FuelPrice_USD2017_gal) * Fuel_HHV_MMBTU_gal * (1/ 1000000) ) # convert MMBTU to TBTU
    
    FARM_Fuel_NAICS_State_LPO <- FARM_Fuel_NAICS_State_adj %>% filter(Fuel == "LP Gas" | Fuel == "Natural Gas" | Fuel == "Other") %>% 
      left_join(filter(FARM_stateData,Fuel == "LP Gas" | Fuel == "Natural Gas" | Fuel == "Other"), by = c("stateName", "Region_ARMS", "Fuel", "State_Abb", "Region_PADD")) %>%
      dplyr::mutate(Energy_TBTU = ((Fuel_w_lube_USD2017 * fraction / Price_USD2017_x) * Fuel_HHV_MMBTU_x * (1/ 1000000) )) # convert MMBTU to TBTU
    
    #Estimated on-farm fuel use disaggregated by state, fuel type, commodity
    #on site renewable energy is handled in own section
    F_FARM_FuelTBTU <- rbind(select(FARM_Fuel_NAICS_State_DG, stateName, commodity_naics, Fuel, State_Abb, Energy_TBTU), select(FARM_Fuel_NAICS_State_LPO, stateName, commodity_naics, Fuel, State_Abb, Energy_TBTU))
    F_FARM_FuelTBTU$Source <- "Fuel"
  }
  
  # Electricity ----
  # Data is from USDA NASS. It is $ spent on farm for utilities and fuel by state/region and NAICS/commodity.
  {
    
    # This sheet combines data pull for utilities and water, converts water $2018 to $2017, utility - water / utility
    # compiled from 2018 water expenditures by state and the 2017 total state-level utility expenditures
    #USDA-NASS: Utility expenditure by state:  https://quickstats.nass.usda.gov/results/ADC4B2C4-2515-3E77-A1D0-FDD8719576E2
    # USDA-NASS: Water expenditure by state: https://quickstats.nass.usda.gov/results/CD9FE015-E028-3A78-BD2C-B8F47AD08FE3
    FARM_Utility_Water_State <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_Utility_Water_state") %>%
      rowwise() %>% mutate(
        Water_USD2017 = Water_USD2018 / 1.024 
        , Utility_noWater_USD2017 = Utilities_USD2017 - Water_USD2017
        , frac_notWater = Utility_noWater_USD2017/Utilities_USD2017
      )
    
    #USDA-NASS: Utility expenditure by state:  https://quickstats.nass.usda.gov/results/ADC4B2C4-2515-3E77-A1D0-FDD8719576E2
    FARM_Utility_State_NAICS_raw <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "R_Utility_State_NAICS")
    
    # Separate out non-specified other and specified other from other category, determine amount of fuel not specified in NAICS level data (remainder)  
    # Same method as fuels
    FARM_Utility_State_NAICS <- FARM_Utility_State_NAICS_raw %>% left_join(BD_naicsLookup, by = "NAICS") %>% select(!NAICS) %>% pivot_wider(names_from = commodity_naics, values_from = Utilities_USD2017) 
    FARM_Utility_State_NAICS <- FARM_Utility_State_NAICS %>% rowwise()%>%
      dplyr::mutate(specificOther =  sum(c_across(c(Tobacco, Cotton , sugarcane_hay_sugarBeet_peanut)),na.rm = TRUE)) 
    
    FARM_Utility_State_NAICS <- FARM_Utility_State_NAICS %>% rowwise() %>%
      dplyr::mutate( otherOther = Other - specificOther)
    
    FARM_Utility_State_NAICS <- FARM_Utility_State_NAICS %>% select(-Other, -specificOther) %>% 
      dplyr::mutate(sumUtilities_USD2017 =  sum(c_across(!stateName),na.rm = TRUE)) 
    
    # Determine how much is left over when you sum the utilities for each NAICS over a state, subtract sum from the utilities total. 
    # The remainder is to be allocated to the NAICS with (D) in the original data file on a mass production basis.
    FARM_utilityStateRemainders <-  FARM_Utility_State_NAICS %>% select(stateName, sumUtilities_USD2017, otherOther) %>% 
      left_join(select(FARM_Utility_Water_State, stateName, Utilities_USD2017), by = "stateName") %>%
      dplyr::mutate(remainder = Utilities_USD2017 - sumUtilities_USD2017)
    
    FARM_Utility_State_NAICS <- FARM_Utility_State_NAICS %>% left_join(select(FARM_utilityStateRemainders, stateName, remainder), by = "stateName")
    
    # filter(Utility_State_NAICS_raw, Utilities_USD2017==0)%>% left_join(BD_naicsLookup)
    # states with large remainders: ME, HI, DE, AZ, GA, LA == states with (D) entries
    # if dairy was one with (D) --> dairy utility = # dairy cows * avg utility/cow, meat/poultry/eggs = sum(other meats) + remainder - dairy
    # if dairy NOT one with (D) --> meat/poultry/eggs = sum(other meats) + remainder
    
    # separate sugar, hay, peanuts
    # other = sum(otherOther, remainder) except for states with (D)s
    # separate grain/oil food vs feed
    # reduce cotton to 17%
    # combine animal products, combine veg/fruit, combine cotton & grain/oil, combine feeds, combine peanuts & other
    
    FARM_sugarsPeanutsHayUtil <- FARM_sugarsPeanutsHay %>% 
      left_join(select(FARM_Utility_State_NAICS, stateName, sugarcane_hay_sugarBeet_peanut), by = "stateName") %>% 
      rowwise() %>% dplyr::mutate(  fuelSSHP = fraction *sugarcane_hay_sugarBeet_peanut)
    
    FARM_sugarsPeanutsHayUtil <- FARM_sugarsPeanutsHayUtil %>% select(-fraction, -sugarcane_hay_sugarBeet_peanut ) %>% 
      pivot_wider(names_from = commodity_naics, values_from = fuelSSHP) %>% dplyr::rename(Other1 = PEANUTS_tons, Feed1 = HAY_noD) 
    
    FARM_sugarsPeanutsHayUtil <- FARM_sugarsPeanutsHayUtil %>% rowwise() %>% dplyr::mutate(Sugar = sum(c_across(c(SUGARBEETS, SUGARCANE)),na.rm = TRUE)) %>% select(-SUGARBEETS, -SUGARCANE)
    
    FARM_cowsUtilities <-FARM_Utility_State_NAICS %>% select(stateName, dairyCattle) %>% left_join(FARM_cows, by = "stateName") %>% dplyr::mutate(
      EI_cow = dairyCattle/Cows)
    # FARM_cowsUtilities <- na_if(FARM_cowsUtilities, 0)
    FARM_cowsUtilitiesEI <- mean(FARM_cowsUtilities$EI_cow, na.rm = TRUE)
    
    FARM_Utilities_NAICS_State_adj <- FARM_Utility_State_NAICS %>% select(stateName, veg_melon, fruit_treeNut,oilSeed_Grain, Cotton, dairyCattle, beefCattleRanch, beefFeedLots, hog_pig, poultry_egg, sheep_goat, aquaculture_other, otherOther, remainder) %>%
      left_join(select(FARM_cows, stateName, Cows), by = "stateName") %>% left_join(FARM_sugarsPeanutsHayUtil, by = "stateName")
    
    FARM_Utilities_NAICS_State_adj <- FARM_Utilities_NAICS_State_adj %>% rowwise() %>% dplyr::mutate(
      Dairy = ifelse(dairyCattle == 0, Cows * FARM_cowsUtilitiesEI, dairyCattle) # if dairy == (D), dairy = cows * cowsEI
      , negDairy = -1 * Dairy
      , meat_poultry1 = sum(c_across(c(beefFeedLots, beefCattleRanch, hog_pig, poultry_egg, sheep_goat)), na.rm = TRUE) # feedlots != (D), meat = sum(all meat)
      , meat_poultry_D = sum(c_across(c(beefCattleRanch, hog_pig, poultry_egg, sheep_goat, remainder, negDairy)),na.rm = TRUE) # Feedlots & Dairy == (D), meat = sum(other meat) + remainder - dairy
      , meat_poultry_nD = sum(c_across(c(beefCattleRanch, hog_pig, poultry_egg, sheep_goat, remainder)),na.rm = TRUE)  # Feedlots & oilGrain == (D), meat = sum(other meat) + 1/2 remainder
      , meat_poultry =
        ifelse(is.na(beefFeedLots), meat_poultry1 , ifelse(beefFeedLots !=0, meat_poultry1, ifelse(dairyCattle==0, meat_poultry_D,   meat_poultry_nD) ))
      , Cotton2 = 0.17 * Cotton
      , Other2 = ifelse(remainder < 5000, sum(c_across(c(remainder,otherOther)), na.rm = TRUE), otherOther) #honestly cheating a bit, but the only way remainder > 5000 is if it is used up above
      , Other = ifelse(sum(c_across(c(Other1,Other2)), na.rm = TRUE) < 0, Other1, sum(c_across(c(Other1,Other2)), na.rm = TRUE))
      , oilGrain_Food = oilSeed_Grain * 0.186931
      , oilGrain = sum(c_across(c(oilGrain_Food,Cotton2)), na.rm = TRUE)
      , oilGrain_feed = oilSeed_Grain * 0.432122
      , Feed = sum(c_across(c(oilGrain_feed,Feed1)), na.rm = TRUE)
      , Fruit_veg = sum(c_across(c(veg_melon,fruit_treeNut)), na.rm = TRUE)
    )
    
    # Final allocation of state + commodity utility expenditures (still have not separated water & electricity)
    FARM_Utilities_NAICS_State_adj <- FARM_Utilities_NAICS_State_adj %>% select(stateName, oilGrain, meat_poultry,Dairy, Fruit_veg, Sugar, aquaculture_other, Other, Feed)
    FARM_Utilities_NAICS_State_adj <- FARM_Utilities_NAICS_State_adj %>% pivot_longer(!stateName, names_to = "commodity_naics", values_to = "Utilities_USD2017")
    
    # convert to electricity expenditures using state level data on water vs electricity
    # assume each commodity spends the same relative amount on water
    # convert to electricity use from state-level pricing (assume each commodity has the same price per state)
    FARM_electricity_State_NAICS <- FARM_Utilities_NAICS_State_adj %>% 
      left_join(select(FARM_Utility_Water_State, stateName, frac_notWater), by = "stateName") %>% 
      left_join(select(filter(FARM_stateData, Fuel == "Electricity"), stateName, State_Abb, Price_USD2017_x), by = "stateName")
    
    FARM_electricity_State_NAICS <-FARM_electricity_State_NAICS %>% rowwise() %>% 
      dplyr::mutate(electricity_USD2017 = Utilities_USD2017 * frac_notWater
                    , electricityMWh = (electricity_USD2017 / Price_USD2017_x) * (1/1000) # $ / ($/kWh) * (1 MWh / 1000 kWh)
                    , electricity_siteTBTU = (electricity_USD2017 / Price_USD2017_x) * 3412.14/1000000000000) # $ / ($/kWh) * (3412.14 Btu / kWh) * (TBTU/1000000000000 Btu)
    
    #Estimated on-farm site-electricity energy use disaggregated by state, commodity
    F_FARM_siteElectricityTBTU <- FARM_electricity_State_NAICS %>% rowwise() %>% 
      dplyr::mutate(Fuel = "Grid Electricity"
                    , Energy_TBTU = electricity_siteTBTU
      ) %>% select(stateName, commodity_naics, State_Abb, Fuel, Energy_TBTU) 
    F_FARM_siteElectricityTBTU$Source <- "Grid Electricity"
    
  }
  # Electricity Source ----
  
  {
    # Combine farm energy data with electricity generation, this will create a set of rows for each state/commodity with each electricity fuel type. 
    # calculate the amount of source energy needed for each fuel type for that amount of electricity
    # (generated using Source / total generated) * total used (Btu) * Btu/kWh * extra for T&D / Btu/kWh
    
    FARM_sourceElectricityTBTU <- GEN_state2016 %>% select(State_Abb, Source, percent, heatRate, tdLossFactor) %>% 
      left_join(select(FARM_electricity_State_NAICS, commodity_naics, State_Abb, electricity_siteTBTU), by = "State_Abb", relationship = "many-to-many")  %>% 
      dplyr::mutate(farmSourceEnergy_TBTU = percent * electricity_siteTBTU * heatRate * tdLossFactor/3412.14) # convert heatRate to Btu/Btu --> heatRate/3412.14
    
    FARM_sourceElectricityTBTU <- FARM_sourceElectricityTBTU %>% select(commodity_naics, State_Abb, Source, farmSourceEnergy_TBTU) %>%
      pivot_wider(names_from = Source, values_from = farmSourceEnergy_TBTU)
    
    FARM_sourceElectricityTBTU <- FARM_sourceElectricityTBTU %>% rowwise() %>% 
      dplyr::mutate("Other" = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                    , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind", "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)) %>%
      select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind", -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage")
    
    F_FARM_sourceElectricityTBTU <- FARM_sourceElectricityTBTU %>% pivot_longer(!c(commodity_naics, State_Abb), names_to = "Fuel", values_to = "Energy_TBTU") %>% 
      dplyr::mutate(Source = "Grid Electricity") %>% left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb")
    
  }
  # On-site Renewable Energy ----
  
  {
    #USDA-NASS: number of renewable operations: https://quickstats.nass.usda.gov/results/32978D49-D84B-3C7B-BC75-EDC910816DE7
    # assume each operation yields ~ same amount of energy for use on farm (does not count large wind farms that just happen to be on the ag farm)
    # separate into electricity and fuel renewables
    
    FARM_renewables_NAICS_State_Type <- read_excel("Data/Farm - USDA-NASS Utility_Fuel Data.xlsx", sheet = "Renewables")
    
    FARM_renewables_NST <- FARM_renewables_NAICS_State_Type %>% left_join(BD_naicsLookup, by = "NAICS") %>% select(!NAICS) %>% pivot_wider(names_from = commodity_naics, values_from = Value) 
    FARM_renewables_NST <- FARM_renewables_NST %>% rowwise()%>%
      dplyr::mutate(specificOther =  sum(c_across(c(Tobacco, Cotton , sugarcane_hay_sugarBeet_peanut)),na.rm = TRUE)
                    , otherOther = Other - specificOther)  %>% 
      select(-Other, -specificOther)
    
    # Do standard reallocation of "other" commodities
    FARM_sugarsPeanutsHayRenew <- FARM_sugarsPeanutsHay %>% left_join(select(FARM_renewables_NST, stateName, sugarcane_hay_sugarBeet_peanut, System), by = "stateName", relationship = "many-to-many") %>% 
      rowwise() %>% dplyr::mutate(fuelSSHP = fraction *sugarcane_hay_sugarBeet_peanut)
    
    FARM_sugarsPeanutsHayRenew <- FARM_sugarsPeanutsHayRenew %>% select(-fraction, -sugarcane_hay_sugarBeet_peanut )   %>% 
      pivot_wider(names_from = commodity_naics, values_from = fuelSSHP)  %>% dplyr::rename(Other1 = PEANUTS_tons, Feed1 = HAY_noD) 
    
    FARM_sugarsPeanutsHayRenew <- FARM_sugarsPeanutsHayRenew %>% rowwise() %>% dplyr::mutate(Sugar = sum(c_across(c(SUGARBEETS, SUGARCANE)),na.rm = TRUE)) %>% select(-SUGARBEETS, -SUGARCANE)
    
    FARM_renewables_NST_adj <- FARM_renewables_NST %>% left_join(FARM_sugarsPeanutsHayRenew, by = c("stateName", "System"))
    
    FARM_renewables_NST_adj <- FARM_renewables_NST_adj %>% rowwise() %>% dplyr::mutate(
      meat_poultry = sum(c_across(c(beefFeedLots, beefCattleRanch, hog_pig, poultry_egg, sheep_goat)), na.rm = TRUE) # feedlots != (D), meat = sum(all meat)
      , Cotton2 = 0.17 * Cotton
      , CottonX = (1-0.17) * Cotton
      , Other = Other1
      , oilGrain_Food = oilSeed_Grain * 0.186931
      , oilGrain_feed = oilSeed_Grain * 0.432122
      , Dairy = dairyCattle
      , oilGrain = sum(c_across(c(oilGrain_Food,Cotton2)), na.rm = TRUE)
      , Feed = sum(c_across(c(oilGrain_feed,Feed1)), na.rm = TRUE)
      , Fruit_veg = sum(c_across(c(veg_melon,fruit_treeNut)), na.rm = TRUE)
      , notFoodFeed = sum(c_across(c(CottonX, Greenhouse, Tobacco)), na.rm = TRUE)
      , TOTAL = sum(c_across(c(oilGrain, meat_poultry,Dairy, Fruit_veg, Sugar, aquaculture_other, Other, Feed, notFoodFeed)), na.rm = TRUE)
    )
    
    #adjusted number of operations for NAICS, State, Type.
    # convert to energy by using total amount of agriculture renewable energy from EIA AEO (134.45 TBTU)
    FARM_renewables_NST_adj <- FARM_renewables_NST_adj %>% select(stateName, System, oilGrain, meat_poultry,Dairy, Fruit_veg, Sugar, aquaculture_other, Other, Feed, notFoodFeed, TOTAL) %>% 
      pivot_longer(!c(stateName, TOTAL, System),  names_to="commodity_naics", values_to="Installations") %>% 
      dplyr::mutate(fraction = Installations/sum(FARM_renewables_NST_adj$TOTAL, na.rm = TRUE)
                    , energy = 134.45 * fraction) #assume that each installation generates the same amount of energy, AEO Ag renewable = 134.45 TBTU
    
    FARM_renewables_NST_adjTOTALs <- FARM_renewables_NST_adj %>% filter(commodity_naics != "notFoodFeed") %>% 
      select(stateName, System, commodity_naics, energy) %>% 
      pivot_wider(names_from = System, values_from = energy)
    
    FARM_renewables_NST_adjTOTALs <- FARM_renewables_NST_adjTOTALs %>% rowwise() %>% 
      dplyr::mutate(  renewFuels = sum(c_across(c(`BIODIESEL SYSTEMS`, `ETHANOL SYSTEMS`, `GEOEXCHANGE SYSTEMS`,`OTHER SYSTEMS`)), na.rm = TRUE)
                      , renewElect = sum(c_across(c(`METHANE DIGESTERS`, `SMALL HYDRO SYSTEMS`, `SOLAR PANELS`, `WIND TURBINES`)), na.rm = TRUE)
                      , renewables = sum(c_across(c(renewFuels, renewElect)), na.rm = TRUE)
                      , bioDieselGHG = `BIODIESEL SYSTEMS` * ((73.48 + 1.1*25/1000 + 0.11 * 298 / 1000))/1000 # (kgCO2/mmbtu) * (1000000mmbtu/tbtu)*(MMT/1,000,000,000 kg) 
                      , ethanolGHG = `ETHANOL SYSTEMS`  * ((68.44 + 1.1*25/1000 + 0.11 * 298 / 1000))/1000 # (kgCO2/mmbtu) * (1000000mmbtu/tbtu)*(MMT/1,000,000,000 kg) 
                      , methaneGHG = `METHANE DIGESTERS`  * ((52.07 + 3.2*25/1000 + 0.63 * 298 / 1000))/1000 # (kgCO2/mmbtu) * (1000000mmbtu/tbtu)*(MMT/1,000,000,000 kg) - taken from landfill gas
                      , renewableGHG = sum(c_across(c(bioDieselGHG, ethanolGHG, methaneGHG)), na.rm = TRUE)
      )
    # GHG emissions: https://www.epa.gov/sites/production/files/2018-03/documents/emission-factors_mar_2018_0.pdf
    
    F_FARM_RenewableTBTU <- FARM_renewables_NST_adjTOTALs %>% select(stateName, commodity_naics, renewables) %>% 
      dplyr::rename(Energy_TBTU = renewables) %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
    F_FARM_RenewableTBTU$Fuel <- "Renewables"
    F_FARM_RenewableTBTU$Source <- "On-site Renewables"
  }
  
  # Fertilizer/Chemicals ----
  {
    
    # Acres for field crops, Veggies, & non-tree fruit
    # https://quickstats.nass.usda.gov/results/01FBEC8C-4A2A-3466-900B-618A33BC0052
    FARM_chem_US_crops_acres_harvested_USDA_raw <-  suppressWarnings(read_excel("Data/Farm - USDA-NASS and other Fertilizer & Chem Data.xlsx", 
                                                                                sheet = "US_crops_acres_harvested_USDA", 
                                                                                col_types = c("skip", "numeric", "skip", "skip", "skip", "text", "skip", "skip", 
                                                                                              "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "skip", 
                                                                                              "skip", "numeric", "skip")) )%>%
      dplyr::mutate(Acres = ifelse(Value == "(D)" | Value == "(Z)" , NA, Value)) %>% select(-Value)
    
    
    
    # Acres for tree fruit & nuts
    # https://quickstats.nass.usda.gov/results/0C01361A-CD1F-363B-9821-DA1FA90BDA0A
    FARM_chem_US_crops_acres_bearing_USDA_raw <-  suppressWarnings(read_excel("Data/Farm - USDA-NASS and other Fertilizer & Chem Data.xlsx", 
                                                                              sheet = "US_crops_acres_bearing", 
                                                                              col_types = c("skip", "numeric", "skip",  "skip", "skip", "text", "skip", "skip", 
                                                                                            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "skip", 
                                                                                            "skip", "numeric", "skip"))) %>%
      dplyr::mutate(Acres = ifelse(Value == "(D)" | Value == "(Z)" , NA, Value)) %>% select(-Value)
    
    # Acres & tons for hey
    # https://quickstats.nass.usda.gov/results/96B6DF40-14F2-3B6B-9E48-2BB5D23C3A9D
    FARM_chem_US_hay_acres_harvested_USDA_raw <-  read_excel("Data/Farm - USDA-NASS and other Fertilizer & Chem Data.xlsx", 
                                                             sheet = "US_hay_acres_harvested_USDA", 
                                                             col_types = c("skip", "numeric", "skip",  "skip", "skip", "text", "skip", "skip", 
                                                                           "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "skip", 
                                                                           "skip", "numeric", "skip")) %>%
      dplyr::mutate(Value = ifelse(Value == "(D)" | Value == "(Z)" , NA, Value)) 
    
    # sheet to match specific commodities to commodity group and commodity NAICS codes
    FARM_chem_Lookup <- read_excel("Data/Farm - USDA-NASS and other Fertilizer & Chem Data.xlsx", sheet = "fert_chemLookup")
    
    # average across years, sum across variations of commodity
    FARM_chem_US_crops_acres_harvested_USDA <-
      FARM_chem_US_crops_acres_harvested_USDA_raw %>% 
      group_by(State, Commodity, `Data Item`) %>% summarise(Acres = mean(as.double(Acres), na.rm = TRUE)) %>%
      ungroup() %>% group_by(State, Commodity) %>% summarise(Acres = sum(Acres, na.rm = TRUE))%>% left_join(FARM_chem_Lookup, by = "Commodity")
    
    FARM_chem_US_crops_acres_bearing_USDA <-
      FARM_chem_US_crops_acres_bearing_USDA_raw %>% 
      group_by(State, Commodity, `Data Item`) %>% summarise(Acres = mean(as.double(Acres), na.rm = TRUE)) %>%
      ungroup() %>% group_by(State, Commodity) %>% summarise(Acres = sum(Acres, na.rm = TRUE))%>% left_join(FARM_chem_Lookup, by = "Commodity")
    
    FARM_chem_US_crops_acres_HB <- rbind(FARM_chem_US_crops_acres_harvested_USDA, FARM_chem_US_crops_acres_bearing_USDA)
    
    #Fertilizer & chemical applications by pound per state & specific commodity
    # https://quickstats.nass.usda.gov/results/994EE3D2-8AFC-39C7-9C30-2538354A9596
    FARM_chem_USDA_all_applied_raw <- suppressWarnings(read_excel("Data/Farm - USDA-NASS and other Fertilizer & Chem Data.xlsx", 
                                                                  sheet = "USDA fert-chem - all yr - lb ap", 
                                                                  col_types = c("skip", "numeric", "skip",  "skip", "skip", "text", "skip", "skip", 
                                                                                "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "text", 
                                                                                "text", "numeric", "skip")))
    
    FARM_chem_filterkeep <- c(" (NITROGEN)"," (PHOSPHATE)"," (POTASH)" , " (SULFUR)" , " (TOTAL)")
    
    FARM_chem_renameDF <- data.frame(
      Chemical_fert = c(" (NITROGEN)"," (PHOSPHATE)"," (POTASH)" , " (SULFUR)" ,rep(" (TOTAL)",4))
      ,     Chemical_chem = c(rep("FERTILIZER", 4), "CHEMICAL, FUNGICIDE" ,"CHEMICAL, HERBICIDE" , "CHEMICAL, INSECTICIDE","CHEMICAL, OTHER")
      , Chemical = c("Nitrogen","Phosphate","Potash" , "Sulfur","Fungicide","Herbicide","Insecticide" , "Other" )
      , Category = c(rep("Fertilizer", 4),rep("Chemical", 4))
    )
    
    # Estimations of the fraction of commodity grown with the intention of animal feed or human food (may not sum to 1 b/c of other uses)
    # from Dong et. al 2021 Analysis: https://www.nature.com/articles/s43247-022-00414-9
    FARM_chem_oilGrainFvF <- data.frame(
      # Commodity = c("BARLEY", "WHEAT",          "SORGHUM", "SOYBEANS", "CORN" ,      "OATS" , "RICE" , "SUNFLOWER", "COTTON")
      # , foodfrac = c(0.56178865, 0.22258912, 0.17000000, 0.48030429, 0.95040412, 0.44896457, 0.95000000, 0.95000000, 0.82480383)
      # , feedfrac = c(0.16673407, 0.39588572, 0.00000000, 0.57241847, 0.04325222, 0.28860129, 0.05000000, 0.05000000, 0.02526470)
      Commodity = c("WHEAT",                "RYE ",              "RICE",           "CORN",           "OATS",         "BARLEY",  "SORGHUM", "SOYBEANS", "SUNFLOWER", "COTTON")
      , foodfrac = c(0.497670401307421, 0.299677604437037, 0.478847332924586, 0.0722103302351611, 0.439078486084281, 0.549814836885084, 1, 1, 1, .17)
      , feedfrac = c(0.0276463907443845, 0.128070271304063, 0.0431775200559581, 0.410710508423929, 0.543750447163197, 0.169634988999823, 0,0,0,0)
      
    )
    
    # Turn USDA data into useable DF
    FARM_chem_USDA_all_applied <-
      suppressWarnings(
        FARM_chem_USDA_all_applied_raw %>% 
          separate(`Data Item`, c("Commodity_sub", "Delete"), sep = "\\ - ") %>% 
          separate(`Domain`, c("Delete_Category", "Delete2"), sep = "\\,") %>% 
          separate(`Domain Category`, c("Chemical_chem", "Chemical_fert"), sep = "\\:") )%>% 
      filter(Chemical_fert %in% FARM_chem_filterkeep)  %>%
      left_join(FARM_chem_renameDF, by = c("Chemical_chem", "Chemical_fert")) %>%
      select(-Delete, -Delete2, -Delete_Category, -Chemical_chem, -Chemical_fert)
    
    # Average application (lb) across years for each state, chemical & commodity (not all commodities surveyed every year, avgeraging around years near to 2016)
    FARM_chem_USDA_State_Comm_avg <-
      FARM_chem_USDA_all_applied %>% filter(Value != "(D)" & Value != "(Z)") %>% group_by(State, Category, Chemical, Commodity) %>%
      dplyr::summarise(lbApplied = mean(as.double(Value), na.rm = TRUE)) %>%
      left_join(FARM_chem_Lookup, by = "Commodity") 
    
    
    # Not all commodities in each state are included in fertilizer survey. Estimate avg application rate for each commodity
    # assuming missing state/commodity combos apply the average amount of chemicals
    FARM_chem_mean_Rate_Com <-
      FARM_chem_USDA_State_Comm_avg %>% 
      left_join(FARM_chem_US_crops_acres_HB, by = c("State", "Commodity", "Commodity_10", "Commodity_N"), relationship = "many-to-many") %>% 
      filter(Acres != 0) %>%
      rowwise%>% dplyr::mutate(appliedRate = lbApplied/Acres) %>%
      ungroup() %>% group_by(Commodity, Chemical, Category) %>% 
      dplyr::summarise(appliedRate = mean(appliedRate, na.rm = TRUE))
    
    # Not all commodities are included in fertilizer survey. -  estimate avg rate for each commodity type
    # Assuming missing commodities apply the average amount of chemicals for that commodity type
    FARM_chem_mean_Rate_Com10 <-
      FARM_chem_USDA_State_Comm_avg %>% 
      left_join(FARM_chem_US_crops_acres_HB, by = c("State", "Commodity", "Commodity_10", "Commodity_N"), relationship = "many-to-many") %>% 
      filter(Acres != 0) %>%
      rowwise%>% dplyr::mutate(appliedRate = lbApplied/Acres) %>%
      ungroup() %>% group_by(Commodity_10, Chemical, Category) %>% 
      dplyr::summarise(appliedRate = mean(appliedRate, na.rm = TRUE)) 
    
    # find all state/commodities w/o application data - apply commodity avg
    FARM_chem_missingApplications_withCom <-
      FARM_chem_US_crops_acres_HB %>% 
      left_join(FARM_chem_USDA_State_Comm_avg   , by = c("State", "Commodity", "Commodity_10", "Commodity_N" ), relationship = "many-to-many") %>% 
      filter(is.na(lbApplied)) %>% select(-Chemical, -Category, -lbApplied) %>%
      left_join(FARM_chem_mean_Rate_Com, by = c("Commodity"), relationship = "many-to-many") %>%
      rowwise() %>%
      dplyr::mutate(lbApplied = Acres * appliedRate) %>% ungroup()
    
    # find all state/commodities w/o application data for specific commodity - apply group avg
    FARM_chem_missingApplications_woCom <- 
      FARM_chem_missingApplications_withCom %>% filter(is.na(lbApplied)) %>%
      select(-lbApplied, -appliedRate, -Chemical, -Category)%>%
      left_join(FARM_chem_mean_Rate_Com10, by = c("Commodity_10"), relationship = "many-to-many") %>%
      rowwise() %>%
      dplyr::mutate(lbApplied = Acres * appliedRate) %>% ungroup()
    
    # Estimate applications for hay
    FARM_chem_US_hay_applications <-
      FARM_chem_US_hay_acres_harvested_USDA_raw %>% 
      filter(`Data Item` != "HAY & HAYLAGE - YIELD, MEASURED IN TONS / ACRE, DRY BASIS") %>%
      group_by(State, Commodity, `Data Item`) %>% 
      summarise(Value = mean(Value, na.rm = TRUE)) %>% ungroup() %>%
      pivot_wider(names_from = `Data Item`, values_from = Value) %>% 
      rename(Acres = `HAY & HAYLAGE - ACRES HARVESTED`, Tons = `HAY & HAYLAGE - PRODUCTION, MEASURED IN TONS, DRY BASIS`) %>% 
      dplyr::mutate(Ton_Acre = Tons / Acres
                    , Nitrogen = 2947580445 * (85198000/95986888) * Tons/sum(Tons) # lb from 2014 * (ton hay 2017 / ton hay 2014) from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9385665/
                    , Phosphate = 369563095 * (85198000/95986888) * Tons/sum(Tons)  # NASS for 2017 / 2017 hay tons https://quickstats.nass.usda.gov/results/F143A3CB-AC8E-3A0B-9315-E9DA247C5BA5
                    , Potash = 684689648 * (85198000/95986888) * Tons/sum(Tons)  # fraction result (13.6%) ~ fraction from report 2014 (11%)
                    , Sulfur = 0 # no data
                    , Fungicide = 399 * (85198000/99930000) * Tons/sum(Tons) # (lb from 2009)* (ton hay 2017 / ton hay 2009)   from https://pubs.usgs.gov/sir/2013/5009/ - est values applied 2009 for "Hay" & "Alfalfa". chemicals categorized based on crossref NASS chemical.  
                    , Herbicide = 999976 * (85198000/99930000) * Tons/sum(Tons) # fraction result (0.265% applications) ~ fraction from 2009 report
                    , Insecticide = 458344* (85198000/99930000)  * Tons/sum(Tons) # NASS for 2009/2017 hay tons: https://quickstats.nass.usda.gov/results/F143A3CB-AC8E-3A0B-9315-E9DA247C5BA5
                    , Other = 132 * Tons/sum(Tons)    ) %>%
      select(-Tons, -Ton_Acre) %>%
      pivot_longer(!c(State, Commodity, Acres), names_to = "Chemical", values_to = "lbApplied") %>%
      mutate(Commodity_10 = "Feed", Commodity_N = "Feed") %>%
      left_join(select(FARM_chem_renameDF, Chemical, Category), by = "Chemical")
    
    # all fert/chem applications - 
    # starting with all crops harvested/bearing x avg state/commodity application rate
    # adding the missing applications data to bottom
    # adding hay applications to bottom
    FARM_chem_app <- FARM_chem_US_crops_acres_HB %>% 
      left_join(FARM_chem_USDA_State_Comm_avg, by = c("State", "Commodity", "Commodity_10", "Commodity_N" ), relationship = "many-to-many") %>% 
      filter(!is.na(lbApplied)) %>% ungroup() %>%
      rbind(select(rbind(filter(FARM_chem_missingApplications_withCom, !is.na(lbApplied)),  filter(FARM_chem_missingApplications_woCom, !is.na(lbApplied))), -appliedRate)) %>%
      rbind(FARM_chem_US_hay_applications) %>%
      mutate(commodity_naics = Commodity_N, stateName = str_to_title(State)) %>%
      select(Commodity, lbApplied, Chemical, Category, commodity_naics, stateName)
    
    # check total application
    # FARM_chem_app %>% ungroup()%>% group_by(Chemical) %>% summarise(total = sum(lbApplied, na.rm = T)) %>%
    # mutate(total_MMT = ((total/2000)/1000000)*0.907185) # Reference has N at 12 MMT for 2016. Potash ~4, Phosphate ~4
    
    # separate the grain crops intended for food or feed from other uses
    FARM_chem_grainOil <-
      FARM_chem_app %>% filter(commodity_naics == "oilGrain") %>% dplyr::rename(commodity_naicsOld = commodity_naics) %>% 
      left_join(FARM_chem_oilGrainFvF, by = "Commodity") %>% rowwise() %>% 
      dplyr::mutate(oilGrain = lbApplied * foodfrac, Feed = lbApplied * feedfrac) %>% 
      select(-lbApplied, -foodfrac, -feedfrac, -commodity_naicsOld) %>%
      pivot_longer(c(oilGrain, Feed), names_to = "commodity_naics", values_to = "lbApplied") 
    
    # all fertilizer/chemical applications (lb) by state/commodity 
    F_FARM_chem_app_adj <- FARM_chem_app %>% filter(commodity_naics != "oilGrain") %>% rbind(FARM_chem_grainOil) 
    
    # Energy data for each fert/chem
    #Fertilizers: https://www.sciencedirect.com/science/article/abs/pii/S016788090100233X; https://www.osti.gov/biblio/10120269
    #Pesticides: ecoInvent & EPA(recalculated the weighted average based on more recent use practices)
    FARM_chemEE <- data.frame(
      Chemical =        c("Fungicide", "Herbicide", "Insecticide", "Other", "Nitrogen", "Phosphate",  "Potash",      "Sulfur"   ) # no numbers for other or sulfur, used average
      , PetroleumMJkg = c(4.2 + 2,       6.0 + 2,     4.9 + 2,    5.9 + 2,    0.01+1.98,   0.4+2.51,      2.05,    mean(0.01+1.98, 0.4+2.51, 2.05)) # Fuel Oil for checms; "Other" Chem = "All Pesticides" MJ/kg; distillate fuel for fert
      , NaturalGasMJkg = c(32.6,          70.1,       24.3,         67.3, 0.33*51.8 +0.091,  0.63+0,   2.69+  0,     mean(0.091, 0.63, 2.69)) # Steam; Other = all MJ/kg. fertilizers - P & K - all NG included.  N - according to Worrel ~ 33% of reformer NG is fuel
      , ElectricityMJkg = c(5.8,          49.7,        28.8,        47.5,    2.76/2.91669, 5.37/2.91669,  2.11/2.91669,   mean(2.76, 5.37, 2.11)/2.91669) #Other - all  MJ/kg #need this in U.S. average; the 2.91669 is from the paper, 0.0105 GJ/kWhe --> 2.91669 GJ/GJ
    )
    
    FARM_chem_SNC <- aggregate(lbApplied ~ stateName + commodity_naics + Chemical + Category, F_FARM_chem_app_adj, FUN = sum, na.rm = TRUE) %>% 
      left_join(FARM_chemEE, by = "Chemical")
    
    FARM_chem_SNC <- FARM_chem_SNC %>% rowwise() %>% dplyr::mutate(
      kgApplied = lbApplied * (1/2.20462) # 1kg/2.2 lb
      , Petroleum = kgApplied * PetroleumMJkg * (947.817/1000000000000) # MJ/TBTU
      , `Natural Gas` = kgApplied * NaturalGasMJkg * (947.817/1000000000000) # MJ/TBTU
      , Electricity = kgApplied * ElectricityMJkg * (947.817/1000000000000) # MJ/TBTU
    ) 
    
    # Fertilizer & pesticide manuf energy by state, naics & category (fert/pest)
    F_FARM_ChemTBTU <-
      FARM_chem_SNC %>% 
      select(stateName, commodity_naics, Chemical,Category,Petroleum,`Natural Gas`, Electricity) %>% 
      pivot_longer(c(Petroleum,`Natural Gas`, Electricity), names_to = "primarySource", values_to = "Energy_TBTU") %>%
      dplyr::rename(Source = Category) %>%
      group_by(stateName, commodity_naics, Source, primarySource) %>%
      summarise(Energy_TBTU = sum(Energy_TBTU, na.rm = TRUE)) %>% ungroup()%>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName") %>%
      mutate(Source = recode_factor(Source, "Chemical" = "Pesticide Manuf.","Fertilizer" = "Fertilizer Manuf."),
             Fuel = recode_factor(primarySource, "Electricity" = "F&P Electricity", "Natural Gas" = "F&P Fuel", "Petroleum" = "F&P Fuel"))
    
    
    # Fertilizer & pesticide manuf's source energy by state, naics & fuel (electricity is a fuel)
    FARM_chem_sourceFuelTBTU <- F_FARM_ChemTBTU %>% filter(primarySource != "Electricity")
    
    # Electricity source energy estimated using total US Electricity MIX
    FARM_chem_sourceElectricity <-  select(GEN_US2016, Fuel, heatRate, percent) %>% 
      merge(select(filter(F_FARM_ChemTBTU %>% filter(primarySource == "Electricity")) , -Fuel, -primarySource) )%>%  
      mutate(Value = percent * Energy_TBTU * heatRate* GEN_tdLossFactorUS/3412) %>% 
      select(commodity_naics, stateName, Fuel, Source,Value) %>%
      pivot_wider(names_from = Fuel, values_from = Value) %>% 
      rowwise() %>%
      dplyr::mutate("Other" = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                    , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind", "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)) %>%
      select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind", -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage") %>% 
      pivot_longer(!c(commodity_naics, stateName, Source), names_to = "primarySource", values_to = "Energy_TBTU") %>% 
      dplyr::mutate(Fuel = "F&P Electricity") %>% left_join(select(BD_stateRegion, State_Abb, stateName), by = "stateName")
    
    # Fertilizer & pesticide manuf's source energy by state, naics & fuel (electricity is broken by state fuel mix)
    F_FARM_Chem_sourceTBTU <-   rbind(FARM_chem_sourceFuelTBTU, FARM_chem_sourceElectricity)
    
    # GHG's handled below
  }
  # Combine energies  ----
  
  # combine all site energies
  {
    # 1 - recode Diesel & gas as Petro, recode LP as other, 2 - aggregate
    FARM_SiteTBTU <- rbind(mutate(F_FARM_FuelTBTU, directUse_yn = T, electricity_yn = F), 
                           mutate(F_FARM_siteElectricityTBTU, directUse_yn = F, electricity_yn = T), 
                           mutate(F_FARM_RenewableTBTU, directUse_yn = T, electricity_yn = F)) %>% 
      mutate(Fuel = recode_factor(Fuel, "Diesel" = "Petroleum","Gasoline" = "Petroleum", "LP Gas" = "Petroleum"),
             primarySource = Fuel) %>% rbind( mutate(F_FARM_ChemTBTU, directUse_yn = F, electricity_yn = ifelse(str_detect(Fuel, "Elect"), T, F))) 
    FARM_SiteTBTU$commodity_naics <- revalue(FARM_SiteTBTU$commodity_naics, c("oilGrain" = "Grain, Oil"
                                                                              , "Dairy" = "Dairy"
                                                                              ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                              , "meat_poultry" = "Meat, Poultry, Eggs"
                                                                              , "Sugar" = "Sugar"
                                                                              ,	"aquaculture_other" = "Seafood"
                                                                              , "Other" = "Other"
                                                                              , "Feed" = "Animal Feed"
    ))
    
    F_FARM_SiteTBTU <- aggregate(Energy_TBTU ~ commodity_naics + State_Abb + stateName + Source + Fuel + primarySource + directUse_yn + electricity_yn, FARM_SiteTBTU, FUN = sum, na.rm = TRUE)
    
    # 1 - recode Diesel & gas as Petro, recode LP as other, 2 - aggregate
    FARM_SourceTBTU <- rbind(mutate(F_FARM_FuelTBTU, directUse_yn = T, electricity_yn = F), 
                             mutate(F_FARM_sourceElectricityTBTU, directUse_yn = F, electricity_yn = T), 
                             mutate(F_FARM_RenewableTBTU, directUse_yn = T, electricity_yn = F)
    ) %>% 
      mutate(Fuel = recode_factor(Fuel, "Diesel" = "Petroleum","Gasoline" = "Petroleum", "LP Gas" = "Petroleum"),
             primarySource = Fuel) %>% rbind( mutate(F_FARM_Chem_sourceTBTU, directUse_yn = F, electricity_yn = ifelse(str_detect(Fuel, "Elect"), T, F))) 
    FARM_SourceTBTU$commodity_naics <- revalue(FARM_SourceTBTU$commodity_naics, c("oilGrain" = "Grain, Oil"
                                                                                  , "Dairy" = "Dairy"
                                                                                  ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                                  , "meat_poultry" = "Meat, Poultry, Eggs"
                                                                                  , "Sugar" = "Sugar"
                                                                                  ,	"aquaculture_other" = "Seafood"
                                                                                  , "Other" = "Other"
                                                                                  , "Feed" = "Animal Feed") )
    
    F_FARM_SourceTBTU <- aggregate(Energy_TBTU ~ commodity_naics + State_Abb + stateName + Source + Fuel+ primarySource + directUse_yn + electricity_yn, FARM_SourceTBTU, FUN = sum, na.rm = TRUE)
    
    
  }
  
  
  # Farm GHG Emissions ----
  # Energy GHG
  {
    #Fuel GHG (site fuel & chem fuel)
    #https://www.epa.gov/sites/default/files/2020-04/documents/ghg-emission-factors-hub.pdf
    FARM_FuelGHG <- data.frame(
      Fuel = c("Natural Gas","Diesel","Gasoline" , "Other" , "LP Gas", "Petroleum" )
      , GHG_kgCO2e_MMBTU = c(53.06 + 25*1/1000 + 298 * 0.1/1000 # NG
                             , mean(73.25, 73.96, 75.04) + 25*3/1000 + 298 * 0.6/1000 #Diesel
                             , 70.22 + 25*11/1000 + 298 * 1.16/1000 #Gasoline
                             , mean(53.06 + 25*1/1000 + 298 * 0.1/1000, mean(73.25, 73.96, 75.04) + 25*3/1000 + 298 * 0.6/1000, 70.22 + 25*11/1000 + 298 * 1.16/1000 )  # mean of NG, Diesel & Gas used, 
                             , 61.71 + 25*3/1000 + 298 * 0.6/1000 #LP Gas
                             , mean(73.25, 73.96, 75.04, 70.22) + 25*3/1000 + 298 * 0.6/1000 ) # assume "Petroleum" = diesel + some HGL (i.e. propane) also
    )#, recall all renewable fuels are bagasse, so that is not included here
    
    FARM_FuelGHG <- rbind(mutate(F_FARM_FuelTBTU, primarySource = Fuel), filter(F_FARM_Chem_sourceTBTU, Fuel == "F&P Fuel")) %>% 
      left_join(rename(FARM_FuelGHG, primarySource = Fuel), by = "primarySource") %>% 
      rowwise() %>% dplyr::mutate(GHG_MMT = Energy_TBTU * GHG_kgCO2e_MMBTU/1000) %>% # 1000000 MMBTU/TBTU * MMT/1000000000
      select(-Energy_TBTU, -GHG_kgCO2e_MMBTU) %>%
      mutate(primarySource = recode_factor(primarySource, "Diesel" = "Petroleum","Gasoline" = "Petroleum", "LP Gas" = "Petroleum"))
    
    
    # Renewable GHG  
    FARM_RenewGHG <- FARM_renewables_NST_adjTOTALs %>% select(stateName, commodity_naics, renewableGHG) %>% dplyr::rename("GHG_MMT" = renewableGHG) %>%
      mutate(Fuel = "Renewables", Source = "On-site Renewables", primarySource = "Renewables") %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
    
    # Electricity GHG (site electricity & chem electricity)
    FARM_site_ElectricityGHG <- select(FARM_electricity_State_NAICS, commodity_naics, State_Abb, electricity_siteTBTU) %>% 
      left_join(select(GEN_state2016GHG, State_Abb, Source, percent, CO2e_MMT_TBTU, tdLossFactor), by = "State_Abb") %>% 
      dplyr::mutate(farmElectGHG =percent * electricity_siteTBTU * CO2e_MMT_TBTU * tdLossFactor ) %>% 
      select(commodity_naics, State_Abb, Source,farmElectGHG) %>%
      pivot_wider(names_from = Source, values_from = farmElectGHG) %>% mutate(Source = "Grid Electricity")
    
    #Pesticides & Fertilizers are not produced in the same state they are used.  Due to lack of production data, U.S. avg grid mix is used instead  
    FARM_chem_ElectricityGHG <-  select(GEN_US2016GHG, Fuel, CO2e_MMT_TBTU, percent) %>% 
      merge(select(filter(F_FARM_ChemTBTU, primarySource == "Electricity") , -Fuel, -primarySource, -State_Abb) )%>%  
      mutate(farmElectGHG = percent * Energy_TBTU * CO2e_MMT_TBTU * GEN_tdLossFactorUS) %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName") %>% 
      select(commodity_naics, State_Abb, farmElectGHG, Fuel, Source)%>%
      pivot_wider(names_from = Fuel, values_from = farmElectGHG) 
    
    
    FARM_ElectricityGHG <- rbind(FARM_site_ElectricityGHG, FARM_chem_ElectricityGHG) %>% rowwise() %>%
      dplyr::mutate(OtherSources = sum(c_across(c( Other,"Other Gases")),na.rm = TRUE)
                    , Renewables = sum(c_across(c("Geothermal", "Other Biomass" , "Wood and Wood Derived Fuels" )),na.rm = TRUE)) %>%
      select( -Other, -"Other Gases",-"Geothermal",-"Other Biomass", -"Wood and Wood Derived Fuels") %>% dplyr::rename("Other" = "OtherSources") %>%
      pivot_longer(!c(commodity_naics, State_Abb, Source), names_to = "Fuel", values_to = "GHG_MMT") %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "State_Abb") %>%
      mutate(primarySource = Fuel)
  }
  # Fugitive GHG
  {
    # EPA Inventory of GHG emissions and sinks, chapter five tables 5-3, (in MMT CO2e), 5-7: 
    # https://www.epa.gov/sites/default/files/2021-04/documents/us-ghg-inventory-2021-chapter-5-agriculture.pdf
    FARM_animalEmissions = data.frame(
      Livestock = c("BeefCattle", "MilkCattle", "Swine", "Horses", "Sheep",  "Bison", "Goats",  "Mules_Asses", "Poultry")
      , entericFermentation = c(123,43, 2.6, 1.4, 1.1, 0.4, 0.3, 0.1, 0)
      ,  MeManure = c(3.3, 31.5, 21.1, 0.2,  0.1,  0,  0,  0,  3.4 )
      , NO2Manure = c(8.1, 6.1, 2,  0.1,  0.3,  0,  0,  0,  1.6)
      , commodity_naics = c("Meat, Poultry, Eggs", "Dairy", "Meat, Poultry, Eggs", "Other", "Meat, Poultry, Eggs", "Meat, Poultry, Eggs", "Meat, Poultry, Eggs", "Other", "Meat, Poultry, Eggs")
    )
    
    FARM_animalEmissions <- FARM_animalEmissions %>% filter(Livestock != "Horses" & Livestock != "Mules_Asses") %>% rowwise()%>%
      dplyr::mutate(emissionMMT_total = sum(c_across(c( entericFermentation,MeManure, NO2Manure)),na.rm = TRUE)) %>% select(Livestock, emissionMMT_total, commodity_naics)
    
    #USDA-NASS: Livestock: https://quickstats.nass.usda.gov/results/8A59A10A-4F50-39C2-B2EE-D3444AB1B7C3
    FARM_AnimalsStateraw <- read_excel("Data/Farm - USDA-NASS Other Farm Data.xlsx", sheet = "animalsInventory"
                                       , col_types = c("text", "text", "skip", "numeric", "skip", "skip", "skip", "text"))
    
    FARM_hogsState <-  filter(FARM_AnimalsStateraw, Livestock == "Swine" & Period == "FIRST OF DEC") %>% 
      dplyr::mutate(
        numberX = ifelse(number == 0, unlist(filter(FARM_AnimalsStateraw, Livestock == "Swine" & Period == "FIRST OF DEC" & stateName == "Other States")$number)/2
                         , number)) %>% select(-number, -Period) %>% dplyr::rename("number" = numberX)
    
    FARM_AnimalsState <- aggregate(number ~ stateName + Livestock, filter(FARM_AnimalsStateraw, Livestock != "Swine"), FUN = sum, na.rm = TRUE) %>%
      rbind(FARM_hogsState) %>% filter(stateName != "Other States")
    
    FARM_AnimalsState <-  FARM_AnimalsState  %>% dplyr::group_by(Livestock) %>% dplyr::mutate(percent = (number/sum(number)))
    F_FARM_AnimalsState <-  FARM_AnimalsState  %>% left_join(FARM_animalEmissions, by = "Livestock")%>% dplyr::mutate(GHG_MMT = percent * emissionMMT_total)
    
    FARM_AnimalState_MMT <- aggregate(GHG_MMT ~ stateName + commodity_naics, F_FARM_AnimalsState, FUN = sum, na.rm= TRUE) %>%
      mutate(Source = "Animal Emissions", Fuel = Source, primarySource = Source) %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
    
    
    # USDA-NASS: Field Crops https://quickstats.nass.usda.gov/results/D603FA9C-8BEB-350E-8FC6-0FA42B9FF9F8
    # USDA-NASS: Pasture https://quickstats.nass.usda.gov/results/B6649227-83E2-324B-88DB-A65882E1548E
    # USDA-NASS: Cropland https://quickstats.nass.usda.gov/results/8B008027-E683-37DA-B188-95D7106D91A3
    # USDA-NASS: Rice https://quickstats.nass.usda.gov/results/D4A2E413-6ED0-31EE-8E6D-4EC87B4A89FA
    FARM_cropLandState <- read_excel("Data/Farm - USDA-NASS Other Farm Data.xlsx", sheet = "combinedCleaned")
    
    FARM_cropLandState <-  FARM_cropLandState  %>% dplyr::group_by(DataFile) %>% dplyr::mutate(percent = (Value/sum(Value)))
    
    # EPA Inventory of GHG emissions and sinks, chapter five: 
    # https://www.epa.gov/sites/default/files/2021-04/documents/us-ghg-inventory-2021-chapter-5-agriculture.pdf
    # oilGrain, fruitVeg, sugar & feed multipliers all from Dong et al. 2021 
    
    FARM_cropEmissions = data.frame(
      emissionSource =   c("RiceCultivations", "soilMgmt_crop_d", "soilMgmt_pasture_d","soilMgmt_crop_id", "soilMgmt_pasture_id", #soilMgmt_pasture some animals directly "applying" to cropland "organic amendment" for cropland and "PRP Manure" = directly, "managed manure" not much
                           "liming", "ureaFert", "fieldBurnMe", "fieldBurnN2O")
      , emissionMMT = c(13.5,  191.3,  88.7,  37.8,  9.6,  3.1,  4,  0.4,  0.2)
      , oilGrainMultiplier = c(0.921292223, 0.214553582, 0, 0.240081994, 0, 0.214553582, 0.214553582,  0.186931467,  0.186931467 )
      , fruitVegMultipler = c(0, 0.165575718, 0, 0.165575718, 0, 0.165575718, 0.165575718, 0, 0)
      , sugarMultipler = c(0, 0.093455066, 0, 0.093455066, 0, 0.093455066, 0.093455066, 0, 0 )
      , feedMultiplier = c(0.066326779, 0.284415437,  1,  0.284415437,  1,  0.284415437,  0.284415437,  0.432121962,  0.432121962)
      , DataFile = c("Rice", "allCrops", "Pasture", "allCrops", "Pasture", "allCrops", "allCrops", "FieldCrops", "FieldCrops")
    )
    
    FARM_cropLandState <-  FARM_cropLandState  %>% left_join(FARM_cropEmissions, by = "DataFile", relationship = "many-to-many") 
    FARM_cropLandStateMMT <-  FARM_cropLandState  %>% mutate(oilGrain = percent * emissionMMT * oilGrainMultiplier
                                                             , Fruit_veg = percent * emissionMMT * fruitVegMultipler
                                                             , Sugar = percent * emissionMMT * sugarMultipler
                                                             , Feed= percent * emissionMMT * feedMultiplier) %>% 
      select(stateName, oilGrain, Fruit_veg, Sugar, Feed, DataFile, emissionSource) %>% 
      pivot_longer(!c(stateName, DataFile, emissionSource), names_to = "commodity_naics", values_to = "GHG_MMT")
    
    FARM_cropLandStateMMT <- aggregate(GHG_MMT ~ stateName + commodity_naics, FARM_cropLandStateMMT, FUN = sum, na.rm = TRUE) %>%
      mutate(Source = "Crop Emissions", Fuel = Source, primarySource = Source)  %>% 
      left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
    
    FARM_oilGrainFrac <-
      FARM_chem_app %>% mutate(isoilGrain = ifelse(commodity_naics=="oilGrain", T, F)) %>%
      group_by(isoilGrain)%>% summarise(lbApplied=sum(lbApplied)) %>% 
      ungroup() %>% mutate(frac = lbApplied/sum(lbApplied))
    
    FARM_fertTotalApp <-
      FARM_chem_app %>%     filter(Chemical == "Nitrogen" | Chemical == "Phosphate") %>%
      group_by(Chemical) %>% summarise(lbApplied = sum(lbApplied))
    
    FARM_chem_AdditionalGHG_all <-
      FARM_chem_app %>% 
      filter(Chemical == "Nitrogen" | Chemical == "Phosphate") %>%
      ungroup() %>% group_by(Commodity,commodity_naics, Chemical, stateName) %>%
      summarise(lbApplied = sum(lbApplied, na.rm = T)) %>%
      ungroup() %>%
      dplyr::mutate( processEmissions = (lbApplied*
                                           ifelse(Chemical == "Nitrogen", .88*10.1946/unname(unlist(FARM_fertTotalApp[1,2])), 0.99798/unname(unlist(FARM_fertTotalApp[2,2]))) # process emissions per lb applied
      ) ) %>% 
      select(-lbApplied)
    
    FARM_chem_AdditionalGHG <-
      FARM_chem_AdditionalGHG_all %>%
      filter(commodity_naics == "oilGrain") %>%
      left_join(FARM_chem_oilGrainFvF, by = "Commodity") %>% rowwise() %>% 
      dplyr::mutate(  oilGrain = processEmissions* foodfrac
                      , Feed = processEmissions * feedfrac) %>% 
      select(-foodfrac, -feedfrac,  -processEmissions, -commodity_naics) %>%
      pivot_longer(c(oilGrain, Feed), names_to = "commodity_naics", values_to = "processEmissions") %>%
      rbind(FARM_chem_AdditionalGHG_all %>% filter(commodity_naics != "oilGrain")) %>%
      group_by( stateName, commodity_naics) %>%
      summarise(GHG_MMT = sum(processEmissions, na.rm = T)) %>%
      mutate(Source = "Fertilizer Manuf.",
             Fuel = "F&P Process", 
             primarySource = "Process",
             directUse_yn = F,
             electricity_yn = F) %>%
      left_join(select(BD_stateRegion, State_Abb, stateName), by = "stateName")
    
  }
  # Aggregate & combine
  {
    #need to aggregate to combine the different petroleums (few lines down)
    F_FARM_GHG <- rbind(mutate(FARM_cropLandStateMMT, directUse_yn = F, electricity_yn = F),
                        mutate(FARM_AnimalState_MMT, directUse_yn = F, electricity_yn = F),
                        mutate(FARM_FuelGHG, directUse_yn = ifelse(Fuel == "F&P Fuel", F, T), electricity_yn = F),
                        mutate(FARM_ElectricityGHG, directUse_yn = F, electricity_yn = T),
                        mutate(FARM_RenewGHG, directUse_yn = T, electricity_yn = F),
                        FARM_chem_AdditionalGHG)
    
    F_FARM_GHG$commodity_naics <- revalue(F_FARM_GHG$commodity_naics, c("oilGrain" = "Grain, Oil", "Dairy" = "Dairy",	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                        , "meat_poultry" = "Meat, Poultry, Eggs", "Sugar" = "Sugar"
                                                                        ,	"aquaculture_other" = "Seafood", "Other" = "Other", "Feed" = "Animal Feed"
    ))
    
    F_FARM_GHG <- aggregate(GHG_MMT ~ commodity_naics + stateName + State_Abb + Source + Fuel + primarySource + directUse_yn + electricity_yn, F_FARM_GHG, FUN = sum)
  }
}
# Remove excess variables from environment
rm(list=ls(pattern="^FARM_"))

# Manufacturing ----
{
  # MECS energy type fractions by region & NAICS ====
  # Match states to census regions
  # MECS energy use by region & NAICS 
  # SOURCE: EIA's Manufacturing Energy Consumption Survey (MECS) Tables 3.2, 4.2, 11.3
  # https://www.eia.gov/consumption/manufacturing/data/2018/
  # electricity is MMBTU electricity, renewable is million kWh
  MANUF_MECS_E2014 <- read_excel("Data/Manuf - MECS Data 2014.xlsx", sheet = "MECS Final Energy") %>% mutate(dataYear = 2014)
  MANUF_MECS_E2018 <- read_excel("Data/Manuf - MECS Data 2018.xlsx", sheet = "MECS Final Energy") %>% mutate(dataYear = 2018)
  
  MANUF_MECS_E <- rbind(MANUF_MECS_E2014, MANUF_MECS_E2018) %>% group_by(NAICS, Subsector, Region_small) %>%
    summarise(across(everything(), mean)) %>% select(-dataYear)
  
  # electricity = summing purchases, transfers in, and generation from noncombustible renewable resources, minus quantities sold and transferred out. 
  # It does not include electricity inputs from onsite co-generation or generation  from combustible fuels 
  # because that energy has already been included as generating fuel (for example, coal). )
  # Fuels were aggregated by type in excel: Petroleum (Residual fuel oil, distillate fuel oil, HGL), Other (Coke & Breeze and "Off site Other"), Other renewables ("Other- Off site Other")
  # Other from table 3.2 is split using data for Other from table 4.2, making "other" and "Fuel_renewable"
  # Renewables comes from table 11.3 from essentially inexhaustible sources. Noncombustible sources include solar power, wind power, hydro-power, and geothermal power. it DOES NOT include any renewables burned as fuel, that is in "Fuel_renewable"
  
  # Data for any NAICS code without specific MECS data was assigned the 311 regional total
  # Data for 31131, Northeast was all zero or "*" - 0.5 was substituted for the * as there are 31131 facilities in that region
  # The MECS data was over written with the total 31131 numbers
  # Within the excel file, green are the original MECS data and blue tabs are calculation steps
  
  # Convert renewable electricity to TBTU (from Million kWh)
  MANUF_MECS_E <- MANUF_MECS_E %>% 
    dplyr::mutate(Elect_renew = Electricity_renew_on_M_kWh * 3412.14 / 1000000
                  , NAICSf = as.factor(NAICS)) %>% ungroup() # 3412.14 MMBtu / M_kWh * 1 TBtu/(1M MMBtu) #double checked20210210
  
  # Calculate fuel ratios: select non electricity columns, calculate total fuel use, gather, calculate fraction for each row (1 region, NAICS, fuel source), 
  MANUF_MECS_fuel_fracs <- MANUF_MECS_E %>%  
    select(-Subsector, -Total, -Electricity_Total, -Electricity_renew_on_M_kWh, -Elect_renew) %>% 
    rowwise() %>%
    dplyr::mutate(Fuel_use = sum(c_across(contains("_TBTU")), na.rm = TRUE)) %>%
    pivot_longer(c(-NAICS, - NAICSf, -Fuel_use, -Region_small),names_to = "Fuel", values_to = "Fuel_use_i")%>% 
    rowwise() %>% dplyr::mutate(fraction = ifelse(is.na(Fuel_use_i/Fuel_use), 0, Fuel_use_i/Fuel_use)) %>% 
    select(-Fuel_use_i) %>% pivot_wider(names_from = Fuel, values_from = fraction, names_prefix = "f_")
  
  # Calculate electricity ratios: select electricity columns, calculate grid electricity, calculate fractions of grid vs. renewable for each row (1 region, NAICS, source), make wide again
  MANUF_MECS_elect_fracs<- MANUF_MECS_E %>%  select(NAICS, NAICSf, Electricity_Total, Elect_renew, Region_small) %>% rowwise() %>%
    dplyr::mutate(Elect_grid = Electricity_Total - Elect_renew) %>%
    pivot_longer(c(-NAICS, - NAICSf, -Electricity_Total, -Region_small),names_to = "Electricity_Type", values_to = "Elect_use_i")%>% 
    rowwise() %>% dplyr::mutate(fraction = ifelse(is.na(Elect_use_i/Electricity_Total), 0, Elect_use_i/Electricity_Total)) %>% 
    select(-Elect_use_i) %>% pivot_wider(names_from = Electricity_Type, values_from = fraction, names_prefix = "f_")
  
  # Join the two fraction tables together, join to BD_stateRegion to get fractions for each state
  MANUF_MECS_fracs <- left_join(select(MANUF_MECS_elect_fracs, -Electricity_Total), select(MANUF_MECS_fuel_fracs, -Fuel_use), by = c("NAICS", "NAICSf", "Region_small"))
  MANUF_NAICS_State_fracs <- left_join(filter(MANUF_MECS_fracs, Region_small != "Total"), filter(BD_stateRegion, State_Abb != "PR"), by = "Region_small", relationship = "many-to-many") %>% 
    select(-Region_ARMS, -Region_PADD)
  
  
  # Connect MECS fuel/electricity ratios to All manufacturing sites data ----
  # This data is an aggregation of most known food manufacturing sites in the U.S. (data purchased from Manufacturers' News, Inc provides location, sales, etc.)
  # Manufacturers' News, Inc. (2020). National Manufacturing Industry Database.   https://www.mni.net/
  # Electricity and Natural Gas use estimated via correlations derived in https://www.osti.gov/biblio/1082107 (https://ntrl.ntis.gov/NTRL/dashboard/searchResults/titleDetail/DE20131082107.xhtml)
  MANUF_Energy_SN <- read_excel("Data/Manuf - MECS Data 2014.xlsx", sheet = "ManufEnergy_State_NAICS_2016") %>%
    mutate(NAICSf = as.factor(NAICSf)) %>%
    left_join(select(mutate(filter(MANUF_NAICS_State_fracs, Region_small != "Total")
                            , NAICSf = recode_factor(NAICSf, "3113" = "31130"))
                     , -NAICS, -stateName, -Region)
              , by = c("NAICSf", "State_Abb"))  
  
  # calculate fuel use based on NG use:
  # select fuel columns, calculate "allFuel_use" based on NG use and NG fraction, gather, rename fuel type levels, calculate fuel use
  MANUF_SN_fuel <- MANUF_Energy_SN %>%  select(-Electricty_TBTU, -f_Elect_renew, -f_Elect_grid, -Region_small, -NAICS_Analysis) %>% rowwise() %>%
    dplyr::mutate(allFuel_use = ifelse(is.na(NG_TBTU / f_Natural_Gas_TBTU), 0, NG_TBTU / f_Natural_Gas_TBTU)) %>% 
    pivot_longer(!c(NAICSf, State_Abb, NG_TBTU, allFuel_use), names_to="Fuel", values_to="Fuel_use_i_f") %>%
    mutate(Fuel = recode_factor(Fuel, "f_Petroleum_TBTU" = "Petroleum_TBTU", "f_Coal_TBTU" = "Coal_TBTU",
                                "f_Other_TBTU" = "Other_TBTU", "f_Fuel_renewable_TBTU" = "FuelRenewable_TBTU")) %>% 
    rowwise() %>% dplyr::mutate(fuelUse = ifelse(is.na(Fuel_use_i_f * allFuel_use), 0, Fuel_use_i_f * allFuel_use)) %>% 
    select(-Fuel_use_i_f, -allFuel_use) %>% pivot_wider(names_from = Fuel, values_from = fuelUse) %>% select(-f_Natural_Gas_TBTU)
  
  # calculate grid electricity & on-site renewable electricity:
  # select electricity columns,  gather, rename levels, calculate electricity use use, make wide again
  MANUF_SN_elect<- MANUF_Energy_SN %>%  select(NAICSf, State_Abb, Electricty_TBTU, f_Elect_renew, f_Elect_grid) %>% 
    mutate(ElectricityUse = Electricty_TBTU /f_Elect_grid
           , Renew_Electricity_TBTU = ElectricityUse * f_Elect_renew) %>% 
    rename(Grid_Electricity_TBTU = Electricty_TBTU ) %>% select(-ElectricityUse, -f_Elect_grid, -f_Elect_renew)
  
  
  # Join fuel use and electricity use 
  # Calculate "renewables" (electricity and fuel) - note: renew electricity is in kWh, fuels in MMBTU
  # Manuf energy by fuel used, NAICS & State.
  MANUF_Energy_All_SN <- left_join(MANUF_SN_elect, MANUF_SN_fuel, by = c("NAICSf", "State_Abb"))  %>% 
    rowwise() %>%
    dplyr::mutate(Renewables_TBTU = Renew_Electricity_TBTU + FuelRenewable_TBTU)
  
  # Manufacturing site energy ----
  MANUF_renameFuelsDF <- data.frame(
    FuelX = c("NG_TBTU" , "Petroleum_TBTU", "Coal_TBTU", "Other_TBTU", "Renewables_TBTU", "Grid_Electricity_TBTU" )
    , Fuel = c("Natural Gas" , "Petroleum", "Coal", "Other", "Renewables", "Grid Electricity")
  )
  
  F_MANUF_SiteTBTU <-
    MANUF_Energy_All_SN  %>% 
    select(-Renew_Electricity_TBTU, -FuelRenewable_TBTU) %>% 
    pivot_longer(!c(NAICSf, State_Abb), names_to = "FuelX", values_to = "Energy_TBTU") %>% 
    # rowwise() %>% mutate(Energy_TBTU = Energy_MMBTU/1000000) %>% 
    left_join(MANUF_renameFuelsDF, by = "FuelX") %>% select(-FuelX) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>% 
    dplyr::rename(NAICS = NAICSf) %>% left_join(mutate(BD_naicsLookup, NAICS = recode_factor(NAICS, "3113" = "31130")), by = "NAICS") %>% select(-NAICS) 
  
  
  # # Coal 31131 = 45; Elect = 4; NG = 16
  # F_MANUF_SiteTBTU %>% pivot_wider(names_from = commodity_naics, values_from =  Energy_TBTU)  %>% 
  #   rowwise() %>%
  #   # dplyr::mutate(sugar_confection = sum(c_across(c(sugarConfectionary, sugarManuf)), na.rm = TRUE)) %>%
  #   select(State_Abb, Fuel, stateName, sugarManuf, sugarConfectionary) %>%
  #   ungroup() %>%
  #   group_by(Fuel) %>%
  #   summarise(sugarManuf = sum(sugarManuf, na.rm = TRUE), sugarConfectionary= sum(sugarConfectionary, na.rm = TRUE))
  # 
  # 
  # F_MANUF_SiteTBTU %>% group_by(commodity_naics, Fuel) %>%
  #   summarise(Energy_TBTU = sum(Energy_TBTU, na.rm = TRUE)) %>% 
  #   pivot_wider(names_from = Fuel, values_from =  Energy_TBTU)%>% write_clip()
  
  F_MANUF_SiteTBTU <- F_MANUF_SiteTBTU %>% pivot_wider(names_from = commodity_naics, values_from =  Energy_TBTU)  %>% rowwise() %>%
    dplyr::mutate(sugar_confection = sum(c_across(c(sugarConfectionary, sugarManuf)), na.rm = TRUE)) %>% select(-sugarConfectionary, -sugarManuf)
  
  F_MANUF_SiteTBTU <- F_MANUF_SiteTBTU %>% pivot_longer(!c(Fuel, State_Abb, stateName), names_to = "commodity_naics", values_to = "Energy_TBTU")
  
  F_MANUF_SiteTBTU <- F_MANUF_SiteTBTU %>% dplyr::mutate(
    Source = ifelse(Fuel == "Renewables", "On-site Renewables",
                    ifelse(Fuel == "Grid Electricity", "Grid Electricity",
                           "Fuel")))
  
  F_MANUF_SiteTBTU$commodity_naics <- revalue(F_MANUF_SiteTBTU$commodity_naics, c("oilSeed_Grain_milling" = "Grain, Oil Milling"
                                                                                  , "dairyProds" = "Dairy Products"
                                                                                  ,	"fruit_vegManuf" = "Fruit, Vegetables, Nuts Products"
                                                                                  , "animalProcess" = "Animal Product Processing"
                                                                                  , "sugar_confection" = "Sugar, Confectionary"
                                                                                  ,	"seafoodProcess" = "Seafood Processing"
                                                                                  , "otherManuf" = "Other Products"
                                                                                  , "feedManuf" = "Animal Feed Processing"
                                                                                  , "bakeries" = "Bakeries, Tortillas"
  ))
  
  # Manufacturing electricity source conversion ----
  # Combine manufacturing energy data with electricity generation, this will create a set of rows for each state/NAICS with each electricity fuel type. 
  # Calculate the amount of source energy needed for each fuel type for that amount of electricity
  MANUF_SourceElectricity <-
    select(MANUF_Energy_All_SN, NAICSf, State_Abb, Grid_Electricity_TBTU ) %>% 
    mutate(Grid_Electricity_kWh = Grid_Electricity_TBTU * 1000000*1000000/3412.14) %>% select(-Grid_Electricity_TBTU) %>%
    left_join(select(GEN_state2016, State_Abb, Source, percent, heatRate, tdLossFactor), by = "State_Abb", relationship = "many-to-many") %>% 
    dplyr::mutate(manufSourceEnergy_TBTU = percent * Grid_Electricity_kWh * heatRate * tdLossFactor/1000000000000) %>% 
    select(NAICSf, State_Abb, Source,manufSourceEnergy_TBTU) %>%
    pivot_wider(names_from = Source, values_from = manufSourceEnergy_TBTU)%>% 
    rowwise() %>%
    dplyr::mutate(OtherSources = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                  , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind"
                                                , "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)) %>%
    select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind"
           , -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage") %>% 
    pivot_longer(!c(NAICSf, State_Abb), names_to = "Fuel", values_to = "Energy_TBTU") %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>% dplyr::rename(NAICS = NAICSf) %>% 
    left_join(mutate(BD_naicsLookup, NAICS = recode_factor(NAICS, "3113" = "31130")), by = "NAICS") %>% select(-NAICS) %>% 
    pivot_wider(names_from = commodity_naics, values_from =  Energy_TBTU)  %>% rowwise() %>% 
    dplyr::mutate(sugar_confection = sum(c_across(c(sugarConfectionary, sugarManuf)), na.rm = TRUE)) %>% 
    select(-sugarConfectionary, -sugarManuf)%>% 
    pivot_longer(!c(Fuel, State_Abb, stateName), names_to = "commodity_naics", values_to = "Energy_TBTU") 
  
  # Source energy from manufacturing grid electricity
  MANUF_SourceElectricity <- transform(MANUF_SourceElectricity, Fuel = revalue(Fuel, c("OtherSources" = "Other"))) %>%
    mutate(Source = "Grid Electricity")
  
  # Combine manufacturing energy data with electricity GHGs, this will create a set of rows for each state/NAICS with each electricity fuel type. 
  # Calculate the GHGS produced by each fuel type for that amount of electricity,  spread out so each fuel type is a column, sum columns together to match classifications, remove unnecessary rows
  MANUF_ElectricityGHG <- select(MANUF_Energy_All_SN, NAICSf, State_Abb, Grid_Electricity_TBTU) %>% 
    mutate(Grid_Electricity_kWh = Grid_Electricity_TBTU * 1000000*1000000/3412.14) %>% select(-Grid_Electricity_TBTU) %>%
    left_join(select(GEN_state2016GHG, State_Abb, Source, percent, CO2e_MMT_TBTU, tdLossFactor), by = "State_Abb", relationship = "many-to-many") %>% 
    dplyr::mutate(manufElectGHG =percent * Grid_Electricity_kWh * CO2e_MMT_TBTU * 3412.14 * tdLossFactor/1000000000000 )%>% 
    select(NAICSf, State_Abb, Source,manufElectGHG) %>%
    pivot_wider(names_from = Source, values_from = manufElectGHG) %>% rowwise() %>%
    dplyr::mutate(OtherSources = sum(c_across(c( Other,"Other Gases")),na.rm = TRUE)
                  , Renewables = sum(c_across(c("Geothermal", "Other Biomass" , "Wood and Wood Derived Fuels" )),na.rm = TRUE)) %>%
    select( -Other, -"Other Gases",-"Geothermal",-"Other Biomass", -"Wood and Wood Derived Fuels") %>% 
    dplyr::rename("Other" = "OtherSources") %>% 
    #GHG emissions from manufacturing grid electricity
    pivot_longer(!c(NAICSf, State_Abb), names_to = "Fuel", values_to = "GHG_MMT") %>% 
    dplyr::rename(NAICS = NAICSf) %>% 
    left_join(mutate(BD_naicsLookup, NAICS = recode_factor(NAICS, "3113" = "31130")), by = "NAICS") %>% select(-NAICS) %>%
    mutate(Source = "Grid Electricity") %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb")
  
  # Manufacturing source Fuel & combine ----
  # MANUF_Energy_All_SN DF, remove electricity, rename fuels, add state name & naics names
  MANUF_Fuel <- MANUF_Energy_All_SN %>% 
    select(-Grid_Electricity_TBTU, -Renewables_TBTU, -Renew_Electricity_TBTU) %>% 
    pivot_longer(!c(NAICSf, State_Abb), names_to = "FuelX", values_to = "Energy_TBTU") %>% 
    separate(FuelX, c("Fuel", "delete"), "_") %>% select(-delete) %>% 
    transform(Fuel = revalue(Fuel, c("NG" = "Natural Gas", "FuelRenewable" = "Renewables"))) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>% dplyr::rename(NAICS = NAICSf) %>% 
    left_join(mutate(BD_naicsLookup, NAICS = recode_factor(NAICS, "3113" = "31130")), by = "NAICS") %>% select(-NAICS) %>%
    #combine sugars & add "source" column
    pivot_wider(names_from = commodity_naics, values_from =  Energy_TBTU)  %>% rowwise() %>%
    dplyr::mutate(sugar_confection = sum(c_across(c(sugarConfectionary, sugarManuf)), na.rm = TRUE)) %>% 
    select(-sugarConfectionary, -sugarManuf)%>%
    pivot_longer(!c(Fuel, State_Abb, stateName), names_to = "commodity_naics", values_to = "Energy_TBTU") %>%
    dplyr::mutate(Source = ifelse(Fuel == "Renewables", "On-site Renewables","Fuel"))
  
  #combine
  F_MANUF_SourceTBTU <- rbind(MANUF_SourceElectricity, MANUF_Fuel)
  
  F_MANUF_SourceTBTU$commodity_naics <- revalue(F_MANUF_SourceTBTU$commodity_naics, c("oilSeed_Grain_milling" = "Grain, Oil Milling"
                                                                                      , "dairyProds" = "Dairy Products"
                                                                                      ,	"fruit_vegManuf" = "Fruit, Vegetables, Nuts Products"
                                                                                      , "animalProcess" = "Animal Product Processing"
                                                                                      , "sugar_confection" = "Sugar, Confectionary"
                                                                                      ,	"seafoodProcess" = "Seafood Processing"
                                                                                      , "otherManuf" = "Other Products"
                                                                                      , "feedManuf" = "Animal Feed Processing"
                                                                                      , "bakeries" = "Bakeries, Tortillas"
  ))
  
  # Manufacturing GHG emissions ----
  MANUF_fuelsGHG <- data.frame(
    Fuel = c("Natural Gas","Petroleum","Coal" , "Other" , "Renewables" )
    , GHG_kgCO2e_MMBTU = c(53.06 + 25*1/1000 + 298 * 0.1/1000 ,75.04 + 25*3/1000 + 298 * 0.6/1000 , 94.67 + 25*11/1000 + 298 * 1.16/1000, 53.1148 , 95.5 + 25*1.9/1000 + 298 * 0.42/1000) 
  )
  
  # assume "Petroleum" = diesel (has some HGL i.e. propane also), recall all renewable fuels are bagasse, Other uses average of all others
  MANUF_fuelGHG <- MANUF_Fuel %>% left_join(MANUF_fuelsGHG, by = "Fuel") %>% rowwise() %>% dplyr::mutate(GHG_MMT = (Energy_TBTU * 1000000 ) * GHG_kgCO2e_MMBTU/1000000000) %>% # Energy is in TBTU, GHG is kg/MMBTU, then kg to MMT
    select(-Energy_TBTU, -GHG_kgCO2e_MMBTU) %>% mutate(Source = "Fuel")
  
  
  # Fugitive
  # https://di.unfccc.int/detailed_data_by_party
  # USA, all years, 2.F.1, aggregate, Mt
  # Industrial Refrig = 9.00 Mt in 2016
  # Data from MECS Table 5.4
  MANUF_MECS_PC_TBTU <- read_excel("Data/Manuf - MECS Data 2014.xlsx", sheet = "Process Cooling") %>%
    rowwise()%>%
    dplyr::mutate(Energy_TBTU = sum(c_across(c(Electricity_T,	R_FO_T,	D_FO_T,	NG_T,	HGL_T,	Coal_T)))) %>% select(-Electricity_T,	-R_FO_T,	-D_FO_T,	-NG_T,	-HGL_T,	-Coal_T) %>%
    pivot_wider(names_from = "End Use", values_from = "Energy_TBTU")
  MANUF_cooling_total <-   unname(unlist(filter(MANUF_MECS_PC_TBTU, commodity_naics == "ALL MANUFACTURING INDUSTRIES")$`Process Cooling and Refrigeration`))
  
  MANUF_MECS_PC_TBTU <- MANUF_MECS_PC_TBTU %>% rowwise() %>%
    dplyr::mutate(fracManufCooling = `Process Cooling and Refrigeration`/MANUF_cooling_total,
                  fracSectorCooling = `Process Cooling and Refrigeration`/`TOTAL FUEL CONSUMPTION`)
  MANUF_foodCoolingFrac <-   unname(unlist(filter(MANUF_MECS_PC_TBTU, commodity_naics == "FOOD")$fracManufCooling ))
  
  MANUF_FugGHG_US <-
    F_MANUF_SiteTBTU %>% filter(Source != "On-site Renewables") %>% group_by(commodity_naics) %>% summarise(Energy_TBTU = sum(Energy_TBTU, na.rm = TRUE)) %>%
    left_join(filter(MANUF_MECS_PC_TBTU,commodity_naics != "ALL MANUFACTURING INDUSTRIES"), by = "commodity_naics")
  
  #Estimate the process cooling energy demand for sectors not specified in MECS, join together with those that were
  MANUF_FugGHG_US <-
    data.frame(
      commodity_naics = unname(filter(MANUF_FugGHG_US, is.na(fracManufCooling ))$commodity_naics) #"Animal Feed Processing" "Bakeries, Tortillas"    "Other Products"         "Seafood Processing"
      ,Energy_TBTU  = unname(filter(MANUF_FugGHG_US, is.na(fracManufCooling ))$Energy_TBTU ) #"Animal Feed Processing" "Bakeries, Tortillas"    "Other Products"         "Seafood Processing"
      
      , fracSectorCooling = c(unname(unlist(filter(MANUF_MECS_PC_TBTU, commodity_naics == "Grain, Oil Milling")$fracSectorCooling)) #assume animal feed is similary to milling
                              , unname(unlist(filter(MANUF_MECS_PC_TBTU, commodity_naics == "FOOD")$fracSectorCooling)) # assume bakeries is similar to food ave rate
                              , 0                                                                                 #will calculate later
                              , unname(unlist(filter(MANUF_MECS_PC_TBTU, commodity_naics == "Animal Product Processing")$fracSectorCooling)) #assume seafood is similar to animal processing
      )) %>%
    dplyr::mutate(fracManufCooling = fracSectorCooling*Energy_TBTU/MANUF_cooling_total) %>%
    rbind(select(filter(MANUF_FugGHG_US, !is.na(fracManufCooling)), commodity_naics, Energy_TBTU, fracSectorCooling, fracManufCooling)) %>%
    dplyr::mutate(GHG_MMT = fracManufCooling * 9,  #9 MMT (2016) of industrial refrigerant GHG emissions https://di.unfccc.int/detailed_data_by_party
                  GHG_MMT = ifelse(commodity_naics == "Other Products", MANUF_foodCoolingFrac* 9 - sum(GHG_MMT), GHG_MMT)
                  , GHG_TBTU = GHG_MMT / Energy_TBTU)
  MANUF_FugGHG <- F_MANUF_SiteTBTU %>% filter(Source != "On-site Renewables") %>% group_by(commodity_naics, stateName, State_Abb) %>%
    summarise(Energy_TBTU = sum(Energy_TBTU, na.rm = TRUE)) %>%
    left_join(select(MANUF_FugGHG_US, commodity_naics, GHG_TBTU), by = "commodity_naics") %>%
    rowwise() %>% dplyr::mutate(GHG_MMT = Energy_TBTU * GHG_TBTU) %>% select(-Energy_TBTU, - GHG_TBTU) %>%
    mutate(Fuel = "Refrigerant", Source = "Refrigerant")
  
  # combine Fuel & Electricity
  F_MANUF_GHG <- rbind(MANUF_fuelGHG, MANUF_ElectricityGHG)%>% 
    pivot_wider(names_from = commodity_naics, values_from = GHG_MMT)  %>% 
    rowwise() %>%
    dplyr::mutate(sugar_confection = sum(c_across(c(sugarConfectionary, sugarManuf)), na.rm = TRUE)) %>% 
    select(-sugarConfectionary, -sugarManuf)%>% 
    pivot_longer(!c(Fuel, State_Abb, stateName, Source), names_to = "commodity_naics", values_to = "GHG_MMT")
  
  F_MANUF_GHG$commodity_naics <- revalue(F_MANUF_GHG$commodity_naics, c("oilSeed_Grain_milling" = "Grain, Oil Milling"
                                                                        , "dairyProds" = "Dairy Products"
                                                                        ,	"fruit_vegManuf" = "Fruit, Vegetables, Nuts Products"
                                                                        , "animalProcess" = "Animal Product Processing"
                                                                        , "sugar_confection" = "Sugar, Confectionary"
                                                                        ,	"seafoodProcess" = "Seafood Processing"
                                                                        , "otherManuf" = "Other Products"
                                                                        , "feedManuf" = "Animal Feed Processing"
                                                                        , "bakeries" = "Bakeries, Tortillas"
  ))
  
  F_MANUF_GHG <- rbind(F_MANUF_GHG,MANUF_FugGHG)
  } 
# Remove excess variables from environment
rm(list=ls(pattern="^MANUF"))

# Distribution ----
{
  #lets you read multiple sheets in the excel file in one call
  read_excel_allsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }
  
  suppressMessages({
    DIST_farm_manufGO <- read_excel_allsheets("Data/Dist - farm to manufacture by state - GO.xlsx")
    DIST_farm_manufnoGO <- read_excel_allsheets("Data/Dist - farm to manufacture by state - non-GO.xlsx")
    
  # Data from another analysis, see paper & SI. in million metric ton - miles
    DIST_truckFM_tonmile <-
      rbind(
        DIST_farm_manufnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_farm_manufnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_farm_manufnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_farm_manufnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_farm_manufnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_farm_manufnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_farm_manufnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_farm_manufnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
        DIST_farm_manufnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
        DIST_farm_manufnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
        DIST_farm_manufGO[["food truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
        DIST_farm_manufGO[["food truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain"), 
        DIST_farm_manufGO[["feed truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed") ,
        DIST_farm_manufGO[["feed truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
    
    DIST_railFM_tonmile <-
      rbind(
        DIST_farm_manufnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy")
        ,  DIST_farm_manufnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy")
        ,  DIST_farm_manufnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") 
        ,  DIST_farm_manufnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg")
        ,  DIST_farm_manufnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") 
        ,  DIST_farm_manufnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts")
        ,  DIST_farm_manufnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood")
        ,  DIST_farm_manufnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
        ,  DIST_farm_manufnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") 
        ,  DIST_farm_manufnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar")
        ,  DIST_farm_manufGO[["food rail tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain")
        ,  DIST_farm_manufGO[["food rail tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain") 
        # , DIST_farm_manufGO[["feed rail tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed")
        # ,  DIST_farm_manufGO[["feed rail tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
    
  })
  
  # Energy intensities currently from Transportation Energy Data Book Tables 2.16 & ?
  # https://tedb.ornl.gov/wp-content/uploads/2021/02/TEDB_Ed_39.pdf
  # add 20% for refrigerated trucks: https://bura.brunel.ac.uk/bitstream/2438/19541/1/FulltextThesis.pdf
  # refrigeration units in rail are rare and probably not a large component of total energy demand  
  
  DIST_truckEI <- 21335 /1000000000000 # Btu/vehicle-mi * TBTU/1000000000000BTU
  DIST_truckPayload <- (1000000*2205) / 34825 #see Excel tab 1000000 MT / MMT * 2205 lb/MT / 34825 lb/vehicle (34825 lb is from below link)
  # truck payload from https://www.osti.gov/biblio/1615795-exploring-use-fhwa-truck-traffic-volume-weight-data-support-national-truck-freight-mobility-study 
  # estimated from fig 4.2 & 5.4
  
  DIST_truckFM_TBTU <- DIST_truckFM_tonmile %>% filter(State_Abb != "DC") %>% mutate(
    Dairy = Dairy * 1.2 * DIST_truckPayload * DIST_truckEI, # MMT-mi * 120% * vehicle/MMT * TBTU/vehicle-mi
    Fruit_veg = Fruit_veg * 1.2 * DIST_truckPayload * DIST_truckEI ,
    animalProducts = (animalProducts) * 1.2 * DIST_truckPayload * DIST_truckEI,
    Seafood = (Seafood) * 1.2 * DIST_truckPayload * DIST_truckEI,
    Sugar = Sugar * DIST_truckPayload * DIST_truckEI,
    oilGrain = oilGrain * DIST_truckPayload * DIST_truckEI,
    feed = feed * DIST_truckPayload * DIST_truckEI,
    Source = "Truck",
    Fuel = "Petroleum",
    Stage = "Distribution (F-M)"
  ) 
  
  DIST_railEI <- 299 * (1.10231) * 1000000 / 1000000000000 # 299Btu/ton-mi * TBTU/1000000000000BTU * 1.10231 ton/MT * 1000000MT/MMT
  
  DIST_railFM_TBTU <- DIST_railFM_tonmile %>% filter(State_Abb != "DC")%>% mutate(
    Dairy = Dairy *  DIST_railEI, # MMT-mi * TBTU/MMT-mi
    Fruit_veg = Fruit_veg *  DIST_railEI,
    animalProducts = (animalProducts) *  DIST_railEI,
    Seafood = Seafood *  DIST_railEI,
    Sugar = Sugar *  DIST_railEI,
    oilGrain = oilGrain *  DIST_railEI,
    feed = 0,
    Source = "Rail",
    Fuel = "Petroleum",
    Stage = "Distribution (F-M)"
  ) 
  
  # What follows is for the unoptimized WR . 
  # To test the optimizaed case, comment out the next three definitions and uncomment the three after them
  # then rerun distribution & combine stages
  suppressMessages({
    DIST_manuf_WRGO <- read_excel_allsheets("Data/Dist - manufacture to WR by state - GO.xlsx")
    DIST_manuf_WRnoGO <- read_excel_allsheets("Data/Dist - manufacture to WR by state - non-GO.xlsx")
    
    # data from another analysis, see paper & SI. in million metric ton - miles
    # 
    # DIST_truckMR_tonmile <-
    #   left_join(DIST_manuf_WR[["Dairy truck ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Dairy = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Dairy)
    #   , left_join(DIST_manuf_WR[["F&V&N truck ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Fruit_veg = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Fruit_veg)
    #   , left_join(DIST_manuf_WR[["M&P&E truck ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(animalProducts = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, animalProducts)
    #   , left_join(DIST_manuf_WR[["Seafood truck ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Seafood = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Seafood)
    #   , left_join(DIST_manuf_WR[["Sugar truck ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Sugar = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Sugar)
    #   , left_join(DIST_manuf_WR[["G&O truck ton mile"]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(oilGrain = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, oilGrain)
    #    , DIST_manuf_WR[["Animal Feed truck ton mile"]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(feed = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, feed)
    #    , by = "State_Abb") , by = "State_Abb"), by = "State_Abb"), by = "State_Abb"), by = "State_Abb"), by = "State_Abb")
    # 
    # DIST_railMR_tonmile <-
    #   left_join(DIST_manuf_WR[["Dairy rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Dairy = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Dairy)
    #   ,left_join(DIST_manuf_WR[["F&V&N rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Fruit_veg = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Fruit_veg)
    #   ,left_join(DIST_manuf_WR[["M&P&E rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(animalProducts = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, animalProducts)
    #   ,left_join(DIST_manuf_WR[["Seafood rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Seafood = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Seafood)
    #   ,left_join(DIST_manuf_WR[["Sugar rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(Sugar = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, Sugar)
    #   ,DIST_manuf_WR[["G&O rail ton mile "]] %>% dplyr::rename(State_Abb = "...1") %>% rowwise() %>% dplyr::mutate(oilGrain = sum(c_across(!State_Abb), na.rm = TRUE)) %>% select(State_Abb, oilGrain)
    #   , by = "State_Abb"), by = "State_Abb"), by = "State_Abb"), by = "State_Abb"), by = "State_Abb")
    # 
    
    DIST_truckMR_tonmile <-
      rbind(
        DIST_manuf_WRnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_manuf_WRnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_manuf_WRnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_manuf_WRnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_manuf_WRnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_manuf_WRnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_manuf_WRnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_manuf_WRnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
        DIST_manuf_WRnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
        DIST_manuf_WRnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
        DIST_manuf_WRGO[["G&O truck ton mile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
        DIST_manuf_WRGO[["G&O truck ton mile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain"),
        DIST_manuf_WRGO[["animal feed truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed") ,
        DIST_manuf_WRGO[["animal feed truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
    
    DIST_railMR_tonmile <-
      rbind(
        DIST_manuf_WRnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_manuf_WRnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_manuf_WRnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_manuf_WRnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_manuf_WRnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_manuf_WRnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_manuf_WRnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_manuf_WRnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
        DIST_manuf_WRnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
        DIST_manuf_WRnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
        DIST_manuf_WRGO[["G&O rail ton mile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
        DIST_manuf_WRGO[["G&O rail ton mile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  })
  
  DIST_truckMR_TBTU <- DIST_truckMR_tonmile %>% filter(State_Abb != "DC") %>% mutate(
    Dairy = Dairy *  1.2 * DIST_truckPayload * DIST_truckEI, 
    Fruit_veg = Fruit_veg * 1.2*  DIST_truckPayload * DIST_truckEI,
    animalProducts = animalProducts * 1.2*  DIST_truckPayload * DIST_truckEI,
    Seafood = Seafood * 1.2 *  DIST_truckPayload * DIST_truckEI,
    Sugar = Sugar *  DIST_truckPayload * DIST_truckEI,
    oilGrain = oilGrain  *  DIST_truckPayload * DIST_truckEI,
    feed = feed *  DIST_truckPayload * DIST_truckEI,
    Source = "Truck",
    Fuel = "Petroleum",
    Stage = "Distribution"
  ) 
  
  DIST_railMR_TBTU <- DIST_railMR_tonmile %>% filter(State_Abb != "DC")%>% mutate(
    Dairy = Dairy *  DIST_railEI, 
    Fruit_veg = Fruit_veg*  DIST_railEI, 
    animalProducts = (animalProducts)*  DIST_railEI, 
    Seafood = Seafood*  DIST_railEI, 
    Sugar = Sugar*  DIST_railEI,
    oilGrain = oilGrain  *  DIST_railEI,
    feed = 0, 
    Source = "Rail",
    Fuel = "Petroleum",
    Stage = "Distribution"
  ) 
  
  F_DIST_TBTU <- rbind(DIST_truckFM_TBTU, DIST_railFM_TBTU, DIST_truckMR_TBTU, DIST_railMR_TBTU) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>% 
    pivot_longer(!c(State_Abb, stateName, Source, Fuel, Stage), names_to = "commodity_naics", values_to = "Energy_TBTU")
  
  F_DIST_TBTU$commodity_naics <- revalue(F_DIST_TBTU$commodity_naics, c("oilGrain" = "Grain, Oil"
                                                                        , "Dairy" = "Dairy"
                                                                        ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                        , "animalProducts" = "Meat, Poultry, Eggs"
                                                                        , "Seafood" = "Seafood"
                                                                        , "Sugar" = "Sugar"
                                                                        , "feed" = "Animal Feed"
  ))
  
  DIST_Energy_GHG <- F_DIST_TBTU %>%
    mutate(GHG_MMT = Energy_TBTU * (mean(73.25, 73.96, 75.04) + 25*3/1000 + 298 * 0.6/1000 )/1000) %>%# 1000000 MMBTU/TBTU * MMT/1000000000
    select(-Energy_TBTU)
  
  # Fugitive emissions from refrigeration
  #Estimate total tonnage moved by refrigerated transport:
  # https://www.winnesota.com/news/refrigeratedtransportation
  DIST_USreeferTotal_MMT <- ((3.25 * 1000000000)/((1+.1244)^6)) * 0.907185/1000000 #3.25 Bt in 2022, ~12.44% CAGR. conversion ton to MMT
  
  DIST_refrigerationGHG <- 5.89 #MMT https://di.unfccc.int/detailed_data_by_party
  
  suppressMessages({
    DIST_farm_manuf_ton <- read_excel_allsheets("Data/Dist - farm to manufacture by state - ton.xlsx")
    # data from another analysis, see paper & SI. in million metric ton - miles

    DIST_truckFM_ton <-
      rbind(
        DIST_farm_manuf_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_farm_manuf_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_farm_manuf_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_farm_manuf_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_farm_manuf_ton[["ship by truck M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_farm_manuf_ton[["ship by truck M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_farm_manuf_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_farm_manuf_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile)
    
    DIST_railFM_ton <-
      rbind(
        DIST_farm_manuf_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_farm_manuf_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_farm_manuf_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_farm_manuf_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_farm_manuf_ton[["ship by rail M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_farm_manuf_ton[["ship by rail M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_farm_manuf_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_farm_manuf_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  })
  
  DIST_truckFM_fugGHG <-
    DIST_truckFM_ton %>% filter(State_Abb != "DC") %>% 
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")    %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      # GHG_MMT = MMT*DIST_truckPayload*(8*.7+3*.3) * .5 * 1430/1000000000, # convert to lbs divide by truck capacity convert to MMT of refrigerant 
      #GHG estimate 70% use highest value (8), 30% use lowest (3) from fugitive emissions https://www.epa.gov/sites/production/files/2015-07/documents/fugitiveemissions.pdf
      Source = "Truck Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution (F-M)"
    ) 
  
  DIST_railFM_fugGHG <- DIST_railFM_ton %>% filter(State_Abb != "DC") %>%
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")  %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      
      # GHG_MMT = ((MMT*(1000000*2205)) / (30*2000))* 8 * .5 *  1430/1000000000,  # convert mass to lbs divide by rail capacity convert to MMT of refrigerant 
      # https://www.wett.ru/es/helpful_information/rail_transport/
      # estimate top of range. from fugitive emissions https://www.epa.gov/sites/production/files/2015-07/documents/fugitiveemissions.pdf
      Source = "Rail Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution (F-M)"
    ) 
  
  suppressMessages({
    DIST_manuf_WR_ton <- read_excel_allsheets("Data/Dist - food manufactured by state - ton.xlsx")
    
    DIST_truckMWR_ton <-
      rbind(
        DIST_manuf_WR_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_manuf_WR_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_manuf_WR_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_manuf_WR_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_manuf_WR_ton[["ship by truck M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_manuf_WR_ton[["ship by truck M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_manuf_WR_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_manuf_WR_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
    
    DIST_railMWR_ton <-
      rbind(
        DIST_manuf_WR_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
        DIST_manuf_WR_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
        DIST_manuf_WR_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
        DIST_manuf_WR_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
        DIST_manuf_WR_ton[["ship by rail M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
        DIST_manuf_WR_ton[["ship by rail M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
        DIST_manuf_WR_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
        DIST_manuf_WR_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ) %>%
      group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  })
  
  DIST_truckMWR_fugGHG <- DIST_truckMWR_ton %>% filter(State_Abb != "DC") %>% 
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")    %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      Source = "Truck Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution"
    ) 
  
  DIST_railMWR_fugGHG <- DIST_railMWR_ton %>% filter(State_Abb != "DC") %>%
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")  %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      Source = "Rail Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution"
    ) 
  
  DIST_Fug_GHG <- rbind(DIST_truckFM_fugGHG, DIST_railFM_fugGHG, DIST_truckMWR_fugGHG, DIST_railMWR_fugGHG) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb")
  
  DIST_Fug_GHG$commodity_naics <- revalue(DIST_Fug_GHG$commodity_naics, c( "Dairy" = "Dairy"
                                                                           ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                           , "animalProducts" = "Meat, Poultry, Eggs"
                                                                           , "Seafood" = "Seafood"  ))

  F_DIST_GHG <- DIST_Energy_GHG %>% rbind(select(DIST_Fug_GHG, -MMT))
}
# Remove excess variables from environment 
rm(list=ls(pattern="^DIST"))
# End of Supply Chain ----
{
  # Warehouse  ----
  #CBECS Microdata - reduced number of columns
  # https://www.eia.gov/consumption/commercial/data/2012/index.php?view=microdata
  EOSC_Commercial <- read_excel("Data/End of Chain - Warehouse USDA-NASS, Retail + Consumption BEA.xlsx", sheet = "CBECS_Reduced")
  EOSC_Commercial$CENDIV <-recode_factor(EOSC_Commercial$CENDIV
                                         ,'1' = 'New England'
                                         ,'2' = 'Middle Atlantic'
                                         ,'3' = 'East North Central'
                                         ,'4' = 'West North Central'
                                         ,'5' = 'South Atlantic'
                                         ,'6' = 'East South Central'
                                         ,'7' = 'West South Central'
                                         ,'8' = 'Mountain'
                                         ,'9' = 'Pacific'
  ) 
  
  EOSC_WR_Warehouse <- EOSC_Commercial %>% filter(PBAPLUS == 10 | PBAPLUS == 20) %>% select(REGION, CENDIV, PBA, PBAPLUS, SQFT, ELBTU, NGBTU, FKBTU )
  
  EOSC_WR_Warehouse$PBAPLUS <-recode_factor(EOSC_WR_Warehouse$PBAPLUS
                                            , "10" = "Dry"
                                            , "20" = "Cold")
  
  EOSC_WR_Warehouse <-EOSC_WR_Warehouse %>% mutate(
    Electricity = (ELBTU/1000)/SQFT # was in thousand BTU, now MMBTU/sqft
    , naturalGas = (NGBTU/1000)/SQFT # was in thousand BTU, now MMBTU/sqft
    , fuelOil = (FKBTU/1000)/SQFT # was in thousand BTU, now MMBTU/sqft
  )
  EOSC_WR_Warehouse <-EOSC_WR_Warehouse %>% select(CENDIV, PBAPLUS, Electricity, naturalGas, fuelOil) %>% pivot_longer(!c(CENDIV, PBAPLUS), names_to = "Fuel", values_to = "EI")
  
  EOSC_WR_Warehouse$EI <- EOSC_WR_Warehouse$EI %>% replace_na(0)
  
  EOSC_WR_WarehouseEI <- 
    aggregate(EI ~ CENDIV + PBAPLUS + Fuel, EOSC_WR_Warehouse,FUN = mean, na.rm = TRUE) %>% 
    dplyr::rename("Region" = CENDIV, "Type" = PBAPLUS)
  # replaced NA with 0 so warehouses that do not use any fuel DO reduce the average EI
  
  EOSC_WR_WarehouseEI <- 
    EOSC_WR_WarehouseEI %>% 
    left_join(select(BD_stateRegion, stateName, Region), by = "Region", relationship = "many-to-many")
  
  # Data from: https://www.cbre.us/research-and-reports/US-Food-in-Demand-Series-Cold-Storage-Logistics-Unpacked-May-2019
  EOSC_WR_warehouseHeight <- (3.6 * 1000000000) / (214 * 1000000) # U.S. cubic feet / U.S. sqft 
  
  # Data from: USDA-NASS cold storage survey
  EOSC_WR_Warehouse_cuft <- read_excel("Data/End of Chain - Warehouse USDA-NASS, Retail + Consumption BEA.xlsx",  sheet = "USDA-NASS - Cold Storage cuft") %>% 
    select(State, `Data Item`, Value) %>%
    mutate(`Data Item` = recode_factor(`Data Item`, 
                                       "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL, GROSS - CAPACITY, MEASURED IN CU FT" = "Total_cuft"
                                       , "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL, GROSS, PRIVATE & SEMI-PRIVATE - CAPACITY, MEASURED IN CU FT" = "Private_cuft"
                                       , "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL, GROSS, PUBLIC - CAPACITY, MEASURED IN CU FT" = "Public_cuft"
    )) %>%
    pivot_wider(names_from = "Data Item", values_from = "Value")
  
  
  EOSC_WR_Warehouse_count <- read_excel("Data/End of Chain - Warehouse USDA-NASS, Retail + Consumption BEA.xlsx",  sheet = "USDA-NASS - Cold Storage count")%>% 
    select(State, `Data Item`, Value) %>%
    mutate(`Data Item` = recode_factor(`Data Item`, 
                                       "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL - NUMBER OF WAREHOUSES" = "Total_n"
                                       , "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL, PRIVATE & SEMI-PRIVATE - NUMBER OF WAREHOUSES" = "Private_n"
                                       , "COLD STORAGE CAPACITY, WAREHOUSE, GENERAL, PUBLIC - NUMBER OF WAREHOUSES" = "Public_n"
    )) %>%
    pivot_wider(names_from = "Data Item", values_from = "Value")
  
  EOSC_WR_Warehouse_count <- EOSC_WR_Warehouse_count%>%
    rbind(data.frame(State = "US TOTAL", Total_n = sum(EOSC_WR_Warehouse_count$Total_n),
                     Private_n = sum(EOSC_WR_Warehouse_count$Private_n),  Public_n = sum(EOSC_WR_Warehouse_count$Public_n)))
  
  EOSC_WR_Warehouse_cuft <- left_join(EOSC_WR_Warehouse_cuft, EOSC_WR_Warehouse_count, by = "State")
  
  EOSC_WR_Warehouse_cuft_TOTALremainder <-  as.double(unname(unlist(filter(EOSC_WR_Warehouse_cuft,State == "US TOTAL")$Total_cuft))) - 
    sum(as.double(filter(EOSC_WR_Warehouse_cuft,Total_cuft != "(D)" & State != "US TOTAL")$Total_cuft))
  
  EOSC_WR_Warehouse_cuftD1 <- EOSC_WR_Warehouse_cuft %>% filter(Total_cuft == "(D)") %>% 
    mutate(Total_cuft = EOSC_WR_Warehouse_cuft_TOTALremainder * Total_n / sum(Total_n) ,
           Private_cuft = ifelse(Private_cuft == "0", 0,
                                 ifelse(Public_cuft == "0", Total_cuft,
                                        Total_cuft * Private_n / Total_n)) ,
           Public_cuft = ifelse(Public_cuft == "0", 0,
                                ifelse(Private_cuft == "0", Total_cuft,
                                       Total_cuft - Private_cuft)) )
  
  EOSC_WR_Warehouse_cuftD2 <- EOSC_WR_Warehouse_cuft %>% filter(Total_cuft != "(D)") %>%  filter(Public_cuft == "(D)" | Private_cuft == "(D)") %>%
    mutate(Total_cuft = as.double(Total_cuft),
           Private_cuft = Total_cuft * Private_n / Total_n, 
           Public_cuft = Total_cuft - Private_cuft )
  
  EOSC_WR_Warehouse_cuft <- EOSC_WR_Warehouse_cuft %>% filter(Total_cuft != "(D)" & Public_cuft != "(D)" & Private_cuft != "(D)" & State != "US TOTAL") %>%
    mutate(Total_cuft = as.double(Total_cuft), Private_cuft = as.double(Private_cuft), Public_cuft = as.double(Public_cuft)) %>%
    rbind(EOSC_WR_Warehouse_cuftD1) %>% rbind(EOSC_WR_Warehouse_cuftD2) %>% 
    mutate(stateName = str_to_title(State)) %>% select(-State)
  
  #estimations of dry/cold storage relation from: https://www.cbre.us/research-and-reports/US-Food-in-Demand-Series-Cold-Storage-Logistics-Unpacked-May-2019
  EOSC_WR_Warehouse_sqft <- EOSC_WR_Warehouse_cuft  %>% rowwise() %>% dplyr::mutate(
    Public_sqft = (Public_cuft / EOSC_WR_warehouseHeight) /0.78 # USDA only collects ~78%., USDA only reports cold (not total capacity)
    , Private_sqft = (Private_cuft / EOSC_WR_warehouseHeight) /0.78
    , Public_sqft_cold = Public_sqft
    , Public_sqft_dry = 0
    , Private_sqft_cold = Private_sqft # should this be private x 0.35
    , Private_sqft_dry = Private_sqft * .65/.35
    , Cold = sum(c_across(c(Public_sqft_cold, Private_sqft_cold)), na.rm = TRUE)
    , Dry = sum(c_across(c(Public_sqft_dry, Private_sqft_dry)), na.rm = TRUE)
  )
  
  EOSC_WR_Warehouse_sqft <- EOSC_WR_Warehouse_sqft %>% select(stateName, Cold, Dry) %>% pivot_longer(!stateName, names_to = "Type", values_to = "sqft")
  
  EOSC_WR_WarehouseEnergy <- 
    EOSC_WR_WarehouseEI %>% 
    left_join(EOSC_WR_Warehouse_sqft, by = c("Type", "stateName")) %>% 
    rowwise() %>% 
    mutate(Energy_TBTU = EI * sqft /1000000) %>% # EI was in MMBTU/sqft
    select(stateName, Type, Fuel, Energy_TBTU)
  
  EOSC_WR_Warehouse_TBTU <- aggregate(Energy_TBTU ~ Fuel + stateName + Type, EOSC_WR_WarehouseEnergy, FUN = sum, na.rm = TRUE) %>%
    mutate(commodity_naics = paste("Warehouse -", Type)) %>% select(-Type)
  
  EOSC_WR_Warehouse_TBTU$Fuel <-recode_factor(EOSC_WR_Warehouse_TBTU$Fuel,
                                              "fuelOil" = "Petroleum"
                                              , "naturalGas" = "Natural Gas"
                                              , "Electricity" = "Grid Electricity")
  
  EOSC_WR_Warehouse_TBTU <- EOSC_WR_Warehouse_TBTU %>% left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
  EOSC_WR_Warehouse_TBTU$Stage <- "W&R"
  
  # Retail & Consumption ----
  EOSC_FS_fracs <-
    EOSC_Commercial %>% filter(PBA == 15) %>% select(ELBTU, NGBTU, FKBTU) %>% summarise_all(sum, na.rm = TRUE) %>% 
    pivot_longer(everything(), names_to = "Fuel", values_to = "kBTU") %>%
    mutate(frac = kBTU/sum(kBTU))
  
  # AEO: https://www.eia.gov/outlooks/aeo/data/browser/#/?id=4-AEO2018&cases=ref2018&sourcekey=0
  # UN: https://di.unfccc.int/detailed_data_by_party
  EOSC_AEO_Retail_Electricity <- 331.596 #TBTU/yr #Food Sales
  EOSC_AEO_FS_NG <- 584.817 * unlist(unname(EOSC_FS_fracs[2,3]))#TBTU/yr # Commercial NG for cooking
  EOSC_AEO_FS_Electricity <- 584.817 * unlist(unname(EOSC_FS_fracs[1,3])) #TBTU/yr ( Food Service total energy - commercial cooking NG use)
  EOSC_AEO_Res_Electricity <- 424.93#TBTU/yr #Residential Freezers + Refrigerator + Cooking (Electricity)
  EOSC_AEO_Res_NG <- 103.04#TBTU/yr # Cooking NG
  EOSC_AEO_Res_Pr <- 17.3#TBTU/yr # Cooking Propane
  EOSC_UN_Commercial_Refrig_GHG <- 45.46 #MMT/yr
  EOSC_UN_Residential_Refrig_GHG <- 1.28 #MMT/yr
  EOSC_AEO_Com_Refridg <- 641.160 #TBTU/yr - Commercial Refrigeration
  
  # Bureau of Economic Analysis Retail & Consumer used retail expenditures. Food Services used food service expenditures.
  # https://apps.bea.gov/
  EOSC_Expendatures <- read_excel("Data/End of Chain - Warehouse USDA-NASS, Retail + Consumption BEA.xlsx", sheet = "Food Expenditures"
                                  , col_types = c("skip", "text", "skip", "text", "numeric"), skip = 4) %>% 
    filter(GeoName != "District of Columbia") %>% rename("stateName" = GeoName, "Expenditure" = `2016`)
  
  EOSC_Expendatures <-EOSC_Expendatures %>%
    mutate(Description = recode_factor(Description
                                       , "Food and beverages purchased for off-premises consumption" = "retailExpendUSD2016"
                                       , "Food services" = "FSExpendUSD2016")) %>%
    pivot_wider(names_from = Description, values_from = Expenditure)
  
  EOSC_sumFSExpendUSD2016 <- sum(EOSC_Expendatures$FSExpendUSD2016)
  EOSC_sumRExpendUSD2016 <- sum(EOSC_Expendatures$retailExpendUSD2016)
  
  # https://www.ers.usda.gov/data-products/food-at-home-monthly-area-prices/
  EOSC_Expendatures_USDA <- read_excel("Data/Retail - FMAP.xlsx", sheet = "Data") %>% 
    filter(str_detect(Metroregion_name, "Census Region")) %>% 
    filter(Year == 2016)
  
  EOSC_RegionalCost <-
    EOSC_Expendatures_USDA%>%
    group_by(Metroregion_name) %>%
    summarise(retailPurchases = sum(Purchase_dollars_wtd),
              retailWeight_gm = sum(Purchase_grams_wtd)) %>%
    mutate(avgWeightperCost = retailWeight_gm/retailPurchases) %>%
    separate(Metroregion_name, c("Metroregion_number", "Region_small"), sep = ":") %>%
    mutate(Region_small = str_remove(Region_small, " ")) %>% 
    select(Region_small, avgWeightperCost)
  
  EOSC_Expendatures <-
    EOSC_Expendatures %>% 
    left_join(BD_stateRegion %>% select(stateName, Region_small), by = "stateName") %>%
    left_join(EOSC_RegionalCost, by = "Region_small") %>%
    ungroup()%>%
    mutate(
      retailExpendEstWt = retailExpendUSD2016 * avgWeightperCost
      , FSExpendEstWt = FSExpendUSD2016 * avgWeightperCost
      , fracFSExpend = FSExpendEstWt/sum(FSExpendEstWt)
      , fracRExpend =  retailExpendEstWt/sum(retailExpendEstWt)
          ) %>% 
    rowwise() %>%
    mutate(
      Services_Electricity = EOSC_AEO_FS_Electricity * fracFSExpend #TBTU/yr * spent in state / total spent
      , Services_NG = EOSC_AEO_FS_NG * fracFSExpend#TBTU/yr * spent in state / total spent
      , Retail_Electricity = EOSC_AEO_Retail_Electricity * fracRExpend #TBTU/yr * spent in state / total spent
      , Residential_Electricity = EOSC_AEO_Res_Electricity* fracRExpend#TBTU/yr * spent in state / total spent
      , Residential_NG = EOSC_AEO_Res_NG* fracRExpend#TBTU/yr * spent in state / total spent
      , Residential_PR = EOSC_AEO_Res_Pr* fracRExpend#TBTU/yr * spent in state / total spent
    ) 
  
  EOSC_TBTU <- EOSC_Expendatures %>% select(-FSExpendUSD2016, -retailExpendUSD2016, -fracFSExpend, -fracRExpend, -Region_small, -avgWeightperCost, -retailExpendEstWt, -FSExpendEstWt ) %>% 
    pivot_longer(!stateName, names_to = "naics_fuel", values_to = "Energy_TBTU") 
  
  EOSC_TBTU <- EOSC_TBTU %>% separate(naics_fuel, c("commodity_naics", "Fuel"), "_") %>% left_join(select(BD_stateRegion, State_Abb, stateName), by = "stateName") %>% 
    mutate(Stage = commodity_naics)
  
  EOSC_TBTU$Stage <-recode_factor(EOSC_TBTU$Stage, "Retail" = "W&R", "Services" = "Consumption", "Residential" = "Consumption")
  EOSC_TBTU$Fuel <-recode_factor(EOSC_TBTU$Fuel, "NG" = "Natural Gas", "PR" = "Petroleum" ,"Electricity" = "Grid Electricity")
  
  F_EOSC_TBTU <- rbind(EOSC_TBTU, EOSC_WR_Warehouse_TBTU)
  
  F_EOSC_TBTU <- F_EOSC_TBTU %>% dplyr::mutate(
    Source =  ifelse(Fuel == "Grid Electricity", "Grid Electricity", "Fuel")
  )
  
  # end of chain source electricity ----
  EOSC_SourceE <- F_EOSC_TBTU %>% filter(Fuel == "Grid Electricity") %>% select(State_Abb, Energy_TBTU, commodity_naics) %>% 
    left_join(select(GEN_state2016, State_Abb, Source, percent, heatRate, tdLossFactor), by = "State_Abb", relationship = "many-to-many") %>% 
    dplyr::mutate(endSourceEnergy_TBTU = percent * Energy_TBTU * heatRate * tdLossFactor/3412.14) # convert heatRate to dimensionless --> heatRate/3412.14
  
  # spread out so each fuel type is a column
  EOSC_SourceE <- EOSC_SourceE %>% select(State_Abb, Source,endSourceEnergy_TBTU, commodity_naics) %>%
    pivot_wider(names_from = Source, values_from = endSourceEnergy_TBTU)
  
  #sum columns together to match classifications, remove unnecessary rows
  EOSC_SourceE <- EOSC_SourceE %>% rowwise() %>%
    dplyr::mutate(OtherSources = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                  , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind", "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)) %>%
    select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind", -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage")
  EOSC_SourceE <- EOSC_SourceE %>% dplyr::rename("Other" = OtherSources)
  
  EOSC_SourceE <- EOSC_SourceE %>% pivot_longer(!c(commodity_naics, State_Abb), names_to = "Fuel", values_to = "Energy_TBTU") %>% 
    dplyr::mutate(Source = "Grid Electricity", Stage = commodity_naics) %>% left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") 
  EOSC_SourceE$Stage <-recode_factor(EOSC_SourceE$Stage, "Warehouse - Cold" = "W&R", "Warehouse - Dry" = "W&R","Retail" = "W&R", "Services" = "Consumption", "Residential" = "Consumption")
  
  # end of chain combine source energy ----
  F_EOSC_SourceTBTU <- F_EOSC_TBTU %>% filter(Fuel != "Grid Electricity") 
  F_EOSC_SourceTBTU$Source <- "Fuel"
  F_EOSC_SourceTBTU <- F_EOSC_SourceTBTU %>% rbind(EOSC_SourceE)
  
  # end of chain GHG ----
  #Electricity GHG
  EOSC_electricityGHG <- F_EOSC_TBTU %>% filter(Fuel == "Grid Electricity") %>% select(State_Abb, Energy_TBTU, commodity_naics) %>% 
    left_join(select(GEN_state2016GHG, State_Abb, Source, percent, CO2e_MMT_TBTU, tdLossFactor), by = "State_Abb", relationship = "many-to-many") %>% 
    dplyr::mutate(eocElectricityGHG =percent * Energy_TBTU * CO2e_MMT_TBTU * tdLossFactor )
  
  EOSC_electricityGHG <- EOSC_electricityGHG %>% select(commodity_naics, State_Abb, Source,eocElectricityGHG) %>%
    pivot_wider(names_from = Source, values_from = eocElectricityGHG)
  
  EOSC_electricityGHG <- EOSC_electricityGHG %>% rowwise() %>%
    dplyr::mutate(OtherSources = sum(c_across(c( Other,"Other Gases")),na.rm = TRUE)
                  , Renewables = sum(c_across(c("Geothermal", "Other Biomass" , "Wood and Wood Derived Fuels" )),na.rm = TRUE)) %>%
    select( -Other, -"Other Gases",-"Geothermal",-"Other Biomass", -"Wood and Wood Derived Fuels") %>% dplyr::rename("Other" = "OtherSources")
  
  EOSC_electricityGHG <- EOSC_electricityGHG %>% pivot_longer(!c(commodity_naics, State_Abb), names_to = "Fuel", values_to = "GHG_MMT")
  EOSC_electricityGHG$Source <- "Grid Electricity"
  EOSC_electricityGHG <- EOSC_electricityGHG %>% left_join(select(BD_stateRegion, stateName, State_Abb), by = "State_Abb")
  
  EOSC__fuelsGHG <- data.frame(
    Fuel = c("Natural Gas","Propane", "Petroleum" )
    , GHG_kgCO2e_MMBTU = c(53.06 + 25*1/1000 + 298 * 0.1/1000 , 62.87 + 25*3/1000 + 298 * 0.6/1000, mean(73.25, 73.96, 75.04) + 25*3/1000 + 298 * 0.6/1000 )
  )
  
  EOSC_FuelGHG <- F_EOSC_TBTU %>% filter(Fuel != "Grid Electricity") %>% select(State_Abb, Energy_TBTU, commodity_naics, Fuel)  %>% 
    left_join(EOSC__fuelsGHG, by = "Fuel") %>% rowwise() %>%
    dplyr::mutate(GHG_MMT = Energy_TBTU * GHG_kgCO2e_MMBTU/1000) %>% # 1000000 MMBTU/TBTU * MMT/1000000000
    select(-Energy_TBTU, -GHG_kgCO2e_MMBTU)
  EOSC_FuelGHG$Source <- "Fuel"
  EOSC_FuelGHG <-EOSC_FuelGHG %>% left_join(select(BD_stateRegion, stateName, State_Abb), by = "State_Abb")
  
  # Commercial Fug GHG 
  # EOSC_UN_Commercial_Refrig_GHG <- 45.46 #MMT/yr
  # EOSC_AEO_Com_Refridg <- 641.160 #TBTU/yr - Commercial Refrigeration
  
  EOSC_Commercial_FugGHG <-
    EOSC_Commercial %>% filter(PBAPLUS %in% c(10, 14, 15, 20, 32, 33, 34)) %>% 
    mutate(commodity_naics = recode_factor(PBAPLUS, 
                                           "10" = "Warehouse - Dry"
                                           ,"14" = "Retail"
                                           , "15" = "Retail"
                                           , "20" = "Warehouse - Cold"
                                           , "32" = "Services"
                                           , "33" = "Services"
                                           , "34" = "Services"))%>% 
    group_by(commodity_naics, CENDIV) %>% 
    summarise(ELMMBTU = sum(ELBTU, na.rm = TRUE)/1000
              , RefMMBTU = sum(ELRFBTU, na.rm = TRUE)/1000
              ,fracFridge = RefMMBTU/ELMMBTU) %>%
    rename("Region" = CENDIV) %>%
    left_join(select(BD_stateRegion, stateName, Region), by = "Region", relationship = "many-to-many") %>% 
    #^ use CBECS to find the average fraction of refrigeration for commercial food electricity
    # V apply fraction to known commercial food electricity use to find commercial food refrigeration energy for each state
    select(commodity_naics, fracFridge, stateName) %>% ungroup() %>%
    # only need Grid Electricity Energy and Commercial
    right_join(filter(F_EOSC_TBTU,Fuel == "Grid Electricity" & commodity_naics != "Residential"), by = c("commodity_naics", "stateName")) %>% 
    mutate(GHG_MMT = fracFridge * Energy_TBTU * EOSC_UN_Commercial_Refrig_GHG/ EOSC_AEO_Com_Refridg
           #fracFridge * Energy_TBTU = portion of group's energy used for refrigeration, 
           # then multiple by total commercial refrigeration GHG/Energy
           # Assumes that everything has about the same fugitive emissions / electricity use.
           , Fuel = "Refrigerant"
           , Source = "Refrigerant"
    ) %>% 
    select(-fracFridge, -Energy_TBTU)
  
  EOSC_Residential_FugGHG <- EOSC_Expendatures %>% rowwise() %>% 
    mutate(GHG_MMT = EOSC_UN_Residential_Refrig_GHG * fracRExpend) %>% 
    select(stateName, GHG_MMT) %>% 
    mutate(Fuel = "Refrigerant", Stage = "Consumption", commodity_naics = "Residential", Source = "Refrigerant") %>%
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "stateName") 
  
  F_EOSC_GHG <- rbind(EOSC_electricityGHG, EOSC_FuelGHG)
  F_EOSC_GHG <- F_EOSC_GHG %>% dplyr::mutate(Stage = commodity_naics)
  F_EOSC_GHG$Stage <-recode_factor(F_EOSC_GHG$Stage, "Warehouse - Dry" = "W&R", "Warehouse - Cold" = "W&R"
                                   ,"Retail" = "W&R", "Services" = "Consumption", "Residential" = "Consumption")
  F_EOSC_GHG <- rbind(F_EOSC_GHG, EOSC_Commercial_FugGHG, EOSC_Residential_FugGHG)
}
# Remove excess variables from environment 
rm(list=ls(pattern="^EOSC"))

# Combine Stages ----
{
  allSiteTBTU <- rbind(F_FARM_SiteTBTU %>% mutate(Stage = "On-farm", subStage = Source), 
                       rbind(
                         F_MANUF_SiteTBTU %>% mutate(Stage = "Manufacturing", subStage = Source), 
                         F_DIST_TBTU %>% mutate(subStage =  recode_factor(Stage, "Distribution (F-M)" = "F-M Dist.", "Distribution" = "Distribution")),
                         F_EOSC_TBTU %>% mutate(subStage = commodity_naics) ) %>%
                         mutate( directUse_yn = ifelse(Source == "Fuel" | Source == "On-site Renewables", T, F)
                                 , electricity_yn = ifelse(Source == "Grid Electricity", T, F)
                                 , primarySource = Fuel
                         )
  )%>%     mutate(Analysis = "Site Energy (TBTU)")%>% dplyr::rename("Value" = Energy_TBTU)
  
  allSourceTBTU <- rbind(F_FARM_SourceTBTU %>% mutate(Stage = "On-farm", subStage = Source), 
                         rbind(
                           F_MANUF_SourceTBTU %>% mutate(Stage = "Manufacturing", subStage = Source, primarySource = Fuel), 
                           F_DIST_TBTU %>% mutate(subStage =  recode_factor(Stage, "Distribution (F-M)" = "F-M Dist.", "Distribution" = "Distribution"), primarySource = Fuel),
                           F_EOSC_SourceTBTU %>% mutate(subStage = commodity_naics, primarySource = Fuel) ) %>%
                           mutate( directUse_yn = ifelse(Source == "Fuel" | Source == "On-site Renewables", T, F)
                                   , electricity_yn = ifelse(Source == "Grid Electricity", T, F)
                                   , primarySource = Fuel
                           ))%>%
    mutate(Analysis = "Primary Energy (TBTU)")%>% dplyr::rename("Value" = Energy_TBTU)
  
  allGHGMMT <- rbind(F_FARM_GHG %>% mutate(Stage = "On-farm", subStage = Source), 
                     rbind(
                       F_MANUF_GHG %>% mutate(Stage = "Manufacturing", subStage = Source, primarySource = Fuel), 
                       F_DIST_GHG %>% mutate(subStage =  recode_factor(Stage, "Distribution (F-M)" = "F-M Dist.", "Distribution" = "Distribution"), primarySource = Fuel),
                       F_EOSC_GHG %>% mutate(subStage = commodity_naics, primarySource = Fuel)) %>%
                       mutate( directUse_yn = ifelse(Source == "Fuel" | Source == "On-site Renewables", T, F)
                               , electricity_yn = ifelse(Source == "Grid Electricity", T, F)
                               , primarySource = Fuel
                       ) )%>%   mutate(Analysis = "GHG Emissions (MMT CO2e)") %>% dplyr::rename("Value" = GHG_MMT)
  
  
  # combine all data with Farm distribution as own stage
  allData <-
    rbind(allSiteTBTU, allSourceTBTU, allGHGMMT) %>%
    mutate(Fuel = recode_factor(Fuel,  "Diesel" ="Petroleum"  , "Gasoline"="Petroleum"  , "LP Gas"="Petroleum"  ),
           primarySource = recode_factor(primarySource, "Electricity" = "Grid Electricity")) %>%
    group_by(commodity_naics ,State_Abb ,stateName  , Source , Fuel ,  primarySource ,  Stage,subStage, directUse_yn, electricity_yn  ,      Analysis) %>%
    summarise(Value = sum(Value, na.rm = T)) %>% 
    mutate(
      Fuel = factor(Fuel, levels = LEVELS_fuel),
      Source = factor(Source, levels = LEVELS_source), 
      Stage = factor(Stage , levels = c("On-farm", "Distribution (F-M)", "Manufacturing", "Distribution", "W&R", "Consumption"))) %>% 
    rowwise() %>%
    dplyr::mutate(
      Value = ifelse(
        commodity_naics == "Animal Feed" | commodity_naics == "Animal Feed Processing",
        Value * BD_percentNotPet, Value    )  ) %>%
    mutate(
      Stage_x = recode_factor(Stage ,  "Distribution (F-M)" = "\nDistribution (F-M)", "Distribution" = "\nDistribution"),
      Stage_x = factor(Stage_x , levels = c("On-farm", "\nDistribution (F-M)", "Manufacturing", "\nDistribution", "W&R", "Consumption")),
      Stage_y = recode_factor(Stage ,  "Distribution (F-M)" = "On-farm"),
      Stage_y = factor(Stage_y , levels = c("On-farm", "Manufacturing", "Distribution", "W&R", "Consumption")),
    )
  
  # move farm distribution & Animal feed to "on-farm"
  BD_animalFeedBreakdown <- data.frame( commodity_naics_z = c("Meat, Poultry, Eggs", "Dairy"), feedFrac = c(BD_percentFeednotDairyinNotPet, BD_percentFeedDairyCowinNotPet))
  
  allData_finalAF <-
    allData %>% filter(commodity_naics == "Animal Feed" | commodity_naics == "Animal Feed Processing") %>%
    merge(BD_animalFeedBreakdown) %>% 
    rowwise() %>% dplyr::mutate(
      Value = Value * feedFrac,
      Stage_z = "On-farm"
      , Fuel_z = #ifelse(Analysis == "Site Energy (TBTU)",
        ifelse(commodity_naics == "Animal Feed Processing", "AF Manuf."
               , ifelse(commodity_naics == "Animal Feed" & Stage_y == "Distribution", "AF Manuf.", "AF Growth"))
      # , "AF Growth" )#Why was this Fuel not AF Growth? as.character(Fuel))
      , Source_z = #ifelse(commodity_naics == "Animal Feed Processing",
        switch(as.character(Source),
               "Fuel" = "AF Fuel",
               "Grid Electricity" = "AF Electricity",
               "On-site Renewables" = "AF Renewables",
               "Renewables" = "AF Renewables",
               "Refrigerant" = "AF Refrig.",
               "Fertilizer Manuf."  = "AF Chemicals",
               "Pesticide Manuf."= "AF Chemicals",
               "Truck" = "AF Transport",
               "Rail" = "AF Transport",
               "Crop Emissions" = "AF Crop Emissions")
    ) %>% select(-feedFrac) %>% 
    mutate(FP_yn = ifelse(Source == "Fertilizer Manuf."|Source == "Pesticide Manuf." , T, F),
           AF_yn = T,
           directUse_yn = F
    ) 
  
  allData_final <- filter(allData, commodity_naics != "Animal Feed" & commodity_naics != "Animal Feed Processing") %>%
    mutate(commodity_naics_z = commodity_naics, Stage_z = as.character(Stage_y), Fuel_z = as.character(Fuel)
           , Source_z = as.character(Source) #, subStage_z = subStage
           , FP_yn = ifelse(Source == "Fertilizer Manuf."| Source == "Pesticide Manuf." | Source == "AF Chemicals", T, F)
           , AF_yn = F
    ) %>%     rbind(allData_finalAF) %>% ungroup() %>%
    mutate(primarySource = factor(primarySource, levels = LEVELS_fuel),
           commodity_naicsF = addline_format(factor(commodity_naics), levels = LEVELS_naicsLine),
           commodity_naicsF_z = addline_format(factor(commodity_naics_z), levels = LEVELS_naicsLine)
    ) %>%  
    select(Analysis,Stage_z, commodity_naics_z, commodity_naicsF_z, Fuel_z, primarySource, subStage, 
           Source_z,  State_Abb, stateName, FP_yn, AF_yn, Value, directUse_yn, electricity_yn) %>% 
    dplyr::rename(Stage = Stage_z
                  , commodity_naics = commodity_naics_z, commodity_naicsF = commodity_naicsF_z
                  , Fuel = Fuel_z, Source = Source_z, subSource = subStage) %>%
    mutate(
      Stage = factor( Stage , levels = c("On-farm", "Distribution (F-M)","Manufacturing",  "Distribution", "W&R", "Consumption"))
      , Fuel_red = recode_factor(Fuel, "F&P Electricity"= "Chemical Manuf.", "F&P Fuel" = "Chemical Manuf."
                                 ,"AF Manuf." = "Animal Feed", "AF Growth" = "Animal Feed", "Other" = "Other Fuel")
      , sourceGroup = recode_factor(Source, 
                                    "F&P Electricity" = "Grid Electricity", 
                                    "F&P Fuel" = "Fuel",
                                    "F&P Process" = "Process/Refrig.",
                                    "On-site Renewables" = "Renewables",
                                    "Refrigerant" = "Process/Refrig.",
                                    "Truck" = "Fuel",   "Rail" = "Fuel",
                                    "Truck Refrig." = "Process/Refrig.", "Rail Refrig." = "Process/Refrig.",
                                    "AF Fuel" = "Fuel", "AF Renewables" = "Renewables", "AF Electricity" = "Grid Electricity", "AF Transport" = "Fuel",
                                    "AF Refrig." = "Process/Refrig.", "AF Crop Emissions" = "Crop Emissions"  ,
                                    "AF F&P Fuel" = "Fuel", "AF F&P Electricity" = "Grid Electricity" )
      , sourceGroup = ifelse(primarySource != "Process", ifelse(Source == "Pesticide Manuf." | Source == "Fertilizer Manuf.", 
                                                                ifelse(Fuel == "F&P Electricity", "Grid Electricity", "Fuel"), as.character(sourceGroup)), as.character(sourceGroup) )
      , sourceGroup = ifelse(Source == "AF Chemicals", ifelse(primarySource == "Grid Electricity", "Grid Electricity", "Fuel"), as.character(sourceGroup))
      , sourceLocation = recode_factor(Source, 
                                       "Fuel" = "Site", "Grid Electricity" = "Site", "On-site Renewables" = "Site", 
                                       "Animal Emissions" = "Site", "Crop Emissions" = "Site","Refrigerant" = "Site",
                                       
                                       "F&P Electricity" = "Chemical Manuf.", "F&P Fuel" = "Chemical Manuf.",
                                       "Truck" = "Transport", "Rail" = "Transport","Truck Refrig." = "Transport", "Rail Refrig." = "Transport",
                                       "AF Fuel" = "Animal Feed", "AF Renewables" = "Animal Feed", "AF Electricity" = "Animal Feed",
                                       "AF Refrig." = "Animal Feed", "AF Transport" = "Animal Feed", "AF Crop Emissions" = "Animal Feed", 
                                       "AF F&P Fuel" = "Animal Feed", "AF F&P Electricity" = "Animal Feed"  )  
      , sourceLocation = ifelse(Source == "Pesticide Manuf." | Source == "Fertilizer Manuf.", 
                                "Chemicals", as.character(sourceLocation)) 
      , sourceLocation = ifelse(Source == "AF Chemicals", "Animal Feed", as.character(sourceLocation))
      , Fuel = recode_factor(Fuel, "Other" = "Other Fuel")
      , primarySource = recode_factor(primarySource, "Other" = "Other Fuel")
      , transport_yn = ifelse(sourceLocation == "Transport" | str_detect(Source, "Transport"), T, F)
      , nonEnergyEmissions_yn = ifelse(str_detect(Source, "Emissions") | str_detect(Source, "Refrig") | str_detect(primarySource, "Process"), T, F)
      
    )
  
  remove("allData_finalAF")
  
  allDataSummary <- aggregate(Value ~ Analysis+Stage_y,allData, FUN = sum) %>% pivot_wider(names_from = Analysis, values_from = Value)
  allData_finalSummary <- aggregate(Value ~ Analysis+Stage,allData_final, FUN = sum) %>% pivot_wider(names_from = Analysis, values_from = Value)
  
}



# Final Database ----
library(writexl)

energyGHG_DB <-
  allData_final %>%
  rename(sourceType = Source) %>%
  rename(sourceDetail = subSource) %>%
  rename(Source = Fuel) %>%
  mutate(siteUse = ifelse(str_detect(Source, "AF"), as.character(Source), sourceDetail)
         , siteUse = ifelse(Stage == "W&R" | Stage == "Consumption", as.character(sourceType), siteUse)
         # , siteUse = ifelse(siteUse == "Fuel", as.character(Fuel), siteUse)
         , sourceGroup = ifelse(electricity_yn, "Grid Electricity", sourceGroup)
         , subSource = ifelse(str_detect(sourceType, "Rail") | str_detect(sourceType, "Truck"), sourceType, sourceDetail)
         , sourceType = ifelse(str_detect(sourceType, "Rail") | str_detect(sourceType, "Truck"), 
                               ifelse(str_detect(sourceType, "Refrig."), "Transport Refrig.", "Transport"), sourceType)
         
         , Source = ifelse(str_detect(Source, "AF"), as.character(sourceType), as.character(Source))
         , Source = ifelse(FP_yn & electricity_yn, ifelse(AF_yn, "AF Chemicals", "F&P Electricity"), as.character(Source))
         , Source = ifelse(electricity_yn & sourceLocation == "Site", "Grid Electricity", as.character(Source))
         , Source = ifelse(siteUse == "F-M Dist.", "F-M Dist.", as.character(Source))
         
         , sourceType = ifelse(str_detect(Source, "F&P"), as.character(Source), as.character(sourceType))
         
         # , Source = ifelse(electricity_yn & FP_yn & !AF_yn, as.character("F&P Electricity"), as.character(Source))
         # , sourceType = ifelse(FP_yn & electricity_yn, "F&P Electricity", as.character(sourceType))
         
  ) %>%
  
  select(Analysis, Stage, commodity_naics, State_Abb, stateName, primarySource, siteUse, Source,sourceGroup, sourceLocation, sourceType, sourceDetail
         , directUse_yn, electricity_yn, FP_yn, AF_yn, transport_yn, Value) 


write_csv(as.data.frame(energyGHG_DB), "Results/finalDB.csv")

write_xlsx(as.data.frame(energyGHG_DB), "Results/finalDatabase.xlsx")





# Extreme Summary ----
allData_final %>% 
  dplyr::mutate(groupA = recode_factor( sourceGroup,"Grid Electricity" = "Energy", "Fuel" = "Energy", "Renewables" = "Energy",
                                        "Animal Emissions" = "Animal/Crop Emissions", "Crop Emissions" = "Animal/Crop Emissions"),
                group = ifelse(groupA == "Energy", as.character(sourceLocation), as.character(groupA) ) ) %>%
  group_by(Analysis, Stage, group) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  dplyr::mutate(Value = signif(Value, digits = 3)) %>%
  pivot_wider(names_from = Stage, values_from = Value ) %>%
  arrange(Analysis) 

# Helpful tables aggregating data in different ways ----

#TOTALS
allData_final %>% group_by(Analysis) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  dplyr::mutate(Values = signif(Values, 4)) %>%
  dplyr::mutate(PJ = Values * ifelse(Analysis == "GHG Emissions (MMT CO2e)", 1, 1.05506)) 

pivot_wider(names_from = Analysis, values_from = Values) %>% 
  select(`Site Energy (TBTU)`, `Primary Energy (TBTU)`, `GHG Emissions (MMT CO2e)`) #%>% write_clip()


#STAGE TOTALS
allData_final %>% group_by(Analysis, Stage) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values)  %>%
  mutate(`Site Energy perc` = percent(`Site Energy (TBTU)   perc`, accuracy = .1),  
         `Primary Energy perc`= percent(`Primary Energy (TBTU)   perc`, accuracy = .1),
         `GHG Emissions perc`= percent(`GHG Emissions (MMT CO2e)   perc`, accuracy = .1) )%>% 
  select(Stage, `Site Energy (TBTU)   Values`  , `Site Energy perc`,`Primary Energy (TBTU)   Values` ,  
         `Primary Energy perc` , `GHG Emissions (MMT CO2e)   Values`, `GHG Emissions perc` ) %>% view()




#STAGE TOTALS PJ

allData_final %>% group_by(Analysis, Stage) %>% summarise(TBTU = sum(Value, na.rm = TRUE)) %>%
  mutate(PJ = TBTU * ifelse(Analysis == "GHG Emissions (MMT CO2e)", 1, 1.05506)) %>%
  pivot_longer(c(PJ, TBTU),names_to = "name1", values_to = "values") %>% ungroup() %>%
  separate(Analysis, c("Analysis", "Unit"), sep = ' \\(') %>%
  mutate(name2 = ifelse(Analysis == "GHG Emissions", "GHG Emissions (MMT CO2e)", paste(Analysis, " (", name1,")", sep = ""))) %>% 
  select(-Analysis, -name1, -Unit) %>% 
  unique()%>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  select(Stage, `Site Energy (TBTU)`  , `Site Energy (PJ)`,`Primary Energy (TBTU)` ,  
         `Primary Energy (PJ)` , `GHG Emissions (MMT CO2e)` ) %>% view

allData_final %>% filter(Stage == "Consumption") %>% group_by(Analysis, commodity_naicsF) %>% summarise(TBTU = sum(Value, na.rm = TRUE)) %>%
  mutate(PJ = TBTU * ifelse(Analysis == "GHG Emissions (MMT CO2e)", 1, 1.05506)) %>%
  pivot_longer(c(PJ, TBTU),names_to = "name1", values_to = "values") %>% ungroup() %>%
  separate(Analysis, c("Analysis", "Unit"), sep = ' \\(') %>%
  mutate(name2 = ifelse(Analysis == "GHG Emissions", "GHG Emissions (MMT CO2e)", paste(Analysis, " (", name1,")", sep = ""))) %>% 
  select(-Analysis, -name1, -Unit) %>% 
  unique()%>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  select(commodity_naicsF, `Site Energy (TBTU)`  , `Site Energy (PJ)`,`Primary Energy (TBTU)` ,  
         `Primary Energy (PJ)` , `GHG Emissions (MMT CO2e)` ) 



#STAGE TOTALS
allData_final %>% group_by(Analysis, Stage, State_Abb, commodity_naics) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  filter(Stage == "On-farm") %>%
  pivot_wider(names_from = Analysis, values_from = Values) 

allData_final %>% 
  filter(Stage == "Manufacturing") %>%
  group_by(Analysis, Stage, commodity_naics) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Analysis) %>% mutate(perc = percent(Value/sum(Value))) %>%
  # pivot_wider(names_from = Analysis, values_from = Value) 
  select(-Value) %>%pivot_wider(names_from = Analysis, values_from = perc) 

allData_final %>% 
  filter(Stage == "On-farm") %>%
  filter(commodity_naics == "Dairy" | str_detect(commodity_naics, "Meat") | str_detect(commodity_naics, "Seafood")) %>%
  group_by(Analysis, sourceLocation) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Analysis) %>% 
  # mutate(perc = percent(Value/sum(Value))) %>%
  pivot_wider(names_from = Analysis, values_from = Value) %>% view()
# select(-Value) %>%pivot_wider(names_from = Analysis, values_from = perc) %>% view()

#FUEL TOTALS
allData_final %>% group_by(Analysis, Fuel) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  select(Fuel, `Site Energy (TBTU)   Values`  , `Site Energy (TBTU)   perc`,`Primary Energy (TBTU)   Values` ,  
         `Primary Energy (TBTU)   perc` , `GHG Emissions (MMT CO2e)   Values`, `GHG Emissions (MMT CO2e)   perc` ) %>%
  view()

#FUEL TOTALS - in PJ
allData_final %>% group_by(Analysis, Fuel) %>% summarise(TBTU = sum(Value, na.rm = TRUE)) %>%
  mutate(PJ = TBTU * ifelse(Analysis == "GHG Emissions (MMT CO2e)", 1, 1.05506)) %>%
  pivot_longer(c(PJ, TBTU),names_to = "name1", values_to = "values") %>% ungroup() %>%
  separate(Analysis, c("Analysis", "Unit"), sep = ' \\(') %>%
  mutate(name2 = ifelse(Analysis == "GHG Emissions", "GHG Emissions (MMT CO2e)", paste(Analysis, " (", name1,")", sep = ""))) %>% 
  select(-Analysis, -name1, -Unit) %>% 
  unique()%>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  select(Fuel, `Site Energy (TBTU)`  , `Site Energy (PJ)`,`Primary Energy (TBTU)` ,  
         `Primary Energy (PJ)` , `GHG Emissions (MMT CO2e)` ) %>%
  view()
allData_final %>% 
  mutate(sourceLocation2 = ifelse(str_detect(Fuel, "Emissions"), paste(sourceLocation, "B"), sourceLocation)) %>%
  group_by(Analysis, Stage, sourceLocation2) %>% summarise(TBTU = sum(Value, na.rm = TRUE)) %>%
  mutate(PJ = TBTU * ifelse(Analysis == "GHG Emissions (MMT CO2e)", 1, 1.05506)) %>%
  pivot_longer(c(PJ, TBTU),names_to = "name1", values_to = "values") %>% ungroup() %>%
  separate(Analysis, c("Analysis", "Unit"), sep = ' \\(') %>%
  mutate(name2 = ifelse(Analysis == "GHG Emissions", "GHG Emissions (MMT CO2e)", paste(Analysis, " (", name1,")", sep = ""))) %>% 
  select(-Analysis, -name1, -Unit) %>% 
  unique()%>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  select(Stage, sourceLocation2, `Site Energy (TBTU)`  , `Site Energy (PJ)`,`Primary Energy (TBTU)` ,  
         `Primary Energy (PJ)` , `GHG Emissions (MMT CO2e)` ) %>% view()


# Fuel total, but Electricity is in Primary
allData_final  %>%
  group_by(Analysis, sourceGroup) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  rename(Source = sourceGroup,
         `Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
         delete1 = `Site Energy (TBTU)   perc`,
         `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
         delete2 =  `Primary Energy (TBTU)   perc` ,
         `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
         delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select( Source, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
          delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% write_clip()


# Fuel/Stage
allData_final %>% 
  group_by(Analysis, Stage, Fuel) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  arrange(Stage)%>%
  rename(`Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
         delete1 = `Site Energy (TBTU)   perc`,
         `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
         delete2 =  `Primary Energy (TBTU)   perc` ,
         `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
         delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select(Stage,Fuel, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
         delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% view #write_clip()

# Fuel Source/Stage
allData_final  %>%
  group_by(Analysis, sourceGroup, Stage) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Analysis, Stage) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  arrange(Stage)%>%
  rename(Source = sourceGroup,
         `Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
         delete1 = `Site Energy (TBTU)   perc`,
         `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
         delete2 =  `Primary Energy (TBTU)   perc` ,
         `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
         delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select(Stage, Source, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
         delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% write_clip()


# NAICS/Stage
allData_final %>% group_by(Analysis, Stage, commodity_naics) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  arrange(Stage)%>%
  rename("Commodity" = commodity_naics,
         `Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
         delete1 = `Site Energy (TBTU)   perc`,
         `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
         delete2 =  `Primary Energy (TBTU)   perc` ,
         `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
         delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select(Stage,Commodity, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
         delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% write_clip()  

# Animal vs. Crop
allData_final %>% 
  filter(Stage == "On-farm" | Stage == "Manufacturing") %>%
  mutate(Group = ifelse(commodity_naics %in% c("Dairy", "Dairy Products", "Meat, Poultry, Eggs", "Animal Product Processing", "Seafood", "Seafood Processing"), "Animal", "Crop")) %>%
  group_by(Analysis, Stage, Group, Fuel) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  arrange(Stage)%>%
  rename(
    `Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
    delete1 = `Site Energy (TBTU)   perc`,
    `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
    delete2 =  `Primary Energy (TBTU)   perc` ,
    `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
    delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select(Stage,Group, Fuel,`Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
         delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% write_clip()  

# STAGE & NAICS & fuel
allData_final %>% group_by(Analysis, Stage, commodity_naics, Fuel) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = signif(Values/sum(Values),2), Values = signif(Values, 4)) %>%
  pivot_longer(c(perc, Values),names_to = "name1", values_to = "values") %>% ungroup() %>%
  mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
  pivot_wider(names_from = name2, values_from = values) %>% 
  arrange(Stage)%>%
  rename("Commodity" = commodity_naics,
         `Site Energy (TBTU)` = `Site Energy (TBTU)   Values`  ,
         delete1 = `Site Energy (TBTU)   perc`,
         `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Values` ,  
         delete2 =  `Primary Energy (TBTU)   perc` ,
         `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Values`,
         delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
  select(Stage, Fuel, Commodity, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
         delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% write_clip()

#Stage & reduced NAICS & Fuel (pivot)
allData_final %>% 
  dplyr::mutate(
    commodity_naics = recode_factor(commodity_naics,
                                    "Meat, Poultry, Eggs" = "Animal Products",
                                    "Fruit, Vegetables, Nuts Products" = "Fruit, Vegetables, Nuts",
                                    "Dairy Products" = "Dairy",
                                    "Seafood Processing" = "Seafood",
                                    "Animal Product Processing" = "Animal Products",
                                    "Sugar, Confectionary" = "Sugar"),
    Group = recode_factor(commodity_naics,
                          "Seafood" = "Animal Products",
                          "Grain, Oil Milling" = "Grain, Oil",
                          "Bakeries, Tortillas"   = "Grain, Oil",
                          "Other Products"  = "Other",
                          
                          "Retail" = "W&R",
                          "Warehouse - Cold" = "W&R",
                          "Warehouse - Dry"   = "W&R"
    ),
    Group = factor(Group, levels = c(  "Animal Products",  "Dairy","Grain, Oil",
                                       "Fruit, Vegetables, Nuts","Sugar",
                                       "Other", "W&R",  "Services", "Residential"))
  ) %>%
  group_by(Analysis, Stage, Group, Fuel) %>% 
  # filter(Stage == "On-farm") %>%
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Stage, values_from = Values) %>%
  # pivot_wider(names_from = Fuel, values_from = Values) %>%
  
  arrange(Analysis, Group) %>% write_clip

#FILTERED STAGE, FILTERED ANALYSIS - FUEL - NAICS
allData_final %>% group_by(Analysis, Stage, commodity_naics, Fuel) %>% 
  # filter(Analysis == "Site Energy (TBTU)") %>% 
  # filter(Analysis == "GHG Emissions (MMT CO2e)") %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>% ungroup()%>%
  group_by(Analysis, commodity_naics)%>%
  # mutate(perc = percent(Values/sum(Values))) %>% select(-Values) %>%
  # pivot_wider(names_from = Fuel, values_from = perc)  %>%
  filter(Stage == "Manufacturing") %>% write_clip()

# AVERAGE U.S. GRID EFF
sum(GEN_US2016$Generation_MWh) * (3412.14*1000/(1000000000000))/sum(GEN_US2016$Generation_TBTU)

# REDONE "SOURCE" ENERGY TO SEPERATE GRID LOSSES FROM ELECTRICITY USED IN GRAPH
allData_final %>% group_by(Analysis, Stage, Fuel, sourceGroup, sourceLocation) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  filter(Analysis != "GHG Emissions (MMT CO2e)") %>%
  filter(sourceGroup == "Grid Electricity") %>% ungroup() %>%
  group_by(Analysis, Stage, sourceLocation) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% 
  mutate(`Generation Losses` = `Primary Energy (TBTU)` - `Site Energy (TBTU)`) %>%
  rename(`Electricity` = `Site Energy (TBTU)`) %>% select(-`Primary Energy (TBTU)`) %>% 
  pivot_longer(c(`Electricity`, `Generation Losses`), names_to = "sourceGroup", values_to = "Values") %>% 
  mutate(Analysis = "Primary Energy (TBTU)") %>% 
  rbind(allData_final %>% group_by(Analysis, Stage, sourceGroup, sourceLocation) %>% 
          summarise(Values = sum(Value, na.rm = TRUE)) %>%
          filter(Analysis == "Primary Energy (TBTU)") %>%
          filter(sourceGroup != "Grid Electricity") %>% ungroup()) %>% ungroup() %>%
  mutate(site = ifelse(sourceGroup != "Generation Losses" & sourceLocation == "Site", "On-site", "Off-site")) %>% 
  ungroup() %>% 
  group_by(sourceGroup) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) #%>% write_clip()

# total off/on site (comment out previous two lines)
group_by(Stage, site) %>%  summarise(Values = sum(Values, na.rm = TRUE)) %>% mutate(perc = percent(Values/sum(Values))) %>%
  select(-Values) %>% pivot_wider(names_from = site, values_from = perc)

# stage off/on site only primary energy
allData_final %>% group_by(Analysis, Stage, Fuel, sourceGroup, sourceLocation) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  filter(Analysis != "GHG Emissions (MMT CO2e)") %>%
  filter(sourceGroup == "Grid Electricity") %>% ungroup() %>%
  group_by(Analysis, Stage, sourceLocation) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% 
  mutate(`Generation Losses` = `Primary Energy (TBTU)` - `Site Energy (TBTU)`) %>%
  rename(`Electricity` = `Site Energy (TBTU)`) %>% select(-`Primary Energy (TBTU)`) %>% 
  pivot_longer(c(`Electricity`, `Generation Losses`), names_to = "sourceGroup", values_to = "Values") %>% 
  mutate(Analysis = "Primary Energy (TBTU)") %>% 
  rbind(allData_final %>% group_by(Analysis, Stage, sourceGroup, sourceLocation) %>% 
          summarise(Values = sum(Value, na.rm = TRUE)) %>%
          filter(Analysis == "Primary Energy (TBTU)") %>%
          filter(sourceGroup != "Grid Electricity") %>% ungroup()) %>% ungroup() %>%
  mutate(site = ifelse(sourceGroup != "Generation Losses" & sourceLocation == "Site", "On-site", "Off-site")) %>% 
  filter(Stage == "On-farm") %>%
  ungroup() %>% group_by(sourceGroup, sourceLocation) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) %>% write_clip()

# On-site vs off-site, filtered by stage
allData_final %>% group_by(Analysis, Stage, Fuel, sourceGroup, sourceLocation) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  filter(Analysis != "GHG Emissions (MMT CO2e)") %>%
  filter(sourceGroup == "Grid Electricity") %>% ungroup() %>%
  group_by(Analysis, Stage, sourceLocation) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% 
  mutate(`Generation Losses` = `Primary Energy (TBTU)` - `Site Energy (TBTU)`) %>%
  rename(`Electricity` = `Site Energy (TBTU)`) %>% select(-`Primary Energy (TBTU)`) %>% 
  pivot_longer(c(`Electricity`, `Generation Losses`), names_to = "sourceGroup", values_to = "Values") %>% 
  mutate(Analysis = "Primary Energy (TBTU)") %>% 
  rbind(allData_final %>% group_by(Analysis, Stage, sourceGroup, sourceLocation) %>% 
          summarise(Values = sum(Value, na.rm = TRUE)) %>%
          filter(Analysis == "Primary Energy (TBTU)") %>%
          filter(sourceGroup != "Grid Electricity") %>% ungroup()) %>% ungroup() %>%
  mutate(site = ifelse(sourceGroup != "Generation Losses" & sourceLocation == "Site", "On-site", "Off-site")) %>% 
  filter(Stage != "On-farm") %>%
  ungroup() %>% group_by(Stage, sourceGroup, sourceLocation) %>%
  summarise(Values = sum(Values, na.rm = TRUE)) %>% write_clip()

# on -farm how much of animal & dairy emissions are from AF?
allData_final %>% filter(Stage == "On-farm") %>% 
  filter(Analysis == "Site Energy (TBTU)") %>%
  filter(commodity_naics == "Dairy" | commodity_naics == "Meat, Poultry, Eggs")%>% 
  mutate(metric = ifelse(str_detect(Fuel, "AF"), "Feed", "Other")) %>%
  group_by(commodity_naics, metric) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>% 
  ungroup() %>% group_by(commodity_naics)%>%
  mutate(perc = percent(Values/sum(Values, na.rm = TRUE)))

#Stage & commodity - change stage as needed
allData_final %>%  
  filter(Stage == "Manufacturing") %>%
  group_by(Analysis, commodity_naics) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Analysis) %>%
  mutate(frac = percent(Values / sum(Values), accuracy = 0.1)) %>%
  select(-Values) %>%
  pivot_wider(names_from = Analysis, values_from = frac) %>%
  write_clip

#naics as a fraction of full FSC
allData_final %>%  
  group_by(Analysis, Stage, commodity_naics) %>% 
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Analysis) %>%
  mutate(frac = percent(Values / sum(Values), accuracy = 0.1)) %>%
  select(-Values) %>%
  pivot_wider(names_from = Analysis, values_from = frac) %>%
  write_clip

filter(allData_final, Analysis == "Site Energy (TBTU)" & Fuel_red == "Animal Feed") %>% 
  group_by(Stage, Fuel, Fuel_red) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE))


# Very specific
#Farm distribution substage
allData_final %>%
  filter(Stage == "On-farm") %>% filter(sourceLocation == "Transport") %>%
  group_by(Analysis) %>% summarise(Value = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Value) %>% 
  select(`Site Energy (TBTU)`  , `Primary Energy (TBTU)` ,  
         `GHG Emissions (MMT CO2e)` )  

# FILTERED STAGE & FUEL PERC
allData_final %>% group_by(Analysis, Stage, Fuel) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = percent(Values/sum(Values))) %>% select(-Values) %>%
  pivot_wider(names_from = Analysis, values_from = perc)  %>% filter(Stage == "Manufacturing")


# STAGE - FUEL
allData_final %>% group_by(Analysis, Stage, Fuel) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% view() #write_clip()


# TOTAL SOURCE/FUEL
allData_final %>% group_by(Analysis, Fuel) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  mutate(perc = percent(Values/sum(Values))) %>% select(-Values) %>%
  pivot_wider(names_from = Analysis, values_from = perc)

allData_final  %>%
  # filter(sourceGroup %in% c("Grid Electricity", "Fuel", "Renewables")) %>%
  group_by(Analysis, sourceGroup) %>% summarise(Values = sum(Value, na.rm = TRUE))

allData_final  %>%
  filter(sourceGroup %in% c("Grid Electricity", "Fuel", "Renewables")) %>%
  group_by(Analysis, sourceGroup) %>% summarise(Values = sum(Value, na.rm = TRUE))%>%
  mutate(perc = percent(Values/sum(Values))) %>% select(-Values) %>%
  pivot_wider(names_from = Analysis, values_from = perc) 

# STAGE REDUCED FUEL - STAGE
allData_final %>% group_by(Analysis, Stage, Fuel_red) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% write_clip()

# STAGE SOURCE - STAGE
allData_final %>% group_by(Analysis, Stage, Source) %>% summarise(Values = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Values) %>% view() #write_clip()

# for FLW changes paper 
allData_final %>% 
  dplyr::mutate(
    commodity_naics = recode_factor(commodity_naics,
                                    "Meat, Poultry, Eggs" = "Animal Products",
                                    "Fruit, Vegetables, Nuts Products" = "Fruit, Vegetables, Nuts",
                                    "Dairy Products" = "Dairy",
                                    "Seafood Processing" = "Seafood",
                                    "Animal Product Processing" = "Animal Products",
                                    "Sugar, Confectionary" = "Sugar"),
    Group = recode_factor(commodity_naics,
                          "Seafood" = "Animal Products",
                          "Grain, Oil Milling" = "Grain, Oil",
                          "Bakeries, Tortillas"   = "Grain, Oil",
                          "Other Products"  = "Other",
                          
                          "Retail" = "W&R",
                          "Warehouse - Cold" = "W&R",
                          "Warehouse - Dry"   = "W&R"
    ),
    Group = factor(Group, levels = c(  "Animal Products",  "Dairy","Grain, Oil",
                                       "Fruit, Vegetables, Nuts","Sugar",
                                       "Other", "W&R",  "Services", "Residential")),
    StageX =       ifelse(Stage == "On-farm" & sourceLocation == "Transport", 
                          "Farm Distribution", as.character(Stage)
    )
    
    # sourceLocationX = recode_factor(sourceLocation, 
    # "Chemical Manuf." = "Site", "Animal Feed" = "Site" ) ,
  ) %>% 
  
  group_by(Analysis, StageX, Group) %>% 
  # filter(Stage == "On-farm") %>%
  summarise(Values = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = StageX, values_from = Values) %>% 
  rowwise()%>%
  mutate(GroupX = recode_factor(Group, "W&R" = "All", "Services" = "All", "Residential" = "All"),
         test = sum(Consumption,`W&R`, na.rm = TRUE)) %>% 
  ungroup()%>% select(-`W&R`, -Consumption) %>%
  pivot_wider(names_from = Group, values_from = test) %>% 
  select(Analysis, GroupX, `On-farm`, `Farm Distribution`, Manufacturing, Distribution, `W&R`,`Services`, `Residential`) %>%
  # pivot_wider(names_from = Fuel, values_from = Values) %>%
  
  arrange(Analysis, GroupX) %>% write_clip



# Main Graphs for manuscript ----
  # Bar graphs w/o EoL ----
# NOTE: these graphs are set up to use the allData_final which combines on-farm distribution with on-farm and allocates the animal feed manuf to on-farm

siteBar <-
  allData_final %>%
  filter( Analysis == "Site Energy (TBTU)") %>%
  group_by(Stage, Fuel_red) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  mutate(Fuel_red = factor(Fuel_red, levels = LEVELS_fuel_red)) %>% 
  ggplot()+
  geom_col(aes(x = Stage, y = Value, fill = Fuel_red), color = "black") +
  scale_fill_manual(values = pivot_wider(filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "Fuel", values_to = "Color" )
                                                , Fuel %in% as.character(unique(filter(allData_final, Analysis == "Site Energy (TBTU)")$Fuel_red)))
                                         , names_from = Fuel, values_from = Color))+
  labs(x = "", y = "Site Energy (TBTU)", fill = "Energy Sources")+
  guides(fill = guide_legend(nrow = 4))+
  THEME_simple +    theme(
    legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
    , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
    , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    , legend.background = element_blank()
    , legend.box.background = element_rect(colour = "black")
    , legend.box.margin=margin(-5,0,0,0)
    , legend.position = c(.6,.8 )
  )#, legend.title = element_blank())

png(filename = "Results/Plots/Fig 2 - siteBar.png", type = "cairo", units =
      "in", width = 6.5, height = 3.6, pointsize = 10, res = 600) 
siteBar 
dev.off() 

sourceBar <-
  allData_final %>%
  filter( Analysis == "Primary Energy (TBTU)") %>%
  group_by(Stage, primarySource) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  mutate(primarySource = factor(primarySource, levels = LEVELS_fuel_red)) %>% 
  ggplot()+
  geom_col(aes(x = Stage, y = Value, fill = primarySource), color = "black") +
  scale_fill_manual(values = pivot_wider(filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "primarySource", values_to = "Color" )
                                                , primarySource %in% as.character(unique(filter(allData_final, Analysis == "Primary Energy (TBTU)")$primarySource)))
                                         , names_from = primarySource, values_from = Color))+
  labs(x = "", y = "Primary Energy (TBTU)", fill = "Energy Sources")+
  guides(fill = guide_legend(nrow = 3))+
  THEME_simple +    theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
                          , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
                          , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          , legend.background = element_blank()
                          , legend.box.background = element_rect(colour = "black")
                          # , legend.box.margin=margin(-5,0,0,0)
                          , legend.position = c(.6,.8 ))#, legend.title = element_blank())  
png(filename = "Results/Plots/Fig 3 - sourceBar.png", type = "cairo", units =
      "in", width = 6.5, height = 3.6, pointsize = 10, res = 600) 
sourceBar 
dev.off() 

GHGBar <-
  allData_final %>%
  filter( Analysis == "GHG Emissions (MMT CO2e)") %>%
  mutate(
    primarySource = recode_factor(primarySource, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig."),
    primarySource = factor(primarySource, levels = LEVELS_fuel_red)) %>%
  group_by(Stage, primarySource) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  ggplot()+
  geom_col(aes(x = Stage, y = Value, fill = primarySource), color = "black", linewidth = 0.5) +
  scale_fill_manual(values = pivot_wider(
    filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "primarySource", values_to = "Color" )
      , primarySource %in% as.character(unique(filter(
        mutate(allData_final, primarySource = recode_factor(primarySource, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig.") )
        , Analysis == "GHG Emissions (MMT CO2e)")$primarySource))
      )
        , names_from = primarySource, values_from = Color)
    )+
  labs(x = "", y = "GHG Emissions (MMT CO2e)", fill = "Emission Sources")+
  scale_y_continuous(breaks = seq(100,700,100))+
  guides(fill = guide_legend(nrow = 4))+
  THEME_simple +    theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
                          , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
                          , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          , legend.background = element_blank()
                          , legend.box.background = element_rect(colour = "black")
                          # , legend.box.margin=margin(-5,0,0,0)
                          , legend.position = c(.6,.8 ))#, legend.title = element_blank())  
png(filename = "Results/Plots/Fig 5 - GHGBar.png", type = "cairo", units =
      "in", width = 6.5, height = 3.6    , pointsize = 10, res = 600) 
GHGBar 
dev.off() 

  # stage specific bars w/o EoL ----

farmNAICS <- 
  c("Animal Feed", "Meat, Poultry, Eggs", "Dairy", "Fruit, Vegetables, Nuts"
    , "Grain, Oil","Seafood", "Sugar", "Other" )

manufNAICS <- 
  c("Animal Feed Processing",  "Animal Product Processing", "Dairy Products"                  
    ,  "Fruit, Vegetables, Nuts Products", "Grain, Oil Milling", "Bakeries, Tortillas"         
    ,  "Seafood Processing", "Sugar, Confectionary", "Other Products")
endNAICS <- 
  c("Truck","Rail" , "Warehouse - Dry", "Warehouse - Cold",  "Retail", "Residential", "Services")

farmBar <-
  allData_final %>% filter(Stage == "On-farm") %>%
  mutate(
      primarySource = recode_factor(primarySource, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig."),
      fillGroup = ifelse(str_detect(Analysis, "Site"), as.character(Fuel_red),as.character(primarySource)),
      fillGroup = factor(fillGroup, levels = LEVELS_fuel_red))%>%
  group_by(Analysis, fillGroup, commodity_naics, Stage) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>%
  filter(commodity_naics %in% farmNAICS) %>% 
  mutate(Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy\n(TBTU)"
                                  , "Primary Energy (TBTU)" = "Primary Energy\n(TBTU)"
                                  , "GHG Emissions (MMT CO2e)" = "GHG Emissions\n(MMTCO2e)")) %>%
  ggplot()+
  geom_col(aes(x = factor(addline_format(commodity_naics), levels = LEVELS_naicsLine), y = Value, fill = fillGroup), color = "black")+
  
  facet_wrap(~Analysis, scales = "free_y", ncol = 1, strip.position = "left")+
  scale_fill_manual(values = COLOR_fuel_alt)+
  # scale_fill_manual(values = pivot_wider(filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "Fuel_red", values_to = "Color" )
  #                                               , Fuel_red %in% as.character(unique(filter(allData_final, Stage == "On-farm")$Fuel_red)))
  #                                        , names_from = Fuel_red, values_from = Color))+
  scale_x_discrete(labels = function(x) str_wrap(ifelse(str_detect(x,"Processing"), str_replace_all(x, "ssing" , "ss-\ning"),
                                                        ifelse(str_detect(x,"Confectionary"), str_replace_all(x, "ionary" , "-\nionary"),
                                                               str_wrap(str_replace_all(x, " " , "\n")))),
                                                 width = 10))+
  guides(fill=guide_legend(nrow=3,title.position="top"))+
  labs(x = "", y = "", fill = "Energy/Emission Sources")+
  THEME_simple +
  theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
        , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
        , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
        , strip.placement = "outside" #, legend.box.margin=margin(-10,-10,-10,-10)
        # , legend.spacing.y = unit(0, 'line')
        
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        , legend.background = element_blank()
        , legend.box.background = element_rect(colour = "black")
        , legend.box.margin=margin(-5,0,0,0)
        , legend.position = "bottom"
        , legend.box.spacing = margin(-5)
        
  )



# png(filename = "Plots/farmBarv2.png", type = "cairo", units =
png(filename = "Results/Plots/Fig 7 - farmBar.png", type = "cairo", units =
      "in", width = 6.5, height = 6.5, pointsize = 10, res = 600) 
farmBar 
dev.off() 

manufBar <-
  allData_final %>% filter(Stage == "Manufacturing") %>% 
  mutate(
    Fuel_red = recode_factor(primarySource, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig."),
    Fuel_red = factor(Fuel_red, levels = LEVELS_fuel_red))%>%
  
  group_by(Analysis, Fuel_red, commodity_naics, Stage) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>%
  filter(commodity_naics %in% manufNAICS) %>%
  mutate(Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy\n(TBTU)"
                                  , "Primary Energy (TBTU)" = "Primary Energy\n(TBTU)"
                                  , "GHG Emissions (MMT CO2e)" = "GHG Emissions\n(MMTCO2e)")) %>%
  ggplot()+
  geom_col(aes(x = factor(addline_format(commodity_naics), levels = LEVELS_naicsLine), y = Value, fill = Fuel_red), color = "black")+
  
  facet_wrap(~Analysis, scales = "free_y", ncol = 1, strip.position = "left")+
  scale_fill_manual(values = COLOR_fuel_alt)+
  # scale_fill_manual(values = pivot_wider(filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "Fuel_red", values_to = "Color" )
  #                                               , Fuel_red %in% as.character(unique(filter(allData_final, Stage == "On-farm")$Fuel_red)))
  #                                        , names_from = Fuel_red, values_from = Color))+
  scale_x_discrete(labels = function(x) str_wrap(ifelse(str_detect(x,"Processing"), str_replace_all(x, "ssing" , "ss-\ning"),
                                                        ifelse(str_detect(x,"Confectionary"), str_replace_all(x, "ionary" , "-\nionary"),
                                                               str_wrap(str_replace_all(x, " " , "\n")))),
                                                 width = 10))+
  guides(fill=guide_legend(nrow=3,title.position="top"))+
  labs(x = "", y = "", fill = "Energy/Emission Sources")+
  THEME_simple +
  theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
        , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line") 
        , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
        , strip.placement = "outside"#, legend.box.margin=margin(-10,-10,-10,-10)
        # , legend.spacing.y = unit(0, 'line')
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        , legend.background = element_blank()
        , legend.box.background = element_rect(colour = "black")
        , legend.box.margin=margin(-5,0,0,0)
        , legend.position = "bottom"
        , legend.box.spacing = margin(-5)
  )
png(filename = "Results/Plots/Fig 8 - manufBar.png", type = "cairo", units =
      "in", width = 6.5, height = 6.5, pointsize = 10, res = 600) 
manufBar 
dev.off() 

endBars <-
  allData_final %>% filter(Stage != "Manufacturing" & Stage != "On-farm" )  %>%
  mutate(
    Fuel_red = recode_factor(primarySource, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig."),
    Fuel_red = factor(Fuel_red, levels = LEVELS_fuel_red), 
    commodity_naics_red = ifelse(Stage == "Distribution", "Distribution", as.character(commodity_naics)),
    commodity_naics_red = recode_factor(commodity_naics_red, "Warehouse - Dry" ="Warehouse", "Warehouse - Cold" = "Warehouse" ),
    Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy\n(TBTU)"
                             , "Primary Energy (TBTU)" = "Primary Energy\n(TBTU)"
                             , "GHG Emissions (MMT CO2e)" = "GHG Emissions\n(MMTCO2e)")) %>% 
  group_by(Analysis, Fuel_red, commodity_naics_red, Stage) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>% 
  ggplot()+
  geom_col(aes(x = factor(commodity_naics_red, levels = c("Distribution", "Warehouse", "Retail", "Services", "Residential"))
               , y = Value, fill = Fuel_red), color = "black")+
  
  facet_wrap(~Analysis, scales = "free_y", ncol = 1, strip.position = "left")+
  # scale_fill_manual(values = COLOR_fuel_alt)+
  scale_fill_manual(values = pivot_wider(filter(pivot_longer(COLOR_fuel_alt, everything(), names_to = "Fuel_red", values_to = "Color" )
                                                , Fuel_red %in% as.character(unique(filter(
                                                  mutate(allData_final, Fuel_red = recode_factor(Fuel_red, "Process" = "Process/Refrig.", "Refrigerant" = "Process/Refrig.") ), Stage != "Manufacturing" & Stage != "On-farm")$Fuel_red)))
                                         , names_from = Fuel_red, values_from = Color))+
  guides(fill=guide_legend(nrow=2,title.position="top"))+
  labs(x = "", y = "", fill = "Energy/Emission Sources")+
  THEME_simple +
  theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
        , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line") 
        , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
        , strip.placement = "outside"#, legend.box.margin=margin(-10,-10,-10,-10)
        # , legend.spacing.y = unit(0, 'line')
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        , legend.background = element_blank()
        , legend.box.background = element_rect(colour = "black")
        , legend.box.margin=margin(-5,0,0,0)
        , legend.position = "bottom"
        , legend.box.spacing = margin(-5)
  )
png(filename = "Results/Plots/Fig 9 - endBars.png", type = "cairo", units =
      "in", width = 6.5, height = 6.5, pointsize = 10, res = 600) 
endBars 
dev.off() 

  # Energy Flow Prep ----

COLOR_breakdown <- tibble( " " = "white",
                           "Natural Gas"  = "#df8610", "Petroleum"=	"#ffcd00", "Coal"= "#a03123","Renewables"= "#84b641", "Other Fuel"= "#bf541b" 
                           , "Electricity"	= "#2abdda" ,   "Generation Losses" = "black"
                           , "Chemicals" = "#306DBE", "Animal Feed" = "7030A0", "Animal Emission" = "#964b00", "Crop Emission" = "#1e7640", "Refrigerant" = "black", "Process" = "black", "Refrig./Process", "Refrig./Process" = "black"
                           ,"On-farm" = "#8DB658", "Manufacturing" = "#DEA645","Distribution" = "#3F9CA6","W&R" = "#5481AF", "Consumption" = "#C4503F") 
LEVEL_breakdown <- c( "Chemicals" , "Animal Feed", 
                      "Natural Gas" , "Petroleum", "Coal","Renewables", "Other Fuel"
                      , "Electricity"	 ,   "Generation Losses" 
                      , "Animal Emission", "Crop Emission" , "Refrigerant" , "Process", "Refrig./Process"
                      ,"On-farm" , "Manufacturing" ,"Distribution" ,"W&R", "Consumption", " " ) 

bar1_FuelBreakdown <-
  allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
  group_by(primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(Breakdown = primarySource) %>% mutate(bar = "Primary\nEnergy")

bar2_ElectricityBreakdown <-
  allData_final %>%   
  filter(Analysis != "GHG Emissions (MMT CO2e)") %>%
  filter(electricity_yn) %>%  
  group_by(Analysis) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis, values_from = Value) %>% 
  mutate(`Generation Losses` = `Primary Energy (TBTU)` - `Site Energy (TBTU)`) %>%
  rename(`Electricity` = `Site Energy (TBTU)`) %>% select(-`Primary Energy (TBTU)`) %>% 
  pivot_longer(c(`Electricity`, `Generation Losses`), names_to = "sourceGroup", values_to = "Value") %>%
  dplyr::rename(Breakdown = sourceGroup) %>% mutate(bar = "Electricity\nBreakdown")

bar3_Transport <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  # filter(str_detect(Source, "AF") | str_detect(Source, "F&P")) %>%
  filter(transport_yn) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(bar = "Transport", Breakdown = "Distribution")


bar4_5_prep <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>% #NOTE - can do with Primary, but need to add filter for electricity_yn instead of Source == electricity
  filter(AF_yn|FP_yn) %>% 
  filter(!str_detect(Source, "Electricity")) %>%
  group_by(Analysis, AF_yn,FP_yn, Source, primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  rbind(
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(AF_yn|FP_yn) %>% 
      filter(str_detect(Source, "Electricity")) %>% 
      group_by(Analysis, AF_yn,FP_yn, Source)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(primarySource = "Electricity")
  ) %>%
  dplyr::rename(End = Source)

bar4_Chem <- bar4_5_prep %>% filter(FP_yn & Analysis != "GHG Emissions (MMT CO2e)") %>%
  ungroup() %>% summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(Breakdown = "Chemicals") %>% mutate(bar = "Chemicals") %>%
  rbind(data.frame(
    bar = "Chemicals", Breakdown = " ", 
    Value = sum(bar1_FuelBreakdown$Value) - sum(filter(bar4_5_prep, FP_yn & Analysis != "GHG Emissions (MMT CO2e)")$Value)  ))

bar5_AF <- bar4_5_prep %>% filter(AF_yn & Analysis != "GHG Emissions (MMT CO2e)") %>%
  ungroup() %>% 
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(Breakdown = "Animal Feed")   %>% mutate(bar = "Animal\nFeed")%>%
  rbind(data.frame(
    bar = "Animal\nFeed", Breakdown = " ", 
    Value = sum(bar1_FuelBreakdown$Value) - sum(filter(bar4_5_prep, AF_yn & Analysis != "GHG Emissions (MMT CO2e)")$Value)  ))

bar6_stage <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  group_by(Stage)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(Breakdown = Stage)  %>% mutate(bar = "End\nUse")%>%
  rbind(data.frame(
    bar = "End\nUse", Breakdown = " ", 
    Value = sum(bar1_FuelBreakdown$Value) - sum(filter(allData_final,  Analysis == "Site Energy (TBTU)")$Value, na.rm = T)))

rbind(bar1_FuelBreakdown, bar2_ElectricityBreakdown, bar3_Transport, bar4_Chem, bar5_AF, bar6_stage) %>%
  filter(Breakdown != " ") %>%
  mutate(bar = str_replace_all(bar, "\n", " ")
         ,bar = factor(bar, levels = c("Primary Energy", "Electricity Breakdown", "Transport", "Chemicals", "Animal Feed", "End Use")),
         Breakdown = factor(Breakdown, LEVEL_breakdown)) %>% ungroup() %>%
  arrange(bar, Breakdown) %>%
  group_by(bar) %>% mutate(frac = percent(Value/sum(Value))) %>% write_clip()





png(filename = "Plots/G_FlowBars.png", type = "cairo", units =
      "in", width = 9, height = 5.5, pointsize = 10, res = 600)
rbind(bar1_FuelBreakdown, bar2_ElectricityBreakdown, bar3_Transport, bar4_Chem, bar5_AF, bar6_stage) %>%
  mutate(bar = factor(bar, levels = c("Primary\nEnergy", "Electricity\nBreakdown", "Transport", "Chemicals", "Animal\nFeed", "End\nUse")),
         Breakdown = factor(Breakdown, LEVEL_breakdown)) %>%
  ggplot()+
  geom_col(aes(x = bar, y = Value, fill = Breakdown)) +
  scale_fill_manual(values = COLOR_breakdown) +
  labs(x = "", y = "Energy (TBTU)", fill = "")+
  THEME_simple + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
dev.off()

#Check
allData_final %>% filter(str_detect(Analysis,"Site") & Stage == "On-farm") %>% ungroup()%>%
  group_by(directUse_yn, electricity_yn, transport_yn, AF_yn, FP_yn) %>%
  summarize(Value = sum(Value, na.rm = TRUE))


lines_fromFuel <-
  rbind(
    allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
      filter(directUse_yn) %>%
      group_by(Analysis, primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
    ,
    allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
      filter(transport_yn) %>%
      group_by(Analysis, primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) %>% mutate(To = "Transport") 
    ,
    allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
      filter(electricity_yn) %>%
      group_by(Analysis, primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) %>% mutate(To = "Electricity")
    ,
    allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
      filter(FP_yn | AF_yn) %>% filter(!electricity_yn) %>% filter(!transport_yn) %>% filter(!directUse_yn) %>%
      group_by(Analysis, primarySource, FP_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>% 
      dplyr::rename(From = primarySource) %>% mutate(To = ifelse(FP_yn, "Chemicals", "Animal Feed")) %>%
      select(-FP_yn)
  ) %>% ungroup() %>% group_by(Analysis, From) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "From Fuel") 

lines_toTransport <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  # filter(str_detect(Source, "AF") | str_detect(Source, "F&P")) %>%
  filter(transport_yn) %>%
  group_by(Analysis,   primarySource) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(To = "Transport") %>%
  dplyr:: rename(From = primarySource) %>% ungroup() %>%   group_by(Analysis) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "To Transport") 

lines_fromTransport <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  filter(transport_yn) %>%
  mutate(To = ifelse(Source == "AF Transport", "Animal Feed", as.character(Stage)))  %>%
  group_by(Analysis,  To) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% 
  mutate(From = "Transport")%>% ungroup() %>%   group_by(Analysis) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "From Transport") 

lines_toElectricity <-
  allData_final %>% filter(Analysis == "Primary Energy (TBTU)" ) %>% ungroup() %>%
  filter(electricity_yn) %>%
  group_by(Analysis, primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(From = primarySource) %>% 
  ungroup() %>%ungroup() %>%   group_by(Analysis) %>%
  mutate(To = "Electricity", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To Electricity") 

lines_fromElectricity <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  filter(electricity_yn) %>%
  mutate(To = ifelse(FP_yn, "Chemicals", ifelse(AF_yn, "Animal Feed", as.character(Stage)))) %>%
  group_by(Analysis,To) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Electricity", percentValue = percent(Value/sum(Value), accuracy = 0.1)) %>%
  rbind(
    bar2_ElectricityBreakdown %>% filter(Breakdown == "Generation Losses") %>% select(-bar) %>% 
      mutate(To = "Electricity", Analysis = "Site Energy (TBTU)", percentValue = "100%") %>% rename(From = Breakdown)
    
  ) %>% mutate(set = "From Electricity")


lines_toChem <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  filter(FP_yn) %>%
  group_by(Analysis,primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(From = primarySource) %>% 
  ungroup() %>%group_by(Analysis) %>%
  mutate(To = "Chemicals", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To Chem") 

lines_fromChem <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  filter(FP_yn) %>%
  mutate(To = ifelse(AF_yn, "Animal Feed", as.character(Stage))) %>%
  group_by(Analysis,To)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Chemicals", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "From Chem") 

lines_toAF <-
  rbind(
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(AF_yn) %>% filter(!FP_yn) %>% filter(!transport_yn)%>% 
      group_by(Analysis,primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) 
    ,
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%group_by(Analysis) %>%
      filter(AF_yn & FP_yn) %>% filter(!transport_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      mutate(From = "Chemicals")
    ,
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%group_by(Analysis) %>%
      filter(AF_yn &  transport_yn) %>% filter(!FP_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      mutate(From = "Transport")  
  ) %>% 
  ungroup() %>%group_by(Analysis) %>%
  mutate(To = "Animal Feed", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To AF") 

lines_fromAF <-
  allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
  filter(AF_yn) %>%
  dplyr::rename(To = Stage) %>%
  group_by(Analysis,To)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Animal Feed", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "From AF") 

lines_toStage <-
  rbind(
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(directUse_yn) %>%
      group_by(Analysis,primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
    ,
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(transport_yn) %>% filter(!AF_yn) %>%
      group_by(Analysis,Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(To = Stage) %>% mutate(From = "Transport") 
    ,
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(electricity_yn) %>% filter(!AF_yn) %>%filter(!FP_yn) %>%
      group_by(Analysis,Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(To = Stage) %>% mutate(From = "Electricity")
    ,
    allData_final %>% filter(Analysis == "Site Energy (TBTU)" ) %>% ungroup() %>%
      filter(FP_yn | AF_yn) %>% 
      group_by(Analysis,Stage, AF_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>% 
      dplyr::rename(To = Stage) %>% mutate(From = ifelse(AF_yn, "Animal Feed", "Chemicals")) %>%
      select(-AF_yn)
  ) %>% ungroup() %>% 
  group_by(Analysis, To) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "To Stage") 

lines_Energy <-
  rbind(lines_fromFuel, lines_fromAF, lines_fromChem, lines_fromElectricity, lines_fromTransport,
        lines_toAF, lines_toChem, lines_toElectricity, lines_toStage, lines_toTransport
        
  )


  # GHG Flow prep ----
allData_final_bars <- allData_final %>% mutate(primarySource = recode_factor(primarySource, "Process" = "Refrig./Process", "Refrigerant" = "Refrig./Process"))


bar1_FuelBreakdownGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  group_by(primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(Breakdown = primarySource) %>% mutate(bar = "Primary\nSource")

bar2_ElectricityBreakdownGHG <-
  allData_final_bars %>% 
  filter(Analysis == "GHG Emissions (MMT CO2e)") %>%
  filter(electricity_yn) %>% 
  ungroup() %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  mutate(Breakdown = "Grid Electricity", bar = "Electricity\nGeneration")


bar3_TransportGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(transport_yn) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(bar = "Transport", Breakdown = "Petroleum")

bar4_ChemGHG <-
  allData_final_bars %>% filter(FP_yn & Analysis == "GHG Emissions (MMT CO2e)") %>%ungroup() %>% 
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(Breakdown = "Chemicals") %>% mutate(bar = "Chemicals") %>%
  rbind(data.frame(
    bar = "Chemicals", Breakdown = " ", 
    Value = sum(bar1_FuelBreakdownGHG$Value) - sum(filter(allData_final_bars, FP_yn & Analysis == "GHG Emissions (MMT CO2e)")$Value)  ))

bar5_AFGHG <- allData_final_bars %>% filter(AF_yn & Analysis == "GHG Emissions (MMT CO2e)") %>%ungroup() %>% 
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(Breakdown = "Animal Feed")   %>% mutate(bar = "Animal\nFeed")%>%
  rbind(data.frame(
    bar = "Animal\nFeed", Breakdown = " ", 
    Value = sum(bar1_FuelBreakdownGHG$Value) - sum(filter(allData_final_bars, AF_yn & Analysis == "GHG Emissions (MMT CO2e)")$Value)  ))

bar6_stageGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  group_by(Stage)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(Breakdown = Stage)  %>% mutate(bar = "End\nUse")

unique(rbind(bar1_FuelBreakdownGHG, bar2_ElectricityBreakdownGHG, bar3_TransportGHG, bar4_ChemGHG, bar5_AFGHG, bar6_stageGHG)$Breakdown) 

rbind(bar1_FuelBreakdownGHG, bar2_ElectricityBreakdownGHG, bar3_TransportGHG, bar4_ChemGHG, bar5_AFGHG, bar6_stageGHG) %>%
  filter(Breakdown != " ") %>%
  mutate(bar = str_replace_all(bar, "\n", " ")
         , bar = factor(bar, levels = c("Primary Source", "Electricity Generation", "Transport", "Chemicals", "Animal Feed", "End Use"))
         , Breakdown = recode_factor(Breakdown, "Crop Emissions" = "Crop Emission", "Animal Emissions" = "Animal Emission",
                                     "Grid Electricity" = "Electricity")
         ,Breakdown = factor(Breakdown, LEVEL_breakdown)
  ) %>% ungroup() %>%
  arrange(bar, Breakdown) %>%
  group_by(bar) %>% mutate(frac = percent(Value/sum(Value), accuracy = 1)) #%>% write_clip()


png(filename = "Plots/G_FlowBarsGHG.png", type = "cairo", units =
      "in", width = 9, height = 5.5, pointsize = 10, res = 600)
rbind(bar1_FuelBreakdownGHG, bar2_ElectricityBreakdownGHG, bar3_TransportGHG, bar4_ChemGHG, bar5_AFGHG, bar6_stageGHG) %>%
  mutate(bar = factor(bar, levels = c("Primary\nSource", "Electricity\nGeneration", "Transport", "Chemicals", "Animal\nFeed", "End\nUse"))
         , Breakdown = recode_factor(Breakdown, "Crop Emissions" = "Crop Emission", "Animal Emissions" = "Animal Emission",
                                     "Grid Electricity" = "Electricity")
         ,Breakdown = factor(Breakdown, LEVEL_breakdown)
  ) %>%
  ggplot()+
  geom_col(aes(x = bar, y = Value, fill = Breakdown)) +
  scale_fill_manual(values = COLOR_breakdown) +
  labs(x = "", y = "Energy (TBTU)", fill = "")+
  THEME_simple + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
dev.off()

lines_fromFuelGHG <-
  rbind(
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(directUse_yn) %>% 
      group_by(Analysis, primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(transport_yn) %>%
      group_by(Analysis, primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) %>% mutate(To = "Transport") 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(electricity_yn) %>%
      group_by(Analysis, primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) %>% mutate(To = "Electricity")
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(FP_yn | AF_yn) %>% filter(!electricity_yn) %>% filter(!transport_yn)  %>%
      group_by(Analysis, primarySource, FP_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>% 
      dplyr::rename(From = primarySource) %>% mutate(To = ifelse(FP_yn, "Chemicals", "Animal Feed")) %>%
      select(-FP_yn)
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(nonEnergyEmissions_yn) %>% filter(!transport_yn) %>% filter(!FP_yn) %>% filter(!AF_yn) %>%
      group_by(Analysis, primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
    
  ) %>% ungroup() %>% group_by(Analysis, From) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 1), set = "From Fuel")  

lines_toTransportGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  # filter(str_detect(Source, "AF") | str_detect(Source, "F&P")) %>%
  filter(transport_yn) %>%
  group_by(Analysis,   primarySource) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% mutate(To = "Transport") %>%
  dplyr:: rename(From = primarySource) %>% ungroup() %>%   group_by(Analysis) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "To Transport")  

lines_fromTransportGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(transport_yn) %>%
  mutate(To = ifelse(Source == "AF Transport", "Animal Feed", as.character(Stage)))  %>%
  group_by(Analysis,  To) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% 
  mutate(From = "Transport")%>% ungroup() %>%   group_by(Analysis) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 0.1), set = "From Transport") 

lines_toElectricityGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(electricity_yn) %>%
  group_by(Analysis, primarySource)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  dplyr::rename(From = primarySource) %>% 
  ungroup() %>%ungroup() %>%   group_by(Analysis) %>%
  mutate(To = "Electricity", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To Electricity") 

lines_fromElectricityGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(electricity_yn) %>%
  mutate(To = ifelse(FP_yn, "Chemicals", ifelse(AF_yn, "Animal Feed", as.character(Stage)))) %>%
  group_by(Analysis,To) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Electricity", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "From Electricity") 

lines_toChemGHG <-
  rbind(
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(FP_yn) %>% filter(!electricity_yn) %>%
      group_by(Analysis,primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(FP_yn) %>% filter(electricity_yn) %>% 
      group_by(Analysis) %>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::mutate(From = "Electricity") 
    
  )%>% 
  ungroup() %>%group_by(Analysis) %>%
  mutate(To = "Chemicals", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To Chem") 

lines_fromChemGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(FP_yn) %>%
  mutate(To = ifelse(AF_yn, "Animal Feed", as.character(Stage))) %>%
  group_by(Analysis,To)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Chemicals", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "From Chem") 

lines_toAFGHG <-
  rbind(
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(AF_yn) %>% filter(!FP_yn) %>% filter(!transport_yn)%>% filter(!electricity_yn) %>%
      group_by(Analysis,primarySource)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource) 
    , 
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(AF_yn) %>% filter(electricity_yn) %>% filter(!FP_yn)%>% 
      group_by(Analysis) %>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::mutate(From = "Electricity") 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%group_by(Analysis) %>%
      filter(AF_yn & FP_yn) %>% filter(!transport_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      mutate(From = "Chemicals")
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%group_by(Analysis) %>%
      filter(AF_yn &  transport_yn) %>% filter(!FP_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      mutate(From = "Transport")  
  ) %>% 
  ungroup() %>%group_by(Analysis) %>%
  mutate(To = "Animal Feed", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "To AF") 

lines_fromAFGHG <-
  allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
  filter(AF_yn) %>%
  dplyr::rename(To = Stage) %>%
  group_by(Analysis,To)%>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%group_by(Analysis) %>%
  mutate(From = "Animal Feed", percentValue = percent(Value/sum(Value), accuracy = 0.1), set = "From AF") 

lines_toStageGHG <-
  rbind(
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(directUse_yn) %>%
      group_by(Analysis,primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(transport_yn) %>% filter(!AF_yn) %>%
      group_by(Analysis,Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(To = Stage) %>% mutate(From = "Transport") 
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(electricity_yn) %>% filter(!AF_yn) %>%filter(!FP_yn) %>%
      group_by(Analysis,Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(To = Stage) %>% mutate(From = "Electricity")
    ,
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(FP_yn | AF_yn) %>% 
      group_by(Analysis,Stage, AF_yn)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>% 
      dplyr::rename(To = Stage) %>% mutate(From = ifelse(AF_yn, "Animal Feed", "Chemicals")) %>%
      select(-AF_yn)
    , 
    allData_final_bars %>% filter(Analysis == "GHG Emissions (MMT CO2e)" ) %>% ungroup() %>%
      filter(nonEnergyEmissions_yn) %>% filter(!transport_yn) %>% filter(!FP_yn) %>% filter(!AF_yn) %>%
      group_by(Analysis, primarySource, Stage)%>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      dplyr::rename(From = primarySource, To = Stage) 
  ) %>% ungroup() %>% 
  group_by(Analysis, To) %>%
  dplyr::mutate(percentValue = percent(Value / sum(Value, na.rm = T), accuracy = 1), set = "To Stage") 

lines_GHG <-
  rbind(lines_fromFuelGHG, lines_fromAFGHG, lines_fromChemGHG, lines_fromElectricityGHG, lines_fromTransportGHG,
        lines_toAFGHG, lines_toChemGHG, lines_toElectricityGHG, lines_toStageGHG, lines_toTransportGHG
        
  )

  # Perspective in total US energy ----
# Data from EIA for 2016: https://www.eia.gov/totalenergy/data/monthly/pdf/sec2_3.pdf
GRAPHPREP_totalEdf <- data.frame(
  Sector = rep(c("Residential", "Commercial", "Industrial", "Transportation"), 3)
  , Analysis = c(rep("Site Energy (TBTU)", 4), rep("Primary Energy (TBTU)", 4), rep("GHG Emissions (MMT CO2e)", 4))
  , fullSector = c(6030, 4321, 21554, 27710, 20179, 18030, 31347, 27786, 999.6, 1061.9, 650.7+1894.8, 1857.6) 
)

GRAPHPREP_perspectiveKey <- data.frame(
  Stage_b = c("Distribution" ,"Distribution (F-M)", "Manufacturing" , "On-farm"   ,  "Residential" , "Services"  ,  "W&R"      )
  , Sector = c("Transportation", "Transportation", "Industrial", "Industrial", "Residential", "Commercial", "Commercial")
  , Stage_new =  c("Distribution" ,"On-farm", "Manufacturing" , "On-farm"   ,  "Consumption" , "Consumption"  ,  "W&R"      )
)

GRAPHPREP_perspectiveLabels <- data.frame(
  Stage_new = rep(c("On-farm", "Manufacturing",  "Distribution","On-farm",
                    "W&R", "Consumption",     "Consumption", rep("Remainder of Sector", 4)), 3)
  , Sector = rep(c("Industrial", "Industrial", "Transportation", "Transportation", "Commercial", "Commercial","Residential", 
                   "Industrial", "Transportation",  "Commercial","Residential"), 3)
  , Analysis = c(rep("Site Energy (TBTU)", 11), rep("Primary Energy (TBTU)", 11), rep("GHG Emissions (MMT CO2e)", 11))
  , height = c(21554 + 1500, 21554 - 4000
               , 27710 - 3500, 27710 + 1500
               , 4321 + 4000, 4321 +2000
               , 6030 +1500
               , rep(1000,4)#21554/2, 27710/2, 4321/2, 6030/2,
               , 31347 + 1500, 31347 - 4500
               , 27786 - 2500 ,  27786 + 1500
               , 18030 + 1500, 18030 -3500
               , 20179 + 1500
               ,  rep(1000,4)#31347/2, 27786/2, 18030/2, 20179/2)
               , 2650, 1700
               , 1700, 2000
               , 1175, 800
               , 1125,  rep(100,4))
)

GRAPHDATA_alldataPerspective <- allData_final %>% #filter(Analysis != "GHG Emissions (MMT CO2e)") %>% 
  mutate(Stage_b = ifelse(Stage == "Consumption", as.character(commodity_naics), as.character(Stage))
         , Stage_b = ifelse(subSource == "F-M Dist.", "Distribution (F-M)", as.character(Stage_b))
         , Stage_b <- recode_factor(Stage_b , "\nDistribution (F-M)"= "Distribution (F-M)" , "\nDistribution" = "Distribution" )
  ) %>%
  group_by(Stage_b, Analysis) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE))%>% left_join(GRAPHPREP_perspectiveKey )

GRAPHPREP_totalEdf <- aggregate(Value ~ Analysis+Sector, GRAPHDATA_alldataPerspective, FUN = sum) %>% left_join(GRAPHPREP_totalEdf) %>% 
  mutate(remainder = fullSector - Value, Stage_b = "Remainder of Sector", Stage_new = "Remainder of Sector") %>% 
  select(Analysis, Sector, remainder, Stage_b, Stage_new) %>% dplyr::rename("Value" = remainder)

GRAPHDATA_alldataPerspective <- rbind(GRAPHDATA_alldataPerspective, GRAPHPREP_totalEdf)

GRAPHDATA_alldataPerspective <-
  GRAPHDATA_alldataPerspective %>% group_by(Analysis, Sector) %>% 
  mutate(percent = ifelse(Stage_b == "Remainder of Sector", percent(Value/sum(Value), accuracy = 1), percent(Value/sum(Value), accuracy = .1)) 
         , sourceLabel = paste(Stage_b, percent)
         , percent2 = ifelse(Stage_b == "Remainder of Sector", percent(Value/sum(Value), accuracy = 1), NA)) 


GRAPHDATA_alldataPerspective %>% 
  mutate(group = ifelse(Stage_b == "Remainder of Sector", "Remainder", "Food")) %>% 
  group_by(Analysis, group) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>% ungroup() %>% group_by(Analysis) %>%
  mutate(perc = Value/sum(Value))

GRAPHDATA_alldataPerspective <- left_join(GRAPHDATA_alldataPerspective, GRAPHPREP_perspectiveLabels, by = c("Stage_new", "Analysis", "Sector")) %>%
  mutate(
    Stage_new = factor(Stage_new, levels = c("On-farm", "Manufacturing",  "Distribution","W&R", "Consumption", "Remainder of Sector"))
    , Sector = recode_factor( Sector, "Industrial" = "Ind.", "Transportation" = "Trans."
                              , "Commercial" = "Com.", "Residential" = "Res.")
    , Sector = factor( Sector, levels = c("Ind.", "Trans.", "Com.", "Res."))
  )%>% #filter(Analysis == "Primary Energy (TBTU)" )%>%
  mutate(Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy (QBTU)", "Primary Energy (TBTU)" = "Primary Energy (QBTU)",
                                  "GHG Emissions (MMT CO2e)" = "GHG Emissions (BMT CO2e)"),
         Analysis = factor(Analysis, levels = c("Site Energy (QBTU)", "Primary Energy (QBTU)", "GHG Emissions (BMT CO2e)"))
         # ,ValueGraph = ifelse(Analysis == "GHG Emissions (MMT CO2e)", Value, Value/1000),
         # height = ifelse(Analysis == "GHG Emissions (MMT CO2e)", height, height/1000)
  )

G_perspective <-
  GRAPHDATA_alldataPerspective  %>%
  ggplot( aes(x = Sector, y = Value/1000, fill = Stage_new))+
  geom_col(color = "black")+
  geom_label(data = filter(GRAPHDATA_alldataPerspective, Stage_new != "Remainder of Sector"),aes(label = percent, y = height/1000), size = 3, show.legend = FALSE, family = "serif")+
  geom_text(data = filter(GRAPHDATA_alldataPerspective, Stage_new == "Remainder of Sector"),aes(label = percent, y = height/1000), size = 3, show.legend = FALSE, family = "serif")+
  
  facet_wrap(~Analysis, scales = "free_y", strip.position = "left")+
  scale_fill_manual(values = c("On-farm" = "#8fbb55", "Manufacturing" = "#e5a940",  "Distribution" = "#3ba2ad",#"Distribution (F-M)" = "#8fbb55",
                               "W&R" = "#5785b7", "Consumption" = "#cb4d3d",     "Remainder of Sector" = "grey")) + 
  labs(x = "", y = "", fill = "")+
  THEME_simple + theme(
    # legend.text = element_text(size = 10)#, legend.box.margin=margin(-10,-10,-10,-10)
    legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
    , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line") 
    , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
    , legend.background = element_blank()
    , legend.box.background = element_rect(colour = "black")
    , legend.box.margin=margin(0,0,0,0)
    , legend.box.spacing = margin(-5)
    , strip.placement = "outside"
  )

png(filename = "Results/Plots/Fig 14 - perspective.png", type = "cairo", units =
      "in", width = 6.5, height = 3.6, pointsize = 10, res = 600) 
G_perspective 
dev.off() 

#total percent
percent(sum(filter(filter(GRAPHDATA_alldataPerspective, Analysis ==	"Primary Energy (QBTU)"), Stage_b != "Remainder of Sector")$Value, na.rm = T)/sum(filter(GRAPHDATA_alldataPerspective, Analysis ==	"Primary Energy (QBTU)")$Value, na.rm = T), accuracy = 0.1)
percent(sum(filter(filter(GRAPHDATA_alldataPerspective, Analysis ==	"Site Energy (QBTU)"), Stage_b != "Remainder of Sector")$Value)/sum(filter(GRAPHDATA_alldataPerspective, Analysis ==	"Site Energy (QBTU)")$Value), accuracy = 0.1)
percent(sum(filter(filter(GRAPHDATA_alldataPerspective, Analysis ==	"GHG Emissions (BMT CO2e)"), Stage_b != "Remainder of Sector")$Value)/sum(filter(GRAPHDATA_alldataPerspective, Analysis ==	"GHG Emissions (BMT CO2e)")$Value), accuracy = 0.1)

  # Graph for electricity generation - Supplemental ----
BD_states <- map_data("state") %>% mutate(stateName = str_to_title(region))
THEME_map <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_blank()
)

gridMap <-
  left_join(BD_states,
            GEN_state2016 %>% rowwise() %>% mutate(TBTUperMWh =  heatRate * percent * tdLossFactor / 3412.14) %>% # MWh * MMBtu/MWh * Wh / 3.41214btu  * 1 tbtu/1000 mmbtu
              group_by(State_Abb) %>% summarise(TBTUperMWh = sum(TBTUperMWh)) %>% filter(State_Abb != "DC") %>% 
              left_join(select(BD_stateRegion, State_Abb, stateName)) , by = "stateName") %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = TBTUperMWh), color = "gray90", size = 0.1) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "YlOrRd", name = "BTU/\nWh", labels = comma, trans = "reverse", breaks = c(1.5,  3))+
  THEME_map + theme(text = element_text(family = "serif", face = "bold.italic"), legend.position = c(0.25, .0)
                    , legend.direction = "horizontal", legend.text = element_text(size = 9), legend.title = element_text(size = 10))
tdLossMap <-
  left_join(BD_states,
            GEN_state2016 %>% group_by(State_Abb) %>% summarise(tdLossFactor = mean(tdLossFactor)) %>% filter(State_Abb != "DC") %>% 
              left_join(select(BD_stateRegion, State_Abb, stateName)) , by = "stateName") %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = tdLossFactor), color = "gray90", size = 0.1) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "YlOrRd", name = "T&D\nLoss Factor", labels = comma, trans = "reverse")+
  THEME_map + theme(text = element_text(family = "serif", face = "bold.italic"), legend.position = c(0.25, .0)
                    , legend.direction = "horizontal", legend.text = element_text(size = 9), legend.title = element_text(size = 10))


library(waffle)
THEME_waffle <-   THEME_simple +theme_enhance_waffle() + theme(legend.position = "bottom", legend.text = element_text(size = 6)
                                                               ,legend.direction = "vertical"
                                                               , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                                               ,panel.border = element_blank()
                                                               ,axis.ticks = element_blank()
                                                               , legend.title = element_blank()
                                                               , plot.margin=unit(c(0, 0, 0, 0),"cm")
                                                               , plot.title = element_text(size=8)
                                                               , legend.background = element_rect(linetype = "solid", color = NA))

GEN_state2016_graphData <-
  GEN_state2016 %>% select(State_Abb, Source,percent) %>%
  pivot_wider(names_from = Source, values_from = percent) %>% 
  #sum columns together to match classifications, remove unnecessary rows
  rowwise() %>%
  dplyr::mutate("Other Fuels" = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind", "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)) %>%
  select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind", -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage") %>% 
  pivot_longer(!c(State_Abb), names_to = "Fuel", values_to = "percent") %>% 
  filter(State_Abb != "DC" & State_Abb != "US-Total") %>% 
  filter(percent>0) %>%  
  arrange(State_Abb) %>% 
  mutate(percent = round(round(percent, digits = 2) * 100, 0),
         Fuel = factor(Fuel, levels = c("Natural Gas", "Grid Electricity", "Petroleum", "Coal", "Renewables",  "Other Fuels"))) 

library(gridExtra)

gridWaff <-
  GEN_state2016_graphData %>% left_join(GEN_state2016_graphData %>% group_by(State_Abb) %>%summarise(StatePercentTotal = sum(percent))) %>%
  pivot_wider(names_from = Fuel, values_from = percent) %>%
  ungroup() %>% rowwise()%>%
  mutate(`Natural Gas` = ifelse(StatePercentTotal > 100
                                , ifelse(`Natural Gas`> Coal, `Natural Gas` - 1, `Natural Gas`)
                                , ifelse(StatePercentTotal < 100
                                         , ifelse(`Natural Gas`> Coal, `Natural Gas` + 1, `Natural Gas`)
                                         , `Natural Gas`)),
         Coal = ifelse(StatePercentTotal > 100
                       , ifelse(`Natural Gas`> Coal, Coal, Coal - 1)
                       , ifelse(StatePercentTotal < 100
                                , ifelse(`Natural Gas`> Coal, Coal, Coal + 1)
                                , Coal))
         # , test = sum(c_across(c(Coal, `Natural Gas`, Petroleum, Renewables, `Other Fuels`)),na.rm = TRUE)
  ) %>% select(-StatePercentTotal) %>% pivot_longer(!c(State_Abb), names_to = "Fuel", values_to = "percent") %>%
  filter(!is.na(percent)) %>%
  # group_by(State_Abb) %>% summarise(percent = sum(percent)) %>% 
  # filter(State_Abb == "OH") %>%
  ggplot(aes(fill = Fuel, values = percent))+
  geom_waffle(n_rows = 10, make_proportional = FALSE) +
  facet_wrap(~State_Abb)+
  scale_fill_manual(values = c("Natural Gas"  = "#df8610", "Petroleum"=	"#ffcd00", "Coal"= "#a03123",
                               "Renewables"= "#84b641", "Other Fuels"= "#bf541b" ))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  coord_equal()+
  THEME_waffle + theme(legend.position = c(.6, .075), legend.direction = "horizontal", legend.margin = margin(0, 0, 0, 0),  
                       legend.spacing.y = unit(0, 'line')     , legend.key.height = unit(0,"line"), legend.key.width =  unit(0.5,"line"), 
                       strip.text  = element_text(size = 8, margin = margin(0,0,0,0, "cm")), 
                       panel.spacing = unit(0, "lines"))
#takes a minute to render the waffle
png(filename = "Results/Plots/Fig S1 - gridPlot.png", type = "cairo", units =
      "in", width = 6.5, height = 4    , pointsize = 10, res = 600) 
grid.arrange(tdLossMap, gridMap, gridWaff
             , ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,3),c(2,3)),
             widths = c(3, 3), heights = c(3,3)) 
dev.off() 


# State Level analysis: map ----
{
  BD_states <- map_data("state") %>% mutate(stateName = str_to_title(region))
  THEME_map <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank()
  )
  
  BD_population<- read_excel("Data/FSC - Background Data.xlsx", sheet = "Population")
  
  MAP_allTBTUwithPop <- aggregate(Value ~ Stage + stateName + Analysis, allData_final, FUN = sum, na.rm = TRUE) %>%
    filter(str_detect(Analysis, "Site")) %>% select(-Analysis) %>%
    rbind( select(mutate(BD_population, Stage = "Population", Value = Population/1000000), stateName, Stage, Value) )
}
  { 
    count <- 0
    breaks_funStage <- function(x) {
      count <<- count + 1L
      switch(
        count,
        c(10, 75, 150), #farm
        # c(10, 25, 40), #DFM
        c(10, 40, 80), #M - site
        # c(10, 50, 90), #M - source
        c(5,  15, 30), # D
        c(10, 25, 40),#WR - Site
        # c(10, 40, 80),#WR - Source
        c(10, 75, 125), #C - Site
        # c(10, 200, 350), #C - Source
        c(5,20, 35 )# pop
      )
    }
    
    EnergyMap <-
      left_join(BD_states, MAP_allTBTUwithPop, by = "stateName") %>% 
      filter(stateName != "District Of Columbia") %>% 
      mutate(unitLab = ifelse(Stage == "Population", "Millions", "TBTU"),
             Stage = factor(Stage, levels = c("On-farm",  "Manufacturing", "Distribution", "W&R", "Consumption", "Population"))
      ) %>%
      mutate(unitLab = ifelse(Stage == "Population" & group ==1 & order ==1, "Millions", ifelse(group == 1 & order == 1, "TBTU", NA))) %>%
      group_split(Stage)  %>% 
      purrr::map(
        ~ggplot(., aes(x = long, y = lat, fill = Value)) + 
          geom_polygon(aes(group = group), color = "gray90", size = 0.1) +
          geom_text(aes(x = -130, y = 31, label = unitLab), hjust = 0, family = "serif", size = 3)+
          scale_fill_distiller(palette = "YlOrRd", labels = comma, trans = "reverse", breaks = breaks_funStage)+
          guides(fill = guide_colourbar(barwidth = 3, barheight = 0.5))+
          # facet_grid(~factor(Stage, levels = c("On-farm", "Distribution (F-M)", "Manufacturing", "Distribution", "W&R", "Consumption")))+
          facet_grid(~Stage)+
          coord_fixed(1.3) +
          THEME_map +
          theme(text = element_text(family = "serif", face = "bold.italic")
                , legend.position = c(0.15,0) 
                , legend.direction = "horizontal"
                , legend.title = element_blank()
                , legend.text = element_text(size = 7)
                , legend.background=element_blank()
                , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
                , strip.placement = "outside")
      ) %>% 
      plot_grid(plotlist = ., align = 'hv', ncol = 3)
    }
  
  png(filename = "Results/Plots/Fig 11 - EnergyMap.png", type = "cairo", units =
        "in", width = 6.5, height = 3.5    , pointsize = 10, res = 600) # this "opens" the png maker and says where to save it
  EnergyMap # this tells it what plot to pull
  dev.off() # this turns it off
  
  # Analysis of state data per capita
  {
   MAP_perCapita <-
    MAP_allTBTUwithPop %>% pivot_wider(names_from = Stage, values_from = Value) %>%
    pivot_longer(!c(stateName, Population), names_to = "Stage", values_to = "Value") %>%
    mutate(perCapita = Value/Population)
  
  perCapitaBoxPlot <- 
    MAP_perCapita %>%
    mutate(  Stage = factor(Stage, levels = c("On-farm",  "Manufacturing", "Distribution", "W&R", "Consumption")) ) %>%
    ggplot(aes(x = Stage, y = perCapita))+
    geom_boxplot(width=0.1, outlier.shape = NA)+
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.1)+
    labs(x = "", y = "Energy per Capita (MMBTU/person)", color = "")+
    THEME_simple
  }
  
  png(filename = "Results/Plots/Fig 12 - perCapitaBoxPlot.png", type = "cairo", units =
        "in", width = 6.5, height = 3.5    , pointsize = 10, res = 600) # this "opens" the png maker and says where to save it
  perCapitaBoxPlot # this tells it what plot to pull
  dev.off() # this turns it off
  
  #Simple statistics for stage disaggregated, state-level data
  MAP_perCapita %>% group_by(Stage) %>% group_modify(~ broom::glance(lm(Value~Population, data = .)))
  MAP_perCapita %>% group_by(Stage) %>% group_modify(~ broom::tidy(lm(Value~Population, data = .)))
  
  #Map of per capita energy use - not used in analysis
  { 
    count <- 0
    breaks_funStage <- function(x) {
      count <<- count + 1L
      switch(
        count,
        c(10, 30, 60), #farm
        # c(10, 25, 40), #DFM
        c(1, 10, 15), #M - site
        # c(10, 50, 90), #M - source
        c(.5,  1.5), # D
        c(.9, 1.4),#WR - Site
        # c(10, 40, 80),#WR - Source
        c(3, 4) #C - Site
        # c(10, 200, 350), #C - Source
        # c(5,20, 35 )# pop
      )
    }
    
    EnergyPerCapitaMap <-
      BD_states %>%
      left_join(    MAP_perCapita        , by = "stateName") %>% 
      filter(stateName != "District Of Columbia") %>% 
      mutate(
        unitLab = "MMBTU/person",
        Stage = factor(Stage, levels = c("On-farm",  "Manufacturing", "Distribution", "W&R", "Consumption"))
      ) %>%
      # mutate(unitLab = ifelse(Stage == "Population" & group ==1 & order ==1, "Millions", ifelse(group == 1 & order == 1, "TBTU", NA))) 
      
      group_split(Stage)  %>% 
      purrr::map(
        ~ggplot(., aes(x = long, y = lat, fill = perCapita)) + 
          geom_polygon(aes(group = group), color = "gray90", size = 0.1) +
          geom_text(aes(x = -130, y = 31, label = unitLab), hjust = 0, family = "serif", size = 3)+
          scale_fill_distiller(palette = "YlOrRd", labels = comma, trans = "reverse", breaks = breaks_funStage)+
          guides(fill = guide_colourbar(barwidth = 3, barheight = 0.5))+
          # facet_grid(~factor(Stage, levels = c("On-farm", "Distribution (F-M)", "Manufacturing", "Distribution", "W&R", "Consumption")))+
          facet_grid(~Stage)+
          coord_fixed(1.3) +
          THEME_map +
          theme(text = element_text(family = "serif", face = "bold.italic")
                , legend.position = c(0.15,0) 
                , legend.direction = "horizontal"
                , legend.title = element_blank()
                , legend.text = element_text(size = 7)
                , legend.background=element_blank()
                , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
                , strip.placement = "outside")
      ) %>% 
      plot_grid(plotlist = ., align = 'hv', ncol = 3)
  } 
  # Energy Intensity for State & EoL ----
  {
  # Calc national EI for each food group
  # From author's previous analysis Dong et al.: https://www.nature.com/articles/s43247-022-00414-9
  EI_farmProduction <- read_excel("Data/EOL & EI.xlsx", sheet = "farmProduction") %>% 
    left_join(BD_peanutsTons, by = "stateName") %>% 
    rowwise() %>% dplyr::mutate(
      grainFood = grain * 0.186931467
      ,  grainFeed = grain * 0.432121962
      ,  oilFood = oilSeed * 0.938631958
      ,  oilFeed = oilSeed * 0.061368042
      ,  Sugar = sugarCaneBeet * 0.878898325
      ,  sugarFeed = sugarCaneBeet * 0.093341121
      ,  Other = PEANUTS_tons/1000000
      ,  oilGrain = sum(c_across(c(grainFood, oilFood)), na.rm = TRUE)
      ,  Fruit_veg = sum(c_across(c(fruit, vegetables, nuts)))
      ,  animalProducts = sum(c_across(c(meatPoultrySeafood, eggs)), na.rm = TRUE)
      ,  Feed = sum(c_across(c(grainFeed, oilFeed, sugarFeed)), na.rm = TRUE)
    ) %>% select(stateName, oilGrain, Dairy, Fruit_veg, animalProducts, Sugar, Feed) #not including "Other"
  
  EI_farmProduction <- EI_farmProduction %>% pivot_longer(!stateName, names_to = "commodity_naics", values_to = "Production_MMT") %>% filter(stateName != "Washington DC") %>% 
    left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")
  
  EI_farmProduction$commodity_naics <- revalue(EI_farmProduction$commodity_naics, c(
    "oilGrain"  = "Grain, Oil Products"
    , "Dairy" = "Dairy Products"
    ,	"Fruit_veg" = "Fruit, Vegetables, Nuts Products"
    ,  "animalProducts"  = "Animal Products"
    , "Sugar" = "Sugar, Confectionary Products"
    , "Feed" = "Animal Feed Products"
  ))
  
  # From author's previous analysis Dong et al.: https://www.nature.com/articles/s43247-022-00414-9
  EI_manufdFoodState <- read_excel("Data/EOL & EI.xlsx",sheet = "Manufactured by state") %>% 
    filter(State_Abb != "DC") %>% 
    rowwise() %>% dplyr::mutate("Animal Products" = sum(c_across(c( meat_poultry_eggs, Seafood)),na.rm = TRUE)    ) %>% 
    select(-meat_poultry_eggs, -Seafood) %>% 
    pivot_longer(!State_Abb, names_to = "commodity_naics", values_to = "Production_MMT")
  
  EI_manufdFoodState$commodity_naics <- revalue(EI_manufdFoodState$commodity_naics, c(
    # farm products
    "oilGrain"  = "Grain, Oil Products"
    , "Dairy" = "Dairy Products"
    ,	"Fruit_veg_nut" = "Fruit, Vegetables, Nuts Products"
    , "Animal Products"  = "Animal Products"
    , "Sugar" = "Sugar, Confectionary Products"
    , "animalFeed" = "Animal Feed Products"
  ))
  
  EI_production <- rbind(select(mutate(EI_farmProduction, Stage = "On-farm"), -stateName)
                         ,mutate(EI_manufdFoodState, Stage = "Manufacturing")) %>% mutate(commodity_naics_red = commodity_naics)
  
  EI_farmFeedMMT <-  sum(filter(EI_farmProduction, commodity_naics == "Animal Feed Products")$Production_MMT, na.rm = TRUE) 
  EI_manufFeedMMT <- sum(filter(EI_manufdFoodState, commodity_naics == "Animal Feed Products")$Production_MMT, na.rm = TRUE)
  
  allData_final$commodity_naics_red <- revalue(allData_final$commodity_naics, c(
    # farm products
    "Grain, Oil"  = "Grain, Oil Products"
    , "Dairy" = "Dairy Products"
    ,	"Fruit, Vegetables, Nuts" = "Fruit, Vegetables, Nuts Products"
    , "Meat, Poultry, Eggs"  = "Animal Products"
    , "Seafood" = "Animal Products"
    , "Sugar" = "Sugar, Confectionary Products"
    #manuf products
    , "Grain, Oil Milling"  = "Grain, Oil Products"
    , "Dairy Products" = "Dairy Products"
    ,	"Fruit, Vegetables, Nuts Products"  = "Fruit, Vegetables, Nuts Products"
    , "Animal Product Processing"      = "Animal Products"
    , "Sugar, Confectionary"  = "Sugar, Confectionary Products"
    , "Seafood Processing"    = "Animal Products"
    , "Bakeries, Tortillas"    =  "Grain, Oil Products"
  ))
  
  allData_finalIntensity <-
    allData_final %>% filter(Stage == "On-farm" | Stage == "Manufacturing") %>% 
    group_by(Analysis, Stage, State_Abb, commodity_naics_red) %>% summarise(Value = sum(Value, na.rm = TRUE)) %>%
    left_join(EI_production, by = c("Stage", "commodity_naics_red", "State_Abb")) %>%
    mutate(Intensity = Value/Production_MMT)
  
  allData_finalIntensity_Nat <-
    allData_finalIntensity %>% group_by(Analysis, Stage, commodity_naics_red) %>% 
    summarise(Value = sum(Value, na.rm = TRUE), Production_MMT = sum(Production_MMT, na.rm = TRUE)) %>%
    mutate(Intensity_Nat = Value / Production_MMT)
  }
  
  # Scatter Plots for State Analysis ----
  {
    #Many of these scatter plots were only used to explore the state level data, but were not directly presented in the work
    # One dot plot is used in the analysis 
    dataEINat <- select(allData_finalIntensity_Nat, Analysis, Stage, commodity_naics_red, Intensity_Nat) %>%
      filter(commodity_naics_red != "Other Products") %>%   filter(commodity_naics_red != "Other") 
    
    dataScatter <- left_join(select(allData_finalIntensity, Analysis, Stage, State_Abb, commodity_naics_red, Intensity, Production_MMT, Value)
                             , dataEINat
                             , by = c("Analysis", "Stage", "commodity_naics_red")) %>%
      rowwise()%>%
      mutate(RelativeDiff = (Intensity - Intensity_Nat)/Intensity_Nat) %>%
      filter(commodity_naics_red != "Other Products") %>%   filter(commodity_naics_red != "Other") 
    
    prodTop10 <- dataScatter %>% ungroup() %>% 
      group_by(Analysis, Stage, commodity_naics_red) %>%
      slice_max(order_by = Production_MMT, n = 10)
    
    minTop10 <- prodTop10%>% ungroup() %>% 
      group_by(Analysis, Stage, commodity_naics_red) %>%
      slice_min(order_by = Production_MMT, n = 1) 
    
    dataLabels <- prodTop10 %>% group_by(Analysis, Stage, commodity_naics_red) %>%
      mutate(ABSRelativeDiff = abs(RelativeDiff)) %>%
      slice_max(order_by = ABSRelativeDiff, n = 5)
    #These plots were not directly used in the analysis
    dataScatter %>%
      filter(Analysis == "Site Energy (TBTU)") %>%
      ggplot()+
      geom_point(aes(x = Production_MMT, y = Value, fill = RelativeDiff), shape = 21, size = 2)+
      geom_abline(data = filter(dataEINat, Analysis == "Site Energy (TBTU)"), aes(slope = Intensity_Nat, intercept = 0))+
      geom_abline(data = filter(dataEINat, Analysis == "Site Energy (TBTU)"), aes(slope = Intensity_Nat*1.25, intercept = 0))+
      geom_abline(data = filter(dataEINat, Analysis == "Site Energy (TBTU)"), aes(slope = Intensity_Nat*0.75, intercept = 0))+
      geom_vline(data = filter(minTop10, Analysis == "Site Energy (TBTU)"), aes(xintercept = Production_MMT))+
      # geom_text_repel(data = dataLabels, aes(x = Production_MMT, y = Value, label = State_Abb ), size = 3, direction = "y")+
      facet_wrap(factor(Stage, levels = c("On-farm", "Manufacturing"))~commodity_naics_red, scales = "free", nrow = 2)+
      scale_fill_distiller(palette = "RdBu"
                           , limit = c(-1,1)#c(-ceiling(colorMinMax*10)/10, ceiling(colorMinMax*10)/10)
                           , breaks = c(-1,0, 1)#c(-ceiling(colorMinMax*10)/10, 0, ceiling(colorMinMax*10)/10)
      )+
      labs( y = "Site Energy (TBTU)", x = "Production (MMT)", fill = "Relative Difference")+
      THEME_simple+theme(legend.position = "bottom")
    
    dataScatter %>%
      filter(Analysis == "GHG Emissions (MMT CO2e)") %>%
      ggplot()+
      geom_point(aes(x = Production_MMT, y = Value, fill = RelativeDiff), shape = 21)+
      # geom_text(aes(x = Production_MMT, y = Value, color = RelativeDiff, label = State_Abb))+
      geom_abline(data = filter(dataEINat, Analysis == "GHG Emissions (MMT CO2e)"), aes(slope = Intensity_Nat, intercept = 0))+
      geom_abline(data = filter(dataEINat, Analysis == "GHG Emissions (MMT CO2e)"), aes(slope = Intensity_Nat*1.25, intercept = 0))+
      geom_abline(data = filter(dataEINat, Analysis == "GHG Emissions (MMT CO2e)"), aes(slope = Intensity_Nat*0.75, intercept = 0))+
      geom_vline(data = filter(minTop10, Analysis == "GHG Emissions (MMT CO2e)"), aes(xintercept = Production_MMT))+
      # geom_text_repel(data = dataLabels, aes(x = Production_MMT, y = Value, label = State_Abb ), size = 3, direction = "y")+
      facet_wrap(factor(Stage, levels = c("On-farm", "Manufacturing"))~commodity_naics_red, scales = "free", nrow = 2)+
      scale_fill_distiller(palette = "RdBu"
                           , limit = c(-1,1)#c(-ceiling(colorMinMax*10)/10, ceiling(colorMinMax*10)/10)
                           , breaks = c(-1,0, 1)#c(-ceiling(colorMinMax*10)/10, 0, ceiling(colorMinMax*10)/10)
      )+
      # labs(title=paste(STAGE,"-" , NAICS), y = ANALYSIS, x = "Production (MMT)") +
      labs( y = "GHG EMissions (MMT CO2e)", x = "Production (MMT)", fill = "Relative Difference")+
      THEME_simple+theme(legend.position = "bottom")
    
    dataScatter %>% ungroup() %>% 
      group_by(Analysis, Stage, commodity_naics_red) %>% filter(Production_MMT != 0) %>%
      dplyr::summarise(mean= mean(Intensity, na.rm = TRUE),
                       std = sd(Intensity, na.rm = TRUE),
                       relStd = std/mean) %>% select(-mean, -std)%>%
      pivot_wider(names_from = commodity_naics_red, values_from = relStd)
    
    GRAPH_scatterPlot <- function(STAGE, NAICS, ANALYSIS){  
      dataEINat <- select(allData_finalIntensity_Nat, Analysis, Stage, commodity_naics_red, Intensity_Nat) %>%
        filter(Analysis == ANALYSIS) %>%
        filter(commodity_naics_red == NAICS) %>%
        filter(Stage == STAGE)
     
      dataScatter <- left_join(select(allData_finalIntensity, Analysis, Stage, State_Abb, commodity_naics_red, Intensity, Production_MMT, Value)
                               , dataEINat
                               , by = c("Analysis", "Stage", "commodity_naics_red")) %>%
        rowwise()%>%
        mutate(RelativeDiff = (Intensity - Intensity_Nat)/Intensity_Nat) %>%
        # left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>%
        filter(Analysis == ANALYSIS) %>%
        filter(commodity_naics_red == NAICS) %>%
        filter(Stage == STAGE)
      
      dataLabels <- dataScatter %>% ungroup() %>% slice_max(order_by = Production_MMT, n = 10) %>%
        mutate(ABSRelativeDiff = abs(RelativeDiff)) %>%
        slice_max(order_by = ABSRelativeDiff, n = 10)
      
      colorMinMax <- dataLabels %>% ungroup() %>% dplyr::summarise(limit = max(ABSRelativeDiff)) %>%
        select(limit) %>% unlist() %>% unname()
      
      dataScatter <- dataScatter %>% dplyr::mutate(
        test = ceiling(colorMinMax*10)/10,
        fillRelativeDiff = ifelse(abs(RelativeDiff) > test, test * RelativeDiff/abs(RelativeDiff), RelativeDiff))
      # return(dataScatter)
      dataScatter %>%
        ggplot()+
        geom_point(aes(x = Production_MMT, y = Value, fill = fillRelativeDiff), shape = 21)+
        # geom_text(aes(x = Production_MMT, y = Value, color = RelativeDiff, label = State_Abb))+
        geom_abline(data = dataEINat, aes(slope = Intensity_Nat, intercept = 0))+
        geom_abline(data = dataEINat, aes(slope = Intensity_Nat*1.25, intercept = 0))+
        geom_abline(data = dataEINat, aes(slope = Intensity_Nat*0.75, intercept = 0))+
        # geom_vline(data = filter(minTop10, Analysis == ANALYSIS), aes(xintercept = Production_MMT))+
        
        geom_text_repel(data = dataLabels, aes(x = Production_MMT, y = Value, label = State_Abb ), size = 3, direction = "y")+
        scale_fill_distiller(palette = "RdBu"
                             , limit = c(-ceiling(colorMinMax*10)/10, ceiling(colorMinMax*10)/10)
                             , breaks = c(-ceiling(colorMinMax*10)/10, 0, ceiling(colorMinMax*10)/10)
        )+
        labs(title=paste(STAGE,"-" , NAICS), y = ANALYSIS, x = "Production (MMT)", fill = "Intensity\nRelative\nDifference") +
        THEME_simple + theme(legend.position = "bottom")}
    GRAPH_scatterPlot("On-farm", "Animal Products", "Site Energy (TBTU)")
    GRAPH_scatterPlot("Manufacturing", "Animal Products", "Primary Energy (TBTU)") 
    GRAPH_scatterPlot("On-farm", "Animal Products", "GHG Emissions (MMT CO2e)") 
    GRAPH_scatterPlot("Manufacturing", "Fruit, Vegetables, Nuts Products", "Primary Energy (TBTU)") 
    GRAPH_scatterPlot("On-farm", "Fruit, Vegetables, Nuts Products", "Site Energy (TBTU)") 
    GRAPH_scatterPlot("On-farm", "Fruit, Vegetables, Nuts Products", "Primary Energy (TBTU)") 
    GRAPH_scatterPlot("On-farm", "Fruit, Vegetables, Nuts Products", "GHG Emissions (MMT CO2e)") 
    GRAPH_scatterPlot("On-farm", "Sugar, Confectionary Products", "Site Energy (TBTU)") 
    GRAPH_scatterPlot("Manufacturing", "Grain, Oil Products", "Site Energy (TBTU)") 
    GRAPH_scatterPlot("On-farm", "Grain, Oil Products", "Site Energy (TBTU)") 
    
    GRAPH_dotPlot <- function(STAGE, NAICS, ANALYSIS, topCount){
      topX <-
        EI_production %>% group_by(commodity_naics, Stage) %>%
        slice_max(order_by = Production_MMT, n = topCount) %>%
        left_join( summarise(
          group_by(
            filter(allData_final, Stage == STAGE)
            , Analysis, Stage, State_Abb, commodity_naics_red)
          , Value = sum(Value, na.rm = TRUE))
          , by = c("State_Abb", "Stage", "commodity_naics_red")) %>% 
        filter(commodity_naics != "Animal Feed Products")%>%
        mutate(Intensity = Value/Production_MMT) %>%
        left_join(select(allData_finalIntensity_Nat, Analysis, Stage, commodity_naics_red, Intensity_Nat)
                  , by = c("Analysis", "Stage", "commodity_naics_red")) %>%
        rowwise()%>%
        mutate(RelativeDiff = (Intensity - Intensity_Nat)/Intensity_Nat) %>%
        left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb")
      
      topXCompare <-
        topX %>% 
        filter(Analysis == ANALYSIS) %>%
        filter(commodity_naics == NAICS ) %>%
        mutate(RelativeDiffMore = (RelativeDiff/abs(RelativeDiff)) *0.4 + RelativeDiff  
               ,color = (RelativeDiff/abs(RelativeDiff))
               , Stage = factor(Stage, levels = c("On-farm", "Manufacturing","Distribution", "W&R", "Consumption"))
        ) %>%
        ungroup() %>%
        arrange(Stage, commodity_naics, RelativeDiff) %>%
        mutate(order = as.numeric(row_number())) 
      
      labelTable<- allData_finalIntensity_Nat   %>%  
        filter(Analysis == ANALYSIS) %>%
        filter(commodity_naics_red == NAICS) %>%
        dplyr::rename( "commodity_naics" = commodity_naics_red) %>%
        dplyr::mutate(IntensityLabel = ifelse(Analysis == "GHG Emissions (MMT CO2e)", " \nIntensity\n(MMT CO2e/MMT)", " \nIntensity\n(TBTU/MMT)")
                      , AnalysisLabel = ifelse(Analysis == "GHG Emissions (MMT CO2e)", " \nEmissions\nMMT CO2e", " \nEnergy\n(TBTU)")
                      , State_Abb = "U.S.")%>%
        # left_join( dplyr::summarise(group_by(topXCompare, Stage, commodity_naics)
        #                             ,label_x = max(order)
        #                             , label_x2 = min(order))
        #            , by = c("Stage", "commodity_naics")) %>%
        mutate(label_x = topCount, label_x2 = 1) %>%
        filter(Stage == STAGE) 
      
      dataTable <- topXCompare %>%
        filter(Stage == STAGE) %>% filter(commodity_naics == NAICS) %>% select(-commodity_naics_red, -Intensity_Nat) %>%
        add_row(data.frame(commodity_naics = labelTable$commodity_naics, Production_MMT = labelTable$Production_MMT
                           , State_Abb = "U.S.", Analysis = labelTable$Analysis, 
                           Stage = labelTable$Stage,
                           Value = labelTable$Value, Intensity = labelTable$Intensity_Nat, 
                           order = topCount+1,
                           RelativeDiff = 0, stateName = "U.S."))
      
      # return(dataTable)
      dataTable%>% filter(State_Abb != "U.S.") %>%
        ggplot(aes(x=order, y=RelativeDiff, color = as.factor(color)), family = "serif") +
        geom_point(stat='identity', fill="black", size=4)  +
        geom_segment(aes(y = 0,x = order,  yend = RelativeDiff , xend = order), color = "black") +
        # geom_text(aes(y = RelativeDiffMore, label= percent(RelativeDiff, 1)), color="black", size=4) +
        geom_text(aes(y = RelativeDiffMore, label= round(Intensity, 1)), color="black", size=3, family = "serif") +
        geom_text(aes(y = -2.5, label = round(Production_MMT,1)), size = 3, family = "serif")+
        geom_text(aes(y = 2.25, label = round(Value,1)), size = 3, family = "serif")+
        geom_text(data = labelTable, aes(y = -2.5, x = label_x+1, label = round(Production_MMT,1)), color = "black", size = 3, family = "serif")+
        geom_text(data = labelTable, aes(y = +2.5, x = label_x+1, label = round(Value,1)), color = "black", size = 3, family = "serif")+
        geom_text(data = labelTable, aes(y = 0, x = label_x+1, label = round(Intensity_Nat,1)), color = "black", size = 3, family = "serif")+
        geom_text(data = labelTable,aes(y = -2.5, x = label_x+2, label = " \nProduction\n(MMT)"), color = "black", size = 3, family = "serif")+
        geom_text(data = labelTable,aes(y = +2.5, x = label_x+2, label = AnalysisLabel), color = "black", size = 3, family = "serif")+
        geom_text(data = labelTable,aes(y = 0, x = label_x+2, label = IntensityLabel), color = "black", size = 3, family = "serif")+
        geom_segment(data = labelTable,
                     aes(y = 0, x = label_x, yend = 0 , xend = label_x2), color = "black", size = 1) +
        scale_color_brewer(palette = "Dark2", guide = "none")+
        scale_y_continuous(labels = scales::percent, limits= c(-3,3), breaks = c(-1,0, 1),name = "Relative Difference from National Average") +
        scale_x_continuous(
          breaks = dataTable$order,
          labels = dataTable$State_Abb,
          name = ""    )+
        labs(title=paste(STAGE,"-" , NAICS)) +
        coord_flip()+
        THEME_simple + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }
    
    GRAPH_dotPlot("On-farm", "Fruit, Vegetables, Nuts Products", "Site Energy (TBTU)", 5) 
    GRAPH_dotPlot("Manufacturing", "Animal Products", "Site Energy (TBTU)", 5)
    GRAPH_dotPlot("On-farm", "Dairy Products", "Site Energy (TBTU)", 5)
    GRAPH_dotPlot("Manufacturing", "Dairy Products", "Site Energy (TBTU)", 5)
    GRAPH_dotPlot("Manufacturing", "Dairy Products", "GHG Emissions (MMT CO2e)", 5)
  }
  
    
  # State Level analysis: farm & manuf analysis ----
  {
  BD_FARM_avgAnimalWeight <- data.frame(
    Livestock = c("BeefCattle", "Bison", "Goats", "Poultry", "Sheep", "Swine"),
    avgWeight = c(mean(1450, 1200, 1000), 1500, 150, 3, 170, 450)
  )
  #https://extension.psu.edu/how-much-do-your-animals-weigh 
  #https://www.nps.gov/articles/15-facts-about-bison.htm#:~:text=Bison%20are%20the%20largest%20mammal,30%E2%80%9370%20pounds%20at%20birth.
  # https://smallfarms.cornell.edu/2014/01/what-is-the-ideal-weight-for-a-market-lamb/#:~:text=The%20ewes%20will%20reach%20about,than%20a%20medium%20sized%20sheep.
  # https://wikifarmer.com/qas-on-goats/#:~:text=Most%20adult%20female%20goats%20weigh,280%20pounds%20(130%20kg) 
  }
  
  FARM_animalGraph <-
    F_FARM_AnimalsState %>%
    left_join(select(BD_stateRegion, stateName, State_Abb), by = "stateName")%>%
    filter(Livestock != "MilkCattle") %>%
    left_join(BD_FARM_avgAnimalWeight, by = "Livestock") %>%
    group_by(State_Abb) %>%
    mutate(estimatedWeight = avgWeight*number
           ,estWeightFrac = estimatedWeight/sum(estimatedWeight)
           , Livestock = recode_factor(Livestock, "BeefCattle"="Beef Cattle"))%>%
    filter(State_Abb %in% c("TX", "IA", "NE","GA", "NC")) %>%
    mutate(Livestock = factor(Livestock, levels = c("Beef Cattle", "Poultry", "Swine","Goats", "Sheep",  "Bison"))) %>%
    ggplot()+
    geom_col(aes(x = 1, y = estWeightFrac, fill = Livestock))+
    scale_fill_brewer(palette = "Dark2")+
    scale_y_continuous(label= percent_format(), name = "Estimated mass breakdown\nof farm-based livestock")+
    facet_wrap(~factor(State_Abb, levels = c("TX", "IA", "NE", "GA", "NC")), strip.position = "left", ncol = 1)+
    guides(fill = guide_legend(ncol = 2))+
    coord_flip()+
    THEME_simple + theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
                         legend.title = element_blank(), legend.key.size = unit(0.8,"line")
                         , legend.text = element_text(size = 10), legend.position = "top" #c(.85, .25)
                         # , legend.box.background = element_blank()
                         , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                         , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
                         , strip.text.y.left = element_text(angle=360)
                         , legend.background = element_blank()
                         , legend.box.background = element_rect(colour = "black")
                         , legend.box.margin=margin(-3,0,0,0)
    )
  
  #for better figure, turn off lab(title) in GRAPH_dotPlot
  png(filename = "Results/Plots/Fig 13 - farmAnimals.png", type = "cairo", units =
        "in", width = 6.5, height = 3.6, pointsize = 10, res = 600)
  plot_grid(  GRAPH_dotPlot("On-farm", "Animal Products", "Site Energy (TBTU)", 5)
              , FARM_animalGraph
              , labels = "", nrow = 1, rel_widths = c(.6, 0.4))
  dev.off()
  

# FLW EoL ----
  {
    # Energy Intensity for EoL ====
    
    # Animal Feed EI for FLW energy ====
    #This needs to stay as "not _alt" data to get AF separate
    {
      EI_AF <- filter(allData, Stage == "On-farm" | Stage == "Manufacturing" | Stage == "Distribution (F-M)" | Stage == "Distribution") %>% select(-stateName) %>%
        filter(str_detect(Analysis, "Site") & str_detect(commodity_naics, "Feed"))# & Stage != "Distribution")
      EI_AF$Stage <- recode_factor(EI_AF$Stage, "Distribution (F-M)" = "Feed Offset", "On-farm" = "Feed Offset", "Manufacturing" = "Processing", "Distribution" = "Processing")
      EI_AF <- aggregate(Value ~ Stage, EI_AF, FUN = sum, na.rm = TRUE)  %>% dplyr::rename("Impact" = Stage)%>%
        mutate(Disposal = "AF", Value = ifelse(Impact == "Feed Offset", -1 * Value/(EI_farmFeedMMT * 1.10231), 1 * Value/(EI_manufFeedMMT * 1.10231))  ) %>% # convert to MMBtu/ton: TBTU*(1000000MMBtu/TBTU)/ (1.010231 ton/MT * 1000000MT/MMT)
        mutate(Analysis = "Site Energy (TBTU)")
      
      GHGI_AF <- filter(allData, Stage == "On-farm" | Stage == "Manufacturing" | Stage == "Distribution (F-M)" | Stage == "Distribution") %>% select(-stateName) %>%
        filter(str_detect(Analysis, "GHG") & str_detect(commodity_naics, "Feed"))# & Stage != "Distribution")
      GHGI_AF$Stage <- recode_factor(GHGI_AF$Stage, "Distribution (F-M)" = "Feed Offset", "On-farm" = "Feed Offset", "Manufacturing" = "Processing", "Distribution" = "Processing")
      GHGI_AF <- aggregate(Value ~ Stage, GHGI_AF, FUN = sum, na.rm = TRUE)  %>% dplyr::rename("Impact" = Stage)%>%
        mutate(Disposal = "AF", Value = ifelse(Impact == "Feed Offset", -1 * Value/(EI_farmFeedMMT * 1.10231), 1 * Value/(EI_manufFeedMMT * 1.10231))  ) %>%# convert to MMBtu/ton: TBTU*(1000000MMBtu/TBTU)/ (1.010231 ton/MT * 1000000MT/MMT)
        mutate(Analysis = "GHG Emissions (MMT CO2e)")
      
      Intensity_AF <- rbind(EI_AF, GHGI_AF) %>% group_by(Analysis, Disposal) %>% summarise(Net = sum(Value))
      
    }
    
    
    
    # Overall EI ====
    {
      #from Dong et al. 2021
      EI_consumedFood <- data.frame(commodity_naics_red = c( "Grain, Oil Products", "Dairy Products", "Fruit, Vegetables, Nuts Products", "Sugar, Confectionary Products", "Animal Feed Products","Animal Products" )
                                    , Type = c("Warehouse - Dry", "Warehouse - Cold", "Warehouse - Cold", "Warehouse - Dry", "Warehouse - Dry", "Warehouse - Cold")
                                    , retailedFood_MMT = c(47.6471774179606,35.6606632517215,69.4269199606184,10.2097456118348,0,33.9293743283999)
                                    , consumedFood_MMT = c(  31.8660265508257,24.1035389724229,40.0284674522583,5.99227169452555,0,24.166000388275)
      )
      
      EI_consumedFood <-  EI_consumedFood %>%group_by(Type) %>% mutate(retailFracType = retailedFood_MMT/sum(retailedFood_MMT)) 
      EI_consumedFood <-  EI_consumedFood %>% ungroup() %>% mutate(retailFrac = retailedFood_MMT/sum(retailedFood_MMT), consumedFrac = consumedFood_MMT/sum(consumedFood_MMT))
      
      
      allData_final$commodity_naics_red <- revalue(allData_final$commodity_naics, c(
        # farm products
        "Grain, Oil"  = "Grain, Oil Products"
        , "Dairy" = "Dairy Products"
        ,	"Fruit, Vegetables, Nuts" = "Fruit, Vegetables, Nuts Products"
        ,  "Meat, Poultry, Eggs"  = "Animal Products"
        ,"Seafood" = "Animal Products"
        , "Sugar" = "Sugar, Confectionary Products"
        # , "Animal Feed" = "Animal Feed Products"
        #manuf products
        , "Grain, Oil Milling"  = "Grain, Oil Products"
        , "Dairy Products" = "Dairy Products"
        ,	"Fruit, Vegetables, Nuts Products"  = "Fruit, Vegetables, Nuts Products"
        ,  "Animal Product Processing"      = "Animal Products"
        ,  "Sugar, Confectionary"  = "Sugar, Confectionary Products"
        ,"Seafood Processing"    = "Animal Products"
        # , "otherManuf" = "Other Products"
        # , "Animal Feed Processing" = "Animal Feed Products"
        , "Bakeries, Tortillas"    =  "Grain, Oil Products"
      ))
      
      #Allocate EOSC energy/ghg to naics 
      EI_Warehouse_naics <- allData_final %>% filter(commodity_naics == "Warehouse - Cold" | commodity_naics == "Warehouse - Dry") %>%
        select(-commodity_naics_red) %>% rename(Type = commodity_naics) %>%
        left_join(select(filter(EI_consumedFood, commodity_naics_red != "Animal Feed Products"), commodity_naics_red, Type, retailFracType), by = "Type", relationship = "many-to-many") %>%
        rowwise()%>%    mutate(Value = Value * retailFracType) %>%
        select(-retailFracType, -Type)
      
      EI_endStages_naics <-
        filter(allData_final, commodity_naics == "Services" | commodity_naics == "Residential" | commodity_naics == "Retail") %>%
        select(-commodity_naics, -commodity_naics_red)%>%
        merge(select(filter(EI_consumedFood, commodity_naics_red != "Animal Feed Products"), commodity_naics_red, retailFrac, consumedFrac)) %>% 
        rowwise() %>% 
        dplyr::mutate(Value = ifelse(subSource == "Retail", Value * retailFrac, Value*consumedFrac)) %>%
        select(-retailFrac, -consumedFrac)
      
      #combine again and calc intensity
      EI_USfoodIntensity <-
        allData_final %>% filter(Stage != "W&R" & Stage != "Consumption") %>% 
        select(-commodity_naics)%>%
        rbind(EI_Warehouse_naics, EI_endStages_naics) %>%
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(select(EI_consumedFood, commodity_naics_red, consumedFood_MMT), by = "commodity_naics_red") %>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(consumedFood_MMT == 0, 0, Value / consumedFood_MMT)) %>%
        rename(commodity_naics = commodity_naics_red, output_MMT = consumedFood_MMT)
      
      
      EI_USfoodIntensitySummary <- aggregate(Intensity ~ Analysis + commodity_naics, EI_USfoodIntensity, FUN = sum) %>% pivot_wider(names_from = commodity_naics, values_from = Intensity) 
      EI_USfoodIntensitySummary$Analysis <- recode_factor(EI_USfoodIntensitySummary$Analysis,"Site Energy (TBTU)" = "Site EI (TBTU/MMT)", "Primary Energy (TBTU)" ="Primary EI (TBTU/MMT)")
      
      
      EI_USfoodIntensityTABLE <- EI_USfoodIntensity %>%
        pivot_longer(!c(commodity_naics, Fuel, Analysis), names_to = "Set", values_to = "Value") %>% 
        pivot_wider(names_from = commodity_naics, values_from = Value)
      
      EI_USfoodIntensityTABLE$Set <- recode_factor(EI_USfoodIntensityTABLE$Set,"Intensity" = "Intensity (X/MMT)", "Value" ="Energy/GHG (TBTU, GHG)", "output_MMT" = "Consumed Food (MMT)")
      # # write.csv(USfoodIntensity, "C:/Users/ak1/Dropbox (ORNL)/Armstrong Work/Food/Energy GHG Plots & Data/Data Prep Files/TotalIntensityTables.csv")
      
      # EI of just farm
      EI_USfoodIntensity_FARM <-
        allData_final %>% filter(Stage == "On-farm") %>% 
        select(-commodity_naics)%>%
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        left_join(summarise(group_by(EI_farmProduction,commodity_naics), 
                            Production_MMT = sum(Production_MMT, na.rm = TRUE)), by = c("commodity_naics_red" = "commodity_naics"))%>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(Production_MMT == 0, 0, Value / Production_MMT))%>%
        rename(commodity_naics = commodity_naics_red, output_MMT = Production_MMT)
      # EI of just manuf
      EI_USfoodIntensity_MANUF <-
        allData_final %>% filter(Stage == "Manufacturing") %>% 
        select(-commodity_naics)%>%
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        left_join(summarise(group_by(EI_manufdFoodState,commodity_naics), 
                            Production_MMT = sum(Production_MMT, na.rm = TRUE)), by = c("commodity_naics_red" = "commodity_naics"))%>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(Production_MMT == 0, 0, Value / Production_MMT))%>%
        rename(commodity_naics = commodity_naics_red, output_MMT = Production_MMT) 
      # EI farm & manuf
      EI_USfoodIntensity_F_MANUF <-
        allData_final %>% filter(Stage == "On-farm" | Stage == "Manufacturing") %>% 
        select(-commodity_naics)%>%
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        left_join(summarise(group_by(EI_manufdFoodState,commodity_naics), 
                            Production_MMT = sum(Production_MMT, na.rm = TRUE)), by = c("commodity_naics_red" = "commodity_naics"))%>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(Production_MMT == 0, 0, Value / Production_MMT)) %>%
        rename(commodity_naics = commodity_naics_red, output_MMT = Production_MMT)
      # EI farm, manuf & dist
      EI_USfoodIntensity_F_DIST <-
        allData_final %>% filter(Stage != "W&R" & Stage != "Consumption") %>% 
        select(-commodity_naics)%>%
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        left_join(summarise(group_by(EI_manufdFoodState,commodity_naics), 
                            Production_MMT = sum(Production_MMT, na.rm = TRUE)), by = c("commodity_naics_red" = "commodity_naics"))%>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(Production_MMT == 0, 0, Value / Production_MMT)) %>%
        rename(commodity_naics = commodity_naics_red, output_MMT = Production_MMT)
      
      # EI farm, manuf, dist & WR
      EI_USfoodIntensity_F_WR <-
        allData_final %>% filter(Stage != "W&R" & Stage != "Consumption") %>% 
        select(-commodity_naics)%>%
        rbind(EI_Warehouse_naics, filter(EI_endStages_naics, Stage != "Consumption")) %>% 
        group_by(commodity_naics_red, Fuel, Analysis) %>%
        summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        left_join(select(EI_consumedFood, commodity_naics_red, retailedFood_MMT), by = "commodity_naics_red") %>% 
        rowwise() %>% 
        mutate(Intensity = ifelse(retailedFood_MMT == 0, 0, Value / retailedFood_MMT)) %>%
        rename(commodity_naics = commodity_naics_red, output_MMT = retailedFood_MMT)
      
      
      EI_USfoodIntensity_FARM$Stage <- "On-farm"
      EI_USfoodIntensity_F_MANUF$Stage <- "Manufacturing"
      EI_USfoodIntensity_F_DIST$Stage <- "Distribution"
      EI_USfoodIntensity_F_WR$Stage <- "W&R"
      EI_USfoodIntensity$Stage <-"Consumption"
      
      EI_USfoodIntensity_Stages <- rbind(EI_USfoodIntensity_FARM, EI_USfoodIntensity_F_MANUF, 
                                         EI_USfoodIntensity_F_DIST,EI_USfoodIntensity_F_WR , EI_USfoodIntensity  ) %>%
        group_by(Stage, Analysis, commodity_naics) %>%
        summarise(Value = sum(Value, na.rm = TRUE), output_MMT = sum(output_MMT, na.rm = TRUE), Intensity = sum(Intensity, na.rm = TRUE))
      
      EI_USfoodIntensity_Stages$Analysis <- recode_factor(EI_USfoodIntensity_Stages$Analysis,"Site Energy (TBTU)" = "Site", "Primary Energy (TBTU)" ="Source", "GHG Emissions (MMT CO2e)" =  "GHG")
      EI_USfoodIntensity_Stages$Stage <-factor(EI_USfoodIntensity_Stages$Stage, levels = c("On-farm",  "Manufacturing", "Distribution", "W&R", "Consumption", "Population"))
      
    }
    # End of Life: 2016 EOL not already included in analysis ====
    {
      # From author's previous analysis: https://www.nature.com/articles/s43247-022-00414-9
      # amount of waste disposed via different pathways
      F_EOL_endWaste <- read_excel("Data/EOL & EI.xlsx", 
                                   sheet = "endwaste20201014") %>%dplyr::rename("MMT" = value)
      
      # site energy & GHG intensity for waste disposal
      # https://www.epa.gov/warm/documentation-waste-reduction-model-warm
      # https://www.oregon.gov/deq/mm/food/Pages/Food-Waste.aspx
      EOL_disposalEI <- read_excel("Data/EOL & EI.xlsx", 
                                   sheet = "disposalEI", col_types = c("text", "text", "skip", "skip", "skip", "skip", 
                                                                       "skip", "numeric", "numeric", "numeric", 
                                                                       "numeric", "text" ,"skip")) 
      
      EOL_disposalGHGI <- read_excel("Data/EOL & EI.xlsx", 
                                     sheet = "disposalGHGI") %>% select(-notes)
      
      # fertilizer manuf is not included in the offsets since it is already applied to the field
      EOL_disposalEI <- EOL_disposalEI %>% filter(Analysis == "Site Energy (TBTU)") %>% select(-fert_tbtu_mmt)
      EOL_disposalEIsite <- EOL_disposalEI %>% mutate(Coal = 0, Renewables = 0) %>% 
        dplyr::rename("Natural Gas" = NG_tbtu_mmt , "Grid Electricity" = Electricity_tbtu_mmt ,  "Petroleum" = petroleum_tbtu_mmt) %>%
        pivot_longer(!c(Disposal, epaLevel, Analysis), names_to = "Fuel_TBTU_MMT", values_to = "unit_MMT") %>% 
        mutate(Fuel = "FLW Mgmt"
               , Source = ifelse(Fuel_TBTU_MMT != "Electricity", "FLW Fuel", "FLW Electricity")
               , Fuel_x = Fuel_TBTU_MMT) %>% select(-Fuel_TBTU_MMT)
      
      #sources uses U.S. level conversion b/c don't know where electricity is used
      EOL_disposalEIsource <-
        select(GEN_US2016, Fuel, heatRate, percent) %>% merge(EOL_disposalEI) %>%
        mutate(Electricity_tbtu_mmtx = percent * Electricity_tbtu_mmt * heatRate* GEN_tdLossFactorUS/3412) %>% 
        select(-Electricity_tbtu_mmt, -heatRate, -percent) %>%
        pivot_wider(names_from = Fuel, values_from = Electricity_tbtu_mmtx) %>% rowwise() %>%
        dplyr::mutate("Other" = sum(c_across(c(Nuclear, Other,"Other Gases")),na.rm = TRUE)
                      , Renewables = sum(c_across(c("Geothermal", "Hydroelectric Conventional", "Other Biomass" , "Wind", "Wood and Wood Derived Fuels" ,  "Solar Thermal and Photovoltaic", "Pumped Storage")),na.rm = TRUE)
        ) %>%
        select(-Nuclear, -Other, -"Other Gases",-"Geothermal", -"Hydroelectric Conventional", -"Other Biomass", -"Wind", -"Wood and Wood Derived Fuels", -"Solar Thermal and Photovoltaic", -"Pumped Storage") %>%
        rename(NG_tbtu_mmt_fuel = NG_tbtu_mmt, petroleum_tbtu_mmt_fuel = petroleum_tbtu_mmt)%>%
        pivot_longer(!c(Disposal, epaLevel, Analysis), names_to = "Fuel_TBTU_MMT", values_to = "unit_MMT") %>%
        mutate(Analysis = "Primary Energy (TBTU)", 
               Fuel = recode_factor(Fuel_TBTU_MMT, NG_tbtu_mmt_fuel = "Natural Gas", petroleum_tbtu_mmt_fuel = "Petroleum"),
               Source = ifelse(Fuel_TBTU_MMT != "Electricity", "FLW Fuel", "FLW Electricity"),
               Fuel_x = Fuel ) %>% select(-Fuel_TBTU_MMT)
      
      EOL_disposalGHGI <- EOL_disposalGHGI  %>% select(-fert_kg_kg) %>%  rowwise()%>%
        dplyr::mutate(Coal = 0, Renewables = 0, `Grid Electricity` = sum(electricity_kg_kg, electricity_displace_kg_kg, na.rm = TRUE), Analysis = "GHG Emissions (MMT CO2e)") %>% 
        select(-electricity_kg_kg, -electricity_displace_kg_kg) %>%
        dplyr::rename("Natural Gas" = NG_kg_kg ,  "Petroleum" = petroleum_kg_kg, "Fugitive" = Fugitive_kg_kg, "Storage" = storage_kg_kg) %>%
        pivot_longer(!c(Disposal, epaLevel, Analysis), names_to = "Source_MMT_MMT", values_to = "unit_MMT") %>% 
        mutate(Fuel = "FLW Mgmt"
               , Source = ifelse(Source_MMT_MMT == "Electricity", "FLW Electricity", 
                                 ifelse(Source_MMT_MMT == "Fugitive" | Source_MMT_MMT == "Storage", "FLW Other", "FLW Fuel"))
               , Fuel_x = Source_MMT_MMT) %>% select(-Source_MMT_MMT)
      
      EOL_disposalXI <- rbind(EOL_disposalEIsite, EOL_disposalEIsource, EOL_disposalGHGI)
      
      #Energy for waste disposal
      F_EOL_total <-
        left_join(F_EOL_endWaste, EOL_disposalXI, by = c("Disposal", "epaLevel"), relationship = "many-to-many")  %>% 
        mutate(Value = MMT * unit_MMT) %>% select(-MMT, -unit_MMT) %>% 
        group_by(Disposal, epaLevel, epaStage, Analysis, Fuel, Source, Fuel_x) %>% 
        dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
        dplyr::rename( "commodity_naics" = Disposal , "Stage" = epaStage, "commodity_naics_red" = epaLevel) %>%
        mutate( subSource = Source         
                ,Stage =  recode_factor(as.factor(Stage),"Distribution (F-M)" = "On-Farm", "On-farm" = "On-Farm") # for using allData_final , comment if using allData
        )
      
      F_EOL_total$Fuel_x <- factor(F_EOL_total$Fuel_x, levels = c(LEVELS_fuel, "Storage", "Fugitive", "Net"))
      F_EOL_total$Stage <- factor(F_EOL_total$Stage , levels = c("On-Farm", "Manufacturing", "Distribution", "W&R", "Consumption"))
      F_EOL_total$commodity_naics_red <- factor(F_EOL_total$commodity_naics_red, levels = c("Food Donation", "Animal Feed",  "Industrial Uses", "Compost/ Land App.", "Waste", "Recycled (other)",   "Unknown", "Water" ))
      F_EOL_total$commodity_naicsF <- recode_factor(F_EOL_total$commodity_naics ,"Donation" = "FD", "Animal Feed" = "AF",  "Co-digestion/anaerobic" = "AD"
                                                    , "Composting/Aerobic" = "C", "Bio-based/bio-chemical" = "BB", "Land Application" = "LA"
                                                    , "Wastewater" = "WW" , "Incineration" = "I" , "Landfill" = "LF"
                                                    , "Recycled (other)" = "R",   "Unknown" = "Unk", "Water" = "W") 
      
      F_EOL_total$commodity_naicsF <- factor(F_EOL_total$commodity_naicsF, levels = c("FD", "AF",  "AD", "BB", "LA", "C" ,"WW", "I", "LF", "R",   "Unk", "W" ))
      F_EOL_total$Value <- na_if(F_EOL_total$Value, 0)
    }
  }
  # rm(list=ls(pattern="^EOL"))
  # EoL 1 Graphs & tables ----
  
  F_EOL_total %>% ungroup() %>%
    # group_by(commodity_naics, Analysis) %>%
    group_by(Analysis) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) #%>%
  # filter(str_detect(Analysis, "GHG"))
  
  F_EOL_total %>% ungroup() %>%
    group_by(Stage, commodity_naics, Analysis) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    mutate(perc = signif(Value/sum(Value),2), Value = signif(Value, 4)) %>%
    pivot_longer(c(perc, Value),names_to = "name1", values_to = "values") %>% ungroup() %>%
    mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
    pivot_wider(names_from = name2, values_from = values) %>% 
    arrange(Stage)%>%
    rename(`Site Energy (TBTU)` = `Site Energy (TBTU)   Value`  ,
           delete1 = `Site Energy (TBTU)   perc`,
           `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Value` ,  
           delete2 =  `Primary Energy (TBTU)   perc` ,
           `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Value`,
           delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
    select(Stage,commodity_naics, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
           delete2 , `GHG Emissions (MMT CO2e)`, delete3 )
  
  F_EOL_total %>% ungroup() %>%
    group_by(Stage,  Analysis) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    mutate(perc = signif(Value/sum(Value),2), Value = signif(Value, 4)) %>%
    pivot_longer(c(perc, Value),names_to = "name1", values_to = "values") %>% ungroup() %>%
    mutate(name2 = paste(Analysis, " ", name1)) %>% select(-Analysis, -name1) %>%
    pivot_wider(names_from = name2, values_from = values) %>% 
    arrange(Stage)%>%
    rename(`Site Energy (TBTU)` = `Site Energy (TBTU)   Value`  ,
           delete1 = `Site Energy (TBTU)   perc`,
           `Primary Energy (TBTU)` = `Primary Energy (TBTU)   Value` ,  
           delete2 =  `Primary Energy (TBTU)   perc` ,
           `GHG Emissions (MMT CO2e)` =  `GHG Emissions (MMT CO2e)   Value`,
           delete3 = `GHG Emissions (MMT CO2e)   perc` ) %>%
    select(Stage, `Site Energy (TBTU)`  , delete1,`Primary Energy (TBTU)` ,  
           delete2 , `GHG Emissions (MMT CO2e)`, delete3 ) %>% view()
  
  rbind(
    F_EOL_total %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% 
      mutate(Group = "EoL", Stage = recode_factor(Stage, "On-Farm" = "On-farm")),
    allData_final %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE))%>% mutate(Group = "StageX")
  ) %>% ungroup()%>% 
    pivot_wider(names_from = Group, values_from = Value) %>%
    # group_by(Analysis) %>% summarise(EoL = sum(EoL), StageX = sum(StageX)) %>%
    mutate(totalStage = EoL + StageX, EoLperc = EoL/totalStage) %>%
    select(Analysis, Stage, EoLperc) %>%
    pivot_wider(names_from = Analysis, values_from = EoLperc) %>%
    select(Stage, `Site Energy (TBTU)`  , `Primary Energy (TBTU)` ,  
           `GHG Emissions (MMT CO2e)` ) %>% write_clip()
  
  FXN_EOL_inStage <- function(AnalysisType){
    rbind(
      F_EOL_total %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        mutate(Group = "EoL", Stage = recode_factor(Stage, "On-Farm" = "On-farm")),
      allData_final %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE))%>% mutate(Group = "Stage")
    ) %>%
      # filter(Analysis == "GHG Emissions (MMT CO2e)") %>%
      filter(Analysis == AnalysisType) %>%
      ggplot()+
      geom_col(aes(x = Stage, y = Value, fill = Group), color = "black")+
      scale_fill_brewer(palette = "Dark2")+
      # facet_wrap(~factor(Analysis, levels = c("Site Energy (TBTU)", "GHG Emissions (MMT CO2e)")), strip.position = "left", scales = "free")+
      # guides(fill = "none")+
      # labs(y = "GHG Emissions (MMT CO2e)", x = "")+
      scale_x_discrete(labels = c("OF", "M", "D", "W&R", "C"))+
      labs(y = paste("Total ", AnalysisType), x = "")+
      THEME_simple + theme(
        legend.title = element_blank(), legend.position = c(0.7, 0.85)
        , panel.spacing = unit(0, "lines")
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        # , legend.background = element_rect(linetype = "solid", color = "black")
        # , legend.box.margin=margin(-10,-10,-10,-10)
        , legend.background = element_blank()
        , legend.box.background = element_rect(colour = "black")
        , legend.box.margin=margin(-5,0,0,0)
        ,  legend.text = element_text(size = 10) , axis.text = element_text(size = (8))
        , axis.title = element_text(size = (10)), legend.key.size = unit(0.8,"line") 
        , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
        , strip.placement = "outside"
        
      )
  }
  EoLgraphFxn2    <- function(AnalysisType){
    F_EOL_totalgraph <- filter(F_EOL_total, Fuel_x != "Net")
    F_EOL_totalgraph <- left_join(F_EOL_totalgraph, data.frame(
      Stage = c("On-Farm", "Manufacturing", "Distribution", "W&R", "Consumption"),
      Stage_lab = c("OF", "Manuf.", "Dist.", "W&R", "Cons.")))
    
    aggregate(Value ~ Stage_lab + Fuel_x + commodity_naicsF + Analysis, 
              F_EOL_totalgraph, FUN = sum, na.rm = TRUE) %>% 
      filter(Analysis == AnalysisType)%>%
      ggplot()+
      geom_col(aes(x = commodity_naicsF, y = Value, fill = Fuel_x), color = "black")+
      geom_col(data = mutate(filter(aggregate(Value ~ Stage_lab + commodity_naicsF + Analysis, F_EOL_totalgraph, FUN = sum, na.rm = TRUE),Analysis == AnalysisType),Fuel = "Net Energy" )
               , aes(x = commodity_naicsF, y = Value
               ), width = 0.25, position = position_dodge(width = 0.9), fill = "grey", color = "#262626", size = .5)+
      facet_grid(~factor(Stage_lab , levels = c("OF", "Manuf.", "Dist.", "W&R", "Cons."))
                 , scales = "free", space = "free_x", switch = "x")+
      labs(x = "")+
      # labs(y=paste("EoL ", AnalysisType), x = "")+
      guides(fill = guide_legend(nrow = 1))+
      scale_y_continuous(name = str_wrap(AnalysisType,width = 14))+
      scale_fill_manual(values = c(
        "Natural Gas"  = "#df8610" #orange
        , "Petroleum"=	"#ffcd00" # yellow
        , "Grid Electricity"	= "#2abdda" #l. blue
        , "Fugitive" = "black" # black
        , "Storage" =  "#1e7640" # green
        , "Net" = "grey"  ), 
        breaks = unique(filter(F_EOL_total, abs(Value) >0 & Analysis == AnalysisType)$Fuel_x), drop = FALSE)+
      THEME_simple + theme(
        legend.title = element_blank()
        , panel.spacing = unit(0, "lines")
        # , strip.text = element_text(size = 9)
        # , legend.position = c(0.5,0.85)#"bottom"
        , legend.position = "bottom"
        , legend.direction = "horizontal"
        # , strip.text = element_text(size = 8,  color = "black", face = "plain")
        , plot.margin = unit(c(.1, .1, 0.1, .1), "cm")
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        , legend.background = element_rect(linetype = "solid", color = "black")
        ,  legend.text = element_text(size = 10) , axis.text.x = element_text(size = (6)), axis.text.y = element_text(size = (9))
        , axis.title = element_text(size = (10)), legend.key.size = unit(0.8,"line") 
        , strip.text = element_text(size = 10, color = "black"), strip.background = element_blank()
        , strip.placement = "outside"
        , legend.box.margin=margin(-10,-10,-10,-10)
      )
    
  }
  
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  
  png(filename = "Results/Plots/Fig S4 - EoL_SUPPLEMENTAL.png", type = "cairo", units =
        "in", width = 6.5, height = 5    , pointsize = 10, res = 600) 
  plot_grid(EoLgraphFxn2("Site Energy (TBTU)") + theme(legend.position = "none", axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_blank(), plot.margin = unit(c(.1, .1, 0, .1), "cm")) 
            , EoLgraphFxn2("Primary Energy (TBTU)") + theme(legend.position = "none", axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_blank(), plot.margin = unit(c(.1, .1, 0, .1), "cm"))
            , EoLgraphFxn2("GHG Emissions (MMT CO2e)") +theme(legend.position = "none",plot.margin = unit(c(.1, .1, 0, .1), "cm"))
            , get_legend(EoLgraphFxn2("GHG Emissions (MMT CO2e)"))
            , labels = "", nrow = 4, rel_heights = c(1,1,1.2, 0.25))
  dev.off() 
  
  
  png(filename = "Results/Plots/Fig 10 - EoL.png", type = "cairo", units =
        "in", width = 6.5, height = 4.7    , pointsize = 10, res = 600) 
  plot_grid(
    F_EOL_total %>%
      mutate(commodity_naics = recode_factor(as.factor(commodity_naics), 
                                             "Co-digestion/anaerobic" = "Anaerobic Dig.", 
                                             "Composting/Aerobic" = "Comp./Land App.", 
                                             "Land Application" = "Compost/Land App.", 
                                             "Bio-based/bio-chemical" = "Other Ind.",
                                             "Unknown" = "Unk.",
                                             "Recycled (other)" = "Recycled"
      ) )  %>% 
      group_by(Stage, Analysis, commodity_naics) %>%
      summarise(Value = sum(Value, na.rm = T)) %>%
      mutate(commodity_naics = factor(commodity_naics, levels = c(
        "Donation","Animal Feed","Anaerobic Dig.","Other Ind."
        ,"Comp./Land App." ,"Wastewater", "Landfill" , "Incineration"
        , "Recycled","Unk." 
      ))
      ,  Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy\n(TBTU)"
                                  , "Primary Energy (TBTU)" = "Primary Energy\n(TBTU)"
                                  , "GHG Emissions (MMT CO2e)" = "GHG Emissions\n(MMTCO2e)")) %>%
      # filter(Analysis == "Site Energy (TBTU)") %>% 
      ggplot()+
      geom_col(aes(x = Stage, y = Value, fill = commodity_naics), color = "black")+
      geom_hline(yintercept = 0)+
      facet_wrap(~Analysis, scales = "free_y", ncol = 1, strip.position = "left")+
      scale_fill_manual(values = c("Donation" =  "#E7298A"
                                   ,"Animal Feed"= "#7570B3" 
                                   ,"Anaerobic Dig."="#1B9E77"
                                   ,"Other Ind."="#66A61E"
                                   ,"Comp./Land App." = "#A6761D"
                                   # , "Land App." = "brown"
                                   ,"Wastewater" = "#E6AB02"
                                   , "Landfill" =  "#D95F02"
                                   , "Incineration"= "brown"
                                   , "Recycled" = "#666666"
                                   , "Unk."  ="black"))+
      labs(x = "", y = "", fill = "Disposal\nMethod")+
      guides(fill = guide_legend(ncol = 4))+
      THEME_simple +    
      theme(legend.text = element_text(size = 9) , axis.text.y = element_text(size = (11)) , axis.text.x = element_text(size = (10))
            , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
            , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
            , strip.placement = "outside" #, legend.box.margin=margin(-10,-10,-10,-10)
            # , legend.spacing.y = unit(0, 'line')
            , plot.margin = unit(c(.1, .1, 1.75, .1), "cm")
            , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
            , legend.background = element_blank()
            , legend.box.background = element_rect(colour = "black")
            , legend.box.margin=margin(0,0,0,0)
            , legend.position =  c(-0.17,-0.2)
            , legend.justification='left'
            , legend.box = "horizontal"
            , legend.direction = "horizontal"
            , legend.box.spacing = margin(-5)
            
      )
    ,
    
    rbind(
      F_EOL_total %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>% 
        mutate(Group = "EoL", Stage = recode_factor(Stage, "On-Farm" = "On-farm")),
      allData_final %>% group_by(Analysis, Stage) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE))%>% mutate(Group = "Stage")
    ) %>%
      ggplot()+
      geom_col(aes(x = Stage, y = Value, fill = Group), color = "black")+
      scale_fill_brewer(palette = "Dark2")+
      facet_wrap(~Analysis, scales = "free_y", ncol = 1, strip.position = "left")+
      scale_x_discrete(labels = c("OF", "M", "D", "W&R", "C"))+
      labs(x = "", y = "")+
      guides(fill = guide_legend(ncol = 1))+
      THEME_simple + theme(
        legend.title = element_blank(), legend.position = "bottom" #c(0.7, 0.85)
        , panel.spacing = unit(0, "lines")
        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        , plot.margin = unit(c(.1, .1, .1, -.2), "cm")
        , legend.background = element_blank()
        , legend.box.background = element_rect(colour = "black")
        , legend.box.margin=margin(0,0,0,0)
        ,  legend.text = element_text(size = 10) , axis.text = element_text(size = (8))
        , axis.title = element_text(size = (10)), legend.key.size = unit(0.8,"line") 
        , strip.text = element_blank()  #element_text(size = 11, color = "black")
        , strip.background = element_blank()
        , strip.placement = "outside"
        
      )
    
    
    ,  rel_widths = c(1, 0.3)
    
  )
  dev.off()
  
  
  # EoL breakdown (supplemental)
  
  endWasteW <- read_csv("Data/FSC - endwaste20201014.csv")
  
  endWaste <- filter(endWasteW, Disposal != "Water")
  epaDisposalLevels <- c("Unknown", "LWI","Recycled (other)" ,"Compost/ Land App.", "Industrial Uses", "Animal Feed", "Food Donation")
  epaStageLevels = c("On-farm" ,  "Manufacturing", "Distribution","W&R", "Consumption")
  
  G_wasteBreakdown <-
    F_EOL_endWaste %>% 
    mutate(epaStage = recode_factor(as.factor(epaStage),"Distribution (F-M)" = "On-Farm", "On-farm" = "On-Farm")
           , epaStage = factor(epaStage, levels = c("On-Farm", "Manufacturing", "Distribution", "W&R", "Consumption"))
           , Disposal = recode_factor(as.factor(Disposal), "Co-digestion/anaerobic" = "Anaerobic Dig.", 
                                      # "Animal Feed" = "AF",
                                      "Composting/Aerobic" = "Compost/Land App", 
                                      "Land Application" = "Compost/Land App", 
                                      "Bio-based/bio-chemical" = "Other Ind.",
                                      "Recycled (other)" = "Recycled"
           ) 
           
    ) %>%
    filter(Disposal != "Water") %>% 
    group_by(epaStage, Disposal) %>%
    summarise(MMT = sum(MMT, na.rm = TRUE)) %>%
    ggplot() +
    geom_col(aes(fill = Disposal
                 , y = MMT, x = epaStage)
             , color= "black")+
    scale_fill_manual(values = c("Donation" =  "#E7298A"
                                 ,"Animal Feed"= "#7570B3" 
                                 ,"Anaerobic Dig."="#1B9E77"
                                 ,"Other Ind."="#66A61E"
                                 ,"Compost/Land App" = "#A6761D"
                                 ,"Wastewater" = "#E6AB02"
                                 , "Landfill" =  "#D95F02"
                                 , "Incineration"= "brown"
                                 , "Recycled" = "#666666"
                                 ,"Unknown"  ="black"))+
    # guides(fill = guide_legend(nrow=3, byrow=TRUE)) +
    scale_x_discrete(labels = function(x) str_wrap(x,width = 14))+
    labs(x = "", y = "Food Loss and Waste (MMT)", fill = "FLW Disposal Methods") +
    guides(fill=guide_legend(nrow=2,title.position="top"))+
    
    THEME_simple +
    theme(legend.text = element_text(size = 10) , axis.text = element_text(size = (11))
          , axis.title = element_text(size = (12)), legend.key.size = unit(0.8,"line")
          , strip.text = element_text(size = 11, color = "black"), strip.background = element_blank()
          , strip.placement = "outside" #, legend.box.margin=margin(-10,-10,-10,-10)
          # , legend.spacing.y = unit(0, 'line')
          
          , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
          , legend.background = element_blank()
          , legend.box.background = element_rect(colour = "black")
          , legend.box.margin=margin(-5,0,0,0)
          , legend.position = "bottom"
          , legend.box.spacing = margin(-5)
          
    )
  
  png(filename = "Results/Plots/Fig S3 - wasteBreakdown.png", type = "cairo", units =
        "in", width = 6.5, height = 2.75, pointsize = 10, res = 600)
  G_wasteBreakdown 
  dev.off() 
  
  
# Optimized distribution scenario ----

{
  suppressMessages({
  DIST_farm_manufGO <- read_excel_allsheets("Data/Dist - farm to manufacture by state - GO.xlsx")
  DIST_farm_manufnoGO <- read_excel_allsheets("Data/Dist - farm to manufacture by state - non-GO.xlsx")
  })
   DIST_truckFM_tonmile <-
    rbind(
      DIST_farm_manufnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_farm_manufnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_farm_manufnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_farm_manufnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_farm_manufnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_farm_manufnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_farm_manufnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_farm_manufnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
      DIST_farm_manufnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
      DIST_farm_manufnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
      DIST_farm_manufGO[["food truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
      DIST_farm_manufGO[["food truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain"), 
      DIST_farm_manufGO[["feed truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed") ,
      DIST_farm_manufGO[["feed truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  DIST_railFM_tonmile <-
    rbind(
      DIST_farm_manufnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy")
      , DIST_farm_manufnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy")
      ,  DIST_farm_manufnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") 
      ,  DIST_farm_manufnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg")
      , DIST_farm_manufnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") 
      ,  DIST_farm_manufnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts")
      ,DIST_farm_manufnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ,DIST_farm_manufnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
      ,  DIST_farm_manufnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") 
      ,  DIST_farm_manufnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar")
      ,  DIST_farm_manufGO[["food rail tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain")
      ,  DIST_farm_manufGO[["food rail tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain") 
      # , DIST_farm_manufGO[["feed rail tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed")
      # ,  DIST_farm_manufGO[["feed rail tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  DIST_truckEI <- 21335 /1000000000000 # Btu/vehicle-mi * TBTU/1000000000000BTU
  DIST_truckPayload <- (1000000*2205) / 34825 #see Excel tab 1000000 MT / MMT * 2205 lb/MT / 34825 lb/vehicle (34825 lb is from below link)
  DIST_truckFM_TBTU <- DIST_truckFM_tonmile %>% filter(State_Abb != "DC") %>% mutate(
    Dairy = Dairy * 1.2 * DIST_truckPayload * DIST_truckEI, # MMT-mi * 120% * vehicle/MMT * TBTU/vehicle-mi
    Fruit_veg = Fruit_veg * 1.2 * DIST_truckPayload * DIST_truckEI ,
    animalProducts = (animalProducts) * 1.2 * DIST_truckPayload * DIST_truckEI,
    Seafood = (Seafood) * 1.2 * DIST_truckPayload * DIST_truckEI,
    Sugar = Sugar * DIST_truckPayload * DIST_truckEI,
    oilGrain = oilGrain * DIST_truckPayload * DIST_truckEI,
    feed = feed * DIST_truckPayload * DIST_truckEI,
    Source = "Truck",
    Fuel = "Petroleum",
    Stage = "Distribution (F-M)"
  ) 
  
  DIST_railEI <- 299 * (1.10231) * 1000000 / 1000000000000 # 299Btu/ton-mi * TBTU/1000000000000BTU * 1.10231 ton/MT * 1000000MT/MMT
  
  DIST_railFM_TBTU <- DIST_railFM_tonmile %>% filter(State_Abb != "DC")%>% mutate(
    Dairy = Dairy *  DIST_railEI, # MMT-mi * TBTU/MMT-mi
    Fruit_veg = Fruit_veg *  DIST_railEI,
    animalProducts = (animalProducts) *  DIST_railEI,
    Seafood = Seafood *  DIST_railEI,
    Sugar = Sugar *  DIST_railEI,
    oilGrain = oilGrain *  DIST_railEI,
    feed = 0,
    Source = "Rail",
    Fuel = "Petroleum",
    Stage = "Distribution (F-M)"
  ) 
  
  #optimized WR
  suppressMessages({
  DIST_manuf_WRGO <- read_excel_allsheets("Data/Dist - manufacture to WR by state - GO - optimized.xlsx")
  DIST_manuf_WRnoGO <- read_excel_allsheets("Data/Dist - manufacture to WR by state - non-GO - optimized.xlsx")
  })
  DIST_truckMR_tonmile <-
    rbind(
      DIST_manuf_WRnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_manuf_WRnoGO[["truck ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_manuf_WRnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_manuf_WRnoGO[["truck ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_manuf_WRnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_manuf_WRnoGO[["truck ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_manuf_WRnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_manuf_WRnoGO[["truck ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
      DIST_manuf_WRnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
      DIST_manuf_WRnoGO[["truck ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
      DIST_manuf_WRGO[["truck ton mile G&O"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
      DIST_manuf_WRGO[["truck ton mile G&O"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain"),
      DIST_manuf_WRGO[["animal feed truck tonmile"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "feed") ,
      DIST_manuf_WRGO[["animal feed truck tonmile"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "feed")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  
  DIST_railMR_tonmile <-
    rbind(
      DIST_manuf_WRnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_manuf_WRnoGO[["rail ton mile Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_manuf_WRnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_manuf_WRnoGO[["rail ton mile F&V&N"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_manuf_WRnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_manuf_WRnoGO[["rail ton mile M&P&E"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_manuf_WRnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_manuf_WRnoGO[["rail ton mile Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood"),
      DIST_manuf_WRnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Sugar") ,
      DIST_manuf_WRnoGO[["rail ton mile Sugar"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Sugar"),
      DIST_manuf_WRGO[["rail ton mile G&O"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "oilGrain") ,
      DIST_manuf_WRGO[["rail ton mile G&O"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "oilGrain")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  
  DIST_truckMR_TBTU <- DIST_truckMR_tonmile %>% filter(State_Abb != "DC") %>% mutate(
    Dairy = Dairy *  1.2 * DIST_truckPayload * DIST_truckEI, 
    Fruit_veg = Fruit_veg * 1.2*  DIST_truckPayload * DIST_truckEI,
    animalProducts = animalProducts * 1.2*  DIST_truckPayload * DIST_truckEI,
    Seafood = Seafood * 1.2 *  DIST_truckPayload * DIST_truckEI,
    Sugar = Sugar *  DIST_truckPayload * DIST_truckEI,
    oilGrain = oilGrain  *  DIST_truckPayload * DIST_truckEI,
    feed = feed *  DIST_truckPayload * DIST_truckEI,
    Source = "Truck",
    Fuel = "Petroleum",
    Stage = "Distribution"
  ) 
  
  DIST_railMR_TBTU <- DIST_railMR_tonmile %>% filter(State_Abb != "DC")%>% mutate(
    Dairy = Dairy *  DIST_railEI, 
    Fruit_veg = Fruit_veg*  DIST_railEI, 
    animalProducts = (animalProducts)*  DIST_railEI, 
    Seafood = Seafood*  DIST_railEI, 
    Sugar = Sugar*  DIST_railEI,
    oilGrain = oilGrain  *  DIST_railEI,
    feed = 0, 
    Source = "Rail",
    Fuel = "Petroleum",
    Stage = "Distribution"
  ) 
  
  F_DIST_TBTU_opt <- rbind(DIST_truckFM_TBTU, DIST_railFM_TBTU, DIST_truckMR_TBTU, DIST_railMR_TBTU) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb") %>% 
    pivot_longer(!c(State_Abb, stateName, Source, Fuel, Stage), names_to = "commodity_naics", values_to = "Energy_TBTU") %>%
    mutate(commodity_naics = recode_factor(commodity_naics, "oilGrain" = "Grain, Oil"
                                           , "Dairy" = "Dairy"
                                           ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                           , "animalProducts" = "Meat, Poultry, Eggs"
                                           , "Seafood" = "Seafood"
                                           , "Sugar" = "Sugar"
                                           , "feed" = "Animal Feed"))
  
  DIST_Energy_GHG <- F_DIST_TBTU_opt %>%
    mutate(GHG_MMT = Energy_TBTU * (mean(73.25, 73.96, 75.04) + 25*3/1000 + 298 * 0.6/1000 )/1000) %>%# 1000000 MMBTU/TBTU * MMT/1000000000
    select(-Energy_TBTU)
  
  # Fugitive emissions from refrigeration
  #Estimate total tonnage moved by refrigerated transport:
  # https://www.winnesota.com/news/refrigeratedtransportation
  DIST_USreeferTotal_MMT <- ((3.25 * 1000000000)/((1+.1244)^6)) * 0.907185/1000000 #3.25 Bt in 2022, ~12.44% CAGR. conversion ton to MMT
  
  DIST_refrigerationGHG <- 5.89 #MMT https://di.unfccc.int/detailed_data_by_party
  
  
  DIST_farm_manuf_ton <-  suppressMessages({read_excel_allsheets("Data/Dist - farm to manufacture by state - ton.xlsx")})
  # data from another analysis, see paper & SI. in million metric ton - miles
  
  DIST_truckFM_ton <-
    rbind(
      DIST_farm_manuf_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_farm_manuf_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_farm_manuf_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_farm_manuf_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_farm_manuf_ton[["ship by truck M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_farm_manuf_ton[["ship by truck M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_farm_manuf_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_farm_manuf_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile)
  
  DIST_railFM_ton <-
    rbind(
      DIST_farm_manuf_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_farm_manuf_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_farm_manuf_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_farm_manuf_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_farm_manuf_ton[["ship by rail M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_farm_manuf_ton[["ship by rail M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_farm_manuf_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_farm_manuf_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  
  DIST_truckFM_fugGHG <-
    DIST_truckFM_ton %>% filter(State_Abb != "DC") %>% 
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")    %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      # GHG_MMT = MMT*DIST_truckPayload*(8*.7+3*.3) * .5 * 1430/1000000000, # convert to lbs divide by truck capacity convert to MMT of refrigerant 
      #GHG estimate 70% use highest value (8), 30% use lowest (3) from fugitive emissions https://www.epa.gov/sites/production/files/2015-07/documents/fugitiveemissions.pdf
      Source = "Truck Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution (F-M)"
    ) 
  
  DIST_railFM_fugGHG <- DIST_railFM_ton %>% filter(State_Abb != "DC") %>%
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")  %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      
      # GHG_MMT = ((MMT*(1000000*2205)) / (30*2000))* 8 * .5 *  1430/1000000000,  # convert mass to lbs divide by rail capacity convert to MMT of refrigerant 
      # https://www.wett.ru/es/helpful_information/rail_transport/
      # estimate top of range. from fugitive emissions https://www.epa.gov/sites/production/files/2015-07/documents/fugitiveemissions.pdf
      Source = "Rail Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution (F-M)"
    ) 
  
  DIST_manuf_WR_ton <-  suppressMessages({read_excel_allsheets("Data/Dist - food manufactured by state - ton.xlsx")})
  DIST_truckMWR_ton <-
    rbind(
      DIST_manuf_WR_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_manuf_WR_ton[["ship by truck Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_manuf_WR_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_manuf_WR_ton[["ship by truck Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_manuf_WR_ton[["ship by truck M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_manuf_WR_ton[["ship by truck M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_manuf_WR_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_manuf_WR_ton[["ship by truck Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  DIST_railMWR_ton <-
    rbind(
      DIST_manuf_WR_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Dairy") ,
      DIST_manuf_WR_ton[["ship by rail Dairy"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Dairy"),
      DIST_manuf_WR_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg") ,
      DIST_manuf_WR_ton[["ship by rail Fruit and Vege"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Fruit_veg"),
      DIST_manuf_WR_ton[["ship by rail M&P"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "animalProducts") ,
      DIST_manuf_WR_ton[["ship by rail M&P"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "animalProducts"),
      DIST_manuf_WR_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_Abb = "...1") %>% pivot_longer(!State_Abb, names_to = "State_alt", values_to = "tonmile") %>% mutate(commodity = "Seafood") ,
      DIST_manuf_WR_ton[["ship by rail Seafood"]] %>% dplyr::rename(State_alt = "...1") %>% pivot_longer(!State_alt, names_to = "State_Abb", values_to = "tonmile") %>% mutate(commodity = "Seafood")
    ) %>%
    group_by(State_Abb, commodity) %>% summarise(tonmile = sum(tonmile, na.rm = TRUE)/2) %>% pivot_wider(names_from = commodity, values_from = tonmile) 
  
  
  DIST_truckMWR_fugGHG <- DIST_truckMWR_ton %>% filter(State_Abb != "DC") %>% 
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")    %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      
      # GHG_MMT = ((MMT*(1000000*2205)) / 34825)*(8*.7+3*.3) * .5 * 1430/1000000000, # convert to lbs divide by truck capacity convert to MMT of refrigerant 
      # see above
      Source = "Truck Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution"
    ) 
  
  DIST_railMWR_fugGHG <- DIST_railMWR_ton %>% filter(State_Abb != "DC") %>%
    pivot_longer(!c(State_Abb), names_to = "commodity_naics", values_to = "MMT")  %>%
    mutate(
      GHG_MMT = DIST_refrigerationGHG * MMT/DIST_USreeferTotal_MMT,
      
      # GHG_MMT = ((MMT*(1000000*2205)) / (30*2000))* 8 * .5 *  1430/1000000000,  # convert to lbs divide by truck capacity convert to MMT of refrigerant 
      # estimate top of range. from fugitive emissions https://www.epa.gov/sites/production/files/2015-07/documents/fugitiveemissions.pdf
      Source = "Rail Refrig.",
      Fuel = "Refrigerant",
      Stage = "Distribution"
    ) 
  
  DIST_Fug_GHG <- rbind(DIST_truckFM_fugGHG, DIST_railFM_fugGHG, DIST_truckMWR_fugGHG, DIST_railMWR_fugGHG) %>% 
    left_join(select(BD_stateRegion, State_Abb, stateName), by = "State_Abb")
  
  DIST_Fug_GHG$commodity_naics <- revalue(DIST_Fug_GHG$commodity_naics, c( "Dairy" = "Dairy"
                                                                           ,	"Fruit_veg" = "Fruit, Vegetables, Nuts"
                                                                           , "animalProducts" = "Meat, Poultry, Eggs"
                                                                           , "Seafood" = "Seafood"  ))
  F_DIST_GHG_opt <- DIST_Energy_GHG %>% rbind(select(DIST_Fug_GHG, -MMT))
  
}

# Remove excess variables from environment 
rm(list=ls(pattern="^DIST"))

F_DIST_optimized <- 
  rbind(
    rename(mutate(F_DIST_TBTU_opt, Analysis = "Site Energy (TBTU)"), "Value" = Energy_TBTU),
    rename(mutate(F_DIST_TBTU_opt, Analysis = "Primary Energy (TBTU)"), "Value" = Energy_TBTU),
    rename( mutate(F_DIST_GHG_opt, Analysis = "GHG Emissions (MMT CO2e)"), "Value" = GHG_MMT)
  ) %>%
  mutate(subStage = recode_factor(Stage, "Distribution (F-M)" = "F-M Dist.", "Distribution" = "Distribution"),
         Fuel = factor(Fuel, levels = LEVELS_fuel),
         Source = factor(Source, levels = LEVELS_source), 
         Stage = factor(Stage , levels = c("On-farm", "Distribution (F-M)", "Manufacturing", "Distribution", "W&R", "Consumption")),
         Stage_y = recode_factor(Stage ,  "Distribution (F-M)" = "On-farm"),
         Stage_y = factor(Stage_y , levels = c("On-farm", "Manufacturing", "Distribution", "W&R", "Consumption"))
  ) %>% 
  #remove pet food manufacturing from datasets
  rowwise() %>%
  dplyr::mutate(
    Value = ifelse(
      commodity_naics == "Animal Feed" | commodity_naics == "Animal Feed Processing",
      Value * BD_percentNotPet, Value    )  )

# allData$Stage_x <- recode_factor(allData$Stage ,  "Distribution (F-M)" = "\nDistribution (F-M)", "Distribution" = "\nDistribution")
# allData$Stage_x <- factor(allData$Stage_x , levels = c("On-farm", "\nDistribution (F-M)", "Manufacturing", "\nDistribution", "W&R", "Consumption"))

F_DIST_optimizedAF <-
  F_DIST_optimized %>% filter(commodity_naics == "Animal Feed" | commodity_naics == "Animal Feed Processing") %>% 
  merge(BD_animalFeedBreakdown) %>% 
  rowwise() %>% dplyr::mutate(
    Value = Value * feedFrac,
    Stage_z = "On-farm"
    , Fuel_z = ifelse(Analysis == "Site Energy (TBTU)",
                      ifelse(commodity_naics == "Animal Feed Processing", "AF Manuf."
                             , ifelse(commodity_naics == "Animal Feed" & Stage_y == "Distribution", "AF Manuf.", "AF Growth"))
                      , as.character(Fuel))
    , Source_z = ifelse(commodity_naics == "Animal Feed Processing"
                        , switch(as.character(Source),
                                 "Fuel" = "AF Fuel",
                                 "Grid Electricity" = "AF Electricity",
                                 "On-site Renewables" = "AF Renewables",
                                 "Refrigerant" = "AF Refrig.")
                        , as.character(Source))
    , subStage_z = ifelse(commodity_naics == "Animal Feed Processing", as.character(Source_z),
                          ifelse(commodity_naics == "Animal Feed" & Stage_y == "Distribution", "AF Dist.", as.character(subStage)))
  ) %>% select(-feedFrac)

DIST_optimized_alt_prep <- filter(F_DIST_optimized, commodity_naics != "Animal Feed" & commodity_naics != "Animal Feed Processing") %>%
  mutate(commodity_naics_z = commodity_naics, Stage_z = as.character(Stage_y), Fuel_z = as.character(Fuel)
         , Source_z = as.character(Source), subStage_z = subStage) %>%
  rbind(F_DIST_optimizedAF) %>% ungroup()

DIST_optimized_alt_prep$commodity_naicsF_z <-addline_format(factor(DIST_optimized_alt_prep$commodity_naics_z), levels = LEVELS_naicsLine)

DIST_optimized_alt_prep <- select(DIST_optimized_alt_prep, Analysis,Stage_z, commodity_naics_z, commodity_naicsF_z, Fuel_z, Source_z, subStage_z, State_Abb, stateName, Value)
DIST_optimized_alt_prep <- DIST_optimized_alt_prep %>% dplyr::rename(Stage = Stage_z
                                                               , commodity_naics = commodity_naics_z, commodity_naicsF = commodity_naicsF_z
                                                               , Fuel = Fuel_z, Source = Source_z, subSource = subStage_z)

DIST_optimized_alt_prep$Stage <- factor( DIST_optimized_alt_prep$Stage , levels = c("On-farm", "Distribution (F-M)","Manufacturing",  "Distribution", "W&R", "Consumption"))

F_DIST_optimized_alt <- 
  DIST_optimized_alt_prep %>% mutate(
    Fuel_red = recode_factor(Fuel, "Pesticide Manuf." = "Chemical Manuf.", "Fertilizer Manuf." = "Chemical Manuf."
                             ,"AF Manuf." = "Animal Feed", "AF Growth" = "Animal Feed", "Other" = "Other Fuel")
    , sourceGroup = recode_factor(Source, "F&P Electricity" = "Grid Electricity", "F&P Fuel" = "Fuel","On-site Renewables" = "Renewables",
                                  "Truck" = "Fuel", "Rail" = "Fuel","Truck Refrig." = "Refrigerant", "Rail Refrig." = "Refrigerant",
                                  "AF Fuel" = "Fuel", "AF Renewables" = "Renewables", "AF Electricity" = "Grid Electricity", "AF Refrig." = "Refrigerant")
    , sourceLocation = recode_factor(Source, "Fuel" = "Site", "Grid Electricity" = "Site", "On-site Renewables" = "Site", 
                                     "Animal Emissions" = "Site", "Crop Emissions" = "Site","Refrigerant" = "Site",
                                     "F&P Electricity" = "Chemical Manuf.", "F&P Fuel" = "Chemical Manuf.",
                                     "Truck" = "Transport", "Rail" = "Transport","Truck Refrig." = "Transport", "Rail Refrig." = "Transport",
                                     "AF Fuel" = "Animal Feed", "AF Renewables" = "Animal Feed", "AF Electricity" = "Animal Feed", "AF Refrig." = "Animal Feed")  
    , Fuel = recode_factor(Fuel, "Other" = "Other Fuel")
    , primarySource = "1"
    , FP_yn = F
    , AF_yn = F
    , directUse_yn = F
    , electricity_yn = F
    , transport_yn = T
    , nonEnergyEmissions_yn = ifelse(Fuel == "Refrigerant", T, F)
  ) %>%
  select(
    Analysis             , Stage                 ,commodity_naics      
   , commodity_naicsF      ,Fuel                  ,primarySource        
    ,subSource             ,Source                ,State_Abb            
    ,stateName             ,FP_yn                 ,AF_yn                
    ,Value                 ,directUse_yn          ,electricity_yn       
    ,Fuel_red              ,sourceGroup           ,sourceLocation       
    ,transport_yn          ,nonEnergyEmissions_yn   
  )

remove("F_DIST_optimizedAF")


F_compareDistOpt <-
  allData_final %>% filter(sourceLocation == "Transport") %>% 
  mutate(Analysis2 = "Original") %>% 
  select(-commodity_naics_red) %>%
  rbind(F_DIST_optimized_alt %>% mutate(Analysis2 = "Optimized"))%>%
  # group_by(Analysis, Analysis2, Stage, commodity_naics,  commodity_naicsF, Fuel, Source 
  #          , subSource,  State_Abb, stateName, Fuel_red ,  sourceGroup , sourceLocation  ) %>%
  group_by(Analysis, Analysis2, Stage, Fuel_red, sourceGroup , sourceLocation ) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Analysis2, values_from = Value) %>%
  rowwise() %>% dplyr::mutate(UnoptimizedImpact = Original - Optimized) %>% 
  filter(UnoptimizedImpact>0) %>% 
  mutate(Analysis = recode_factor(Analysis, "Site Energy (TBTU)" = "Site Energy\n(TBTU)"
                                  , "Primary Energy (TBTU)" = "Primary Energy\n(TBTU)"
                                  , "GHG Emissions (MMT CO2e)" = "GHG Emissions\n(MMTCO2e)"),
         percChange = percent(UnoptimizedImpact /Original )) 





