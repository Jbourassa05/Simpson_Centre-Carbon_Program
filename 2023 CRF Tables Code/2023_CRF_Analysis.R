#-------------------------------------------------------------------------------#

# 2023 NIR DATA -Data used in "Canada’s 2023 National Inventory Submission" webinar on May 2, 2023

# Webinar link: 

# by: Joshua Bourassa

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
# Suggested Reading: 
# IPCC 2019 Refinement methodology: Animal Production: https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch10_Livestock.pdf
# IPCC 2019 Refinement methodology: Crop Production: https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch11_Soils_N2O_CO2.pdf

# System Setup ----

library(tidyverse)
library(data.table)
library(readxl)
library(ggrepel)
library(tidytext)

#-------------------------------------------------------------------------------#

# CRF Table Extraction ----

#-------------------------------------------------------------------------------#
# Data used in the analysis was downloaded from the UNFCCC National Inventory Submission site: 
# https://unfccc.int/process-and-meetings/transparency-and-reporting/reporting-and-review-under-the-convention/greenhouse-gas-inventories-annex-i-parties/national-inventory-submissions-2023
#-------------------------------------------------------------------------------#

setwd("~/Simpson Centre/NIR/2023 CRF Tables") # Will need to be reset for working directory ***

# 2023 NIR as of 04/26/2023 missing Ukraine, Country list includes EU average (EUA)

CRF.1 <-c("AUT", "BEL", "BGR", "BLR", "CAN", "CHE", "CYP", "DNK",
          "ESP", "EST", "EUA", "FIN", "GBR", "GRC","HRV", "HUN", "IRL",
          "ISL", "ITA","JPN", "KAZ", "LIE","LTU", "LUX", "LVA", "MLT",
          "NLD", "NOR","NZL", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", 
          "SWE", "TUR", "USA", "DEU", "FRA", "CZE") 

# MCO was removed due to lack of Ag emissions
                
# 2023 CRF Tables included a new reporting format used by Australia

CRF.2 <-c("AUS")

#-------------------------------------------------------------------------------#
## CRF Type 1 Extraction ----
#-------------------------------------------------------------------------------#

#Search Vectors

### Vectors used to filter CRF Tables for variables of interest

# Cattle Types includes (option A, Option B, Option C)
# Cattle used in Tables 3.A and Table 3.B

Cattle<-unique(tolower(c("Dairy cattle(4)", "Dairy cattle(3)", "Non-dairy cattle", "Mature dairy cattle", "Other mature cattle", "Growing cattle",
                         "Dairy Cattle", "Beef Cattle - Pasture", "Beef Cattle - Feedlot", "Bulls", "Calves", "Young Cattle", "Suckler Cows",
                         "Bulls under 2 years", "Dairy Cows", "dairy cows", "non-lactating cows", "bulls", "calves", "growing cattle 1-2 years", 
                         "Bulls (older than 2 years)", "Non-dairy Heifers (older than 2 years)", "Non-dairy Young Cattle (younger than 1 year)",
                         "Non-dairy Young Cattle (1-2 years)", "Dairy cows", "Other cows", "Steer Stocker", "Heifer Stocker", "Beef Cows",
                         "Dairy Replacements", "Beef Replacements", "Steer Feedlot", "Heifer Feedlot",  
                         "Dairy Cows", "Beef Calves", "Dairy Calves", "Dairy cattle(4)", "Non-dairy Cattle"))) 

# Ag Soils used in Table 3.D
                            
Ag.Soils <-c("a. Direct N2O emissions from managed soils", "1.   Inorganic N fertilizers(3)", "2.   Organic N fertilizers(3)",
            "3.   Urine and dung deposited by grazing animals", "4.   Crop residues", "5.  Mineralization/immobilization associated with loss/gain of soil organic matter (4)(5)",
            "6.   Cultivation of organic soils (i.e. histosols)(2)", "7.   Other", "b. Indirect N2O Emissions from managed soils", "1.   Atmospheric deposition(6)", "2.   Nitrogen leaching and run-off")

# Land.Use used in Table 4

Land.Use <-c("B. Total Cropland", "1. Cropland remaining cropland", "2. Land converted to cropland(10)","C. Total grassland", "1. Grassland remaining grassland", "2. Land converted to grassland(9)")


# Generating empty dataframes to start the loop

Table.3.A <- data.frame()
Table.3.B <- data.frame()
Table.3.D <- data.frame()
Table.4 <- data.frame()
Table.3.Cattle<-data.frame()
Table.3 <- data.frame()
National.Emissions <-data.frame()


# Single loop to extract data and create tables for countries in CRF.1 and years 1990-2021

# if run from start to finish will take a couple hours... Sorry to code is not super efficient

for (i in CRF.1){
  File <-paste(i,"_2023_", 1990:2021, ".xlsx", sep = "")
  for (i in File) {

#-------------------------------------------------------------------------------#
# Enteric Fermentation: Cattle
#-------------------------------------------------------------------------------#

# Data collected from headline figures
    
    x<-read_excel(i, sheet = "Table3.As1")|>
      select(1,2,3,4,5,6)|>
      rename(`Cattle Type` = 1, 
             Population = 2, # Population size Measured in 1000's
             GE = 3, # Average gross energy intake (MJ/head/day)
             Ym = 4, # Average CH4 Conversion Rate (%)
             IEF = 5, # Implied emission factor (kg CH4/head/year)
             CH4.kt = 6)|> # Emissions kt CH4
      mutate(`Cattle Type` = tolower(`Cattle Type`),
             `Cattle Type` = as.character(`Cattle Type`))|>
      filter(`Cattle Type` %in% Cattle)|>
      mutate(file = i)|>
      separate(file, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Drop = ifelse(is.na(Population)& is.na(CH4.kt), T, F))|>
      filter(Drop != T)|>
      select(7,8,1:6)|>
      mutate(Type.A = ifelse(`Cattle Type` %in% c("dairy cattle(3)", "mature dairy cattle", "dairy cattle", "dairy cows"),
                             "Dairy Cattle", "Non-Dairy Cattle"),
             `Cattle Type` = ifelse(`Cattle Type` == "dairy cattle(3)", "dairy cattle", `Cattle Type`),
             Year = as.integer(Year))|>
      group_by(Country, Year, Type.A)|> # Estimating weights to calculate weighted average... Find outside loop
      mutate(Population = as.numeric(Population), 
             Total.Pop = sum(Population),
             Share.Pop = Population/Total.Pop)
    
    Names<-unique(x$`Cattle Type`) # used to identify cattle types kept in analysis
      
# Collecting Background Data form Tier 2 and 3 Calculations (Table 3.A.S2)
    
    y<-read_excel(i, sheet = "Table3.As2")
    y<-data.table(t(y))
    row.names(y)<-NULL
    
    y<-y|>
      select(5:12)|>
      rename(`Cattle Type` = 1,
             Weight = 2, # Average weight (kg/head)
             `Feeding Situation` = 3, # Pasture, Confined etc.
             `Milk Yield`= 4, # (Kg Milk/day)
             Work = 5, # (hours/day)
             Pregnant = 6, # (%) 
             DE = 7, # Digestibility of feed (%)
             GE = 8)|> # Gross energy (MJ/day)
      select(1,2,4,7)|>
      mutate(`Cattle Type` = tolower(`Cattle Type`))|>
      filter(`Cattle Type` %in% Names,
             !is.na(Weight) & !is.na(`Milk Yield`) & !is.na (DE))
    
    
    join<-full_join(x,y) # joining two tables
    
    Table.3.A<-rbind(Table.3.A,join) #Table 3.A Enteric Fermentation for cattle production combining both Table 3.As1 and Table 3.As2
    
#-------------------------------------------------------------------------------#
# Table 3b Manure Management: Cattle
#-------------------------------------------------------------------------------#
# CH4 Emissions
#-------------------------------------------------------------------------------#    
    Names<-unique(append(Names,c("dairy cattle", "non-dairy cattle"))) # names needed to be modified as multiple countries switched Option C to Option A classification for manure management.
    
    x<-read_excel(i,sheet = "Table3.B(a)s1")|>
      rename(`Cattle Type` = 1, 
             Population = 2, # Population size 1000's
             Cool.Temp = 3, # Allocation by climate region (%)
             Temperate.Temp = 4, # Allocation by climate region (%)
             Warm.Temp = 5, # Allocation by climate region (%)
             kg = 6, # Typical Animal Mass (kg)
             VS = 7, # Volatile Solids, Average Daily Production (kg dry matrer/head/day)
             B0 = 8, # maximum methane producing capacity for manure (m^3 CH4/ kg VS) (pp. 10.42 and 10.43 of chapter 10, volume 4 of the 2006 IPCC Guidelines)
             IEF = 9, # Implied Emission Factor (kg CH4/head/year)
             CH4.kt = 10)|> # Emissions kt CH4
      select(-3,-4,-5)|>
      mutate(`Cattle Type` = tolower(`Cattle Type`),
             `Cattle Type` = as.character(`Cattle Type`),
             `Cattle Type` = ifelse(`Cattle Type` == "dairy cattle(3)"| `Cattle Type` == "dairy cattle(4)","dairy cattle",`Cattle Type`),
             Type.A = ifelse(`Cattle Type` %in% c("dairy cattle", "mature dairy cattle", "dairy cows"),"Dairy Cattle","Non-Dairy Cattle"),
             file = i)|>
             filter(`Cattle Type` %in% Names)

## Note Climate regions are defined in terms of annual average temperature as follows: cool = less than 15 °C; temperate = 15–25 °C inclusive; and  warm = greater than 25 °C (see table 10.17 of chapter 10, volume 4 of the 2006 IPCC Guidelines).

#-------------------------------------------------------------------------------#
# N2O Emissions from manure management Cattle
#-------------------------------------------------------------------------------#
   
     y<-read_excel(i,sheet = "Table3.B(b)", col_types = c("text", "numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                                         "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric",
                                                         "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric"))|>
      rename( `Cattle Type` = 1, 
              Population = 2, # Population size 1000's
              `Nitrogen excretion rate` = 3, # kg N/head/year
              `Typical animal mass` = 4, # kg/Animal
              #-----------------------------------------------------------------#
              #Nitrogen excretion per manure management system (MMS)  (kg N/yr)
              Anarobic.Lagoon = 5, 
              Liquid.System = 6, 
              Daily.Spread = 7, 
              Solid.Storage = 8, 
              Pasture = 9, 
              Composting = 10, 
              Digester = 11, 
              Burned = 12, 
              Other = 13, 
              #-----------------------------------------------------------------#
              Total.N = 14, # Total Nitrogen excreted (kg/yr)
              Total.N.Vol = 15, # Total N volatilized as NH3 and NOx (kgN/yr)
              Total.N.Leaching = 16, # Total N Lost through leaching and run-off (kg N/yr)
              Direct.IEF = 17, # Implied emission factor, Direct N2O (kg N2O-N/year) *** N2O-N * (48/22) = N2O
              Indirect.IEF.Atmos =18, # Implied emission factor, indirect emissions 
              Indirect.IEF.Leaching = 19, # Implied emission factor, indirect emissions 
              Direct.N2O = 20, # Direct emissions (kt N2O)
              Indirect.N2O.Atmos = 21, # indirect emissions (kt N2O)
              Indirect.N2O.Leaching = 22)|> # indirect emissions (kt N2O)
      drop_na(Population)|>
      mutate(`Cattle Type` = tolower(`Cattle Type`),
             `Cattle Type` = as.character(`Cattle Type`),
             file = i,
             `Cattle Type` = ifelse(`Cattle Type` == "dairy cattle(4)","dairy cattle",`Cattle Type`))|>
      filter(`Cattle Type` %in% Names)|>
      select(-Population)
    
    join<-full_join(x,y)|>
      separate(file, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Year = as.integer(Year))
    
### indirect N2O emissions from N volatilisation and leaching/runoff
  ### disagregated values are not provided in the CRF Tables.
  ### to estimate values first total N excreted is calculated and then divided by N2O.Vol and by N2O.Runoff
  ### Values will be later calculated by multiplying (Total N - PRP N) by the implied factors for N2O.Vol and by N2O.Runoff. 
    
    
    z<-read_excel(i,sheet = "Table3.B(b)")|>
      select(1,9,14,21,22)|>
      rename(Livestock = 1,
             Pasture = 2, 
             Total.N = 3,
             N2O.Vol = 4,
             N2O.Runoff = 5)|>
      filter(Livestock %in% c("1.  Cattle", "2.  Sheep", "3.  Swine", "4.    Other livestock(6)", "5. Indirect N2O emission"))|>
      mutate(across(c("Pasture","Total.N", "N2O.Vol","Pasture","N2O.Runoff"), as.numeric),
             Pasture = ifelse(is.na(Pasture), 0, Pasture),
             Total.N = ifelse(is.na(Total.N), 0, Total.N),
             N2O.Vol = ifelse(is.na(N2O.Vol), 0, N2O.Vol),
             N2O.Runoff = ifelse(is.na(N2O.Runoff), 0, N2O.Runoff),
             N.MM = Total.N - Pasture)|>
      summarise(N.MM = sum(N.MM),
                N2O.Vol = sum(N2O.Vol),
                N2O.Runoff  = sum(N2O.Runoff))|>
      mutate(N2O.Vol.IEF = (N2O.Vol*10^6)/N.MM,
             N2O.Runoff.IEF = (N2O.Runoff*10^6)/N.MM)|>
      select(N2O.Vol.IEF,N2O.Runoff.IEF)
    
    join<-cross_join(join,z)
    
 
    Table.3.B <-rbind(Table.3.B ,join) # Combination of Table 3.B(a)s1 and Table 3.B(b) 
    
    
#-------------------------------------------------------------------------------#
# Table 3.D Agricultural Soils 
#-------------------------------------------------------------------------------#

    Ag.Soils<-c("a. Direct N2O emissions from managed soils", "1.   Inorganic N fertilizers(3)", "2.   Organic N fertilizers(3)",
      "3.   Urine and dung deposited by grazing animals", "4.   Crop residues", "5.  Mineralization/immobilization associated with loss/gain of soil organic matter (4)(5)",
      "6.   Cultivation of organic soils (i.e. histosols)(2)", "7.   Other", "b. Indirect N2O Emissions from managed soils", "1.   Atmospheric deposition(6)", "2.   Nitrogen leaching and run-off")
    

    x<-read_excel(i,sheet = "Table3.D")|>
      rename(Emission.Source = 1, 
             Description = 2, 
             Application = 3, # kg of nitrogen or ha of organic soil cultivated
             IEF = 4, # implied emission factor (kg N2O-N/kg N) *** N2O-N * (48/22) = N2O
             Emissions.kt = 5)|> #total emissions kt N2O
      filter(Emission.Source %in% Ag.Soils)|>
      mutate(File = i)|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Emission.Source = recode(Emission.Source ,
                                      "a. Direct N2O emissions from managed soils" = "Direct N2O emissions", 
                                      "1.   Inorganic N fertilizers(3)" = "Inorganic N emissions"  ,
                                      "2.   Organic N fertilizers(3)" = "Organic N emissions",
                                      "3.   Urine and dung deposited by grazing animals" = "N from grazing animals", # classified as emissions from animal production under economic sector approach
                                      "4.   Crop residues"= "Crop residues",
                                      "5.  Mineralization/immobilization associated with loss/gain of soil organic matter (4)(5)" = "Mineralization/immobilization",
                                      "6.   Cultivation of organic soils (i.e. histosols)(2)" = "Organic Soils",
                                      "7.   Other" = "Other",
                                      "b. Indirect N2O Emissions from managed soils" = "Indirect N2O emissions",
                                      "1.   Atmospheric deposition(6)" = "Atmospheric deposition",
                                      "2.   Nitrogen leaching and run-off" = "leaching and run-off"),
             Application = as.numeric(Application),
             IEF = as.numeric(IEF),
             Emissions.kt = as.numeric(Emissions.kt))|>
      select(6,7,1:5)
    
#-------------------------------------------------------------------------------#
# Indirect Emission fractions
#-------------------------------------------------------------------------------#   
   
     y<-read_excel(i,sheet = "Table3.D")|>
      rename(Fraction = 1,
             F.Description  = 2,
             Value = 3)|>
      select(-2,-4,-5)|>
      filter(Fraction %in% c("FracGASF", "FracGASM", "FracLEACH-(H)"))|>
      spread(Fraction, Value)
    
    join<-cross_join(x,y)|>
      mutate(Emission = "N2O")
    
# Description:
  # FracGASF: Fraction of synthetic fertilizer N applied to soils that volatilises as NH3 and NOX
  # FracGASM:  Fraction of livestock N excretion that volatilises as NH3 and NOX
  # FracLEACH-(H):  of N input to managed soils that is lost through leaching and run-off

    
#-------------------------------------------------------------------------------#
#Table 3.H + Table 3.I  CO2 emissions from Urea and other Carbon Containing Fertilizers (UAN) 
#-------------------------------------------------------------------------------#
    
    y<-read_excel(i,sheet = "Table3.G-I")|>
      rename(Emission.Source = 1,
             Application = 2, # Urea or Other Carbon Containing Fertilizer Application (t/yr)
             IEF = 3, # Implied Emission Factor, Tonnes CO2/t applied (t CO2–C/t)
             Emissions.kt = 4)|> # Emissions kt CO2
      mutate(Application = as.numeric(Application),
             IEF = as.numeric(IEF),
             Emissions.kt = as.numeric(Emissions.kt),
             Description = NA,
             FracGASF = NA,
             FracGASM = NA,
             `FracLEACH-(H)` = NA)|>
      filter(Emission.Source %in% c("H.  Urea application", "I.  Other carbon-containing fertlizers"))|>
      mutate(Emission.Source = recode(Emission.Source, 
                                      "H.  Urea application" = "Urea",
                                      "I.  Other carbon-containing fertlizers" = "Other carbon-containing fertlizers"),
             Emission = "CO2",
             File = i)|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)
    
    
    
    Table.3.D <- rbind(Table.3.D, join, y) # Table 3.D and Table 3. G-I  including additional information from fraction lost to NH3 and L&R    

#-------------------------------------------------------------------------------# 
 
# Table 4.B + Table 4.C Agricultural Land Use (cropland + grassland)
    
#-------------------------------------------------------------------------------#
# Cropland: Table4.B
#-------------------------------------------------------------------------------#
    
    x <- read_excel(i,sheet = "Table4.B") 
    
    x<-x|>
      select(1, 3, 18)|>
      rename(`Land Use` = 1, 
             Total.Area.kha = 2, # Total Area measured in (kha)
             Emissions.kt= 3)|> #Total CO2 emissions and removals (kt), negitive values equal carbon removals
      filter(`Land Use` %in% Land.Use)|>
      mutate(`Land Use` = recode(`Land Use`, 
                               "B. Total Cropland" = "Total Cropland", # total area and total emissions
                               "1. Cropland remaining cropland" = "Remaining Cropland", # total area and emissions for land use remaining unchanged
                               "2. Land converted to cropland(10)" = "Conversion to Cropland"), # Total area and emissions from land use conversion
             Total.Area.kha = as.numeric(Total.Area.kha),
             Emissions.kt = as.numeric(Emissions.kt ),
             t.ha = Emissions.kt/Total.Area.kha, #Emissions per hectare (Negitive values indicate net carbon sink) measured in t CO2/ha
             File = i )|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      select(5,6,1,2,3,4)

#-------------------------------------------------------------------------------#
# Grasslands: Table4.C
#-------------------------------------------------------------------------------#    
       
    y <- read_excel(i,sheet = "Table4.C")
    
    y<-y|>
      select(1, 3, 18)|>
      rename(`Land Use` = 1,
             Total.Area.kha = 2, # Total Area measured in (kha)
             Emissions.kt= 3)|> #Total CO2 emissions and removals (kt), negitive values equal carbon removals
      filter(`Land Use` %in% Land.Use)|>
      mutate(`Land Use` = recode(`Land Use`, 
                               "C. Total grassland" = "Total Grassland", # total area and total emissions
                               "1. Grassland remaining grassland" = "Remaining Grassland", # total area and emissions for land use remaining unchanged
                               "2. Land converted to grassland(9)" = "Conversion to Grassland "), # Total area and emissions from land use conversion
             Total.Area.kha = as.numeric(Total.Area.kha),
             Emissions.kt = as.numeric(Emissions.kt ),
             t.ha = Emissions.kt/Total.Area.kha, #Emissions per hectare (Negitive values indicate net carbon sink) measured in t CO2/ha
             File = i )|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      select(5,6,1,2,3,4)
    
    Table.4  <- rbind(Table.4, x, y) # Table combining both Table 4.B and Table 4.C
    

#-------------------------------------------------------------------------------#
    
# Table 3: Agriculture Summary
    
#-------------------------------------------------------------------------------#
# Table 3s1: Livestock emissions from enteric fermentation and Manure management  
#-------------------------------------------------------------------------------#
    x <- read_excel(i, sheet = "Table3s1", col_types = c("text","numeric", "numeric",
                                                         "numeric","text", "text", "text"))|>
      select(1,2,3,4)|>
      rename(Category = 1,
             CO2 = 2, #measured in kt
             CH4 = 3, #measured in kt
             N2O = 4)|> #measured in kt
      filter(Category %in% c("3. Total agriculture",
                             "A. Enteric fermentation",
                             "B.  Manure management"))|>
      mutate(File = i)
#-------------------------------------------------------------------------------#
# Table 3s2:  Emissions from other Ag Sources
#-------------------------------------------------------------------------------#
    y <- read_excel(i, sheet = "Table3s2",col_types = c("text","numeric", "numeric", "numeric", 
                                                        "text", "text", "text"))|>
      select(1,2,3,4)|>
      rename(Category = 1,
             CO2 = 2,
             CH4 = 3, 
             N2O = 4)|>
      filter(Category %in% c("C.  Rice cultivation",
                             "D.  Agricultural soils(2) (3) (4)",
                             "E.  Prescribed burning of savannas",
                             "F.  Field burning of agricultural residues",
                             "G.  Liming",
                             "H.  Urea application",
                             "I.  Other carbon-containing fertilizers",
                             "J.  Other (please specify)"))%>%
      mutate(File = i)
    
    join<-rbind(x,y)|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Year = as.integer(Year),
             CO2 = ifelse(is.na(CO2),0,CO2), #measured in kt
             CH4 = ifelse(is.na(CH4),0,CH4), #measured in kt
             N2O = ifelse(is.na(N2O),0,N2O), #measured in kt
             CO2eq.kt = CO2+(CH4*25)+(N2O*298), #measured in kt *** 100 GWP used, CH4 = 25, N2O = 298
             Category = recode(Category,
                              "3. Total agriculture" = "Total Agriculture",
                              "A. Enteric fermentation" = "Enteric Fermentation",
                              "B.  Manure management" = "Manure Management", 
                              "C.  Rice cultivation" = "Rice cultivation", 
                              "D.  Agricultural soils(2) (3) (4)" = "Agricultural soils",
                              "E.  Prescribed burning of savannas" = "Prescribed Burning of Savannas",
                              "F.  Field burning of agricultural residues" = "Field Burning of Agricultural Residues",
                              "G.  Liming" = "Liming",
                              "H.  Urea application" = "Urea Application",
                              "I.  Other carbon-containing fertilizers" = "Other Carbon-containing Fertilizers",
                              "J.  Other (please specify)" = "Other Sources"))|>
               select(5,6,1,2,3,4,7)
   
    Table.3 <-rbind(Table.3, join)
    
#-------------------------------------------------------------------------------#
# Cattle Specific Summary   
#-------------------------------------------------------------------------------# 
# Table 3s1: Cattle emissions from enteric fermentation and Manure management  
#-------------------------------------------------------------------------------#    
    Names.2<-tolower(append(Names, c("I. Livestock", "A. Enteric fermentation", "1.   Cattle(1)", "B.  Manure management", "1.    Cattle(1)", "5. Indirect N2O emissions")))
    
    x <- read_excel(i, sheet = "Table3s1", col_types = c("text","numeric", "numeric",
                                                         "numeric","text", "text", "text"))|>
      select(1,2,3,4)|>
      rename(Category = 1,
             CO2 = 2,
             CH4 = 3, 
             N2O = 4)|>
      mutate(Category = tolower(Category))|>
      filter(Category %in% Names.2)|>
      mutate(File = i)|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Year = as.integer(Year),
             CO2 = ifelse(is.na(CO2),0,CO2), #measured in kt
             CH4 = ifelse(is.na(CH4),0,CH4), #measured in kt
             N2O = ifelse(is.na(N2O),0,N2O), #measured in kt
             CO2eq.kt = CO2+(CH4*25)+(N2O*298), #measured in kt *** 100 GWP used, CH4 = 25, N2O = 298
             Category = ifelse(Category == "i. livestock", "total livestock",
                               ifelse(Category == "a. enteric fermentation", "enteric fermentation",
                                      ifelse(Category =="1.   cattle(1)", "cattle subtotal",
                                             ifelse(Category == "b.  manure management", "manure management",
                                                    ifelse(Category == "1.    cattle(1)", "cattle subtotal",
                                                           ifelse(Category == "5. indirect n2o emissions","indirect n2o emissions", Category)))))),
             EM.Source = ifelse(Category %in% c("total livestock", "enteric fermentation", "manure management"), "total",
                                ifelse(N2O == 0, "enteric fermentation", "manure management")))
    
    Table.3.Cattle<-rbind(Table.3.Cattle, x)
    
#-------------------------------------------------------------------------------#
# National Emissions Summary  
#-------------------------------------------------------------------------------# 
# Table Summary 2, values shown in kt CO2 eq for each source
    
    x<-read_excel(i,sheet = "Summary2", col_types = c("text","text", "text", "text", "text", "text", 
                                                      "text", "text", "text", "numeric"))|>
      select(1,10)|>
      rename(File = 1,
             CO2eq = 2)|> # Measured in kt CO2 eq
      filter(File == "Total (net emissions)(1)")|>
      mutate(File = i,
             CO2eq = round((CO2eq/1000),2))|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      mutate(Year = as.numeric(Year))
    
    National.Emissions<-rbind(National.Emissions, x)
    
  }}
    
# Saving Tables 

fwrite(National.Emissions, file = "~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")
fwrite(Table.3, file = "~/Simpson Centre/NIR/02-Data/Table_3_2023.csv")
fwrite(Table.3.Cattle, file = "~/Simpson Centre/NIR/02-Data/Table_3_Cattle_2023.csv")
fwrite(Table.3.A, file = "~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")
fwrite(Table.3.B, file = "~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")
fwrite(Table.3.D, file = "~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
fwrite(Table.4, file = "~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")

#-------------------------------------------------------------------------------#

## CRF Type 2 Loop ----

#-------------------------------------------------------------------------------# 

CRF.2 <-c("AUS")

Table.3.A.2 <- data.frame()
Table.3.B.2 <- data.frame()
Table.3.D.2 <- data.frame()
Table.4.2 <- data.frame()
National.Emissions.2 <-data.frame()

#-------------------------------------------------------------------------------#
# Enteric Fermentation=      
#-------------------------------------------------------------------------------# 
for (i in CRF.2){
  File <-paste(i,"_2023_", 1990:2021, ".xlsx", sep = "")
  for (i in File) {
    
    x<-read_excel(i, sheet = "Table3.A")
    y<-x
    
    x<-x|>
      select(1:6)|>
      rename(`Cattle Type` = 1,
             Population = 2, 
             GE = 3,
             Ym = 4,
             IEF = 5,
             CH4.kt = 6)|>
      filter(`Cattle Type` %in% c("Dairy Cattle", "Beef Cattle - Pasture", "Beef Cattle - Feedlot"))|> #AUS Categories
      mutate(`Cattle Type` = tolower(`Cattle Type`),
             Type.A = ifelse(`Cattle Type` == "dairy cattle", "Dairy Cattle", "Non-Dairy Cattle"),
             File = i)|>
      separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
      select(-`NIR Year`, -Ext)|>
      group_by(Country, Year, Type.A)|> # Estimating weights to calculate weighted average... Find outside loop
      mutate(Population = as.numeric(Population), 
             Total.Pop = sum(Population),
             Share.Pop = Population/Total.Pop)
    
    y<-y|>select(-1:-7)
    y<-data.table(t(y))
    row.names(y)<-NULL

    y<-y|>
      select(6, 9:14)|>
      rename(`Cattle Type` = 1,
             Weight = 2, # Average weight (kg/head)
             `Feeding Situation` = 3, # Pasture, Confined etc.
             `Milk Yield`= 4, # (Kg Milk/day)
             Work = 5, # (hours/day)
             Pregnant = 6, # (%) 
             DE = 7)|> # Digestibility of feed (%)
      select(1,2,4,7)|>
      mutate(`Cattle Type` = tolower(`Cattle Type`))|>
      filter(`Cattle Type` %in% c("dairy cattle", "beef cattle - pasture", "beef cattle - feedlot"))
      
      
      join<-full_join(x,y)
      
      Table.3.A.2<-rbind(Table.3.A.2, join)
      
#-------------------------------------------------------------------------------#      
# Manure Management CH4    
#-------------------------------------------------------------------------------#      
      
      x<-read_excel(i,sheet = "Table3.B(a)")|>
        select(1:10)|>
        rename(`Cattle Type` = 1, 
               Population = 2, # Population size 1000's
               Cool.Temp = 3, # Allocation by climate region (%)
               Temperate.Temp = 4, # Allocation by climate region (%)
               Warm.Temp = 5, # Allocation by climate region (%)
               kg = 6, # Typical Animal Mass (kg)
               VS = 7, # Volatile Solids, Average Daily Production (kg dry matrer/head/day)
               B0 = 8, # maximum methane producing capacity for manure (m^3 CH4/ kg VS) (pp. 10.42 and 10.43 of chapter 10, volume 4 of the 2006 IPCC Guidelines)
               IEF = 9, # Implied Emission Factor (kg CH4/head/year)
               CH4.kt = 10)|> # Emissions kt CH4
        select(-3,-4,-5)|>
        mutate(`Cattle Type` = tolower(`Cattle Type`),
               `Cattle Type` = as.character(`Cattle Type`),
               `Cattle Type` = ifelse(`Cattle Type` == "dairy cattle(3)"| `Cattle Type` == "dairy cattle(4)","dairy cattle",`Cattle Type`),
               Type.A = ifelse(`Cattle Type` %in% c("dairy cattle", "mature dairy cattle", "dairy cows"),"Dairy Cattle","Non-Dairy Cattle"),
               file = i)|>
        filter(`Cattle Type` %in% c("dairy cattle", "beef cattle - pasture", "beef cattle - feedlot"))
      
#-------------------------------------------------------------------------------#      
# Manure Management N2O    
#-------------------------------------------------------------------------------#      

      y<-read_excel(i,sheet = "Table3.B(b)")
        
      z<-y
      
      y<-y|>
        rename( `Cattle Type` = 1, 
                Population = 2, # Population size 1000's
                `Nitrogen excretion rate` = 3, # kg N/head/year
                `Typical animal mass` = 4, # kg/Animal
                #-----------------------------------------------------------------#
                #Nitrogen excretion per manure management system (MMS)  (kg N/yr)
                Anarobic.Lagoon = 5, 
                Liquid.System = 6, 
                Daily.Spread = 7, 
                Solid.Storage = 8, 
                Pit.Storage = 9,
                Dry.Lot = 10, 
                Deep.Bedding = 11,
                Pasture = 12, 
                Composting = 13, 
                Digester = 14, 
                Burned = 15, 
                Other = 16, 
                #-----------------------------------------------------------------#
                Total.N = 17, # Total Nitrogen excreted (kg/yr)
                Total.N.Vol = 18, # Total N volatilized as NH3 and NOx (kgN/yr)
                Total.N.Leaching = 19, # Total N Lost through leaching and run-off (kg N/yr)
                Direct.IEF = 20, # Implied emission factor, Direct N2O (kg N2O-N/year) *** N2O-N * (48/22) = N2O
                Indirect.IEF.Atmos =21, # Implied emission factor, indirect emissions 
                Indirect.IEF.Leaching = 22, # Implied emission factor, indirect emissions 
                Direct.N2O = 23, # Direct emissions (kt N2O)
                Indirect.N2O.Atmos = 24, # indirect emissions (kt N2O)
                Indirect.N2O.Leaching = 25)|> # indirect emissions (kt N2O)
        drop_na(Population)|>
        mutate(`Cattle Type` = tolower(`Cattle Type`),
               `Cattle Type` = as.character(`Cattle Type`),
               file = i)|>
        filter(`Cattle Type` %in% c("dairy cattle", "beef cattle - pasture", "beef cattle - feedlot"))|>
        select(-Population)|>
        mutate(across(c("Nitrogen excretion rate":"Indirect.N2O.Leaching"), as.numeric))
      
      join<-full_join(x,y)|>
        separate(file, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)|>
        mutate(Year = as.integer(Year))
        
      ### indirect N2O emissions from N volatilisation and leaching/runoff
      ### disagregated values are not provided in the CRF Tables.
      ### to estimate values first total N excreted is calculated and then divided by N2O.Vol and by N2O.Runoff
      ### Values will be later calculated by multiplying (Total N - PRP N) by the implied factors for N2O.Vol and by N2O.Runoff. 
      
      
      z<-select(z,1,12,17,24,25)|>
        rename(Livestock = 1,
               Pasture = 2, 
               Total.N = 3,
               N2O.Vol = 4,
               N2O.Runoff = 5)|>
        filter(Livestock %in% c("3.B.1. Cattle", "3.B.2. Sheep", "3.B.3. Swine", "3.B.4.    Other livestock(6)", "3.B.5. Indirect N2O emissions"))|>
        mutate(across(c("Pasture","Total.N", "N2O.Vol","Pasture","N2O.Runoff"), as.numeric),
               Pasture = ifelse(is.na(Pasture), 0, Pasture),
               Total.N = ifelse(is.na(Total.N), 0, Total.N),
               N2O.Vol = ifelse(is.na(N2O.Vol), 0, N2O.Vol),
               N2O.Runoff = ifelse(is.na(N2O.Runoff), 0, N2O.Runoff),
               N.MM = Total.N - Pasture)|>
        summarise(N.MM = sum(N.MM),
                  N2O.Vol = sum(N2O.Vol),
                  N2O.Runoff  = sum(N2O.Runoff))|>
        mutate(N2O.Vol.IEF = (N2O.Vol*10^6)/N.MM,
               N2O.Runoff.IEF = (N2O.Runoff*10^6)/N.MM)|>
        select(N2O.Vol.IEF,N2O.Runoff.IEF)
      
      join<-cross_join(join,z)
      
      
      Table.3.B.2 <-rbind(Table.3.B.2 ,join)

#-------------------------------------------------------------------------------#
# Table 3.D Agricultural Soils 
#-------------------------------------------------------------------------------#
      
      x<-read_excel(i,sheet = "Table3.D")
      y<-x
      
      x<-select(x,1:5)|>
        rename(Emission.Source = 1, 
               Description = 2, 
               Application = 3, # kg of nitrogen or ha of organic soil cultivated
               IEF = 4, # implied emission factor (kg N2O-N/kg N) *** N2O-N * (48/22) = N2O
               Emissions.kt = 5)|> #total emissions kt N2O
        filter(Emission.Source %in% c("3.D.1. Direct N2O emissions from managed soils",
                                      "3.D.1.a.  Inorganic N fertilizers (3)",
                                      "3.D.1.b.   Organic N fertilizers (3)" ,
                                      "3.D.1.b.i. Animal manure applied to soils",
                                      "3.D.1.c.   Urine and dung deposited by grazing animals",
                                      "3.D.1.d.   Crop residues",
                                      "3.D.1.e.   Mineralization/immobilization associated with loss/gain of soil organic matter (4,5)",
                                      "3.D.1.f.   Cultivation of organic soils (i.e. histosols) (2)",
                                      "3.D.1.g.   Other",
                                      "3.D.2. Indirect N2O Emissions from managed soils",
                                      "3.D.2.a.   Atmospheric deposition (6)",
                                      "3.D.2.b.   Nitrogen leaching and run-off"))|>
        mutate(File = i)|>
        separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)|>
        mutate(Emission.Source = recode(Emission.Source ,
                                        "3.D.1. Direct N2O emissions from managed soils" = "Direct N2O emissions", 
                                        "3.D.1.a.  Inorganic N fertilizers (3)" = "Inorganic N emissions"  ,
                                        "3.D.1.b.   Organic N fertilizers (3)" = "Organic N emissions",
                                        "3.D.1.c.   Urine and dung deposited by grazing animals" = "N from grazing animals", # classified as emissions from animal production under economic sector approach
                                        "3.D.1.d.   Crop residues"= "Crop residues",
                                        "3.D.1.e.   Mineralization/immobilization associated with loss/gain of soil organic matter (4,5)" = "Mineralization/immobilization",
                                        "3.D.1.f.   Cultivation of organic soils (i.e. histosols) (2)" = "Organic Soils",
                                        "3.D.1.g.   Other" = "Other",
                                        "3.D.2. Indirect N2O Emissions from managed soils" = "Indirect N2O emissions",
                                        "3.D.2.a.   Atmospheric deposition (6)" = "Atmospheric deposition",
                                        "3.D.2.b.   Nitrogen leaching and run-off" = "leaching and run-off"),
               Application = as.numeric(Application),
               IEF = as.numeric(IEF),
               Emissions.kt = as.numeric(Emissions.kt))|>
        select(6,7,1:5)
      
#-------------------------------------------------------------------------------#
# Indirect Emission fractions
#-------------------------------------------------------------------------------#   
      
      y<-select(y,7,8,9)|>
        rename(Fraction = 1,
               F.Description  = 2,
               Value = 3)|>
        filter(Fraction %in% c("FracGASF", "FracGASM", "FracLEACH-(H)"))|>
        select(-F.Description)|>
        spread(Fraction, Value)
      
      join<-cross_join(x,y)|>
        mutate(Emission = "N2O")
      
      # Description:
      # FracGASF: Fraction of synthetic fertilizer N applied to soils that volatilises as NH3 and NOX
      # FracGASM:  Fraction of livestock N excretion that volatilises as NH3 and NOX
      # FracLEACH-(H):  of N input to managed soils that is lost through leaching and run-off
      
      
#-------------------------------------------------------------------------------#
#Table 3.H + Table 3.J  CO2 emissions from Urea and other Carbon Containing Fertilizers (UAN) 
#-------------------------------------------------------------------------------#
      
      y<-read_excel(i,sheet = "Table3.G-J")|>
        rename(Emission.Source = 1,
               Application = 2, # Urea or Other Carbon Containing Fertilizer Application (t/yr)
               IEF = 3, # Implied Emission Factor, Tonnes CO2/t applied (t CO2–C/t)
               Emissions.kt = 4)|> # Emissions kt CO2
        mutate(Application = as.numeric(Application),
               IEF = as.numeric(IEF),
               Emissions.kt = as.numeric(Emissions.kt),
               Description = NA,
               FracGASF = NA,
               FracGASM = NA,
               `FracLEACH-(H)` = NA)|>
        filter(Emission.Source %in% c("3.H.  Urea application","3.I.   Other carbon-containing fertilizers"))|>
        mutate(Emission.Source = recode(Emission.Source, 
                                        "3.H.  Urea application" = "Urea",
                                        "3.I.   Other carbon-containing fertilizers" = "Other carbon-containing fertlizers"),
               Emission = "CO2",
               File = i)|>
        separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)
      
      
      
      Table.3.D.2 <- rbind(Table.3.D.2, join, y) # Table 3.D and Table 3. G-I  including additional information from fraction lost to NH3 and L&R    
      
#-------------------------------------------------------------------------------# 
      
# Table 4.B + Table 4.C Agricultural Land Use (cropland + grassland)
      
#-------------------------------------------------------------------------------#
# Cropland: Table4.B
#-------------------------------------------------------------------------------#
      
      x <- read_excel(i,sheet = "Table4.B") 
      
      x<-x|>
        select(1, 3, 18)|>
        rename(`Land Use` = 1, 
               Total.Area.kha = 2, # Total Area measured in (kha)
               Emissions.kt= 3)|> #Total CO2 emissions and removals (kt), negitive values equal carbon removals
        filter(`Land Use` %in% c("4.B. Total cropland", "4.B.1. Cropland remaining cropland", "4.B.2. Land converted to cropland (12)"))|>
        mutate(`Land Use` = recode(`Land Use`, 
                                   "4.B. Total cropland" = "Total Cropland", # total area and total emissions
                                   "4.B.1. Cropland remaining cropland" = "Remaining Cropland", # total area and emissions for land use remaining unchanged
                                   "4.B.2. Land converted to cropland (12)" = "Conversion to Cropland"), # Total area and emissions from land use conversion
               Total.Area.kha = as.numeric(Total.Area.kha),
               Emissions.kt = as.numeric(Emissions.kt ),
               t.ha = Emissions.kt/Total.Area.kha, #Emissions per hectare (Negitive values indicate net carbon sink) measured in t CO2/ha
               File = i )|>
        separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)|>
        select(5,6,1,2,3,4)
      
#-------------------------------------------------------------------------------#
# Grasslands: Table4.C
#-------------------------------------------------------------------------------#    
      
      y <- read_excel(i,sheet = "Table4.C")
      
      y<-y|>
        select(1, 3, 18)|>
        rename(`Land Use` = 1,
               Total.Area.kha = 2, # Total Area measured in (kha)
               Emissions.kt= 3)|> #Total CO2 emissions and removals (kt), negitive values equal carbon removals
        filter(`Land Use` %in% c("4.C. Total grassland", "4.C.1. Grassland remaining grassland", "4.C.2. Land converted to grassland (11)"))|>
        mutate(`Land Use` = recode(`Land Use`, 
                                   "4.C. Total grassland" = "Total Grassland", # total area and total emissions
                                   "4.C.1. Grassland remaining grassland" = "Remaining Grassland", # total area and emissions for land use remaining unchanged
                                   "4.C.2. Land converted to grassland (11)" = "Conversion to Grassland "), # Total area and emissions from land use conversion
               Total.Area.kha = as.numeric(Total.Area.kha),
               Emissions.kt = as.numeric(Emissions.kt ),
               t.ha = Emissions.kt/Total.Area.kha, #Emissions per hectare (Negitive values indicate net carbon sink) measured in t CO2/ha
               File = i )|>
        separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)|>
        select(5,6,1,2,3,4)
      
      Table.4.2  <- rbind(Table.4.2, x, y) # Table combining both Table 4.B and Table 4.C
      
#-------------------------------------------------------------------------------#
# National Emissions Summary  
#-------------------------------------------------------------------------------# 
      # Table Summary 2, values shown in kt CO2 eq for each source
      
      x<-read_excel(i,sheet = "Summary2", col_types = c("text","text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "numeric"))|>
        select(1,10)|>
        rename(File = 1,
               CO2eq = 2)|> # Measured in kt CO2 eq
        filter(File == "Total (net emissions)(1)")|>
        mutate(File = i,
               CO2eq = round((CO2eq/1000),2))|>
        separate(File, c("Country", "NIR Year", "Year", "Ext"), "[_.]")|>
        select(-`NIR Year`, -Ext)|>
        mutate(Year = as.numeric(Year))
      
      National.Emissions.2<-rbind(National.Emissions.2, x)
  }}

#-------------------------------------------------------------------------------#
# Joining CRF 1 and 2 Datasets
#-------------------------------------------------------------------------------#
Table.3.A<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")
Table.3.B<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")
Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
Table.4<-read_csv("~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")
National.Emissions<-read_csv("~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")


Table.3.A.2<-mutate(Table.3.A.2, Year = as.numeric(Year))

Table.3.A<-rbind(Table.3.A, Table.3.A.2)

Table.3.B.2<-select(Table.3.B.2, -"Pit.Storage", -"Dry.Lot",-"Deep.Bedding") # Removing Additional categories, not used in analysis. 

Table.3.B<-rbind(Table.3.B, Table.3.B.2)

Table.3.D<-rbind(Table.3.D, Table.3.D.2)

Table.4<-rbind(Table.4, Table.4.2)

National.Emissions<-rbind(National.Emissions, National.Emissions.2)

fwrite(National.Emissions, file = "~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")
fwrite(Table.3.A, file = "~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")
fwrite(Table.3.B, file = "~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")
fwrite(Table.3.D, file = "~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
fwrite(Table.4, file = "~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")

# Data can be found at: https://github.com/Jbourassa05/Simpson_Centre-Carbon_Program


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
# Data Cleaning ----
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Data Cleaning for Cattle Variables
#-------------------------------------------------------------------------------#

## Manure Management

### The section is collapsing rows into Type A groups (Dairy and Non-Dairy Cattle)
### This section is also calculating total CH4 and N2O from Managed Manure

Table.3.B<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")|>
  select(9,10,1, 8,2,7,17,22,28, 31,32)|>
  mutate(across(c("Year","Population", "CH4.kt","Pasture","Direct.N2O"), as.numeric),
         Pasture = ifelse(is.na(Pasture), 0, Pasture), # replacing NA with O
         CH4.kt = ifelse(is.na(CH4.kt), 0, CH4.kt), # replacing NA with O
         Direct.N2O = ifelse(is.na(Direct.N2O), 0, Direct.N2O))|> # replacing NA with O
  filter(!is.na(Population)) # removing rows where population is NA
  
#-------------------------------------------------------------------------------#         
# imputing missing values for indirect emissions
#-------------------------------------------------------------------------------#
### EU average used for N2O.Runoff.IEF for EU countries
### Canadian Values are used for the United States
#-------------------------------------------------------------------------------#

EU<-filter(Table.3.B, Country == "EUA" &  Type.A == "Dairy Cattle")|> select(Year, N2O.Runoff.IEF, N2O.Vol.IEF)|>rename(Runoff.EU = N2O.Runoff.IEF, Vol.EU= N2O.Vol.IEF)
CAN<-filter(Table.3.B, Country == "CAN" &  Type.A == "Dairy Cattle")|> select(Year, N2O.Runoff.IEF, N2O.Vol.IEF)|>rename(Runoff.CAN = N2O.Runoff.IEF, Vol.CAN= N2O.Vol.IEF)

Table.3.B<-full_join(Table.3.B,EU)
Table.3.B<-full_join(Table.3.B,CAN)

Table.3.B<-Table.3.B|>
  mutate(N2O.Vol.IEF = ifelse(Pasture != Total.N & Country == "USA" & N2O.Vol.IEF == 0, Vol.CAN, 
                              ifelse(Pasture != Total.N & N2O.Vol.IEF == 0 , Vol.EU, N2O.Vol.IEF)),
         N2O.Runoff.IEF = ifelse(Pasture != Total.N & Country == "USA" & N2O.Runoff.IEF ==0, Runoff.CAN,
                                ifelse(Pasture != Total.N & N2O.Runoff.IEF == 0, Runoff.EU, N2O.Runoff.IEF)))|>
  select(-12,-13,-14,-15)|>
  mutate(Indirect.N2O = ((((Total.N-Pasture)*N2O.Vol.IEF))+((Total.N-Pasture)*N2O.Runoff.IEF))/10^6)|>
  group_by(Country, Year, Type.A)|>
  summarise(N.Pasture = sum(Pasture), #total N left on Pasture and Rangeland
            CH4.MM = sum(CH4.kt), #Total kg CH4 produced
            N2O.MM = sum(Direct.N2O),
            Indirect.N2O = sum(Indirect.N2O))|> # Total N2O produced
  mutate(Indirect.N2O = ifelse(Country == "NZL", 0, Indirect.N2O))
#-------------------------------------------------------------------------------#
## Emissions from PRP
#-------------------------------------------------------------------------------#
### IPCC classifies dung and urine deposited by grazing animals (PRP) as emissions from Agricultural soils
### This section is selecting PRP emission factors from Table 3.D and adding them to emissions from manure management. 
### Indirect emissions from PRP are estimated using reported IEF or IPCC default values with values are not reported

Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")|>
  filter(Emission.Source %in% c("N from grazing animals", "Atmospheric deposition","leaching and run-off"))|>
  select(1,2,3,6,9,10)

Table.3.D.Indirect<-Table.3.D|> # Values will be used to calculate indirect emissions from PRP
  filter(Emission.Source %in% c("Atmospheric deposition","leaching and run-off"))|>
  select(-5,-6)|>
  spread(Emission.Source, IEF)

Table.3.D<-full_join(Table.3.D, Table.3.D.Indirect)|>
  filter(!Emission.Source %in% c("Atmospheric deposition","leaching and run-off"))|>
  select(-Emission.Source)|>
  rename(EF3.PRP = IEF,
         EF4.NVOL = 6,
         EF5.Leaching = 7,
         FracLEACH = `FracLEACH-(H)`)|>
  mutate(EF3.PRP = ifelse(is.na(EF3.PRP), 0.004, EF3.PRP), # IPCC 2019 refinement aggregated default value:  TABLE 11.1 <-see suggested reading at top for crop production
         EF4.NVOL = ifelse(is.na(EF4.NVOL), 0.01, EF4.NVOL), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         EF5.Leaching = ifelse(is.na(EF5.Leaching), 0.011, EF5.Leaching), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracGASM = ifelse(is.na(FracGASM), 0.21, FracGASM), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracLEACH = ifelse(is.na(FracLEACH), 0.24, FracLEACH)) # IPCC 2019 refinement aggregated default value:  TABLE 11.3

#-------------------------------------------------------------------------------#
# Total Emissions from Manure - indirect emissions from MM
#-------------------------------------------------------------------------------#

MM<-full_join(Table.3.B, Table.3.D)|>
  mutate(PRP.N2O = ((N.Pasture*EF3.PRP)*(44/28))/10^6, # See Equation 11.1 in 2019 Refinement N2O-N_prp: kt N2O
         PRP.Vol = (((N.Pasture*FracGASM)*EF4.NVOL)*(44/28))/10^6, # kt N2O
         PRP.Leach = (((N.Pasture*FracLEACH)*EF5.Leaching)*(44/28))/10^6, # kt N2O
         PRP.Ind = PRP.Vol+PRP.Leach, # See Equation 11.10 and 11.11 in 2019 Refinement, kt N2O
         EM.MM = (CH4.MM*25) + ((N2O.MM+PRP.N2O+PRP.Ind+Indirect.N2O)*298)) # Value shown in kt CO2eq
#-------------------------------------------------------------------------------#
# Enteric Methane Emissions
#-------------------------------------------------------------------------------#

Table.3.A<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")|>
  mutate(across(c("Year","Population", "GE", "Ym", "IEF","CH4.kt", "Total.Pop", "Share.Pop","Weight", "Milk Yield", "DE"), as.numeric),
         `Milk Yield` = ifelse(Type.A == "Non-Dairy Cattle", 0, `Milk Yield`),
         W.Weight = Weight*Share.Pop, #estimating weights
         W.GE = GE*Share.Pop,
         W.Ym = Ym*Share.Pop)|>
  filter(!is.na(CH4.kt)|!is.na(Population))|>
  group_by(Type.A, Year, Country)|>
  summarise(Population = sum(Population), # Total population for dairy and Non-dairy cattle
            EM.CH4 = sum(CH4.kt), #Total emissions (kt CH4) for dairy and non-dairy cattle
            GE = sum(W.GE), # weightted average GE
            Ym = sum(W.Ym), # Weighted Average Ym
            Weight = sum(W.Weight), # Average Weight
            `Milk Yield` = sum(`Milk Yield`)) # Average milk production 

# Correcting for missing Values or large outliers or miscalculation
# missing Value using EU average over period
# US Values used for Canadian non-dairy cattle weight (reporting issue in NIR)

EU<-Table.3.A|>filter(Country == "EUA")|>select(Year, Weight,  Type.A)|>rename(Weight.EU = Weight)
USA <- Table.3.A|>filter(Country == "USA")|>select(Year,Type.A, Weight)|>rename(Weight.US = Weight)

Table.3.A<-full_join(Table.3.A, EU)
Table.3.A<-full_join(Table.3.A, USA)
  
Enteric<-Table.3.A|>
  mutate(Weight = ifelse(is.na(Weight), Weight.EU, Weight), # Correcting for NA Values
         Weight = ifelse(Country == "DNK", Weight.EU, # Cattle Weight in Denmark outlier 
                         ifelse(Country =="CAN" & Type.A == "Non-Dairy Cattle", Weight.US, Weight)),# non-dairy Cattle Weight in Canada outlier  
         EM.CO2eq = EM.CH4*25)|>select(-10,-11)


NIR_2023_Cattle<-full_join(Enteric, MM, by=c("Year" = "Year", "Country" = "Country", "Type.A" = "Type.A"))|>
  mutate(`Total Emissions kt.CO2.eq` = EM.CO2eq+EM.MM,
         `kg.CO2.eq/head` = (`Total Emissions kt.CO2.eq`/Population)*1000,
         `head/day` = `kg.CO2.eq/head` /365,
         Mcal = GE/4.184,
         `EM/Mcal` = `head/day`/Mcal,
         `EM/kg` = `kg.CO2.eq/head`/Weight,
         `EM/kg.Milk` = `head/day`/`Milk Yield`)
  
fwrite(NIR_2023_Cattle, file = "~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")                           
                           
#-------------------------------------------------------------------------------# 

# Crop and Land use Variables

#-------------------------------------------------------------------------------#

Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
Table.4<-read_csv("~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")                         

#------------------------------------------------------------------------------#                          
# Fertilizer Emissions (Synthetic induced N2O emissions to CO2 emissions from Urea and carbon Containing fertilizers)
#------------------------------------------------------------------------------#  

Table.3.D<-Table.3.D|>
  mutate(FracGASF = ifelse(is.na(FracGASF) & Emission == "N2O", 0.11, FracGASF), # 2019 refinement default values
         FracGASM = ifelse(is.na(FracGASM ) & Emission == "N2O", 0.21, FracGASM), # 2019 refinement default values
         `FracLEACH-(H)` = ifelse(is.na(`FracLEACH-(H)`) & Emission == "N2O", 0.21, `FracLEACH-(H)` )) # 2019 refinement default values

fwrite(Table.3.D, file = "~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")

Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")

F.EM.Direct <- Table.3.D|> 
  filter(Emission.Source %in% c("Inorganic N emissions", "Organic N emissions"))|>
  select(-4,-11)|>
  rename(Direct.EM = Emissions.kt)|>
  mutate(T.Frac.GAS = ifelse( Emission.Source == "Inorganic N emissions", FracGASF*Application,FracGASM*Application),
         T.Frac.Leach = `FracLEACH-(H)`*Application) # Nitrogen loss from Organic and inorganic fertilizers -> see 2019 Refinement for description of methodology

F.EM.Indirect<-Table.3.D|>
  filter(Emission.Source %in% c("Atmospheric deposition", "leaching and run-off"))|>
  select(1,2,3,6)|>
  spread(Emission.Source, IEF)|>
  rename(IEF.GAS = `Atmospheric deposition`,
         IEF.Leach = `leaching and run-off`)

F.EM <-full_join(F.EM.Direct, F.EM.Indirect)|>
  mutate(EM.GAS = ((T.Frac.GAS*IEF.GAS)*(44/28))/10^6, # estimating indirect emissions from fertilizer use -> see 2019 Refinement for description of methodology
         EM.Leach = ((T.Frac.Leach*IEF.Leach)*(44/28))/10^6, # estimating indirect emissions from fertilizer use -> see 2019 Refinement for description of methodology
         N2O.EM = Direct.EM+EM.GAS+EM.Leach) # Total emissions from agricultural soils for fertilizer use

F.CO2<-Table.3.D|>
  filter(Emission.Source %in% c("Urea", "Other carbon-containing fertlizers"))|>
  select(1,2,3,7)|>
  mutate(Emissions.kt = ifelse(is.na(Emissions.kt),0,Emissions.kt))|>
  spread(Emission.Source, Emissions.kt)

F.EM<-full_join(F.EM, F.CO2)|>
  mutate(`Other carbon-containing fertlizers` = ifelse(Emission.Source == "Organic N emissions", 0, `Other carbon-containing fertlizers`),
         Urea = ifelse(Emission.Source == "Organic N emissions", 0, Urea),
         Application = ifelse(Country == "AUS", Application*1000, Application), # AUS reported Application rate in Tonnes not kg
         kt.CO2eq = N2O.EM*298 + `Other carbon-containing fertlizers` + Urea, # total emissions kt CO2eq
         kg.CO2eq.kg.N = (kt.CO2eq*10^6)/Application) # Intensity measure kg CO2eq/ha

Land.Area<-Table.4|> # Land area
  filter(`Land Use` == "Total Cropland")|>
  select(1,2,4)

F.EM<-full_join(F.EM,Land.Area)|>
  mutate(EM.Ha = kt.CO2eq/Total.Area.kha) # estimated intensity t CO2eq/ha

fwrite(F.EM, file = "~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")

#-------------------------------------------------------------------------------#
# Crop Production Based Emissions
#-------------------------------------------------------------------------------#
Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
Table.4<-read_csv("~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")              

EM.Crop<-Table.3.D|> # Select headline emissions related to crop production
  select(1,2,3,7,11)|>
  filter(Emission.Source %in% c("Direct N2O emissions", "Indirect N2O emissions", "Urea", "Other carbon-containing fertlizers"))|>
  mutate(CO2eq = ifelse(Emission == "N2O", Emissions.kt*298, Emissions.kt),
         CO2eq = ifelse(is.na(CO2eq),0,CO2eq))|>
  group_by(Country, Year)|>
  summarise(CO2eq = sum(CO2eq))


EM.PRP<-Table.3.D|> # grazing animal data
  filter(Emission.Source %in% c("N from grazing animals"))|>
  select(1,2,3,5,6,7,9,10)

EM.PRP.Indirect<-Table.3.D|>  # indirect emissions data 
  filter(Emission.Source %in% c("Atmospheric deposition","leaching and run-off"))|>
  select(1,2,3,6)|>
  spread(Emission.Source, IEF)

EM.PRP<-full_join(EM.PRP, EM.PRP.Indirect)|>
  select(-3)|>
  rename(N = Application,
         EF3.PRP = IEF,
         EF4.NVOL = 8,
         EF5.Leaching = 9,
         FracLEACH = `FracLEACH-(H)`)|>
  #-------------------------------------------------------------------------------#  
  # Estimating emissions from grazing animal to be subtracted from Total emissions from crop production
  #-------------------------------------------------------------------------------#  
  mutate(N = ifelse(Country == "AUS", N*1000, N),
         EF3.PRP = ifelse(is.na(EF3.PRP), 0.004, EF3.PRP), # IPCC 2019 refinement aggregated default value:  TABLE 11.1
         EF4.NVOL = ifelse(is.na(EF4.NVOL), 0.01, EF4.NVOL), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         EF5.Leaching = ifelse(is.na(EF5.Leaching), 0.011, EF5.Leaching), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracGASM = ifelse(is.na(FracGASM), 0.21, FracGASM), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracLEACH = ifelse(is.na(FracLEACH), 0.24, FracLEACH),# IPCC 2019 refinement aggregated default value:  TABLE 11.3
         N2O.GAS = ((N*FracGASM)*EF4.NVOL)*(44/28),
         N2O.Leach = ((N*FracLEACH)*EF5.Leaching)*(44/28),
         N2O.PRP = Emissions.kt+(N2O.GAS/10^6)+(N2O.Leach/10^6),
         CO2.PRP = N2O.PRP*298)|>
  select(Country, Year, N2O.PRP, CO2.PRP)
  
EM.PRP[is.na(EM.PRP)]<-0 # replacing NA values

EM.Crop<-full_join(EM.Crop, EM.PRP)|>
  mutate(kt.CO2 = CO2eq-CO2.PRP) # removing PRP emissions from Crop production

Land.Use<-Table.4|> # Selecting Emissions and land area data for cropland
  filter(`Land Use` == "Total Cropland")|>
  select(1,2,4,5)|>
  rename(Land.Use.EM = 4)

EM.Crop<-full_join(EM.Crop, Land.Use)|> # Calculating Net Emissions from crop production
  mutate(Total.CO2eq = Land.Use.EM+kt.CO2,
         Total.CO2eq.ha = Total.CO2eq/Total.Area.kha,
         CO2eq.ha = kt.CO2/Total.Area.kha)

fwrite(EM.Crop, file = "~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv") # Can be Found at: https://github.com/Jbourassa05/Simpson_Centre-Carbon_Program
  
#-------------------------------------------------------------------------------#
## National Emission Trends + Intensity
#-------------------------------------------------------------------------------# 

Country.List<-c("AUS","AUT", "BEL", "BGR", "BLR", "CAN", "CHE", "CYP", "DNK",
                "ESP", "EST", "EUA", "FIN", "GBR", "GRC","HRV", "HUN", "IRL",
                "ISL", "ITA","JPN", "KAZ", "LIE","LTU", "LUX", "LVA", "MLT",
                "NLD", "NOR","NZL", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", 
                "SWE", "TUR", "USA", "DEU", "FRA", "CZE") 



Emissions <- fread("~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")

Emissions.2005 <-filter(Emissions, Year == 2005)|>rename(CO2eq.2005 = 3)|>select(-2) # selecting 2005 emissions

Emissions<-full_join(Emissions,Emissions.2005)|> # Estimating change since 2005
  mutate(d.2005 = ((CO2eq- CO2eq.2005)/CO2eq.2005)*100)

# GDP Data was collected from the World banks Data bank: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

GDP <- read_csv("~/Simpson Centre/NIR/02-Data/2022_CanSectorData/World_GDP.csv")|>
  select(-1,-2,-3)|>
  rename(Country = 1)|>
  mutate(Country = ifelse(Country == "EUU", "EUA", Country))|>
  filter(Country %in% Country.List)|>
  gather(Year, GDP, 2:18)|>
  mutate(Year = as.numeric(Year))

Emissions<-full_join(Emissions, GDP)|> # joining Emission data to GDP data
  filter(!is.na(GDP))|>
  mutate(GDP = as.numeric(GDP),
         t.CO2 = CO2eq*10^6,
         EM.IN = t.CO2/(GDP/10^6)) # t per Million dollards GDP

fwrite(Emissions, file = "~/Simpson Centre/NIR/02-Data/Emission_Intensity.csv") # Can be Found at: https://github.com/Jbourassa05/Simpson_Centre-Carbon_Program

#-------------------------------------------------------------------------------#

# Webinar Figures and Further Analysis ----

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
## Change in Emissions Since 2005 Comparison
#-------------------------------------------------------------------------------#
Emissions <- read.csv(file = "~/Simpson Centre/NIR/02-Data/Emission_Intensity.csv")|>
  filter(Year == 2021,
         !Country %in% c("TUR"))|> # Large Outlier
  mutate(Rank = rank(-CO2eq))|>
  filter(Rank <=20)|> # selecting the 20 largest emitters for figure
  mutate(Country = fct_reorder(Country, d.2005),
         highlight = ifelse(Country %in% c("CAN", "USA", "EUA", "AUS", "RUS", "JPN"), T, F)) # highlighted values (Large Emitters)

## Figure

ggplot(Emissions,aes(Country, d.2005, color = highlight, fill = highlight))+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_col(color = "#333333", alpha = 0.95)+
  theme_light()+
  labs(title = "Change In Total CO2eq Emissions from 2005 to 2021, Including LULUCF",
       subtitle = "Includes the 20 largest emitting annex I parties",
       caption = "Note: Figure excludes TUR: emissions increased by 95 percent since 2005 from 265.80 to 517.24 Mt.",
       x = "",
       y = "Change in Emissions (%)")+
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x=element_blank())+
  geom_text(data = subset(Emissions, d.2005 <= 0),
            aes(x = Country, d.2005-1.5,
                label = paste0(round(d.2005,0), "%")),
            size = 4, 
            fontface = "bold")+
  geom_text(data = subset(Emissions, d.2005 <= 0),
            aes(x = Country, 1.5,
                label = paste0(Country)),
            size = 4, 
            fontface = "bold")+
  geom_text(data = subset(Emissions, d.2005 > 0),
            aes(x = Country, d.2005+1.5,
                label = paste0(round(d.2005,0), "%")),
            size = 4, 
            fontface = "bold")+
  geom_text(data = subset(Emissions, d.2005 > 0),
            aes(x = Country, -1.5,
                label = paste0(Country)),
            size = 4, 
            fontface = "bold")+
  scale_fill_manual(values = c("#004c6d", "red"))+
  scale_color_manual(values = c("#004c6d", "red"))+
  scale_y_continuous(breaks = seq(-60,30,10),
                     expand = expansion(mult = c(0.15, 0.15)))
    
ggsave("~/Simpson Centre/NIR/02-Data/C_International_Emissions.png", unit = "cm", width = 22, height = 15.75, dpi = "retina")  

#-------------------------------------------------------------------------------#
# Total Emissions in 2021 by Annex I Party
#-------------------------------------------------------------------------------#

Emissions <- fread("~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")|>
  filter(Year == 2021)|>
  mutate(Country = fct_reorder(Country, CO2eq),
         Rank = rank(-CO2eq))|>
  filter(Rank <= 30)

ggplot(Emissions, aes(Country, CO2eq))+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = CO2eq), color = "#333333", size = 1)+
  geom_point(color = "#333333", fill = "#004c6d", shape = 21, size = 4)+
  geom_point(data = subset(Emissions, Country == "CAN"), aes(Country, CO2eq),
             color = "#333333", fill = "red", shape = 21, size = 4)+
  geom_text(aes(x = Country, y= CO2eq, label = round(CO2eq,0)),
            hjust = "center",
            vjust = -1,
            color = "#333333")+
  geom_text(aes(x = Country, y= 0, label = paste0(" ", Country)),
            hjust = "left",
            angle = -90,
            color = "#333333")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank())+
  labs(title = "2021 National Inventory Levels for Annex I Parties to the UNFCCC",
       subtitle = "Measured in Mt CO2eq, estimates include LULUFC",
       caption = "Note: The figure shows the 30 largest national inventories from 2021",
       y = "",
       x = "")+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))

ggsave("~/Simpson Centre/NIR/02-Data/C_International_Emission_total.png", unit = "cm", width = 22, height = 15.75, dpi = "retina")

#-------------------------------------------------------------------------------#
# Emission Intensity Figure
#-------------------------------------------------------------------------------#

Emissions <- read.csv(file = "~/Simpson Centre/NIR/02-Data/Emission_Intensity.csv")|>
  filter(Year %in% c(2005, 2021),
         !Country %in% c("KAZ"))|>
  group_by(Year)|>
  mutate(Rank = rank(-CO2eq))|>
  ungroup()

EM.List<-filter(Emissions, Year == 2021 & Rank <=20)|>select(Country)
EM.List<-c(EM.List$Country)

Emissions<- filter(Emissions, Country %in% EM.List)|>
  mutate(Year = as.factor(Year),
         highlight = ifelse(Country %in% c("CAN", "USA", "EUA", "AUS", "DEU", "JPN"), T, F))



# Figure

ggplot(Emissions,aes(Year,EM.IN, group = Country, color = highlight))+
  geom_path(alpha = 0.5, size = 1)+
  geom_point(size = 4, alpha = .75)+
  geom_path(data = subset(Emissions, highlight == T), 
            aes(Year,EM.IN,group = Country),
            size = 1,
            alpha = 0.5)+
  geom_point(data = subset(Emissions, highlight == T), 
             aes(Year,EM.IN,group = Country),
             size = 4,
             alpha = .75)+
  geom_text_repel(data = subset(Emissions, Year == 2005),
                  aes(x = Year, y = EM.IN,  color = highlight,
                      label = paste0(Country, ": ", round(EM.IN,0), " t")),
                  force        = 0.5,
                  nudge_x      = -0.25,
                  direction    = "y",
                  hjust        = 1,
                  segment.size = 0.2,
                  fontface = "bold", 
                  size = 4)+
  geom_text_repel(data = subset(Emissions, Year == 2021),
                  aes(x = Year, y = EM.IN, color = highlight,
                      label = paste0(Country, ": ", round(EM.IN,0), " t")),
                  force        = 0.5,
                  nudge_x      = 0.25,
                  direction    = "y",
                  hjust        = 0,
                  segment.size = 0.2,
                  fontface = "bold", 
                  size = 4)+
  scale_color_manual(values = c("#004c6d", "red"))+
  labs(title = "Change In Emission Intensity Measured in Tonnes CO2eq per Million Dollars GDP",
       subtitle = "GDP values measured in 2015 constant dollar (USD), includes the 20 largest emitting annex I parties",
       caption = "Note: Figure excludes KAZ, emission intensity decreased from 3420 to 1590 t/$M between 2005 and 2021",
       x = "",
       y = "")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 15, 
                                   face = "bold"))+
  scale_x_discrete(position  = "top")+
  scale_y_continuous(limits = c(-150, 1400), expand =  expansion(mult = c(0, 0.0)))
  
  
ggsave("~/Simpson Centre/NIR/02-Data/C_International_Emission_IN.png", unit = "cm", width = 22, height = 20, dpi = "retina")  


#-------------------------------------------------------------------------------#
# Canadian Emissions By Economic Sector
#-------------------------------------------------------------------------------# 

# Data for this section was collected from : https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/
# https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/B-Economic-Sector/?lang=en

EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")

# Data is provided by province and region for each sector and subsector from 1990 to 2021 

National.EM <- EM.Sector|>
  filter(Region == "Canada",
         Index == 0)|>
  mutate(Year = as.numeric(Year))

Sector<-EM.Sector|>
  filter(Region == "Canada",
         Index %in% c(0,1,44,16,17,25,45,33,40,36))|> # Selecting ag related variables
  select(-2,-5:-8,-10)|>
  mutate(Sector = ifelse(Index %in% c(44,45,40), "Other",
                         ifelse(Index == 0, "National Inventory", Source)),
         CO2eq = as.numeric(CO2eq))|>
  group_by(Year, Sector)|>
  summarise(Mt.CO2eq = sum(CO2eq))|>
  ungroup()

Sector$Sector<-factor(Sector$Sector, levels = c("National Inventory","Oil and Gas", "Transport", "Buildings",
                                                "Heavy Industry","Agriculture","Electricity",
                                                "Other"))

Col.7<-c("#004c6d", "#2d6484", "#4c7c9b", "#6996b3", "#86b0cc", "#a3cbe5", "#c1e7ff")


ggplot(subset(Sector,Sector !="National Inventory"))+
  geom_area(aes(Year, Mt.CO2eq, fill = Sector),alpha = 0.95)+
  geom_line(data = National.EM, aes(x = Year, y = as.numeric(CO2eq)), size = 1, color = "#333333")+
  geom_point(aes(x = 2021, y = 670.4276866), size = 3, color = "#333333",fill = "red" , shape = 21)+
  geom_point(aes(x = 2005, y = 732.2187885), size = 3, color = "#333333",fill = "red", shape = 21)+
  geom_hline(yintercept = 732.2187885*.6, color = "azure2", linetype = "dashed", size = 1)+
  scale_fill_manual(values = Col.7)+
  theme_classic()+
  annotate(geom = "text", x = 2017, y = 672,
           label = "Current Emissions:\n~670 Mt CO2eq ",
           hjust = 0.5,
           vjust = 1.5,
           color = "azure2")+
  annotate(geom = "text", x = 2005, y = 732.2187885,
           label = "2005 Baseline:\n~732 Mt CO2 eq",
           hjust = .5,
           vjust = 1.5,
           color = "azure2")+
  annotate(geom = "text", x = 2005, y = 732.2187885*.6,
           label = "eNDC Commitment: ~440 Mt CO2 eq",
           hjust = .5,
           vjust = -1.1,
           color = "azure2")+
  scale_x_continuous(breaks = seq(1990,2021,5),
                    expand = expansion(mult = c(0,0.01)))+
  scale_y_continuous(breaks = seq(0,800,100), 
                     expand = expansion(mult = c(0.00,.05)))+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  guides(fill = guide_legend(nrow = 1))+
  labs(title = "Canada's GHG Emissions from 1990 to 2021",
       subtitle = "Total CO2eq emissions by economic sector, excluding LULUCF",
       y = "Emissions(Mt CO2 eq)",
       x = "")
  
ggsave("~/Simpson Centre/NIR/02-Data/National_Emissions.png", unit = "cm", width = 22, height = 15.75, dpi = "retina")  

#-------------------------------------------------------------------------------#
# Change in National Emissions since 2005
#-------------------------------------------------------------------------------#

# Same source as above

Sector.2005<-Sector|> # selecting 2005 for baseline
  filter(Year == 2005)|>
  rename(CO2eq.2005 = Mt.CO2eq)|>
  select(-1)
Sector<-full_join(Sector, Sector.2005)|>
  mutate(d.EM = (Mt.CO2eq-CO2eq.2005)/CO2eq.2005) # Calculating change since 2005

ggplot(subset(Sector,Year >=2005), aes(Year, d.EM*100, color = Sector))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(group=Sector), size = 2)+
  geom_point(size = 4)+
  geom_hline(yintercept = -40, linetype = "dashed")+
  annotate("text",x = 2005, y = -40, label = " eNDC Commitment: -40% by 2030",
           hjust = 0,
           vjust = -1)+
  scale_x_continuous(breaks = seq(2005, 2020,5),
                     expand = expansion(mult = c(0.05,0.23)))+
  scale_y_continuous(breaks = seq(-60,30,10))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("red", Col.7))+
  geom_text_repel(data = subset(Sector, Year == 2021), 
                  aes(label = paste0(Sector, ": ", round(d.EM*100,0), "%")) , 
                  force        = 0.5,
                  nudge_x      = 0.25,
                  direction    = "y",
                  hjust        = 0,
                  segment.size = 0.2,
                  fontface = "bold", 
                  size = 3,
                  color = "#333333")+
  labs(title = "Change in Canadian Emissions Since 2005 by Economic Sector",
       subtitle = "Change in Total CO2eq emissions from 2005, excluding LULUCF",
       x = "",
       y = "Change in Emissions (%)")
  
ggsave("~/Simpson Centre/NIR/02-Data/C_National_Emissions.png", unit = "cm", width = 23, height = 15.75, dpi = "retina")  


#-------------------------------------------------------------------------------#
# Agriculture Emissions 
#-------------------------------------------------------------------------------#

# Tables required for analysis

Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")
CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")
Fertilizer <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")
Table.3.Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_Cattle_2023.csv")
Table.3 <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_2023.csv")
EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")

#-------------------------------------------------------------------------------#
# Total Emissions from the Ag Sector
#-------------------------------------------------------------------------------#

Ag.Sector<-EM.Sector|>
  filter(Index %in% c(37,38,39), # selecting sub sectors related to Ag
         Region == "Canada")|>
  select(Year, Sector, CO2eq)

Ag.Can<-EM.Sector|> # selecting for labels
  filter(Index %in% c(36), 
         Region == "Canada",
         Year %in% c(1990,1995,2000,2005,2010,2015,2021))|>
  select(Year, Sector, CO2eq)|>
  mutate(Sector = "Total")

Ag.Sector.labs = full_join(Ag.Sector, Ag.Can)|> # Label Placement
  filter(Year %in% c(1990,1995,2000,2005,2010,2015,2021))|>
  mutate(CO2eq = as.numeric(CO2eq))|>
  spread(Sector, CO2eq)|>
  mutate(OFFU = `On Farm Fuel Use`/2,
         CP = `On Farm Fuel Use`+(`Crop Production`/2),
         AP = `On Farm Fuel Use`+`Crop Production`+(`Animal Production`/2))|>
  select(1,6,7,8)|>
  gather(Sector, Values, 2:4)

Ag.Sector.2<-full_join(Ag.Sector, Ag.Can)|>
  filter(Year %in% c(1990,1995,2000,2005,2010,2015,2021),
         Sector != "Total")|>
  mutate(CO2eq = as.numeric(CO2eq),
         Sector = ifelse(Sector == "On Farm Fuel Use", "OFFU",
                       ifelse(Sector == "Crop Production", "CP", "AP")))
 
Ag.Sector.labs<-full_join(Ag.Sector.labs, Ag.Sector.2)

Ag.Sector$Sector<-factor(Ag.Sector$Sector, levels = c("Animal Production", "Crop Production", "On Farm Fuel Use"))


# Figure 

ggplot(Ag.Sector, aes(x = Year, y = as.numeric(CO2eq)))+
  geom_col(aes(fill = Sector),alpha = 0.9, color = "#333333", width = 1)+
  geom_point(data = Ag.Can, 
             aes(x = as.numeric(Year), y = as.numeric(CO2eq)), 
             size = 3, 
             fill = "azure2", 
             color = "#333333", 
             shape = 21)+
  scale_fill_manual(values = c("#004c6d", "#5b87a8", "#a3c8e7"))+
  geom_text(data = Ag.Can,
            aes(x = Year, 
                y = as.numeric(CO2eq)+3,
                label = paste0(round(as.numeric(CO2eq),0), " Mt")),
            size = 4, 
            fontface = "bold")+
  geom_text(data = Ag.Sector.labs,
            aes(x = Year, 
                y = Values,
                label = paste0(round(CO2eq,0), "\nMt")),
            color = "azure2",
            #angle = 90,
            size = 4, 
            fontface = "bold")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_x_continuous(expand= expansion(mult= .01))+
  scale_y_continuous(expand= expansion(mult= c(0,.01)))+
  labs(title = "Canadian Agricultural Emissions from 1990 to 2021",
       subtitle = "Grouping based on Canadian economic sector classification",
       y = "Mt CO2eq",
       x = "")
  
ggsave("~/Simpson Centre/NIR/02-Data/Ag_Emissions.png", unit = "cm", width = 23, height = 15.75, dpi = "retina")  

#-------------------------------------------------------------------------------#
# Animal Production
#-------------------------------------------------------------------------------#

# Change in Emissions Since 2005

Table.3 <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_2023.csv")
EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")
Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")


Animal.Production <-filter(EM.Sector, Index == 39, Region == "Canada", Year >=2005)|>
  mutate(CO2eq = as.numeric(CO2eq),
         d.2005 = (CO2eq-42.46011549)/42.46011549)|>
  select(1,5,9,11)

Can.Cattle.Total<-Cattle|>
  filter(Year >=2005,
         Country == "CAN")|>
  select(1,2,4,25)|>
  group_by(Year)|>
  summarise(CO2eq.Cattle = sum(`Total Emissions kt.CO2.eq`))
  
Animal.Production<-full_join(Animal.Production, Can.Cattle.Total)|>
  mutate(CO2eq.Cattle = CO2eq.Cattle/1000,
         CO2eq.Other = CO2eq-CO2eq.Cattle)


Can.Cattle <- Cattle|>
  filter(Year >=2005,
         Country == "CAN")|>
  rename(T.EM.CO2eq = 25)|>
  select(1,2, 25 )|>
  spread(Type.A, T.EM.CO2eq)
 
Animal.Production<-full_join(Animal.Production, Can.Cattle)|>
  mutate(`Dairy Cattle` = `Dairy Cattle`/1000,
        `Non-Dairy Cattle` = `Non-Dairy Cattle`/1000)|>
  select(-4,-5)|>
  rename(`Animal Production` = 3, 
         `Other Livestock` = 4)|>
  gather(Category, Value, 3:6)|>
  mutate(d.2005 = ifelse(Category == "Animal Production", ((Value-42.460115)/42.460115)*100,
                         ifelse(Category == "Other Livestock",((Value-6.564720)/6.564720)*100,
                                ifelse(Category == "Dairy Cattle",((Value-4.407268)/4.407268)*100,
                                       ((Value-31.488127)/31.488127)*100))))

ggplot(Animal.Production, aes(Year, d.2005, color = Category))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  geom_hline(yintercept= 0, linetype = "dashed", color = "#333333")+
  geom_text(data = subset(Animal.Production, Year == 2021),
            aes(x = Year, 
                y = d.2005,
                label = paste0("  ",round(d.2005, 1), "%")),
            color = "#333333",
            hjust = "left",
            vjust = 0,
            size = 5, 
            fontface = "bold")+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.1)))+
  scale_color_manual(values = c("red","#004c6d", "#5b87a8", "#a3c8e7"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  labs(title = "Change In Emissions From Animal Production between 2005 and 2021",
       subtitle = "Measured in percent change from a 2005 baseline",
       y = "Percent Change (%)",
       x = "",
       caption = "Note: Emission values include enteric fermentation, manure management, and manure left on pasture.\nOther Livestock was calculated by substracting emissions from cattle from total animal production.")


ggsave("~/Simpson Centre/NIR/02-Data/Animal_Emissions.png", unit = "cm", width = 15*1.5, height = 12*1.5, dpi = "retina")  

#-------------------------------------------------------------------------------#
# Estimating Breakdown of emissions by livestock , source, and gas
#-------------------------------------------------------------------------------#

Animal.Production.Sector<-Animal.Production|>
  filter(Year == 2021 & Category != "Animal Production")|>
  select(-2,-5)|>
  mutate(Axis.Label = "Livestock Type")

Can.Cattle.Source <- Cattle|>
  filter(Year ==2021,
         Country == "CAN")|>
  rename(T.EM.CO2eq = 25)|>
  group_by(Year)|>
  summarise(EF = sum(EM.CH4),
            CH4.MM = sum(CH4.MM),
            N2O.MM = sum(N2O.MM),
            Indirect.MM = sum(Indirect.N2O),
            PRP.N2O = sum(PRP.N2O), 
            PRP.Ind = sum(PRP.Ind),
            N = sum(N.Pasture))

Animal.Production.Source<-Table.3|>
  filter(Category %in% c("Enteric Fermentation", "Manure Management"),
         Country == "CAN",
         Year == "2021")|>
  mutate(Cattle.Value = ifelse(Category == "Enteric Fermentation",(935.719*25)/1000,((78.61102*25)+((8.736899+1.351679)*298))/1000),
         Non.Cattle = (CO2eq.kt/1000) - Cattle.Value,
         d.Animal.Production = 34.888297-sum(CO2eq.kt/1000),
         PRP.Manure = (286990641.48 - 270352285)/286990641.48,
         `Manure on Pasture` = PRP.Manure*d.Animal.Production)|>
  select(3,9,12)|>
  spread(Category, Non.Cattle)|>
  gather(Source, Value, 1:3)|>
  mutate(Category = "Other Livestock")

Can.Cattle.Source <- Cattle|>
  filter(Year ==2021,
         Country == "CAN")|>
  mutate(`Enteric Fermentation` = (EM.CH4*25)/1000,
         `Manure Management` = ((CH4.MM*25)+((N2O.MM+Indirect.N2O)*298))/1000,
         `Manure on Pasture` = (N.Pasture/286990641.48)*2.59434)|>
  select(1, 32, 33,34)|>
  rename(Category = Type.A)|>
  gather(Source, Value, 2:4)

#------------------------------------------------------------------------------#
# Comparison of Cattle and Dairy Cattle Characteristics
#------------------------------------------------------------------------------#

Cattle.Emissions <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")|>
  filter(Country == "CAN")|>
  select(1,2,25)

Table.3.A<- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")|>
  filter(Country == "CAN")|>
  select(2,9, 4, 5,6, 13,14)
  
Cattle.Sum <- full_join(Cattle.Emissions, Table.3.A)|>
  rename(`Total Emissions` = 3)|>
  mutate(`Emission Factor` = `Total Emissions`/Population)|>
  filter(Year>=2005)|>
  gather(Metric, Value, 3:9)

Cattle.Sum.2005<-Cattle.Sum|>
  filter(Year == 2005)|>
  select(-2)|>
  rename(Y.2005= 3)

Cattle.Sum.Join<-full_join(Cattle.Sum, Cattle.Sum.2005)|>
  mutate(Value = as.numeric(Value),
         Y.2005 = as.numeric(Y.2005),
         d.2005 = ((Value-Y.2005)/Y.2005)*100)|>
  filter(!Metric %in% c( "Ym", "DE"))

Cattle.Sum.Join$Metric<-factor(Cattle.Sum.Join$Metric, 
                               levels = c("Total Emissions", "Population", "Emission Factor", "GE","Milk Yield"))


ggplot(Cattle.Sum.Join, aes(Year, d.2005))+
  facet_wrap(~Type.A, scale = "free")+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_line(aes(color = Metric), size = 2)+
  geom_point(aes(fill = Metric), size = 4, shape = 21, color = "#333333")+
  scale_color_manual(values = c("red","#004c6d", "#4c7c9b", "#86b0cc", "#c1e7ff"))+
  scale_fill_manual(values = c("red","#004c6d", "#4c7c9b", "#86b0cc", "#c1e7ff"))+
  geom_text(data = subset(Cattle.Sum.Join, Year == 2021), 
            aes(x = Year, y = d.2005,
                label = paste0("  ", round(d.2005,1), "%")),
            size =4, 
            fontface="bold",
            hjust = "left")+
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))+
  labs(title = "Change in Emissions and Model Variables for Dairy and Non-Dairy Cattle",
       subtitle = "Measured as percent change from 2005",
       caption = "Note: Model variables collected from Table3.As1 and Table3.AS2 from Canada's 2023 NIR\n",
       x = "",
       y = "Percent Change (%)")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 12))
  


ggsave("~/Simpson Centre/NIR/02-Data/Can_Cattle.png", unit = "cm", width = 38, height = 16, dpi = "retina")  


#Cattle.Emissions <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")|>
#  filter(Country %in% c("CAN", "AUS", "USA", "EUA", "NZL"),
#         Year == 2021)|>
#  mutate(GE = round(GE,0))


#-------------------------------------------------------------------------------#
# Emission Per head comparison for dairy and non-dairy cattle
#-------------------------------------------------------------------------------#
Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")

Cattle.Int <- Cattle|>
  filter(Year %in% c(2021))|>
  group_by(Type.A)|>
  mutate(Rank = rank(-`Total Emissions kt.CO2.eq`))|>
  ungroup()|>
  filter(Rank <= 20)|>
  mutate(Country.Lab = Country,
         Country = reorder_within(Country,`kg.CO2.eq/head`, Type.A ),
         IEF = `kg.CO2.eq/head`/1000,
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA", "NDL", "CAN", "USA"), T, F))

ggplot(Cattle.Int, aes(Country, IEF))+
  geom_col(aes(fill = highlight))+
  facet_wrap(~Type.A, scales = "free", nrow = 2)+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_text(data = Cattle.Int,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            hjust = "center",
            vjust = -1,
            size = 4, 
            fontface = "bold")+
  geom_text(data = subset(Cattle.Int, highlight == T),
            aes(x = Country,
                y = IEF,
                label = paste0(round(IEF, 2))),
            color = "#333333",
            hjust = "center",
            vjust = 1.5,
            size = 4, 
            fontface = "bold")+
  geom_text(data = subset(Cattle.Int, highlight == F),
            aes(x = Country,
                y = IEF,
                label = paste0(round(IEF, 2))),
            color = "azure2",
            hjust = "center",
            vjust = 1.5,
            size = 4, 
            fontface = "bold")+
  labs(title = "Average Annual Emissions per Head for Dairy and Non-Dairy Cattle",
       subtitle = "Measured in tonnes CO2eq per head per Year",
       caption = "Note: Values includes enteric methane and emissions from both managed and unmanged manure",
       x = "",
       y = "t CO2eq/head/year")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 10))+
  scale_fill_manual(values = c("#004c6d", "red"))+
  scale_color_manual(values = c("azure2", "#333333"))
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("~/Simpson Centre/NIR/02-Data/Cattle_IEF.png", unit = "cm", width = 23, height = 16, dpi = "retina")  

#-------------------------------------------------------------------------------#
## Intensity EM/mcal for non-dairy cattle
#-------------------------------------------------------------------------------#
  
Cattle.Int <- Cattle|>
  filter(Year %in% c(2021),
         Country != "JPN", #GE data missing from JPN due to differences in methodology
         Type.A != "Dairy Cattle")|>
  group_by(Type.A)|>
  mutate(Rank = rank(-`Total Emissions kt.CO2.eq`),
         Ratio = `kg.CO2.eq/head`/GE)|>
  ungroup()|>
  filter(Rank <= 20)|>
  mutate(Country.Lab = Country,
         Country = reorder_within(Country,`EM/Mcal`, Type.A),
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA",  "CAN", "USA"), T, F))

ggplot(Cattle.Int, aes(Country, `EM/Mcal`*1000))+
  geom_col(aes(fill = highlight))+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_text(data = Cattle.Int,
            aes(x = Country,
                y = `EM/Mcal`*1000,
                label = paste0(round(`EM/Mcal`*1000,0)),
                color = highlight),
            vjust = -.5)+
  geom_text(data = Cattle.Int,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            vjust = 1.2)+
  labs(title = "Ratio of Emissions to Energy Consumption for Non-Dairy Cattle",
       subtitle = "Measured in g CO2eq/Mcal",
       caption = "Note: Energy consuption based on reported gross energy intake, GE * 4.184 = Mcal",
       x = "",
       y = "")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 10))+
  scale_fill_manual(values = c("#004c6d", "red"))+
  scale_color_manual(values = c("#004c6d", "red"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("~/Simpson Centre/NIR/02-Data/Cattle_Mcal.png", unit = "cm", width = 23, height = 16, dpi = "retina")  

#-------------------------------------------------------------------------------#
## Intensity EM/kg Milk for dairy cattle
#-------------------------------------------------------------------------------#

Cattle.Int.Dairy <- Cattle|>
  filter(Year %in% c(2021),
         Type.A == "Dairy Cattle")|>
  mutate(Rank = rank(-`Total Emissions kt.CO2.eq`),
         Country.Lab = Country,
         Country = reorder_within(Country,`EM/kg.Milk`, Type.A ),
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA",  "CAN", "USA"), T, F))|>
  filter(Rank <=20)


ggplot(Cattle.Int.Dairy, aes(Country, `EM/kg.Milk`))+
  geom_col(aes(fill = highlight))+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_text(data = Cattle.Int.Dairy,
            aes(x = Country,
                y = `EM/kg.Milk`,
                label = paste0(round(`EM/kg.Milk`,2)),
                color = highlight),
            hjust = "center",
            vjust = -.5,
            size = 4,
            fontface = "bold")+
  geom_text(data = Cattle.Int.Dairy,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            hjust = "center",
            vjust = 1.25,
            size = 4,
            fontface = "bold")+
  labs(title = "Ratio of Emissions to Milk Produced for Dairy Cattle",
       subtitle = "Measured in kg CO2eq/kg Milk",
       caption = "Note: Estimates use average daily milk production for dairy cattle as reported in the 2021 CRF tables",
       x = "",
       y = "")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 10))+
  scale_fill_manual(values = c("#004c6d", "red"))+
  scale_color_manual(values = c("#004c6d", "red"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("~/Simpson Centre/NIR/02-Data/Cattle_Milk.png", unit = "cm", width = 23, height = 16, dpi = "retina") 

#-------------------------------------------------------------------------------#
# Crop Production Figures
#-------------------------------------------------------------------------------#
CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")
Fertilizer <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")

#-------------------------------------------------------------------------------#
# Fertilizer Emissions per kg N 
#-------------------------------------------------------------------------------#

Fert <- Fertilizer|>
  filter(Year == 2021,
         Emission.Source == "Inorganic N emissions")|>
  mutate(Rank = rank(-kt.CO2eq),
         Country.Lab = Country,
         Country = reorder(Country, kg.CO2eq.kg.N),
         highlight = ifelse(Country.Lab %in% c("AUS", "FRA", "CAN", "EUA", "USA", "RUS"), T, F))|>
  filter(Rank <=20)

ggplot(Fert, aes(Country, kg.CO2eq.kg.N, color = highlight, fill = highlight))+
  geom_col()+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_text(data = Fert,
            aes(x = Country,
                y = kg.CO2eq.kg.N,
                label = paste0(round(kg.CO2eq.kg.N,1)),
                color = highlight),
            hjust = "centre",
            vjust = -1,
            size = 4,
            fontface = "bold")+
  geom_text(data = Fert,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            hjust = "centre",
            vjust = 1.1,
            size = 4,
            fontface = "bold")+
  labs(title = "Average Emissions from Inorganic Nitrogen Application",
       subtitle = "Measured in kg CO2eq/kg N",
       caption = "Note: Emissions include direct and Indirect N2O emissions and CO2 emissions from Urea application",
       x = "",
       y = "")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 10))+
  scale_color_manual(values = c("#004c6d", "red"))+
  scale_fill_manual(values = c("#004c6d", "red"))

ggsave("~/Simpson Centre/NIR/02-Data/FertIEF.png", unit = "cm", width = 23, height = 16, dpi = "retina")


#-------------------------------------------------------------------------------#
# Fertilizer emissions (Organic + Inorganic) Per ha
#-------------------------------------------------------------------------------#

Fert <- Fertilizer|>
  filter(Year == 2021)|>
   group_by(Country, Year)|>
  summarise(kt.CO2eq = sum(kt.CO2eq ),
            kha = mean(Total.Area.kha))|>
  ungroup()|>
  mutate(EM.Ha = kt.CO2eq/kha,
         Rank = rank(-kt.CO2eq),
         Country.Lab = Country,
         Country = reorder(Country, EM.Ha),
         highlight = ifelse(Country.Lab %in% c("AUS", "FRA", "CAN", "EUA", "USA", "RUS", "TUR"), T, F))|>
  filter(Rank <=20)
  


ggplot(Fert, aes(Country, EM.Ha, color = highlight, fill = highlight))+
  geom_col()+
  geom_hline(yintercept = 0, color = "#333333")+
  geom_text(data = Fert,
            aes(x = Country,
                y = EM.Ha,
                label = paste0(round(EM.Ha,2)),
                color = highlight),
            hjust = "center",
            vjust = -1.1,
            size = 4,
            fontface = "bold")+
  geom_text(data = Fert,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            hjust = "Center",
            vjust = 1.1,
            size = 4,
            fontface = "bold")+
  labs(title = "Average Emissions From Fertilizer Application Per Hectare of Cropland",
       subtitle = "Measured in t CO2eq/ha",
       caption = "Note: Includes emissions from organic and inorganic nitrogen use. The analysis assumes fertilizer is only applied to Cropland",
       x = "",
       y = "")+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "black",
                                  face = "bold",
                                  size = 10))+
  scale_color_manual(values = c("#004c6d", "red"))+
  scale_fill_manual(values = c("#004c6d", "red"))


ggsave("~/Simpson Centre/NIR/02-Data/FertHA.png", unit = "cm", width = 23, height = 16, dpi = "retina")

#------------------------------------------------------------------------------#
# Canadian Crop Production overview slide
#------------------------------------------------------------------------------#
Fertilizer.EM <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")|>
  filter(Country == "CAN",
         Year >= 2005)|>
  select(2,3, 19)|>
  spread(Emission.Source, kt.CO2eq)


Fertilizer.N <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")|>
  filter(Country == "CAN",
         Year >= 2005)|>
  select(2,3,4)|>
  spread(Emission.Source, Application)|>
  rename("Inorganic"= 2, 
         "Organic" = 3)

EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")|>
  filter(Year >=2005 & Region == "Canada" & Sector == "Crop Production")|>
  select(1,9)

Crop.Production<-full_join(EM.Sector, Fertilizer.N)
Crop.Production<-full_join(Crop.Production, Fertilizer.EM)

Crop.Total<-Crop.Production|>
  rename(`Total Emissions` = CO2eq,
         `Inorganic Nitrogen` = 3,
         `Organic Nitrogen` = 4, 
         `Inorganic Emissions` = 5,
         `Organic Emissions` = 6)|>
  mutate(`Inorganic Emissions` = `Inorganic Emissions`/1000,
         `Organic Emissions` = `Organic Emissions`/1000)|>
  gather(Metric, Value, 2:6)

Total.Crop.2005<-Crop.Total|>
  filter(Year == 2005)|>
  rename(Y.2005 = 3)|>
  select(-1)


Crop.Total<-full_join(Crop.Total, Total.Crop.2005)|>
  mutate(Y.2005 = as.numeric(Y.2005),
         Value = as.numeric(Value),
         d.2005 = ((Value - Y.2005)/Y.2005)*100)|>
  mutate(Type = ifelse(Metric %in% c("Total Emissions", "Inorganic Emissions", "Organic Emissions"), "Emission Level", "N Application"),
         Metric = ifelse(Metric %in% c("Total Emissions", "Total Nitrogen"), "Total", 
                         ifelse(Metric %in% c("Inorganic Nitrogen", "Inorganic Emissions"), "Inorganic", "Organic")))
T.Nitrogen<-Crop.Total|>
  filter(Type == "N Application")|>
  group_by(Year)|>
    summarise(Value = sum(Value),
              Y.2005 = sum(Y.2005))|>
  mutate(d.2005 = ((Value - Y.2005)/Y.2005)*100,
         Type = "N Application",
         Metric = "Total")

Crop.Total<-rbind(Crop.Total, T.Nitrogen)

Crop.Total$Metric <-factor(Crop.Total$Metric, levels = c("Total", "Inorganic", "Organic"))


ggplot(Crop.Total, aes(Year, d.2005))+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_line(aes(color = Metric, linetype = Type), size = 2)+
  geom_point(aes(fill = Metric), size = 4, color = "#333333", shape = 21)+
  scale_color_manual(values = c("red", "#004c6d", "#c1e7ff"))+
  scale_fill_manual(values = c("red", "#004c6d", "#c1e7ff"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  labs(title = "Changes in Emission and Activity from Crop Production from 2005 to 2021",
       subtitle = "Percent Change measured from a 2005 baseline, values exclude LULUC",
       x = "",
       y = "Percent Change (%)",
       caption = "Note: Values used in the analysis for Nitrogen Use was reported in Table 3.D. Emission levels were estimated using reported emission factors")+
  geom_text_repel(data = subset(Crop.Total, Year == 2021),
                  aes(x = Year, y = d.2005, label = paste0(" ", round(d.2005, 0), "%")), 
                  color = "#333333",
                  force        = 0.5,
                  nudge_x      = 0.25,
                  direction    = "y",
                  hjust        = 0,
                  segment.size = 0.2,
                  fontface = "bold", 
                  size = 4)

ggsave("~/Simpson Centre/NIR/02-Data/Crop_Overview.png", unit = "cm", width = 23, height = 16, dpi = "retina")


#-------------------------------------------------------------------------------#
# Total and Net Emissions from Crop Production
#-------------------------------------------------------------------------------#

CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")

CP <- CropProduction|>
  filter(Year == 2021)|>
  mutate(Rank = rank(Total.CO2eq.ha),
         Country = reorder(Country, CO2eq.ha),
         highlight = ifelse(Country %in% c("AUS", "CAN", "EUA", "USA", "RUS", "FRA"), T, F))|>
  filter(Rank <= 15)|>
  select(Year, Country, highlight, Total.CO2eq.ha, CO2eq.ha)|>
  rename(`Including LULUC` = Total.CO2eq.ha, 
         `Excluding LULUC` = CO2eq.ha)|>
  gather(Measurement, Value, 4:5)
  


ggplot(CP, aes(Country, Value, fill = Measurement))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("#004c6d", "#5b87a8"))+
  scale_color_manual(values = c("#333333", "red"))+
  geom_hline(yintercept = 0, color = "#333333")+
  labs(title = "Total and Net Emissions from Crop Production and Agricultural Land Use",
       subtitle = "Measured in t CO2eq/ha",
       caption = "Note: Analysis assumes all emissions from production occure on cropland",
       x = "",
       y = "")+
  theme_light()+
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  geom_text(data = subset(CP, highlight != T),
            aes(x = Country,
                y = 0,
                label = paste0(Country)),
            color = "#333333",
            hjust = "center",
            vjust = 1.25,
            size = 4,
            fontface = "bold")+
  geom_text(data = subset(CP, highlight == T),
            aes(x = Country,
                y = 0,
                label = paste0(Country)),
            color = "red",
            hjust = "center",
            vjust = 1.25,
            size = 4,
            fontface = "bold")+
  geom_text(data = subset(CP, highlight == T),
            aes(x = Country,
                y = Value,
                label = paste0(round(Value, 2))),
            position = position_dodge(width = 0.9),
            color = "red",
            hjust = "center",
            vjust = -.5,
            size = 3,
            fontface = "bold")+
  geom_text(data = subset(CP, highlight != T),
            aes(x = Country,
                y = Value,
                label = paste0(round(Value, 2))),
            position = position_dodge(width = 0.9),
            color = "#333333",
            hjust = "center",
            vjust = -.5,
            size = 3,
            fontface = "bold")
  scale_y_continuous(limits = c(-1,6), breaks = seq(-1,6,1), expand = expansion(mult = c(0.01, 0.1)))

ggsave("~/Simpson Centre/NIR/02-Data/CP.png", unit = "cm", width = 23, height = 16, dpi = "retina")

#-------------------------------------------------------------------------------#

         









  

