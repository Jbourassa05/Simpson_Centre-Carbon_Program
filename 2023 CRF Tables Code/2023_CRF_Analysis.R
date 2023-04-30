#-------------------------------------------------------------------------------#

# 2023 NIR DATA -Data used in "Canada’s 2023 National Inventory Submission" webinar on May 2, 2023

# Webinar link: 

# by: Joshua Bourassa

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

# System Setup ----

#-------------------------------------------------------------------------------#
#Loading Packages

library(tidyverse)
library(data.table)
library(readxl)
library(ggrepel)

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

#-------------------------------------------------------------------------------#

# Data Cleaning For Analysis ----

#-------------------------------------------------------------------------------#

Table.3.A<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")
Table.3.B<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")
Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
Table.4<-read_csv("~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")


#-------------------------------------------------------------------------------#
# Data Cleaning for Cattle Variables + Analysis
#-------------------------------------------------------------------------------#

## Manure Management

### The section is collapsing rows into Type A groups (Dairy and Non-Dairy Cattle)
### This section is also calculating total CH4 and N2O from Managed Manure

Table.3.B<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_B_2023.csv")|>
  select(9,10,1, 8,2,7,17,22,28, 31,32)|>
  mutate(across(c("Year","Population", "CH4.kt","Pasture","Direct.N2O"), as.numeric),
         Pasture = ifelse(is.na(Pasture), 0, Pasture),
         CH4.kt = ifelse(is.na(CH4.kt), 0, CH4.kt),
         Direct.N2O = ifelse(is.na(Direct.N2O), 0, Direct.N2O))|>
  filter(!is.na(Population))
  
#-------------------------------------------------------------------------------#         
### Correcting for Missing Values or Zero Values
  ### EU average used for N2O.Runoff.IEF for EU countries
  # Canadian Values are used for the United States
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

## Emissions from PRP
### IPCC classifies dung and urine deposited by grazing animals (PRP) as emissions from Agricultural soils
### This section is selecting PRP emission factors from Table 3.D and adding them to emissions from manure management. 
### Indirect emissions from PRP are estimated using reported IEF or IPCC default values with values are not reported

Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")|>
  filter(Emission.Source %in% c("N from grazing animals", "Atmospheric deposition","leaching and run-off"))|>
  select(1,2,3,6,9,10)

Table.3.D.Indirect<-Table.3.D|>
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
  mutate(EF3.PRP = ifelse(is.na(EF3.PRP), 0.004, EF3.PRP), # IPCC 2019 refinement aggregated default value:  TABLE 11.1
         EF4.NVOL = ifelse(is.na(EF4.NVOL), 0.01, EF4.NVOL), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         EF5.Leaching = ifelse(is.na(EF5.Leaching), 0.011, EF5.Leaching), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracGASM = ifelse(is.na(FracGASM), 0.21, FracGASM), # IPCC 2019 refinement aggregated default value:  TABLE 11.3
         FracLEACH = ifelse(is.na(FracLEACH), 0.24, FracLEACH)) # IPCC 2019 refinement aggregated default value:  TABLE 11.3

## Total Emissions from Manure - indirect emissions from MM

MM<-full_join(Table.3.B, Table.3.D)|>
  mutate(PRP.N2O = ((N.Pasture*EF3.PRP)*(44/28))/10^6, # See Equation 11.1 in 2019 Refinement N2O-N_prp
         PRP.Vol = (((N.Pasture*FracGASM)*EF4.NVOL)*(44/28))/10^6,
         PRP.Leach = (((N.Pasture*FracLEACH)*EF5.Leaching)*(44/28))/10^6,
         PRP.Ind = PRP.Vol+PRP.Leach, # See Equation 11.10 and 11.11 in 2019 Refinement
         EM.MM = (CH4.MM*25) + ((N2O.MM+PRP.N2O+PRP.Ind+Indirect.N2O)*298))

## Enteric Methane Emissions

Table.3.A<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_A_2023.csv")|>
  mutate(across(c("Year","Population", "GE", "Ym", "IEF","CH4.kt", "Total.Pop", "Share.Pop","Weight", "Milk Yield", "DE"), as.numeric),
         `Milk Yield` = ifelse(Type.A == "Non-Dairy Cattle", 0, `Milk Yield`),
         W.Weight = Weight*Share.Pop,
         W.GE = GE*Share.Pop,
         W.Ym = Ym*GE)|>
  filter(!is.na(CH4.kt)|!is.na(Population))|>
  group_by(Type.A, Year, Country)|>
  summarise(Population = sum(Population),
            EM.CH4 = sum(CH4.kt),
            GE = sum(W.GE),
            Ym = sum(W.Ym),
            Weight = sum(W.Weight),
            `Milk Yield` = sum(`Milk Yield`))


# Correcting for missing Values or large outliers

EU<-Table.3.A|>filter(Country == "EUA")|>select(Year, Weight,  Type.A)|>rename(Weight.EU = Weight)
USA <- Table.3.A|>filter(Country == "USA")|>select(Year,Type.A, Weight)|>rename(Weight.US = Weight)

Table.3.A<-full_join(Table.3.A, EU)
Table.3.A<-full_join(Table.3.A, USA)
  
Enteric<-Table.3.A|>
  mutate(Weight = ifelse(!is.na(Weight), Weight.EU, Weight), # Correcting for NA Values
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
                           
### Fertilizer Emissions (Synthetic induced N2O emissions to CO2 emissions from Urea and carbon Containing fertilizers)

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
         T.Frac.Leach = `FracLEACH-(H)`*Application)

F.EM.Indirect<-Table.3.D|>
  filter(Emission.Source %in% c("Atmospheric deposition", "leaching and run-off"))|>
  select(1,2,3,6)|>
  spread(Emission.Source, IEF)|>
  rename(IEF.GAS = `Atmospheric deposition`,
         IEF.Leach = `leaching and run-off`)

F.EM <-full_join(F.EM.Direct, F.EM.Indirect)|>
  mutate(EM.GAS = ((T.Frac.GAS*IEF.GAS)*(44/28))/10^6,
         EM.Leach = ((T.Frac.Leach*IEF.Leach)*(44/28))/10^6,
         N2O.EM = Direct.EM+EM.GAS+EM.Leach)

F.CO2<-Table.3.D|>
  filter(Emission.Source %in% c("Urea", "Other carbon-containing fertlizers"))|>
  select(1,2,3,7)|>
  mutate(Emissions.kt = ifelse(is.na(Emissions.kt),0,Emissions.kt))|>
  spread(Emission.Source, Emissions.kt)

F.EM<-full_join(F.EM, F.CO2)|>
  mutate(`Other carbon-containing fertlizers` = ifelse(Emission.Source == "Organic N emissions", 0, `Other carbon-containing fertlizers`),
         Urea = ifelse(Emission.Source == "Organic N emissions", 0, Urea),
         Application = ifelse(Country == "AUS", Application*1000, Application), # AUS reported Application rate in Tonnes not kg
         kt.CO2eq = N2O.EM*298 + `Other carbon-containing fertlizers` + Urea,
         kg.CO2eq.kg.N = (kt.CO2eq*10^6)/Application)

Land.Area<-Table.4|>
  filter(`Land Use` == "Total Cropland")|>
  select(1,2,4)

F.EM<-full_join(F.EM,Land.Area)|>
  mutate(EM.Ha = kt.CO2eq/Total.Area.kha)

fwrite(F.EM, file = "~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")

#-------------------------------------------------------------------------------#
#Crop Production Based Emissions
#-------------------------------------------------------------------------------#
Table.3.D<-read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")
Table.4<-read_csv("~/Simpson Centre/NIR/02-Data/Table_4_2023.csv")              

EM.Crop<-Table.3.D|>
  select(1,2,3,7,11)|>
  filter(Emission.Source %in% c("Direct N2O emissions", "Indirect N2O emissions", "Urea", "Other carbon-containing fertlizers"))|>
  mutate(CO2eq = ifelse(Emission == "N2O", Emissions.kt*298, Emissions.kt),
         CO2eq = ifelse(is.na(CO2eq),0,CO2eq))|>
  group_by(Country, Year)|>
  summarise(CO2eq = sum(CO2eq))


EM.PRP<-Table.3.D|>
  filter(Emission.Source %in% c("N from grazing animals"))|>
  select(1,2,3,5,6,7,9,10)

EM.PRP.Indirect<-Table.3.D|>
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
  
EM.PRP[is.na(EM.PRP)]<-0

EM.Crop<-full_join(EM.Crop, EM.PRP)|>
  mutate(kt.CO2 = CO2eq-CO2.PRP)

Land.Use<-Table.4|>
  filter(`Land Use` == "Total Cropland")|>
  select(1,2,4,5)|>
  rename(Land.Use.EM = 4)

EM.Crop<-full_join(EM.Crop, Land.Use)|>
  mutate(Total.CO2eq = Land.Use.EM+kt.CO2,
         Total.CO2eq.ha = Total.CO2eq/Total.Area.kha,
         CO2eq.ha = kt.CO2/Total.Area.kha)

fwrite(EM.Crop, file = "~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")  
  
#-------------------------------------------------------------------------------#
## National Emission Trends + Intensity ----
#-------------------------------------------------------------------------------# 

Country.List<-c("AUS","AUT", "BEL", "BGR", "BLR", "CAN", "CHE", "CYP", "DNK",
                "ESP", "EST", "EUA", "FIN", "GBR", "GRC","HRV", "HUN", "IRL",
                "ISL", "ITA","JPN", "KAZ", "LIE","LTU", "LUX", "LVA", "MLT",
                "NLD", "NOR","NZL", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", 
                "SWE", "TUR", "USA", "DEU", "FRA", "CZE") 



Emissions <- fread("~/Simpson Centre/NIR/02-Data/National_Emissions_2023.csv")

Emissions.2005 <-filter(Emissions, Year == 2005)|>rename(CO2eq.2005 = 3)|>select(-2)

Emissions<-full_join(Emissions,Emissions.2005)|>
  mutate(d.2005 = ((CO2eq- CO2eq.2005)/CO2eq.2005)*100)

GDP <- read_csv("~/Simpson Centre/NIR/02-Data/2022_CanSectorData/World_GDP.csv")|>
  select(-1,-2,-3)|>
  rename(Country = 1)|>
  mutate(Country = ifelse(Country == "EUU", "EUA", Country))|>
  filter(Country %in% Country.List)|>
  gather(Year, GDP, 2:18)|>
  mutate(Year = as.numeric(Year))

Emissions<-full_join(Emissions, GDP)|>
  filter(!is.na(GDP))|>
  mutate(GDP = as.numeric(GDP),
         t.CO2 = CO2eq*10^6,
         EM.IN = t.CO2/(GDP/10^6))

fwrite(Emissions, file = "~/Simpson Centre/NIR/02-Data/Emission_Intensity.csv")

#-------------------------------------------------------------------------------#
## Emission Comparison Figure
#-------------------------------------------------------------------------------#

Emissions <- read.csv(file = "~/Simpson Centre/NIR/02-Data/Emission_Intensity.csv")|>
  filter(Year == 2021,
         !Country %in% c("SWE", "TUR", "LVA"))|>
  mutate(Rank = rank(-CO2eq))|>
  filter(Rank <=20)|>
  mutate(Country = fct_reorder(Country, d.2005),
         highlight = ifelse(Country %in% c("CAN", "USA", "EUA", "AUS", "RUS", "JPN"), T, F))

ggplot(Emissions,aes(Country, d.2005, color = highlight, fill = highlight))+
  geom_hline(yintercept = 0, color = "#333333")+
  #geom_segment(aes(x = Country, xend = Country, y = 0, yend = d.2005),  color = "#333333")+
  #geom_point(size = 4, fill = "#007574", color = "#333333", shape = 21)+
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
## Emission Intensity Figure
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
## National Emissions ----
#-------------------------------------------------------------------------------#  

#-------------------------------------------------------------------------------#
# Canadian Emissions
#-------------------------------------------------------------------------------# 

EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")

National.EM <- EM.Sector|>
  filter(Region == "Canada",
         Index == 0)|>
  mutate(Year = as.numeric(Year))

Sector<-EM.Sector|>
  filter(Region == "Canada",
         Index %in% c(0,1,44,16,17,25,45,33,40,36))|>
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
# Change in National Emissions
#-------------------------------------------------------------------------------#

Sector.2005<-Sector|>
  filter(Year == 2005)|>
  rename(CO2eq.2005 = Mt.CO2eq)|>
  select(-1)
Sector<-full_join(Sector, Sector.2005)|>
  mutate(d.EM = (Mt.CO2eq-CO2eq.2005)/CO2eq.2005)

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
## Agriculture Emissions ----
#-------------------------------------------------------------------------------#

Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")
CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")
Fertilizer <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")
Table.3.Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_Cattle_2023.csv")
Table.3 <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_2023.csv")
EM.Sector <- read_csv("~/Simpson Centre/NIR/02-Data/2023 Data/EN_GHG_Econ_Can_Prov_Terr.csv")



Ag.Sector<-EM.Sector|>
  filter(Index %in% c(37,38,39), 
         Region == "Canada")|>
  select(Year, Sector, CO2eq)

Ag.Can<-EM.Sector|>
  filter(Index %in% c(36), 
         Region == "Canada",
         Year %in% c(1990,1995,2000,2005,2010,2015,2021))|>
  select(Year, Sector, CO2eq)|>
  mutate(Sector = "Total")

Ag.Sector.labs = full_join(Ag.Sector, Ag.Can)|>
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
# Emission Source
#-------------------------------------------------------------------------------#

Cattle.Can<- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")|>
  filter(Country == "CAN",
         Year == 2021)|>
  select(1,5,12, 13,14,11, 20, 23)|>
  reframe(Enteric.Fermentation = sum(EM.CH4),
            Managed.Manure.CH4 = sum(CH4.MM),
            Managed.Manure.N2O = sum(N2O.MM),
            Managed.Manure.Ind = sum(Indirect.N2O), 
            N.Pasture  = sum(N.Pasture),
            Unmanged.Manure = sum(PRP.N2O),
            Unmanged.Manure.Ind = sum(PRP.Ind))

Enteric.Fermentation <-c(Cattle.Can$Enteric.Fermentation)
Managed.Manure.CH4 <-c(Cattle.Can$Managed.Manure.CH4)
Managed.Manure.N2O <-c(Cattle.Can$Managed.Manure.N2O + Cattle.Can$Managed.Manure.Ind)
N.Pasture <- c(Cattle.Can$N.Pasture)
Unmanged.Manure <-c(Cattle.Can$Unmanged.Manure)
Unmanged.Manure.Ind <-(Cattle.Can$Unmanged.Manure.Ind)


Can.Sum<- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_2023.csv")|>
  filter(Category %in% c("Enteric Fermentation", "Manure Management"),
         Country == 'CAN',
         Year == 2021)|>
  mutate(`Livestock Type` = "Other Livestock",
         CH4 = ifelse(Category == "Enteric Fermentation", CH4-Enteric.Fermentation,
                      ifelse(Category == "Manure Management", CH4-Managed.Manure.CH4, CH4)),
         N2O = ifelse(Category == "Manure Management", N2O - Managed.Manure.N2O, N2O))

Can.PRP <- read_csv("~/Simpson Centre/NIR/02-Data/Table_3_D_2023.csv")|>
  filter(Emission.Source == "N from grazing animals",
         Country == 'CAN',
         Year == 2021)|>
  mutate(N.PRP = Application - N.Pasture, 
         N2O.PRP = ((N.PRP*IEF)*(44/28))/10^6,
         N2O.IND = ((((N.PRP*FracGASF)*0.00739324918008)+((N.PRP*`FracLEACH-(H)`)*0.0075))*(44/28))/10^6,
         N2O = N2O.PRP+N2O.IND,
         CH4 = 0, 
         CO2 = 0,
         CO2eq.kt = N2O*298,
         `Livestock Type` = "Other Livestock", 
         Category = "Manure: Unmanaged")|>
  select(-3:-14)

Cattle.Can<- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")|>
  filter(Country == "CAN",
         Year == 2021)|>
  mutate(`Manure: Managed CH4` = CH4.MM,
         `Manure: Managed N2O`= N2O.MM+Indirect.N2O,
         `Manure: Unmanaged` = PRP.N2O+PRP.Ind)|>
  rename(`Livestock Type` = Type.A,
         `Enteric Fermentation` = EM.CH4)|>
  select(Year, Country, `Livestock Type`,`Manure: Managed CH4`, `Manure: Managed N2O`,`Manure: Unmanaged`, `Enteric Fermentation`)|>
  gather(Category, Value, 4:7)|>
  mutate(CO2 = 0,
         CH4 = ifelse(Category %in% c("Manure: Managed CH4","Enteric Fermentation"), Value, 0),
         N2O = ifelse(!Category %in% c("Manure: Managed CH4","Enteric Fermentation"), Value, 0),
         Category = ifelse(Category %in% c("Enteric Fermentation", "Manure: Unmanaged"), Category, "Manure: Managed"))|>
  group_by(Year, Country, `Livestock Type`, Category)|>
  summarise(CO2 = sum(CO2),
            CH4 = sum(CH4),
            N2O = sum(N2O))|>
  mutate(CO2eq.kt = CO2+(CH4*25)+(N2O*298))
  




Animal.Production<-rbind(Can.Sum, Can.PRP, Cattle.Can)|>
  mutate(Category = ifelse(Category == "Manure Management", "Manure: Managed", Category))|>
  mutate(CO2eq.kt = CO2+(CH4*25)+(N2O*298))








#-------------------------------------------------------------------------------#
install.packages("tidytext")
library(tidytext)


Cattle <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Cattle.csv")

Cattle.Int <- Cattle|>
  filter(Year %in% c(2021))|>
  group_by(Type.A)|>
  mutate(Rank = rank(-`Total Emissions kt.CO2.eq`))|>
  ungroup()|>
  filter(Rank <= 25)|>
  mutate(Country.Lab = Country,
         Country = reorder_within(Country,`kg.CO2.eq/head`, Type.A ),
         IEF = `kg.CO2.eq/head`/1000,
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA", "GBR", "JPN", "NDL", "CAN", "USA"), T, F))


ggplot(Cattle.Int, aes(Country, IEF))+
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = IEF), color = "#333333")+
  geom_point(size = 4, color = "#333333", fill = "#007574", shape = 21 )+
  geom_point(data = subset(Cattle.Int, highlight == T,), aes(x = Country, y = IEF),
             size = 4, color = "#333333", fill = "red", shape = 21)+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_text(data = Cattle.Int,
            aes(x = Country,
                y = IEF,
                label = paste0("  ",Country.Lab),
                color = highlight),
            hjust = "left",
            angle = 65)+
  labs(title = "Average Emissions per Head by Cattle Type",
       subtitle = "Measured in t CO2eq/head/year",
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
  scale_color_manual(values = c("#333333", "red"))+
  facet_wrap(~Type.A, scales = "free", nrow = 2,)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("~/Simpson Centre/NIR/02-Data/Cattle_IEF.png", width = 12, height = 7, dpi = "retina")  

#-------------------------------------------------------------------------------#
## Intensity EM/mcal
#-------------------------------------------------------------------------------#
  
Cattle.Int <- Cattle|>
  filter(Year %in% c(2021),
         Country != "JPN")|>
  group_by(Type.A)|>
  mutate(Rank = rank(-`Total Emissions kt.CO2.eq`))|>
  ungroup()|>
  filter(Rank <= 25)|>
  mutate(Country.Lab = Country,
         Country = reorder_within(Country,`EM/Mcal`, Type.A),
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA", "GBR", "NLD", "CAN", "USA"), T, F))

ggplot(Cattle.Int, aes(Country, `EM/Mcal`))+
  geom_col(aes(fill = highlight))+
  #geom_segment(aes(x = Country, xend = Country, y = 0, yend = `EM/Mcal`), color = "#333333")+
  #geom_point(size = 4, color = "#333333", fill = "#007574", shape = 21)+
  #geom_point(data = subset(Cattle.Int, highlight == T,), aes(x = Country, y = `EM/Mcal`),
  #           size = 4, color = "#333333", fill = "red", shape = 21)+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_text(data = subset(Cattle.Int, highlight == T),
            aes(x = Country,
                y = `EM/Mcal`,
                label = paste0(round(`EM/Mcal`,3)),
                color = highlight),
            vjust = -.5)+
  geom_text(data = Cattle.Int,
            aes(x = Country,
                y = 0,
                label = paste0(Country.Lab),
                color = highlight),
            vjust = 1.2)+
  labs(title = "Emission Intensity: Average Emissions per Mcal Consumed",
       subtitle = "Measured in kg CO2eq/Mcal",
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
  scale_fill_manual(values = c("#004C54", "red"))+
  scale_color_manual(values = c("#004C54", "red"))+
  facet_wrap(~Type.A, scales = "free", nrow = 2,)+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("~/Simpson Centre/NIR/02-Data/Cattle_Mcal.png", width = 12, height = 7, dpi = "retina")  

#-------------------------------------------------------------------------------#
## Intensity EM/kg Milk
#-------------------------------------------------------------------------------#

Cattle.Int.Dairy <- Cattle|>
  filter(Year %in% c(2021))|>
  mutate(Country.Lab = Country,
         Country = reorder_within(Country,`EM/kg.Milk`, Type.A ),
         highlight = ifelse(Country.Lab %in% c("NZL", "AUS", "EUA", "GBR", "JPN", "NDL", "CAN", "USA"), T, F))|>
  filter(Type.A == "Dairy Cattle")


ggplot(Cattle.Int.Dairy, aes(Country, `EM/kg.Milk`))+
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = `EM/kg.Milk`), color = "#333333")+
  geom_point(size = 4, color = "#333333", fill = "#007574", shape = 21)+
  geom_point(data = subset(Cattle.Int.Dairy, highlight == T,), aes(x = Country, y = `EM/kg.Milk`),
             size = 4, color = "#333333", fill = "red", shape = 21)+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_text(data = Cattle.Int.Dairy,
            aes(x = Country,
                y = `EM/kg.Milk`,
                label = paste0("  ",Country.Lab, ": ", round(`EM/kg.Milk`,2)),
                color = highlight),
            hjust = "left",
            angle = 90,
            size = 4,
            fontface = "bold")+
  labs(title = "Emission Intensity: Average Emissions per kg Milk Produced",
       subtitle = "Measured in kg CO2eq/kg Milk",
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
  scale_color_manual(values = c("#333333", "red"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("Cattle_Milk.png", width = 12, height = 7, dpi = "retina") 

#-------------------------------------------------------------------------------#
# Crop Production Figures ----
#-------------------------------------------------------------------------------#
CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")
Fertilizer <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_Fertilizer.csv")

Fert <- Fertilizer|>
  filter(Year == 2021,
         Emission.Source == "Inorganic N emissions")|>
  mutate(Country.Lab = Country,
         Country = reorder(Country, kg.CO2eq.kg.N),
         highlight = ifelse(Country.Lab %in% c("AUS", "JPN", "CAN", "EUA", "USA", "RUS", "KAZ"), T, F))



ggplot(Fert, aes(Country, kg.CO2eq.kg.N))+
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = kg.CO2eq.kg.N), color = "#333333")+
  geom_point(size = 4, color = "#333333", fill = "#007574", shape = 21)+
  geom_point(data = subset(Fert, highlight == T,), aes(x = Country, y = kg.CO2eq.kg.N),
             size = 4, color = "#333333", fill = "red", shape = 21)+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_text(data = Fert,
            aes(x = Country,
                y = kg.CO2eq.kg.N,
                label = paste0("  ",Country.Lab, ": ", round(kg.CO2eq.kg.N,1)),
                color = highlight),
            hjust = "left",
            angle = 90,
            size = 4,
            fontface = "bold")+
  labs(title = "Emission Intensity: Average Emissions per kg Nitrogen Applied",
       subtitle = "Measured in kg CO2eq/kg N",
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
  scale_color_manual(values = c("#333333", "red"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("Fertilizer_Rate.png", width = 12, height = 7, dpi = "retina")


#-------------------------------------------------------------------------------#
# Emissions Per ha
#-------------------------------------------------------------------------------#

Fert <- Fertilizer|>
  filter(Year == 2021)|>
  mutate(Country.Lab = Country,
         Country = reorder(Country, EM.Ha),
         highlight = ifelse(Country.Lab %in% c("AUS", "JPN", "CAN", "EUA", "USA", "RUS", "KAZ"), T, F))



ggplot(Fert, aes(Country, EM.Ha))+
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = EM.Ha), color = "#333333")+
  geom_point(size = 4, color = "#333333", fill = "#007574", shape = 21)+
  geom_point(data = subset(Fert, highlight == T,), aes(x = Country, y = EM.Ha),
             size = 4, color = "#333333", fill = "red", shape = 21)+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  geom_text(data = Fert,
            aes(x = Country,
                y = EM.Ha,
                label = paste0("  ",Country.Lab, ": ", round(EM.Ha,2)),
                color = highlight),
            hjust = "left",
            angle = 90,
            size = 4,
            fontface = "bold")+
  labs(title = "Emission Intensity: Average Emissions per hectare of Cropland",
       subtitle = "Measured in t CO2eq/ha",
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
  scale_color_manual(values = c("#333333", "red"))+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.2)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

ggsave("Fertilizer_ha.png", width = 12, height = 7, dpi = "retina")


#-------------------------------------------------------------------------------#
# Emissions from Crop Production Figure
#-------------------------------------------------------------------------------#

CropProduction <- read_csv("~/Simpson Centre/NIR/02-Data/NIR_2023_CropProduction.csv")

CP <- CropProduction|>
  filter(Year == 2021)|>
  filter(Total.CO2eq.ha<=10)|>
  mutate(Rank = rank(-kt.CO2))|>
  filter(Rank <= 25)|>
  mutate(Country.Lab = Country,
         Country = reorder(Country, CO2eq.ha),
         highlight = ifelse(Country.Lab %in% c("AUS", "JPN", "CAN", "EUA", "USA", "RUS", "KAZ"), T, F),
         LULUC = ifelse(CO2eq.ha>Total.CO2eq.ha, "Sink", "Source"))


ggplot(CP, aes(Country, CO2eq.ha, color = highlight, fill = highlight))+
  geom_hline(yintercept = 0, color = "#333333", linetype = "dashed")+
  #geom_col(color = "#333333", fill = "#007574")+
  geom_segment(aes(x = Country, xend = Country, y = CO2eq.ha, yend = Total.CO2eq.ha), size = 2)+
  geom_point(aes(x = Country, y = CO2eq.ha), size = 4,  color = "#333333", shape = 21)+
  geom_point(aes(x = Country, y = Total.CO2eq.ha, shape = LULUC), size = 4, color = "#333333")+
  scale_fill_manual(values = c("#004C54", "red"))+
  scale_color_manual(values = c("#004C54", "red"))+
  scale_shape_manual(Values = c(24,25))+
  theme(legend.position = "none")
  
  #geom_col(data = subset(CP, highlight == T,), aes(x = Country, y = CO2eq.ha),
  #         color = "#333333", fill = "red")+
  #geom_point(data = subset(CP, highlight == T,), aes(x = Country, y = Total.CO2eq.ha),
  #           size = 2, fill = "red", color = "#333333", shape = 21)+
  labs(title = "Emission Intensity: Average Emissions per Hectare of Cropland",
       subtitle = "Measured in t CO2eq/ha, Points Indicade Net Emissions With Carbon Removals",
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
  scale_color_manual(values = c("#333333", "red"))+
  scale_y_continuous(limits = c(-2,8),expand = expansion(mult = c(0.01, 0.1)))+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))+
  geom_text(data = CP,
            aes(x = Country,
                y = 0,
                label = paste0("  ",Country.Lab, ": ", round(CO2eq.ha, 1), "(", round(Total.CO2eq.ha,1),")"),
                color = highlight),
            hjust = "left",
            angle = -90,
            size = 4,
            fontface = "bold")
  
  
  
ggsave("Crop_ha.png", width = 12, height = 7, dpi = "retina")
         









  

