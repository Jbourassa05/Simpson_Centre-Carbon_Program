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

#-------------------------------------------------------------------------------#

# CRF Table Extraction ----

#-------------------------------------------------------------------------------#
# Data used in the analysis was downloaded from the UNFCCC National Inventory Submission site: 
# https://unfccc.int/process-and-meetings/transparency-and-reporting/reporting-and-review-under-the-convention/greenhouse-gas-inventories-annex-i-parties/national-inventory-submissions-2023
#-------------------------------------------------------------------------------#

setwd("~/Simpson Centre/NIR/2023 CRF Tables") # Will need to be reset for working directory ***

# 2023 NIR as of 04/20/2023 missing Germany, France, Czechia, and Ukraine, Country list includes EU average (EUA)

CRF.1 <-c("AUT", "BEL", "BGR", "BLR", "CAN", "CHE", "CYP", "DNK",
          "ESP", "EST", "EUA", "FIN", "GBR", "GRC","HRV", "HUN", "IRL",
          "ISL", "ITA","JPN", "KAZ", "LIE","LTU", "LUX", "LVA", "MLT",
          "NLD", "NOR","NZL", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", 
          "SWE", "TUR", "USA") 

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
             `Milk Yield `= 4, # (Kg Milk/day)
             Work = 5, # (hours/day)
             Pregnant = 6, # (%) 
             DE = 7, # Digestibility of feed (%)
             GE = 8)|> # Gross energy (MJ/day)
      select(1,2,4,7)|>
      mutate(`Cattle Type` = tolower(`Cattle Type`))|>
      filter(`Cattle Type` %in% Names)
    
    
    join<-full_join(x,y) # joining two tables
    
    Table.3.A<-rbind(Table.3.A,join) #Table 3.A Enteric Fermentation for cattle production combining both Table 3.As1 and Table 3.As2
    
#-------------------------------------------------------------------------------#
# Table 3b Manure Management: Cattle
#-------------------------------------------------------------------------------#
# CH4 Emissions
#-------------------------------------------------------------------------------#    
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
              Pasutre = 9, 
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
      
    Table.3.B <-rbind(Table.3.B ,join) # Combination of Table 3.B(a)s1 and Table 3.B(b)

#-------------------------------------------------------------------------------#
# Table 3.D Agricultural Soils 
#-------------------------------------------------------------------------------#

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




#-------------------------------------------------------------------------------#

# CRF Type 2 Loop ----

#-------------------------------------------------------------------------------# 

CRF.2 <-c("AUS")


