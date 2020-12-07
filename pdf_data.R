setwd("C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/practice")

library(rmarkdown)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(kableExtra)

#dplyr::select data from "ALL DTA" Linelist sheet:
datefront <- ymd("2020-12-07")
dateback <- ymd("2020-11-15")
starter <- "Nov30 Report - "

#####################
# LINE LIST TESTING #
#####################
linelist2 <- read.csv("C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/DATA/Linelist/LINELIST_ALL.csv", 
                      stringsAsFactors=FALSE, na.strings=c("","NA"))

names(linelist2) <- c("Facility Name","Type","Lab Result","Specimine Collection","StartDate","First Name (Legal)","Last Name",
                      "DOB","Confirmed COVID case","Fever","Admission Date","Date of symptom onset","Acute respiratory symptoms (specify)",
                      "Other symptom notes","(Resident) Date symptom free","(Resident) Discharge/ transfer date","(Resident) Deceased date",
                      "(Resident) Known co-morbidities","(Resident) Type of test: PCR or Serology","(Resident) Room Number",
                      "(Resident) Dialysis? (Y/N)","(Resident) Dialysis facility name","(Resident) Dialysis schedule",
                      "(Resident) ventilator?   (Y/N)","Hospitalized? (Y/N)","Hospital name","Hospitalization date",
                      "(Staff) Phone number","(Staff) Occupation","(Staff) Units/Areas worked",
                      "(Staff) Name of other facility(s) where staff works","(Staff) Date returned to work","Notes","filename","UUID" )

linelist3 <- linelist2 %>%
  mutate(., `Specimine Collection` = ymd(`Specimine Collection`)) %>%
  mutate(., DOB = ymd(DOB)) %>%
  mutate(., `Admission Date` = ymd(`Admission Date`)) %>%
  mutate(., `Date of symptom onset` = ymd(`Date of symptom onset`)) %>%
  dplyr::select(., c("Facility Name",
                     "Type",
                     "Lab Result",
                     "Specimine Collection"))

#GRAPHING
linelist4 <- linelist3 %>%
  mutate(., Positive = ifelse(`Lab Result` == "POS", 1, 0)) %>%
  mutate(., Negative = ifelse(`Lab Result` == "NEG", 1, 0)) %>%
  filter(., `Specimine Collection` > "2020-02-15") %>%
  #CHANGE THIS AREA IF NEEDED
  filter(., `Specimine Collection` <= datefront) %>%            
  melt(., id.vars = c("Facility Name","Type", "Specimine Collection","Lab Result")) %>%
  #Cluster by week 
  group_by(.,
           week = week(ymd(`Specimine Collection`)),
           `Facility Name`,
           Type,
           `Lab Result`,
           variable,
           value
  ) %>%
  #Create new week variable
  mutate(., week2 = as.Date(paste(week, 1, sep="-"), "%U-%u")) %>%
  mutate(LTC = ifelse(`Facility Name` %in% c("Archie Hendricks",
                                             "Brookdale Tanque Verde",
                                             "Casas Adobes Post Acute Rehab Center",
                                             "Catalina Post Acute and Rehabilitation",
                                             "Copper Health Oro Valley",
                                             "Devon Gables Rehabilitation Center",
                                             "Encompass Health Rehabilitation Institute of Tucson",
                                             "Foothills Rehabilitation Center",
                                             "Handmaker Home for the Aging",
                                             "Haven of Saguaro Valley",
                                             "Haven of Tucson",
                                             "La Canada Care Center",
                                             "Life Care Center of Tucson",
                                             "Mountain View Care Center",
                                             "Park Avenue Health and Rehabilitation Center",
                                             "Pueblo Springs Rehabilitation Center",
                                             "Sabino Canyon Rehabilitation & Care Center",
                                             "Santa Rita Nursing & Rehabilitation Center",
                                             "Santa Rosa Care Center",
                                             "Sapphire of Tucson Nursing and Rehab",
                                             "Splendido at Rancho Vistoso",
                                             "The Center at Tucson",
                                             "Villa Maria Care Center: Drug & Alcohol Rehab",
                                             "Villa Maria Care Center: Long Term Care"),
                      "Skilled Nursing",
                      "Assisted Living"))

table(linelist4$`Facility Name`, useNA = "always")

#######################
# PIMA COUNTY TESTING #
#######################
pt.univ <- read.csv("C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/DATA/Test Results Tracker.csv",  stringsAsFactors=FALSE, na.strings=c("","NA"))

#table(pt.univ$`Setting Type`)
#table(pt.univ$`Result`)

pt.univ.a <- pt.univ %>%
  dplyr::select(., c("Agency.Name","Setting.Type", "Patient.Last.Name","Patient.First.Name",
                     "DOB","Age","Sex","Race","Ethnicity","Patient.Zip.Code",
                     "Collection.Date",
                     "Reported.Date","Result","Lab","Test.Type","Turnaround.Time"))

names(pt.univ.a) <- c("Facility Name","Setting Type","Patient Last Name","Patient First Name", "DOB","Age",
                      "Sex","Race","Ethnicity","Patient Zip Code","Collection Date","Reported Date",
                      "Result","Lab","Test Type","Turnaround Time" )

table(pt.univ.a$`Setting Type`)
table(pt.univ.a$`Result`)
table(pt.univ.a$`Facility Name`)

pt.univ2 <- pt.univ.a %>%
  dplyr::filter(., `Setting Type` %in% c(
    #"Behavioral Health",
    "Assisted Living",
    "Long-Term Care")) %>%
  dplyr::select(., c("Facility Name",
              "Collection Date",
              "Result",
              "Test Type",
              "Lab")) %>%
  #Clean lab names
  mutate(., Lab = ifelse(Lab == "AccuReference", "Accureference", Lab)) %>%
  #Clean facility name
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Amity", "Amity", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades", "Cascades of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Bridgewater at La Cholla", "Solterra Bridgewater Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Arroyo Gardens", "Arroyo Gardens Independent and Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Aspen Care II", "Aspen", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Desert Treausre", "Desert Treasure", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Elsa's Adult Care", "Elsa's Adult Care Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Encompass Health", "Encompass Health Rehabilitation Institute of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Jazmine'sAdult Care Home", "Jazmine's Adult Care Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Oasis Assisted living", "The Oasis at Fellowship Square Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Oasis Assisted Living", "The Oasis at Fellowship Square Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Oasis Assisted Living", "The Oasis at Fellowship Square Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "OASIS Assisted Living", "The Oasis at Fellowship Square Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sonora Behavorial Health Hospital", "Sonora Behavioral Health Hospital", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Brookdale", "Brookdale Tanque Verde", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Archie Hendricks Nursing", "Archie Hendricks", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Casas Adobes", "Casas Adobes Post Acute Rehab Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina Post Acute", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Devon Gables", "Devon Gables Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Encompass", "Encompass Health Rehabilitation Institute of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Foothills rehab", "Foothills Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Foothills Rehab", "Foothills Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Hacienda", "Hacienda at the River", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Handmaker", "Handmaker Home for the Aging", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven", "Haven of Saguaro Valley", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven Inc", "Haven of Saguaro Valley", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven of Saguaro", "Haven of Saguaro Valley", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven of Tucson", "Haven of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "La Canada Care Center", "La Canada Care Center", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Life Care", "Life Care Center of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Life Care Center", "Life Care Center of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Mountain View", "Mountain View Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Park Avenue", "Park Avenue Health and Rehabilitation Center", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Prestige", "Prestige Assisted Living", `Facility Name`)) %>% 
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Pueblo Springs", "Pueblo Springs Rehabilitation Center", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sabino Canyon", "Sabino Canyon Rehabilitation & Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Santa Rita Nursing", "Santa Rita Nursing & Rehabilitation Center", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Santa Rita Nursiing", "Santa Rita Nursing & Rehabilitation Center", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Santa Rosa Care Center", "Santa Rosa Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sapphire", "Sapphire of Tucson Nursing and Rehab", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Splendido", "Splendido at Rancho Vistoso", `Facility Name`)) %>% 
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Spendido of Rancho Vistoso", "Splendido at Rancho Vistoso", `Facility Name`)) %>%  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "St Luke's Home", "St. Luke's Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "St Lukes Home", "St. Luke's Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "St. Lukes Home", "St. Luke's Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Stafish Care Homes", "Starfish Care Homes", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sunrise at River Rd", "Sunrise at River Road", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sunrise At River Rd", "Sunrise at River Road", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sunrise At River Road", "Sunrise at River Road", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "The Forum", "The Forum at Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "The Forum Tucson", "The Forum at Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Villa Mari", "Villa Maria Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Villa Maria", "Villa Maria Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Villa maria", "Villa Maria Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Via Elegante", "Via Elegante, Tucson Mountains", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Curahealth", "Curahealth Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Foothills Rehab Center", "Foothills Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Park Ave Health and Rehab", "Park Avenue Health and Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Santa Rita Nursing Rehab ", "Santa Rita Nursing & Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sabino Canyon Rehab and Care Center ", "Sabino Canyon Rehabilitation & Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Archie Hendricks ", "Archie Hendricks", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Archie Hendricks  ", "Archie Hendricks", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Archie Hendricks Nursing ", "Archie Hendricks", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Arista ", "Arista", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Arroyo Gardens ", "Arroyo Gardens Independent and Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Aslan ", "Aslan", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Aspen ", "Aspen", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "AspenCare Home", "Aspen", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Atria Campana Del Rio ", "Atria Campana Del Rio", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Brookdale ", "Brookdale Tanque Verde", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Carlton Village ", "Carlton Village", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Casas Adobes ", "Casas Adobes Post Acute Rehab Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Casas Adobes Post Acute Rehab Center ", "Casas Adobes Post Acute Rehab Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades ", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades\t", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina Post Acute Rehab", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina Post Acute ", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina In Home Services ", "Catalina Group Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cherry's Assited Care Home", "Cherry's Assisted Care Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "CuraHealth ", "CuraHealth", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "CuraHealth", "CuraHealth", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Curahealth ", "Curahealth Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Desert Treasure ", "Desert Treasure", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Devon Gables ", "Devon Gables Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "El Rancho Encanto ", "El Rancho Encanto", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Elsa's Adult Care Home ", "Elsa's Adult Care Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Elsa's Adult Care ", "Elsa's Adult Care Home", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Encompass ", "Encompass Health Rehabilitation Institute of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Encompass Health ", "Encompass Health Rehabilitation Institute of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Fellowship Square ", "Fellowship Square", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Foothills Rehab ", "Foothills Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Fountains at La Cholla ", "Fountains at La Cholla", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Hacienda ", "Hacienda at the Canyon", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Hacienda at the Canyon ", "Hacienda at the Canyon", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Handmaker ", "Handmaker Home for the Aging", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven of Saguaro ", "Haven of Saguaro Valley", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven of Tucson ", "Haven of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Haven of Tucson  ", "Haven of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Hope Springs ", "Hope Springs", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Life Care ", "Life Care Center of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Life Care Center ", "Life Care Center of Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Maria Apuletegui", "Maria Apuletegui", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Marie Apaletegui", "Maria Apuletegui", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Marie Apaletgui", "Maria Apuletegui", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Mountain View ", "Mountain View Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Pacifica Senior Living ", "Pacifica Senior Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Prestige ", "Prestige Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sabino Canyon ", "Sabino Canyon Rehabilitation & Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Santa Rita Nursing ", "Santa Rita Nursing & Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Senior Care ", "Senior Care", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Solterra Bridgewater", "Solterra Bridgewater Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Splendido ", "Splendido at Rancho Vistoso", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Starfish Care Homes ", "Starfish Care Homes", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sunrise at River Rd ", "Sunrise at River Road", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sunshine Tucson", "Sunrise at River Road", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "The Forum ", "The Forum at Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Via Elegante\t\t\t\t\t\t\t", "Via Elegante, Tucson Mountains", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Villa Hermosa ", "Villa Hermosa", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Villa Maria ", "Villa Maria Care Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Watermark ", "Watermark at Continental Ranch", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades  ", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Catalina ", "Catalina Post Acute and Rehabilitation", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Infinity ", "Infinity", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Casa De Sunshine", "Casa De Sonshine Assisted Living", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Devon Gables Rehab", "Devon Gables Rehabilitation Center", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Hope Springs", "Hope Springs Memory Care", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "The Center of Tucson", "The Center at Tucson", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Watermark", "Watermark at Continental Ranch", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Harmony House Tucson", "Harmony House", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Sapphire Estate Rehab", "Sapphire of Tucson Nursing and Rehab", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Harmony House ", "Harmony House", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Bridgewater", "Solterra Bridgewater Assisted Living", `Facility Name`)) %>%
  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Archie HendricksNursing ", "Archie Hendricks", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Aspen Care Home", "Aspen", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Casa De Sonshine", "Casa De Sonshine Assisted Living", `Facility Name`)) %>%
  
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades of Tucson", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades of Tucsonof Tucsonof Tucson", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades of Tucsonof Tucsonof Tucson ", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades of Tucsonof Tucsonof Tucsonof Tucson", "Cascades", `Facility Name`)) %>%
  mutate(., `Facility Name` = ifelse(`Facility Name` == "Cascades of Tucsonof Tucsonof Tucsonof Tucsonof Tucson", "Cascades", `Facility Name`)) %>%
  
  
  #Remove facilities that do not appear in Django
  filter(., `Facility Name` != "Curahealth Tucson") %>%
  filter(., `Facility Name` != "Unknown") %>%
  filter(., `Facility Name` != "Catalina Pediatrics, PC") %>%
  
  #Reassign as Skilled Nursing or Assisted Living
  mutate(LTC = ifelse(`Facility Name` %in% c("Archie Hendricks",
                                             "Brookdale Tanque Verde",
                                             "Casas Adobes Post Acute Rehab Center",
                                             "Catalina Post Acute and Rehabilitation",
                                             "Copper Health Oro Valley",
                                             "Devon Gables Rehabilitation Center",
                                             "Encompass Health Rehabilitation Institute of Tucson",
                                             "Foothills Rehabilitation Center",
                                             "Handmaker Home for the Aging",
                                             "Haven of Saguaro Valley",
                                             "Haven of Tucson",
                                             "La Canada Care Center",
                                             "Life Care Center of Tucson",
                                             "Mountain View Care Center",
                                             "Park Avenue Health and Rehabilitation Center",
                                             "Pueblo Springs Rehabilitation Center",
                                             "Sabino Canyon Rehabilitation & Care Center",
                                             "Santa Rita Nursing & Rehabilitation Center",
                                             "Santa Rosa Care Center",
                                             "Sapphire of Tucson Nursing and Rehab",
                                             "Splendido at Rancho Vistoso",
                                             "The Center at Tucson",
                                             "Villa Maria Care Center"),
                      "Skilled Nursing",
                      "Assisted Living")) %>%
  filter(., Result %in% c("Positive","Negative"))

table(pt.univ2$`Facility Name`)

#ALL PCR RESULTS
pt.univ3 <- pt.univ2 %>%
  #select(., c("Facility Name", "Collection Date", "Result")) %>%
  mutate(., Positive = ifelse(Result == "Positive", 1, 0)) %>%
  mutate(., Negative = ifelse(Result == "Negative", 1, 0))  %>%
  mutate(`Collection Date` = mdy(`Collection Date`)) %>%
  melt(., id.vars = c("Facility Name","Collection Date","Result", "Test Type", "Lab","LTC"))  %>%
  group_by(., `Facility Name`,
           `Collection Date`,
           week = week(ymd(`Collection Date`)),
           variable,
           `Test Type`,
           `Lab`,
           `LTC`) %>%
  summarise(val2 = sum(value, na.rm = T)) %>%
  mutate(val2 = as.numeric(val2)) %>%
  filter(., `Collection Date` <= datefront) %>%
  filter(., `Collection Date` > "2020-01-01") %>%
  mutate(., week2 = as.Date(paste(week, 1, sep="-"), "%U-%u")) %>%
  arrange((variable)) 

#########
# MERGE #
#########

direct_ll <- linelist4 %>%
  ungroup() %>%
  rename(., `Patient Type` = `Type`) %>%
  mutate(., `Test Type` = NA) %>%
  mutate(., `Lab` = NA) %>%
  mutate(., `Data Source` = "Linelist") %>%
  dplyr::select(., c("Data Source","Facility Name","Specimine Collection","week","variable","Test Type","Lab","value","week2","Patient Type","LTC"))

direct_pchd <- pt.univ3 %>%
  rename(., `Specimine Collection` = `Collection Date`,
         `value` = `val2`) %>%
  mutate(., `Patient Type` = NA) %>%
  mutate(., `Lab Result` = NA) %>%
  mutate(., `Data Source` = "PCHD Testing") %>%
  dplyr::select(., c("Data Source","Facility Name","Specimine Collection","week","variable","Test Type","Lab","value","week2","Patient Type","LTC"))

direct_match1 <- as.data.frame(rbind(as.data.frame(direct_ll), as.data.frame(direct_pchd))) %>%
  mutate(., value = as.numeric(value)) %>%
  dplyr::select(., c("Data Source","Facility Name","Specimine Collection","week",
                     "variable","Test Type","Lab","value","week2","Patient Type","LTC")) %>% 
  filter(.,!is.na(`Facility Name`)) %>%
  filter(., `Specimine Collection` >= dateback)

direct_all <- as.data.frame(rbind(as.data.frame(direct_ll), as.data.frame(direct_pchd))) %>%
  mutate(., value = as.numeric(value)) %>%
  dplyr::select(., c("Data Source","Facility Name","Specimine Collection","week",
                     "variable","Test Type","Lab","value","week2","Patient Type","LTC")) %>% 
  filter(.,!is.na(`Facility Name`))

###############
# Villa Maria #
###############

vm.viewer <- direct_all %>% filter(.,`Facility Name` %in% c("Villa Maria Care Center",
                                       "Villa Maria Care Center: Long Term Care",
                                       "Villa Maria Care Center: Drug & Alcohol Rehab"))

villa.maria.ltc <- direct_match1 %>% filter(., `Facility Name` == "Villa Maria Care Center: Long Term Care") %>%
  filter(., variable == "Positive" & value == 1) %>%
  distinct(`Facility Name`)

villa.maria.drug <- direct_match1 %>% filter(., `Facility Name` == "Villa Maria Care Center: Drug & Alcohol Rehab") %>%
  filter(., variable == "Positive" & value == 1) %>%
  distinct(`Facility Name`)


villa.function <- function(villa1, villa2, dataset) {
  if (villa2  %in% "Villa Maria Care Center: Drug & Alcohol Rehab" & villa1  %in% "Villa Maria Care Center: Long Term Care") {
    mutate(dataset, `Facility Name` = ifelse(`Facility Name` %in% c("Villa Maria Care Center: Drug & Alcohol Rehab",
                                                                    "Villa Maria Care Center: Long Term Care"), 
                                               "Villa Maria Care Center", `Facility Name`))
    
  } else if (villa2  %in% "Villa Maria Care Center: Drug & Alcohol Rehab") {
    mutate(dataset, `Facility Name` = ifelse(`Facility Name` %in% c("Villa Maria Care Center"),
                                             "Villa Maria Care Center: Drug & Alcohol Rehab", `Facility Name`))
    
  } else if (villa1  %in% "Villa Maria Care Center: Long Term Care") {
    mutate(dataset, `Facility Name` = ifelse(`Facility Name` %in% c("Villa Maria Care Center"),
                                             "Villa Maria Care Center: Long Term Care", `Facility Name`))
    
  } else if (nrow(villa1) == 0 & nrow(villa2) == 0) {
    mutate(dataset, `Facility Name` = ifelse(`Facility Name`%in% c("Villa Maria Care Center: Drug & Alcohol Rehab",
                                                                   "Villa Maria Care Center: Long Term Care"),
                                             "Villa Maria Care Center", `Facility Name`))
  } 
}

direct_match <- villa.function(villa.maria.ltc,villa.maria.drug, direct_match1)

#Investigate specific claims
#XXXXX

#View facilities to be printed
sort(unique(direct_match$`Facility Name`), decreasing=F)
kable(sort(unique(direct_match$`Facility Name`), decreasing=F)) %>% kable_styling("striped", full_width = F)

#View kable, with and without linelist
kable(direct_match %>% filter(., `Specimine Collection` >= dateback & `Data Source` == "Linelist") %>% distinct(`Facility Name`) %>% arrange((`Facility Name`))) %>% kable_styling("striped", full_width = F)
kable(direct_match %>% filter(., `Specimine Collection` >= dateback & `Data Source` != "Linelist") %>% distinct(`Facility Name`) %>% arrange((`Facility Name`))) %>% kable_styling("striped", full_width = F)

#View in console, with and without linelist
direct_match %>% filter(., `Specimine Collection` >= dateback & `Data Source` == "Linelist") %>% distinct(`Facility Name`) %>% arrange((`Facility Name`))
direct_match %>% filter(., `Specimine Collection` >= dateback & `Data Source` != "Linelist") %>% distinct(`Facility Name`) %>% arrange((`Facility Name`))

#####################
# REPORT FACILITIES #
#####################

snf.out <- direct_match %>% filter(., `Specimine Collection` >= dateback & 
                                     LTC == "Skilled Nursing" &
                                     variable == "Positive" &
                                     value >= 1) %>% distinct(`Facility Name`)

snf.non <- direct_match %>% 
  filter(., `Specimine Collection` >= dateback & LTC == "Skilled Nursing" ) %>%
  subset(., !(`Facility Name` %in% snf.out$`Facility Name`)) %>%
  distinct(`Facility Name`)

alf.out <- direct_match %>% filter(., `Specimine Collection` >= dateback & 
                                     LTC == "Assisted Living" &
                                     variable == "Positive" &
                                     value >= 1) %>% distinct(`Facility Name`)

alf.non <- direct_match %>% 
  filter(., `Specimine Collection` >= dateback & LTC == "Assisted Living" ) %>%
  subset(., !(`Facility Name` %in% alf.out$`Facility Name`)) %>%
  distinct(`Facility Name`)


kable(sort(unique(snf.out$`Facility Name`), decreasing=F)) %>% kable_styling("striped", full_width = F)
kable(sort(unique(alf.out$`Facility Name`), decreasing=F)) %>% kable_styling("striped", full_width = F)
kable(sort(unique(snf.non$`Facility Name`), decreasing=F)) %>% kable_styling("striped", full_width = F)
kable(sort(unique(alf.non$`Facility Name`), decreasing=F)) %>% kable_styling("striped", full_width = F)

#Check to make sure there are no duplicates
snf.non %in% snf.out
alf.non %in% snf.out

#Generate outcome columns based on 4 facility types
direct_match2 <- direct_match %>%
  mutate(., outcome = ifelse(`Facility Name` %in% snf.out$`Facility Name`, "Outbreak - SNF",
                             ifelse(`Facility Name` %in% snf.non$`Facility Name`, "Non Outbreak - SNF",
                                    ifelse(`Facility Name` %in% alf.out$`Facility Name`, "Outbreak - ALF",
                                           ifelse(`Facility Name` %in% alf.non$`Facility Name`, "Non Outbreak- ALF",NA)))))

###############################
# MERGE CONTACT FOR OUTBREAKS #
###############################
cont <- read.csv("C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/DATA/contacts.csv", 
                      stringsAsFactors=FALSE, na.strings=c("","NA"))

contact <- cont %>%
  dplyr::select(., "Facility Name" = "Facility.Name",
         "Contact Phone" = "Contact.Phone",
         "Contact Email" = "Contact.Email")

#Set up the outbreak data to include facility name, testing result, data reported 
outbreaks <- direct_match %>% filter(., `Specimine Collection` >= dateback & 
                                     variable == "Positive" &
                                     value >= 1) %>% 
  select(., c("Data Source","Facility Name","Specimine Collection","value")) %>%
  group_by(., 
           `Data Source`,
           `Facility Name`,
           `Specimine Collection`
  ) %>%
  summarise(value = sum(value, na.rm = T))

out1 <- outbreaks %>%
  filter(., `Data Source` == "Linelist")

out2 <- outbreaks %>%
  filter(., `Data Source` == "PCHD Testing")

out3 <- left_join(out1, out2, by = "Facility Name") %>%
  select(., 2:4) %>%
  arrange((`Facility Name`))

outbreaks2 <- left_join(out3, contact, by = "Facility Name")


#Create a table of "outbreak facilities within the last two weeks
kable(outbreaks2) %>% kable_styling("striped", full_width = F)

write.csv(outbreaks2, file = "C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/Practice/outbreak_contacts.csv")

########
# PLOT #
########

plot <- as.data.frame(rbind(as.data.frame(direct_ll), as.data.frame(direct_pchd))) %>%
  mutate(., value = as.numeric(value)) %>%
  dplyr::select(., c("Data Source","Facility Name","Specimine Collection","week",
                     "variable","Test Type","Lab","value","week2","Patient Type","LTC")) %>% 
  filter(variable == "Positive" & `Data Source` == "Linelist")

ggplot(data = plot, aes(x = week2, #`Specimine Collection`,
                        y = value, 
                        fill = LTC)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("lightblue","darkblue")) +
  #ggtitle("COVID-19 PCR Test Results in Pima County LTC settings") +
  xlab("Specimen Collection Date") +
  ylab("Positive Count") +
  theme_hc() +
  scale_colour_hc() +
  facet_wrap(~LTC, ncol = 2) +
  labs(caption = "Graphs display weekly case counts for assisted living and skilled nursing facilities") +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.caption = element_text(size=12, face = "italic"))

ggsave("C:/Users/Person/OneDrive/z1_Research/ABRC/County/Protocol_advisory/practice/allplot.png", width = 10, height = 4)

#############
# DEBUGGING #
#############

#Debugging
debug <- direct_match %>% filter(., `Facility Name` == "Prestige Assisted Living") 
debug <- direct_match %>% filter(., `Facility Name` == "Devon Gables: Assisted Living") 
debug <- direct_match %>% filter(., `Facility Name` == "Sapphire of Tucson Nursing and Rehab") 
debug <- direct_match %>% filter(., `Facility Name` == "Sunshine Tucson") 
debug <- direct_match %>% filter(., `Facility Name` == "Splendido at Rancho Vistoso") 

debug <- direct_match %>% filter(., `Facility Name` == "Villa Maria Care Center" & variable == "Positive" & value == 1) 
debug <- direct_match %>% filter(., `Facility Name` == "Splendido at Rancho Vistoso" & variable == "Positive" & value == 1) 

#subgroup <- direct_match %>% filter(., `Facility Name` == "Santa Rita Nursing & Rehabilitation Center") 

#Temporary for speed
#direct_match <- direct_match %>% 
#  filter(., `Facility Name` %in% c("The Center at Tucson","Solterra Bridgewater Assisted Living","The Oasis at Fellowship Square Tucson"))
#facility <- "Sunshine Tucson"
#facility <- "The Center at Tucson"

for (fac in unique(direct_match2$outcome)){
  subgroup <- direct_match2[direct_match2$outcome == fac,]
  tryCatch( render("pdf_report.rmd",output_file = paste0(starter,fac,'.pdf')), error=function(e) NULL)  
}

#for (facility in unique(direct_match$`Facility Name`)){
#  subgroup <- direct_match[direct_match$`Facility Name` == facility,]
#  tryCatch( render("pdf_report.rmd",output_file = paste0(starter,facility,'.pdf')), error=function(e) NULL)  
#}
