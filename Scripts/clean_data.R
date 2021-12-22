# TITLE: clean_data
# AUTHOR: Bixuan Yang
# DESCRIPTION: 
## This script contains the data cleaning code for the paper by
## Frank Asche, Bixuan Yang, Jessica A. Gephart, Martin D. Smith, James L. Anderson, Edward V. Camp, 
## Taryn M. Garlock, David C. Love, Atle Oglend, and Hans-Martin Straume. China's seafood imports - not for domestic consumption? 

#----------------------------------------------------------------------------------------------------#
#                             Input trade and production datasets
#----------------------------------------------------------------------------------------------------#
imports_q <- read.csv("Data/import_quantity.csv")
exports_q <- read.csv("Data/export_quantity.csv")
production<-read.csv("Data/production_quantity.csv")

## Reshape wide datasets into long form
library("reshape2")
imports_q.l <- melt(imports_q[,c(2,6:40)],id.vars = c("Commodity..Name."))
imports_q.l$Year <- as.numeric(substr(imports_q.l$variable,3,6))
colnames(imports_q.l)[colnames(imports_q.l)=="value"] <- "Import_quantity"

exports_q.l <- melt(exports_q[,c(2,6:40)],id.vars = c("Commodity..Name."))
exports_q.l$Year <- as.numeric(substr(exports_q.l$variable,3,6))
colnames(exports_q.l)[colnames(exports_q.l)=="value"] <- "Export_quantity"

production.l <- melt(production[,c(3,7:76)],id.vars = c("Name"))
production.l$Year <- as.numeric(substr(production.l$variable,3,6))
colnames(production.l)[colnames(production.l)=="value"] <- "Production"


#----------------------------------------------------------------------------------------------------#
#                     Group imports and production by species
#----------------------------------------------------------------------------------------------------#
## Input the spreadsheets we made to classify FAO products to species
match_trade <- read.csv("Data/match_names_trade.csv")  
match_production <- read.csv("Data/match_names_prod.csv") 

imports_q.l <- merge(imports_q.l,match_trade,by="Commodity..Name.",all.x = T)
imports_q.12_19 <- imports_q.l %>% 
  filter(Species !="-") %>% 
  filter(Year>=2012 & Year<=2019) %>% 
  group_by(Species) %>% 
  summarize(Import_quantity=sum(Import_quantity)/1000) %>% 
  arrange(desc(Import_quantity)) %>% 
  mutate(share=Import_quantity/sum(Import_quantity)) 

exports_q.l <- merge(exports_q.l,match_trade,by="Commodity..Name.",all.x = T)
exports_q.12_19 <- exports_q.l %>% 
  filter(Species !="-") %>% 
  filter(Year>=2012 & Year<=2019) %>% 
  group_by(Species) %>% 
  summarize(Export_quantity=sum(Export_quantity)/1000) %>% 
  arrange(desc(Export_quantity))

production.l <- merge(production.l,match_production,by="Name",all.x = T)
production.12_19 <- production.l %>% 
  filter(Year>=2012 & Year<=2019) %>% 
  group_by(Species) %>% 
  summarize(Production=sum(Production)/1000) %>% 
  arrange(desc(Production))

#----------------------------------------------------------------------------------------------------#
#                 Select the top 15 imported, exported, and 20 most produced species
#----------------------------------------------------------------------------------------------------#
## Top 15 imported species
Top_15_imports <- imports_q.12_19 %>% 
  slice(1:17) %>% 
  filter(Species !="Unspecified fish", Species !="Unspecified crustaceans and mollusc") %>% ## excludes unspecified species
  mutate(select=1)## select the top 15 imported species

## Top 15 exported species
Top_15_exports <- exports_q.12_19 %>% 
  slice(1:17) %>% 
  filter(Species !="Unspecified fish", Species !="Unspecified crustaceans and mollusc") %>% ## excludes unspecified species
  mutate(select=1)## select the top 15 exported species

## Top 20 produced species
Top_20_produced <- production.12_19 %>% 
  slice(1:22) %>% 
  filter(Species !="Unspecified fish", Species !="Unspecified crustaceans and mollusc") %>% ## excludes unspecified species
  mutate(select=1)

selected_species <- data.frame(unique(rbind(Top_15_imports[,c(1,4)],Top_15_exports[,c(1,3)],Top_20_produced[,c(1,3)])))
selected_species <- selected_species %>% 
  filter(Species !="Gadiformes") ## exclude unspecified Gadiformes

#----------------------------------------------------------------------------------------------------#
#                 Calculate live-weight equivalents for the selected species
#----------------------------------------------------------------------------------------------------#
## Given that HS codes have been updated in 2017, we calculate live-weights equivalents for the period 2012-2016 and
## 2017-2019 separately.

## Import conversion factors
# Based on 2012 HS codes
cf_2012 <- read.csv("Data/conversion_factors_2012.csv")

names(cf_2012) <- c("HS_code_2012","Species","Product form","cf")
cf_2012$cf <- as.numeric(cf_2012$cf) ## Import conversion factors based on 2012 HS codes

# Based on 2017 HS codes
cf_2017 <- read.csv("Data/conversion_factors_2017.csv") ## Import conversion factors based on 2017 HS codes
colnames(cf_2017) <- c("HS_code_2017","Species","Product form","cf")
cf_2017$cf <- as.numeric(cf_2017$cf)

## Imports, 2012-2016
imports_q.12_16 <- imports_q.l %>% 
  filter(Species !="-") %>% 
  filter(Year>=2012 & Year<=2016) %>% 
  group_by(Species,HS_code_2012) %>% 
  summarize(Import_quantity=sum(Import_quantity,na.rm = T)/1000)
imports_q.12_16 <- merge(imports_q.12_16,selected_species,by="Species",all.x = T) %>% filter(select==1)

imports_q.12_16 <- merge(imports_q.12_16,cf_2012[,c(1,4)],by=c("HS_code_2012"),all.x = T) ## conversion factors and imports are merged by HS codes
imports_q.12_16$Import_live_weight <- imports_q.12_16$Import_quantity*imports_q.12_16$cf ## calculate live weights
imports_q.12_16.lw <- imports_q.12_16 %>% 
  group_by(Species) %>% 
  summarize(Import_quantity=sum(Import_quantity,na.rm = T),Import_live_weight=sum(Import_live_weight,na.rm = T))

## Exports, 2012-2016
exports_q.12_16 <- exports_q.l %>% 
  filter(Export_quantity !=0) %>% 
  filter(Year>=2012 & Year<=2016) %>% 
  group_by(Species,HS_code_2012) %>% 
  summarize(Export_quantity=sum(Export_quantity,na.rm = T)/1000)
exports_q.12_16 <- merge(exports_q.12_16,selected_species,by="Species",all.x = T) %>% filter(select==1)

exports_q.12_16 <- merge(exports_q.12_16,cf_2012[,c(1,4)],by=c("HS_code_2012"),all.x = T) ## conversion factors and imports are merged by HS codes
exports_q.12_16$Export_live_weight <- exports_q.12_16$Export_quantity*exports_q.12_16$cf ## calculate live weights
exports_q.12_16.lw <- exports_q.12_16 %>% 
  group_by(Species) %>% 
  summarize(Export_quantity=sum(Export_quantity,na.rm = T),Export_live_weight=sum(Export_live_weight,na.rm = T))

## Imports, 2017-2019
imports_q.17_19 <- imports_q.l %>% 
  filter(Import_quantity !=0) %>% 
  filter(Year>=2017 & Year<=2019) %>% 
  group_by(Species,HS_code_2017) %>% 
  summarize(Import_quantity=sum(Import_quantity,na.rm = T)/1000)
imports_q.17_19 <- merge(imports_q.17_19,selected_species,by="Species",all.x = T) %>% filter(select==1)


imports_q.17_19 <- merge(imports_q.17_19,cf_2017[,c(1,4)],by=c("HS_code_2017"),all.x = T) ## conversion factors and imports are merged by HS codes
imports_q.17_19$Import_live_weight <- imports_q.17_19$Import_quantity*imports_q.17_19$cf ## calculate live weights
imports_q.17_19.lw <- imports_q.17_19 %>% 
  group_by(Species) %>% 
  summarize(Import_quantity=sum(Import_quantity,na.rm = T),Import_live_weight=sum(Import_live_weight,na.rm = T))

## Exports, 2017-2019
exports_q.17_19 <- exports_q.l %>% 
  filter(Export_quantity !=0) %>% 
  filter(Year>=2017 & Year<=2019) %>% 
  group_by(Species,HS_code_2017) %>% 
  summarize(Export_quantity=sum(Export_quantity,na.rm = T)/1000)
exports_q.17_19 <- merge(exports_q.17_19,selected_species,by="Species",all.x = T) %>% filter(select==1)

exports_q.17_19 <- merge(exports_q.17_19,cf_2017[,c(1,4)],by=c("HS_code_2017"),all.x = T) ## conversion factors and imports are merged by HS codes
exports_q.17_19$Export_live_weight <- exports_q.17_19$Export_quantity*exports_q.17_19$cf ## calculate live weights
exports_q.17_19.lw <- exports_q.17_19 %>% 
  group_by(Species) %>% 
  summarize(Export_quantity=sum(Export_quantity,na.rm = T),Export_live_weight=sum(Export_live_weight,na.rm = T))

## Combine the two sample periods into one
imports_q.lw <- merge(imports_q.12_16.lw,imports_q.17_19.lw,by="Species")
imports_q.lw$Import_quantity <- imports_q.lw$Import_quantity.x+imports_q.lw$Import_quantity.y 
imports_q.lw$Import_live_weight <- imports_q.lw$Import_live_weight.x+imports_q.lw$Import_live_weight.y

exports_q.lw <- merge(exports_q.12_16.lw,exports_q.17_19.lw,by="Species")
exports_q.lw$Export_quantity <- exports_q.lw$Export_quantity.x+exports_q.lw$Export_quantity.y
exports_q.lw$Export_live_weight <- exports_q.lw$Export_live_weight.x+exports_q.lw$Export_live_weight.y

## Subset production dataset to only include selected species
production.s <- merge(production.12_19,selected_species,by="Species",all.x = T) %>% filter(select==1)

