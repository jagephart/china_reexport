# TITLE: make_tableS1
# AUTHOR: Bixaun Yang

# Load cleaned data
source("Scripts/clean_data.R")

#----------------------------------------------------------------------------------------------------#
#                         Estimates of distant-water fishing (Table S1)
#----------------------------------------------------------------------------------------------------#
## (1) Estimates based on FAO's standard definition
production.dwf <- production %>% filter(Production_source=="Capture production"& (Fishing_area !="Asia - Inland waters" &
                                                                                    Fishing_area !="Pacific, Northwest"))
production.dwf <- melt(production.dwf[,c(3,7:76)],id.vars = c("Name"))
production.dwf$Year <- as.numeric(substr(production.dwf$variable,2,5))
colnames(production.dwf)[colnames(production.dwf)=="value"] <- "Production"
production.dwf <- merge(production.dwf, match_production,by="Name",all.x=T)
production.dwf <- production.dwf %>% 
  filter(Year>=2012, Year<=2019) %>% 
  group_by(Species) %>% 
  summarize(Production_dwf=sum(Production)/1000)

## (2) Estimates based on FAO (2017) 
production.dwf.2017 <- production %>% filter(Production_source=="Capture production"& (Fishing_area !="Pacific, Northwest"))

production.dwf.2017 <- melt(production.dwf.2017[,c(3,7:76)],id.vars = c("Name"))
production.dwf.2017$Year <- as.numeric(substr(production.dwf.2017$variable,2,5))
colnames(production.dwf.2017)[colnames(production.dwf.2017)=="value"] <- "Production"
production.dwf.2017 <- merge(production.dwf.2017, match_production,by="Name",all.x=T)
production.dwf.2017 <- production.dwf.2017 %>% 
  filter(Year>=2012, Year<=2019) %>% 
  group_by(Species) %>% 
  summarize(Production_dwf=sum(Production)/1000)

## (3) Estimates based on Chinese Fishery Yearbook. Data is merged in the table with excel.


write.csv(production.dwf, "Outputs/TableS1_FAOdwf.csv", row.names = FALSE)
