# TITLE: make_figureS1
# AUTHOR: Bixuan Yang

# Load packages
library(tidyverse)

# Load cleaned data
source("Scripts/clean_data.R")

#----------------------------------------------------------------------------------------------------#
#                                        Figure S1
#----------------------------------------------------------------------------------------------------#
production_tech <- melt(production[,c(3,5,7:76)],id.vars = c("Name","Production_source"))
production_tech$Year <- as.numeric(substr(production_tech$variable,3,6))
colnames(production_tech)[colnames(production_tech)=="value"] <- "Production"

## Group production by year
production_tech <- production_tech %>% 
  filter(Year>=1950, Year<=2019) %>% 
  group_by(Year,Production_source) %>% 
  summarize(Production=sum(Production))

png("Outputs/FigureS1.png")
ggplot(production_tech, aes(x = Year, y = Production, fill = Production_source)) +
  geom_area(position = "stack") +
  labs(fill = "", y = "Production (t)") +
  theme_minimal() +
  theme(legend.position = "bottom") 
dev.off()

# Presented Figure S1 was made with excel
