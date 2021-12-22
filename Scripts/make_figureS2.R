# TITLE: make_figureS1
# AUTHOR: Bixaun Yang

# Load packages
library(tidyverse)

# Load cleaned data
source("Scripts/clean_data.R")

#----------------------------------------------------------------------------------------------------#
#                                        Figure S2
#----------------------------------------------------------------------------------------------------#

## Group imports and exports by year
imports_q_year <- imports_q.l %>% 
  group_by(Year) %>% 
  summarize(Import_quantity=sum(Import_quantity))

exports_q_year <- exports_q.l %>% 
  group_by(Year) %>% 
  summarize(Export_quantity=sum(Export_quantity))

plot_df <- imports_q_year %>%
  full_join(exports_q_year, by = "Year") %>%
  rename("Import" = "Import_quantity", "Export" = "Export_quantity") %>%
  pivot_longer(Import:Export)

ggplot(plot_df, aes(x = Year, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_minimal() +
  theme(legend.position = "bottom")

png("Outputs/FigureS2.png")
ggplot(plot_df, aes(x = Year, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Quantity (t)") +
  theme_minimal() +
  theme(legend.position = "bottom")
dev.off()

# Presented Figure S2 was made with excel