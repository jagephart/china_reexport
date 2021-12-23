# TITLE: make_figure1
# AUTHOR: Jessica Gephart and Bixuan Yang

# Load packages
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Load cleaned data
source("Scripts/clean_data.R")

# Load data from Table S4
table_S4 <- read.csv("Outputs/tableS4.csv")

#----------------------------------------------------------------------------------------------------#
#                                       Figure 1
#----------------------------------------------------------------------------------------------------#
architypes <- data.frame(x = c(5, 5, 95, 95), 
                         y = c(45, 55, 45, 55),
                         label = c("Domestic-oriented production", 
                                   "Imports for domestic market", 
                                   "Export-oriented production",
                                   "Re-exports"))
## Plot production origin versus consumer destination, with points scaled by apparent consumption
# Manually jitter points to avoid overlapping
table_S4$consumer_dest_indicator[table_S4$Species=="Cod"] <- 99.5
table_S4$consumer_dest_indicator[table_S4$Species=="Haddock"] <- 100.5
table_S4$consumer_dest_indicator[table_S4$Species=="Tuna"] <- 99.2
table_S4$production_orig_indicator[table_S4$Species=="Cod"] <- 99.3
table_S4$production_orig_indicator[table_S4$Species=="Haddock"] <- 99
table_S4$production_orig_indicator[table_S4$Species=="Tuna"] <- 100.3

# Create labels
table_S4$Label[table_S4$Species=="Alaska pollock"] <- "Alaska pollock"
table_S4$Label[table_S4$Species=="Atlantic salmon"] <- "Atlantic salmon"
table_S4$Label[table_S4$Species=="Blue whiting"] <- "Blue whiting"
table_S4$Label[table_S4$Species=="Carp"] <- "Carp"
table_S4$Label[table_S4$Species=="Catfish"] <- "Catfish"
table_S4$Label[table_S4$Species=="Cod"] <- "Cod"
table_S4$Label[table_S4$Species=="Cuttlefish and squid"] <- "Cuttlefish & squid"
table_S4$Label[table_S4$Species=="Flatfish"] <- "Flatfish"
table_S4$Label[table_S4$Species=="Haddock"] <- "Haddock"
table_S4$Label[table_S4$Species=="Herring"] <- "Herring"
table_S4$Label[table_S4$Species=="Mackerel"] <- "Mackerel"
table_S4$Label[table_S4$Species=="Octopus"] <- "Octopus"
table_S4$Label[table_S4$Species=="Pacific salmon"] <- "Pacific salmon"
table_S4$Label[table_S4$Species=="Sardines"] <- "Sardines"
table_S4$Label[table_S4$Species=="Shrimps and prawns"] <- "Shrimps & prawns"
table_S4$Label[table_S4$Species=="Tilapia"] <- "Tilapia"
table_S4$Label[table_S4$Species=="Tuna"] <- "Tuna"


pdf("Outputs/Figure1.pdf", width = 8, height = 5)
ggplot(table_S4, aes(x = consumer_dest_indicator, y = production_orig_indicator)) +
  geom_point(aes(size = apparent_consumption/1000, alpha = 0.9)) + # change from 1000 t to 1000000 t units
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-5, 105)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-5, 105)) +
  scale_size_continuous(range = c(3, 8)) + # change the size of the points
  scale_colour_distiller(palette = 5, na.value = "black", direction = 1) +
  geom_text_repel(aes(label = Label), 
                  force = 1, point.padding = 0.2, box.padding = 0.2,
                  size = 3.25) + 
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_text(data = architypes, aes(x = x, y = y, label = label), size = 3.5) +
  labs(x = "Consumer destination indicator",
       y = "Production origin indicator",
       size = "Apparent consumption (million t live weight)",
       alpha = element_blank()) +
  guides(size = "legend", color = "legend", alpha = "none") +
  theme_minimal() + 
  theme(legend.position="bottom", legend.box="vertical")
dev.off()
