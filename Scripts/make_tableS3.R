# TITLE: make_tableS3
# AUTHOR: Jessica Gephart 

# Load data from Table S4
FigureS3_data <- read.csv("Outputs/FigureS3_data.csv")

#----------------------------------------------------------------------------------------------------#
#                                        Table S3
#----------------------------------------------------------------------------------------------------#

tableS3 <- data.frame("Error not added to re-export quantity" = numeric(2),
                      "Error added to re-export quantity" = numeric(2))
rownames(tableS3) <- c("Lower", "Upper")

# Min re-export rate w/out error
tableS3$Error.not.added.to.re.export.quantity[1] <- round(100*sum(FigureS3_data$Reexport_Lower)/sum(FigureS3_data$Import_lw_1000t), 1)

# Min re-export rate w/ error as import
tableS3$Error.added.to.re.export.quantity[1] <- round(100*sum(FigureS3_data$Reexport_Lower + FigureS3_data$Error_Lower)/sum(FigureS3_data$Import_lw_1000t), 1)

# Max re-export rate w/out error
tableS3$Error.not.added.to.re.export.quantity[2] <- round(100*sum(FigureS3_data$Reexport_Upper)/sum(FigureS3_data$Import_lw_1000t), 1)

# Max re-export rate w/ error as import
tableS3$Error.added.to.re.export.quantity[2] <- round(100*sum(FigureS3_data$Reexport_Upper + FigureS3_data$Error_Upper)/sum(FigureS3_data$Import_lw_1000t), 1)

write.csv(tableS3, "Outputs/TableS3.csv", row.names = FALSE)

