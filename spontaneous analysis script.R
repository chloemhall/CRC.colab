# script to analyse the spons from MEA data CRC colab project 
#load libraries 
library(tidyverse)
library(data.table)
#load data ####
setwd("\\\\fs02/hallchlo$/Dokumente/R/CRC colab/spons_select")

csv_files <- list.files(pattern = "\\.csv$")
df_list <- list()

for (file in csv_files) {
  df_name <- gsub("\\.csv$", "", file)  # Remove ".csv" extension from filename
  df <- read.table(file, header = TRUE, sep = ";",
                   )   # Adjust other parameters as needed
df_list[[df_name]] <- df[, 1:2]

}
#add a Genotype column to the imported data ####
for (df_name in names(df_list)) {
  # Check if any of the specified values are present in df_name
  if (grepl("M12|M8|M7|M4", df_name)) {
    genotype_label <- "GAD67-GFP"
  } else {
    genotype_label <- "WT"
  }
  
  # Add "Genotype" column to the data frame
  df_list[[df_name]]$Genotype <- genotype_label
}

#add a Contra or Ipsi column to the imported data ####
for (df_name in names(df_list)) {
  # Check if any of the specified values are present in df_name
  if (grepl("contra", df_name)) {
    hem_label <- "contra"
  } else {
    hem_label <- "ipsi"
  }
  
  # Add "Genotype" column to the data frame
  df_list[[df_name]]$Hemisphere <- hem_label
}
# Assuming "ipsi_dfs" contains the list of data frames with filenames containing "ipsi"

# Create a mapping of M values to Genotype labels #### 
#no longer needed. 
#genotype_mapping <- c("M12" = "GAD67-GFP",
         #             "M11" = "WT",
        #              "M10" = "WT",
        #              "M9" = "WT",
         #             "M8" = "GAD67-GFP",
         #             "M7" = "GAD67-GFP",
          #            "M6" = "WT",
          #            "M5" = "WT",
        #              "M4" = "GAD67-GFP")

#### okay now split the df_list according to contra or ipsi #### 
contra_dfs <- list()
ipsi_dfs <- list()

for (df_name in names(df_list)) {
  if (grepl("contra", df_name, ignore.case = TRUE)) {
    contra_dfs[[df_name]] <- df_list[[df_name]]
  } else if (grepl("ipsi", df_name, ignore.case = TRUE)) {
    ipsi_dfs[[df_name]] <- df_list[[df_name]]
  }
}


 
#^^ not sure I need this anymore tbf^
# standardise number of rows per df ####
#Find the maximum number of rows across all data frames # 

max_rows <- max(sapply(df_list, nrow))

# Standardize the row count for each data frame####
for (df_name in names(df_list)) {
  df <- df_list[[df_name]]
  
  # Check if the data frame has fewer rows than the maximum
  if (nrow(df) < max_rows) {
    # Add rows with NA values to match the maximum row count
    df <- rbind(df, data.frame(matrix(NA, ncol = ncol(df), nrow = max_rows - nrow(df))))
  }
  
  # Update the data frame back to df_list
  df_list[[df_name]] <- df
}

# Now you can view the standardized data frames
view(df_list)

# need a column for the layers ####
for (df_name in names(df_list)) {
    df <- df_list[[df_name]]  # Get the current data frame
  # Check if any of the specified values are present in df_name
  if (any(grepl("11|21|31|41|51|61|71|81", df$Channel)) ) {
    cortex_layer<- "I"
    } else if (any(grepl("12|22|32|42|52|62|72|82|13|23|33|43|53|63|73|83", df$Channel))) {
      cortex_layer<- "II/III"
    } else if (any(grepl("14|24|34|44|54|64|74|84", df$Channel))) {
      cortex_layer<- "IV"
    } else { 
      cortex_layer <- "unnamed"
  }
    # Add "Cortex Layer" column to the data frame
  df$Cortex_Layer <- cortex_layer
    
    # Update the modified data frame back to df_list
   # df_list[[df_name]]$Cortex_Layer <- cortex_layer
  df_list[[df_name]] <- df
}
view(df_list$M9_spons_ipsi_s3)



#### graphs ####
ggplot(df_list, aes(x = Genotype, y = Number.of.Spikes, colour = hem_label))+
  geom_dotplot()+
  theme_classic()
  
