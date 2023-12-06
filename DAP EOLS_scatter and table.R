2+2
log(2)
log2 <- log(2) #comment example# 
# comments created by putting # in front 

#Step1: create a working directory 
#Step2: download files from DAP into the directory 
#Step3: Decide variables needed & Data Linkage --> taking variables needed from file and create table 
#Step4: Data Quality --> making inquires and figures of the data, figuring out potential problems and useibilty 
#Step5: Figure out what questions can be answered; may need to add more variable 
#Step6: Constructing lifetables 
#Step7: Using lifetable and other data to answer questions 


#download heaven package and then able to use read_sav 


#Graph 
# Load necessary libraries
library(readr)
library(ggplot2)

# Read the CSV file
data <- read_csv("DAP_Overview_Oct17th.csv")

# Convert days to years
data$AaD_years <- data$AaD / 365

# Filter out the "Insufficient information to classify" category
data <- data[data$Breed_Size_Class_at_HLES != "Insufficient information to classify", ]

# Create the scatter plot with custom colors
ggplot(data, aes(x = DAP_Pack_Date, y = AaD_years, color = Breed_Size_Class_at_HLES)) +
  geom_point(alpha = 0.5) +
  labs(x = "Date of Enrollment", y = "Age at Death (years)",
       title = "Comparison of Breed Size Classes in Terms of Age at Death and Date of Enrollment") +
  scale_color_manual(values = c("lightblue", "darkblue", "tomato", "lightpink4", "lightpink", "darkred", "khaki4", "orange", "black", "blueviolet"),
                     labels = c("Gaint AKC pure", "Gaint nAKC mix", "Large AKC pure", "Large nAKC mix", "Medium AKC pure",
                                "Medium nAKC mix", "Stan AKC pure", "Stan nAKC mix", "Small AKC pure", "Small nAKC mix")) +
  theme_minimal()




#table
# Load necessary libraries
library(tidyverse)
library(gt)

# Read the data from the CSV file
data <- read.csv("DAP_Overview_Oct17th.csv")

# Filter out rows with "Insufficient information to classify"
filtered_data <- data %>%
  filter(Breed_Size_Class_at_HLES != "Insufficient information to classify")

# Create a table of counts for each combination of Breed_Size_Class_at_HLES and Weight
table_data <- filtered_data %>% 
  count(Breed_Size_Class_at_HLES, Weight_Class_5KGBin_at_HLES)

# Remove the column corresponding to "Insufficient information to classify"
table_data <- table_data %>% 
  filter(Breed_Size_Class_at_HLES != "Insufficient information to classify") %>%
  spread(key = Weight_Class_5KGBin_at_HLES, value = n, fill = 0) %>%
  select(-matches("Insufficient information to classify"))

# Change row names 
new_row_names <- c("Abbreviation1", "Abbreviation2", "Abbreviation3", "Abbreviation4", "Abbreviation5", "Abbreviation6", "Abbreviation7", "Abbreviation8", "Abbreviation9", "Abbreviation10") # Customize the row names as needed
rownames(table_data) <- new_row_names

# Create the gt table with updated title
gt_table <- gt(table_data) %>%
  tab_header(
    title = "Sample Size of All Dogs Based on Their Size Class and AKC Status"
  )

# Display the table
gt_table

#box and whiskers 
library(ggplot2)
library(dplyr)

data$AaD_years <- data$AaD / 365

filtered_data <- data %>%
  filter(Breed_Size_Class_at_HLES != "Insufficient information to classify")

ggplot(filtered_data, aes(x = factor(Breed_Size_Class_at_HLES, levels = unique(Breed_Size_Class_at_HLES)), y = AaD_years)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Age at death distribution by breed size and status",
       x = "Breed Size Class at HLES",
       y = "Age at Death (AaD) in days")
#scale_x_discrete(labels = c("Giant AKC pure", "Giant nAKC mix", "Large AKC pure", "Large nAKC mix", 
#                             "Medium AKC pure", "Medium nAKC mix", "Stan AKC pure", "Stan nAKC mix", 
#                              "Small AKC pure", "Small nAKC mix"))
