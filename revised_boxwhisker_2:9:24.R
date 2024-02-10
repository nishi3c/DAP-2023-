library(ggplot2)
library(dplyr)

data <- read_csv("DAP_Overview_Oct17th.csv")

data$AaD_years <- data$AaD / 365

filtered_data <- data %>%
  filter(Breed_Size_Class_at_HLES != "Insufficient information to classify")

ggplot(filtered_data, aes(x = ((Breed_Size_Class_at_HLES)), y = AaD_years)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Age at death distribution by breed size and status",
       x = "Breed Size Class at HLES",
       y = "Age at Death (AaD) in days") + 
scale_x_discrete(labels = c("Giant AKC pure", "Giant nAKC mix", "Large AKC pure", "Large nAKC mix",
                             "Medium AKC pure", "Medium nAKC mix",  "Stan AKC pure", "Stan nAKC mix", "Small AKC pure", "Small nAKC mix"))


