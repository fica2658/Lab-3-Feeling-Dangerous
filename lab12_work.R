library(tidyverse)


agg_data <- read_csv("agricultural_land_percent_of_land_area.csv")
agg_data <- agg_data %>% gather(key = "Year", value = "%_agg_land ", -country)

for_data <- read_csv("forest_coverage_percent.csv")
for_data <- for_data %>% gather(key = "Year", value = "%_for_land ", -country)

data <- left_join(agg_data, for_data, by = c("Year", "country"))
data <- data %>% filter(Year >=1990)



#questions: is their a correlation between %forrest and %agg
#           has the mean amount of forrest land changed 
#           has the mean amount of agg land changed


perm_cor <- function(perms = 1000, x, y)
{
  ## Variables ##
  # perms: The number of permutations 
  # x: Vector of Variable 1 - for computing correlation
  # y: Vector of Variable 2 - for computing correlation
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms"
  perm_cor_diffs <- numeric(perms)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2:
    # Randomly mix up the values in the vector "y"
    y_shuffled <- sample(y)
    x_shuffled <- sample(x)
    
    # Step 3:
    # Compute the correlation between x and the randomly mixed
    # up y-vector. Store this value in the vector from step 1.
    perm_cor_diffs[i] <- cor(y_shuffled, x_shuffled, use="complete.obs")
  }
  
  # Step 4:
  # Return new updated vector, created in step 1
  return(perm_cor_diffs)
}

for_vector <- data$`%_for_land ` #creating 
agg_vector <- data$`%_agg_land `

cor_real<- cor(for_vector, agg_vector, use="complete.obs") #-.466

cor_vector <- perm_cor(perms = 1000, for_vector, agg_vector) #cor permutation saved to vector
cor_graph <- as_tibble(cor_vector) #changing vector to tibble for graphing

ggplot(data = cor_graph)+
  geom_histogram(mapping= aes(x=value)) #histogram of cor permutation

sum(cor_real<cor_vector) #100th percentile


