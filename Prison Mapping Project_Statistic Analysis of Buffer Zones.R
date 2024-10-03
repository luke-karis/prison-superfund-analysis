install.packages(c("sf", "dplyr", "here", "ggplot2", "tmap", "stats"))
library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(tmap)
library(stats)


# Define file paths using the here() function
prison_shp_path <- here("Data", "Prison_Boundaries.shp")
superfund_shp_path <- here("Data", "Superfund_National_Priorities_List_(NPL)_Sites_with_Status_Information.shp")

# Read the shapefiles
prison_data <- st_read(prison_shp_path)
superfund_data <- st_read(superfund_shp_path)

# Ensure both datasets are in the same CRS
prison_data <- st_transform(prison_data, crs = 9311) # 4326 (was previously using 4326 but was encountering )
superfund_data <- st_transform(superfund_data, crs = 9311)

# Create buffer zones around all Superfund sites for future analysis
superfund_buffer_3 <- st_buffer(superfund_data, dist = 3 * 1609.34)

prisons_within_3_miles <-st_intersection(prison_data, superfund_buffer_3)
prisons_within_3_miles_clean <- prisons_within_3_miles[!duplicated(prisons_within_3_miles$FID), ]


index_outside <- setdiff(prison_data$FID, prisons_within_3_miles_clean$FID)
length(index_outside)
dim(prisons_outside_3_miles <- prison_data[index_outside,])


###  chi-square analysis: Prisons by State
#create counts of prisons by state
within_counts <- table(prisons_within_3_miles_clean$STATE)
outside_counts <- table(prisons_outside_3_miles$STATE)

print(within_counts)
print(outside_counts)

# Identify the common and unique states
common_states <- intersect(names(within_counts), names(outside_counts))
states_in_within_only <- setdiff(names(within_counts), names(outside_counts))
states_in_outside_only <- setdiff(names(outside_counts), names(within_counts))

# Add missing states with zero counts
within_counts[states_in_outside_only] <- 0  # Add missing states to within_counts
outside_counts[states_in_within_only] <- 0  # Add missing states to outside_counts

# Ensure both are sorted by state names for consistency
within_counts <- within_counts[sort(names(within_counts))]
outside_counts <- outside_counts[sort(names(outside_counts))]

print(within_counts)
print(outside_counts)

# Perform a Chi-square test on the adjusted counts
chisq_test <- chisq.test(within_counts, outside_counts)

# Print the result of the Chi-square test
print(chisq_test)

####
###  chi-square analysis: Prisons by facility ownership
#create counts of prisons by facility ownership
within_ownership_counts <- table(prisons_within_3_miles_clean$TYPE)
outside_ownership_counts <- table(prisons_outside_3_miles$TYPE)

print(within_ownership_counts)
print(outside_ownership_counts)

# Identify the common and unique states
common_ownership <- intersect(names(within_ownership_counts), names(outside_ownership_counts))
ownership_in_within_only <- setdiff(names(within_ownership_counts), names(outside_ownership_counts))
ownership_in_outside_only <- setdiff(names(outside_ownership_counts), names(within_ownership_counts))

# Add missing states with zero counts
within_ownership_counts[ownership_in_outside_only] <- 0  # Add missing states to within_ownership_counts
outside_ownership_counts[ownership_in_within_only] <- 0  # Add missing states to outside_ownership_counts

# Ensure both are sorted by state names for consistency
within_ownership_counts <- within_ownership_counts[sort(names(within_ownership_counts))]
outside_ownership_counts <- outside_ownership_counts[sort(names(outside_ownership_counts))]

print(within_ownership_counts)
print(outside_ownership_counts)

# Perform a Chi-square test on the adjusted counts
chisq_ownership_test <- chisq.test(within_ownership_counts, outside_ownership_counts)

# Print the result of the Chi-square test
print(chisq_ownership_test)

# ## practiced code by creating a subset of both datasets using state of Texas for quicker analysis (selected Texas because of decent Prison and Superfund sample sizes)
# texas_superfund_buffer_3 <- superfund_buffer_3[which(superfund_buffer_3$State=="Texas"),]
# texas_prison_data <- prison_data[which(prison_data$STATE=="TX"),]
# texas_within_3_miles <-st_intersection(texas_prison_data, texas_superfund_buffer_3)

# # Remove duplicates based on the FID column in texas_prison_data
# texas_within_3_miles_nodupes <- texas_within_3_miles[!duplicated(texas_within_3_miles$FID), ]
### generates a total of 39 unique facilities within superfund

######sophia -- testing
# texas_within_3_miles_buffer <-st_intersection(texas_superfund_buffer_3, texas_prison_data)
# length(unique(texas_within_3_miles$FID))
# length(unique(texas_within_3_miles_nodupes$FID))
# any(is.na(texas_outside_3_miles_nodupes))

### used index_outside to find everyt
# index_outside <- setdiff(texas_prison_data$FID, texas_within_3_miles_nodupes$FID)
# length(index_outside)
# dim(texas_outside_3_miles <- texas_prison_data[index_outside,])


###### #### ###### ######### ######### ######### ###

superfund_buffer_3 <- st_buffer(superfund_data, dist = 3 * 1609.34)

# Perform the spatial intersection between prisons and the Superfund buffer
# This will return all prisons that intersect with any 3-mile Superfund buffer
# Keep the columns from superfund_data such as SEMS_ID and Site_Score
prisons_superfund_intersect <- st_intersection(prison_data, superfund_buffer_3) %>%
  mutate(SEMS_ID = superfund_data$SEMS_ID, Site_Score = superfund_data$Site_Score)

# Select the required columns, ensuring the SEMS_ID and Site_Score are present
prisons_superfund_scores <- prisons_superfund_intersect %>%
  select(prison_FID = FID, prison_NAME = NAME, superfund_SEMS_ID = SEMS_ID, Site_Score)

# Check the output
print(prisons_superfund_scores)


# Calculate the average Site_Score for prisons that touch a Superfund site
average_prison_site_score <- prisons_superfund_scores %>%
  summarise(mean_site_score = mean(Site_Score, na.rm = TRUE))

print(average_prison_site_score)


# Find the superfund sites that do not intersect with any prisons using SEMS_ID
superfunds_without_prisons <- superfund_data[!superfund_data$SEMS_ID %in% prisons_superfund_scores$superfund_SEMS_ID, ]

# Calculate the average Site_Score for Superfund sites that do not touch a prison
average_non_prison_site_score <- superfunds_without_prisons %>%
  summarise(mean_site_score = mean(Site_Score, na.rm = TRUE))

print(average_non_prison_site_score)


# Extract the Site_Score values for prisons that touch Superfund sites and for Superfund sites that do not touch prisons
prison_site_scores <- prisons_superfund_scores$Site_Score
non_prison_site_scores <- superfunds_without_prisons$Site_Score

# Perform a t-test to compare the average Site_Score of the two groups
t_test_result <- t.test(prison_site_scores, non_prison_site_scores, alternative = "two.sided")

# Print the result of the t-test
print(t_test_result)


################################################################ 

##################################. ############################## 
##################################. ############################## 
# Remove Superfund sites where Site_Score is 0 or NA
superfund_data_clean <- superfund_data %>%
  filter(!is.na(Site_Score) & Site_Score > 0)

# Perform the spatial intersection using the cleaned superfund data
prisons_superfund_intersect_clean <- st_intersection(prison_data, superfund_buffer_3) %>%
  mutate(SEMS_ID = superfund_data_clean$SEMS_ID, Site_Score = superfund_data_clean$Site_Score)

# Select relevant columns
prisons_superfund_scores_clean <- prisons_superfund_intersect_clean %>%
  select(prison_FID = FID, prison_NAME = NAME, superfund_SEMS_ID = SEMS_ID, Site_Score)

# Find the superfund sites that do not intersect with any prisons using the cleaned superfund data
superfunds_without_prisons_clean <- superfund_data_clean[!superfund_data_clean$SEMS_ID %in% prisons_superfund_scores_clean$superfund_SEMS_ID, ]

# Calculate the average Site_Score for Superfund sites that intersect with prisons
average_prison_site_score_clean <- prisons_superfund_scores_clean %>%
  summarise(mean_site_score = mean(Site_Score, na.rm = TRUE))

print(average_prison_site_score_clean)

# Calculate the average Site_Score for Superfund sites that do not intersect with prisons
average_non_prison_site_score_clean <- superfunds_without_prisons_clean %>%
  summarise(mean_site_score = mean(Site_Score, na.rm = TRUE))

print(average_non_prison_site_score_clean)

# Extract the Site_Score values for the two groups
prison_site_scores_clean <- prisons_superfund_scores_clean$Site_Score
non_prison_site_scores_clean <- superfunds_without_prisons_clean$Site_Score

# Perform a t-test to compare the average Site_Score of the two groups
t_test_result_clean <- t.test(prison_site_scores_clean, non_prison_site_scores_clean, alternative = "two.sided")

# Print the result of the t-test
print(t_test_result_clean)




# # Find the superfund sites that do not intersect with any prisons
# superfunds_without_prisons <- superfund_data[!superfund_data$FID %in% prisons_superfund_scores$superfund_FID, ]
# 
# # Calculate the average Site_Score for Superfund sites that do not touch a prison
# average_non_prison_site_score <- superfunds_without_prisons %>%
#   summarise(mean_site_score = mean(Site_Score, na.rm = TRUE))
# 
# print(average_non_prison_site_score)
# 
# ###
# # Extract site scores for superfund sites within the 3-mile buffer
# buffered_superfund_data <- st_intersection(superfund_data, superfund_buffer_3)
# site_scores_within_3_miles <- buffered_superfund_data$Site_Score
# 
# 
# # Replace 0.00 values with NA
# site_scores_within_3_miles[site_scores_within_3_miles == 0.00] <- NA
# print(site_scores_within_3_miles)
# # average_score_within_3_miles <- mean(site_scores_within_3_miles, na.rm = TRUE)
# # print(average_score_within_3_miles)
# # Remove NA values
# site_scores_within_3_miles_clean <- na.omit(site_scores_within_3_miles)
# 
# # Print the cleaned values
# print(site_scores_within_3_miles_clean)
# 
# 
# 
# ########
# 
# # calculate average scores
# average_score_within_3_miles_clean <- mean(site_scores_within_3_miles_clean, na.rm = TRUE)
# print(average_score_within_3_miles_clean)
# 
# # Recalculate t-test
# t_test_result_clean <- t.test(site_scores_within_3_miles_clean, site_scores_all)
# 
# ###Probably delete the following 4 lines?
# 
# # Print updated results
# # cat("Average Site Score within 3 miles of a prison (cleaned):", average_score_within_3_miles_clean, "\n")
# # cat("T-test result (cleaned):\n")
# # print(t_test_result_clean)
# 
# 
# #call in prisons within a 3-mile radius of a Superfund site
# prisons_within_3_miles_clean
# prisons_outside_3_miles
# 
# # Identify superfund sites within 3 miles of a prison
# superfund_within_prison_buffer <- st_intersection(superfund_data, prison_buffer_3_miles)
# 
# # Identify superfund sites outside of 3 miles of any prison
# superfund_outside_prison_buffer <- superfund_data[!apply(st_intersects(superfund_data, prison_buffer_3_miles, sparse = FALSE), 1, any), ]
# 
# # Clean the Site_Score column by replacing 0.00 values with NA and then removing NA values
# superfund_within_prison_buffer$Site_Score[superfund_within_prison_buffer$Site_Score == 0.00] <- NA
# superfund_outside_prison_buffer$Site_Score[superfund_outside_prison_buffer$Site_Score == 0.00] <- NA
# 
# superfund_within_prison_buffer <- superfund_within_prison_buffer %>% drop_na(Site_Score)
# superfund_outside_prison_buffer <- superfund_outside_prison_buffer %>% drop_na(Site_Score)
# 
# # Calculate average site scores for both groups
# average_score_within_3_miles_clean <- mean(superfund_within_prison_buffer$Site_Score, na.rm = TRUE)
# average_score_outside_3_miles_clean <- mean(superfund_outside_prison_buffer$Site_Score, na.rm = TRUE)
# 
# # Perform a t-test between site scores within 3 miles of prisons and those outside
# t_test_result_clean <- t.test(superfund_within_prison_buffer$Site_Score, superfund_outside_prison_buffer$Site_Score)
# 
# # Print the updated results
# cat("Average Site Score within 3 miles of a prison (cleaned):", average_score_within_3_miles_clean, "\n")
# cat("Average Site Score outside 3 miles of any prison (cleaned):", average_score_outside_3_miles_clean, "\n")
# cat("T-test result (cleaned):\n")
# print(t_test_result_clean)









# Calculate average site scores by state for Superfund sites
# average_site_scores_superfund <- superfund_data %>%
#   group_by(State) %>%
#   summarise(average_site_score_superfund = mean(Site_Score, na.rm = TRUE))



### BELOW DID NOT WORK RIGHT!!! 

###
#TRYING AGAIN BELOW. 
##########.    ############### ##########.    ############### ##########.    ############### ##########.    ############### ##########.    ############### 

# # Check for intersection with any prison buffer
# superfund_intersects_prison <- apply(st_intersects(superfund_data, prison_buffer_3_miles, sparse = FALSE), 1, any)
# 
# # Subset to superfund sites that do not intersect with any prison buffer
# superfund_outside_prison_buffer <- superfund_data[!superfund_intersects_prison, ]
# 
# # Remove NA and zero values from the Site_Score for sites outside of prison buffers
# superfund_outside_prison_buffer <- superfund_outside_prison_buffer[!is.na(superfund_outside_prison_buffer$Site_Score) & superfund_outside_prison_buffer$Site_Score != 0, ]
# 
# # Remove NA and zero values from the Site_Score for sites within prison buffers
# superfund_within_prison_buffer <- superfund_within_prison_buffer[!is.na(superfund_within_prison_buffer$Site_Score) & superfund_within_prison_buffer$Site_Score != 0, ]
# 
# # Calculate the average site score for superfund sites outside of prison buffers
# average_score_without_prisons <- mean(superfund_outside_prison_buffer$Site_Score, na.rm = TRUE)
# 
# # Calculate the average site score for superfund sites within 3 miles of prisons
# average_score_within_3_miles <- mean(superfund_within_prison_buffer$Site_Score, na.rm = TRUE)
# 
# # Perform a t-test between site scores within 3 miles of prisons and those outside
# t_test_result_take2 <- t.test(superfund_within_prison_buffer$Site_Score, 
#                               superfund_outside_prison_buffer$Site_Score)
# 
# # Print the results
# cat("Average Site Score within 3 miles of a prison:", average_score_within_3_miles, "\n")
# cat("Average Site Score outside 3 miles of any prison:", average_score_without_prisons, "\n")
# cat("T-test result:\n")
# print(t_test_result_take2)


########## Failed attempt down here

# ### Generate 3mile radius CSV w/ Superfund data
# 
# # Join the superfund data to the prisons data (many-to-many relationship)
# prisons_with_superfund_info <- st_join(prisons_within_3_miles, superfund_data, join = st_intersects)
# 
# # Convert the resulting spatial data frame to a data frame (non-spatial) for CSV export
# prisons_with_superfund_info_df <- as.data.frame(prisons_with_superfund_info)
# 
# # Define the output file path
# output_csv_path <- here("Data", "3 mile Prisons and superfund variables.csv")
# 
# # Write the data frame to a CSV file
# write.csv(prisons_with_superfund_info_df, file = output_csv_path, row.names = FALSE)
# 
# # Print the path to the output file
# cat("CSV file created at:", output_csv_path, "\n")

