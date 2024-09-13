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
prison_data <- st_transform(prison_data, crs = 3857) # 4326 (was previously using 4326 but was encountering )
superfund_data <- st_transform(superfund_data, crs = 3857)

st_crs(superfund_data)
# Create buffer zones around all Superfund sites for future analysis
superfund_buffer_3 <- st_buffer(superfund_data, dist = 3 * 1609.34)

##subset both datasets for quicker analysis (selected Texas because of decent Prison and Superfund sample sizes)
texas_superfund_buffer_3 <- superfund_buffer_3[which(superfund_buffer_3$State=="Texas"),]
texas_prison_data <- prison_data[which(prison_data$STATE=="TX"),]

texas_within_3_miles <-st_intersection(texas_prison_data, texas_superfund_buffer_3)
# Remove duplicates based on the FID column in texas_prison_data
texas_within_3_miles_nodupes <- texas_within_3_miles[!duplicated(texas_within_3_miles$FID), ]
### generates a total of 33 unique facilities within superfund

### identify prisons in Texas OUTSIDE of the 3-mile Superfund buffer
texas_outside_3_miles <- st_difference(texas_prison_data,st_union(texas_superfund_buffer_3))

#texas_outside_3_miles_nodupes <- texas_outside_3_miles[!duplicated(texas_outside_3_miles$FID), ]
### no duplicates found

### Checking duplicates
# duplicates <- texas_outside_3_miles$FID[duplicated(texas_outside_3_miles$FID)]
# duplicates <- texas_within_3_miles$FID[duplicated(texas_within_3_miles$FID)]
# duplicates <- texas_prison_data$FID[duplicated(texas_prison_data$FID)]

# Assuming 'df1' and 'df2' are your data frames and 'facility_id' is the common identifier
# Check which facilities in df1 are in df2
df2_common <- texas_outside_3_miles[texas_outside_3_miles$facility_id %in% texas_within_3_miles_nodupes$facility_id, ]

# Print the common facilities
print(df1_common)


# Print the duplicate values
print(duplicates)



########### chi-square analysis
# Create a contingency table
prison_contingency_table <- table(data$prisons_within_3_miles, data$prisons_outside_3_miles)

# Print the contingency table
print(prison_contingency_table)


###### ###

###
# Extract site scores for superfund sites within the 3-mile buffer
buffered_superfund_data <- st_intersection(superfund_data, superfund_buffer_3)
site_scores_within_3_miles <- buffered_superfund_data$Site_Score


# Replace 0.00 values with NA
site_scores_within_3_miles[site_scores_within_3_miles == 0.00] <- NA

# Remove NA values
site_scores_within_3_miles_clean <- na.omit(site_scores_within_3_miles)

# Print the cleaned values
print(site_scores_within_3_miles_clean)



########

# calculate average scores
average_score_within_3_miles_clean <- mean(site_scores_within_3_miles_clean, na.rm = TRUE)

# Recalculate t-test
t_test_result_clean <- t.test(site_scores_within_3_miles_clean, site_scores_all)

# Print updated results
cat("Average Site Score within 3 miles of a prison (cleaned):", average_score_within_3_miles_clean, "\n")
cat("T-test result (cleaned):\n")
print(t_test_result_clean)




#3333
###### next ATTEMPT
# Ensure necessary packages are installed and loaded
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
prison_data <- st_transform(prison_data, crs = 4326)
superfund_data <- st_transform(superfund_data, crs = 4326)

# Create buffer zones around superfund sites and prisons for future analysis
superfund_buffer_3 <- st_buffer(superfund_data, dist = 3 * 1609.34)
prison_buffer_3_miles <- st_buffer(prison_data, dist = 3 * 1609.34)

# Identify superfund sites within 3 miles of a prison
superfund_within_prison_buffer <- st_intersection(superfund_data, prison_buffer_3_miles)

# Identify superfund sites outside of 3 miles of any prison
superfund_outside_prison_buffer <- superfund_data[!apply(st_intersects(superfund_data, prison_buffer_3_miles, sparse = FALSE), 1, any), ]

# Clean the Site_Score column by replacing 0.00 values with NA and then removing NA values
superfund_within_prison_buffer$Site_Score[superfund_within_prison_buffer$Site_Score == 0.00] <- NA
superfund_outside_prison_buffer$Site_Score[superfund_outside_prison_buffer$Site_Score == 0.00] <- NA

superfund_within_prison_buffer <- superfund_within_prison_buffer %>% drop_na(Site_Score)
superfund_outside_prison_buffer <- superfund_outside_prison_buffer %>% drop_na(Site_Score)

# Calculate average site scores for both groups
average_score_within_3_miles_clean <- mean(superfund_within_prison_buffer$Site_Score, na.rm = TRUE)
average_score_outside_3_miles_clean <- mean(superfund_outside_prison_buffer$Site_Score, na.rm = TRUE)

# Perform a t-test between site scores within 3 miles of prisons and those outside
t_test_result_clean <- t.test(superfund_within_prison_buffer$Site_Score, superfund_outside_prison_buffer$Site_Score)

# Print the updated results
cat("Average Site Score within 3 miles of a prison (cleaned):", average_score_within_3_miles_clean, "\n")
cat("Average Site Score outside 3 miles of any prison (cleaned):", average_score_outside_3_miles_clean, "\n")
cat("T-test result (cleaned):\n")
print(t_test_result_clean)









# Calculate average site scores by state for Superfund sites
average_site_scores_superfund <- superfund_data %>%
  group_by(State) %>%
  summarise(average_site_score_superfund = mean(Site_Score, na.rm = TRUE))

## Need to do same with prisons. 
###But how to add site score to this file? Unsure. 




##
### BELOW DID NOT WORK RIGHT!!! 

###
#TRYING AGAIN BELOW. 
##########.    ############### ##########.    ############### ##########.    ############### ##########.    ############### ##########.    ############### 

# Check for intersection with any prison buffer
superfund_intersects_prison <- apply(st_intersects(superfund_data, prison_buffer_3_miles, sparse = FALSE), 1, any)

# Subset to superfund sites that do not intersect with any prison buffer
superfund_outside_prison_buffer <- superfund_data[!superfund_intersects_prison, ]

# Remove NA and zero values from the Site_Score for sites outside of prison buffers
superfund_outside_prison_buffer <- superfund_outside_prison_buffer[!is.na(superfund_outside_prison_buffer$Site_Score) & superfund_outside_prison_buffer$Site_Score != 0, ]

# Remove NA and zero values from the Site_Score for sites within prison buffers
superfund_within_prison_buffer <- superfund_within_prison_buffer[!is.na(superfund_within_prison_buffer$Site_Score) & superfund_within_prison_buffer$Site_Score != 0, ]

# Calculate the average site score for superfund sites outside of prison buffers
average_score_without_prisons <- mean(superfund_outside_prison_buffer$Site_Score, na.rm = TRUE)

# Calculate the average site score for superfund sites within 3 miles of prisons
average_score_within_3_miles <- mean(superfund_within_prison_buffer$Site_Score, na.rm = TRUE)

# Perform a t-test between site scores within 3 miles of prisons and those outside
t_test_result_take2 <- t.test(superfund_within_prison_buffer$Site_Score, 
                              superfund_outside_prison_buffer$Site_Score)

# Print the results
cat("Average Site Score within 3 miles of a prison:", average_score_within_3_miles, "\n")
cat("Average Site Score outside 3 miles of any prison:", average_score_without_prisons, "\n")
cat("T-test result:\n")
print(t_test_result_take2)














##########







### Generate 3mile radius CSV w/ Superfund data

# Join the superfund data to the prisons data (many-to-many relationship)
prisons_with_superfund_info <- st_join(prisons_within_3_miles, superfund_data, join = st_intersects)

# Convert the resulting spatial data frame to a data frame (non-spatial) for CSV export
prisons_with_superfund_info_df <- as.data.frame(prisons_with_superfund_info)

# Define the output file path
output_csv_path <- here("Data", "3 mile Prisons and superfund variables.csv")

# Write the data frame to a CSV file
write.csv(prisons_with_superfund_info_df, file = output_csv_path, row.names = FALSE)

# Print the path to the output file
cat("CSV file created at:", output_csv_path, "\n")

