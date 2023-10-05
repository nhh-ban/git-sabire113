#===============================================================================

#                    R programming for Data Science 

#                            Assignment 5 

#                          Problem 2, 3, 4

#===============================================================================

#===============================================================================
#                              Problem 2
#===============================================================================

# Set working directory 

# Load packages 

library(tidyverse)
library(dplyr)

# read file 
raw_file <- readLines(con = "suites_dw_Table1.txt")


# Step 1: 
# Identify the line number L of the separator line between the column names and
# the rest of the data table. "-----+----" 


substr(x = raw_file, start =1 , stop =2) # to extract the first two letters 

L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>% # the "---+---" line is now shorten to "--" 
  which() %>%  # find out which line starts with "--" 
  min() # pick out the first one 

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function. The first argument is
# the information that we want to print out. In order to get each element in the
# "raw_file"-vector on a separate line we also provide the sep-argument, where
# we put the "end-of-line"-character "\n". We also need to come up with a file
# name. Replace the question marks:


# Step 2: 
# save the variable descriptions in a text-file, we see that variable descriptions are in rows 1-12 (1:(L-2)) in the raw_file

cat(raw_file[1:(L-2)], sep = "\n", file = "variable descriptions") #get each element in the raw_file"-vector on a separate line whith sep, \n means "end-of-line" 
# come up with a new file name 

# Step 3: 
# Extract the variable names (i.e. line (L-1)), store the names in a vector.
variable_names <- 
  str_split(string = raw_file[(L-1)]  , pattern = "\\|") %>% #split the string in row 13, and all the "|", "|" has special meaning in R, therefore add \\
  unlist() %>%  #unlist to get the vector 
  str_trim() # get rid of all the empty space 

# Let us try the approach described above. It is quite transparent, but could
# probably be done quicker. We take the elements in "raw_file" containing data,
# replace all "|" with "," and remove all empty space. The gsub-function is
# super for this kind of search-and-replace. Replace the question mark below.


# Step 4: 
# search-and-replace all "|" with ","  and delete all the empty spaces
# The gsub-function is super for this kind of search-and-replace.

comma_separated_values <- 
  raw_file %>% 
  gsub("\\|", ",", .) %>% # replace all "|" with "," 
  gsub(" ", "", .) # remove all the empty space


# Step 5: 
# Now, we can remove all the variables names and L (row 14) "---+----" and basically rows 1-14 to get cleaner data 
new_comma_separated_values <- comma_separated_values[-c(1:14)]

# Step 6: 
# We then just add the variable names (separated with commas) on top, and
# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    new_comma_separated_values)    

cat(comma_separated_values_with_names [1:797], sep = "\n", file = "new_raw_file")

# Read the file back in as a normal csv-file
galaxies <- read_csv("new_raw_file")


#===============================================================================
#                              Problem 3 
#===============================================================================

# Load the relevant package needed
library(ggplot2)

# Create a scatter plot
ggplot(galaxies, aes(x = log_mhi, y = log_m26)) +
  geom_point() +
  labs(x = "Logarithm of Hydrogen Mass (log_mhi)",
       y = "Logarithm of Indicative Mass (log_m26)",
       title = "Scatter Plot of Hydrogen Mass vs. Indicative Mass") +
  theme_minimal()


# Explanation: 


#===============================================================================
#                              Problem 4 
#===============================================================================

# read file 

bigbang_data <- readLines(con = "UCNG_Table4.txt")

# extract the to first letters to get "--" instead of "----+----+---"
substr(x= bigbang_data, start = 1, stop = 2) 


# Define line l which corresponds "--"
l <- 
  (substr(x = bigbang_data, start = 1, stop = 2) == "--") %>% # the "---+---" line is now shorten to "--" 
  which() %>%  # find out which line starts with "--" 
  min() # pick out the first one 

# search-and-replace all "|" with ","  and delete all the empty spaces
# The gsub-function is super for this kind of search-and-replace.

seperate_values_with_comma <-  
  bigbang_data %>% 
  gsub("\\|",",", .) %>% 
  gsub(" ", "", .)

# remove unnecessary lines (i.e., line 2)
clean_data <- seperate_values_with_comma[-c(2)]

# cat()-the whole ting to a .csv-file 
cat(clean_data [1:762], sep = "\n", file = "Bigbang_clean_data")

# Read the file back in as a normal csv-file
bigbang <- read_csv("Bigbang_clean_data")

# Now, we have clean data for bigbang 

# ------------------------------------------------------------------------------
#                 Velocity vs. Distance from Us (create scatter plot)
#-------------------------------------------------------------------------------
# Velocity is defined by variable cz in "bigbang", whereas distance "D" is in data "galaxies.
# therefore we need to merge these to data sets in one

# Merge "bigbang" and "galaxies" and by using left join based on the "name" column

merged_data <- left_join(bigbang, galaxies, by = "name") 

# Create a scatter plot of velocity (cz) against distance from us (D)
ggplot(merged_data, aes(x = D, y = cz)) +
  geom_point() +
  labs(x = "Distance from Us (D) [Mpc]",
       y = "Velocity (cz) [km/s]",
       title = "Scatter Plot of Velocity vs. Distance from Us") +
  theme_minimal()
# ------------------------------------------------------------------------------    
#                    Sub_question 2 - estimate constant H 
#-------------------------------------------------------------------------------
hubble_model <- lm(cz ~ D, data = merged_data)

# Extract the estimated Hubble's constant (H0)
h0_estimate <- coef(hubble_model)["D"]

# Print the estimated Hubble's constant
cat("Estimated Hubble's constant (H0):", h0_estimate, "(km/s)/Mpc\n")
