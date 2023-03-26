library(robustbase)
library(car)
library(stats)

# Load Needed Data #
setwd("...")
ins <- read.csv(file = "aggregated_policy_nolabel_2023.csv", header = TRUE)

# select columns with numeric data that is non-binary
continuous_vars <- names(ins)[sapply(ins, function(x) is.numeric(x) & 
                                       length(unique(x)) > 2)]

# all two-way combinations of the continuous variables
combinations <- combn(continuous_vars, 2)

# create an empty data frame to store the farthest points for each combination
farthest_points <- data.frame()

# loop through each combination and calculate the robust Mahalanobis distance
for(i in 1:ncol(combinations)) {
  vars <- combinations[, i]
  df <- ins[, vars]
  
  # MCD Adjustment to MD #
  mcdresult <- covMcd(df)
  robustcenter <- mcdresult$center
  robustcov <- mcdresult$cov
  
  # calculate Mahalanobis distance for each observation
  mahalanobis_dist <- mahalanobis(df, center = robustcenter, cov = robustcov, 
                                  inverted = TRUE)
  
  # create a scatter plot of the data with an ellipse of the Mahalanobis distance
  plot(df, main = paste(vars[1], "vs.", vars[2]), pch = 19)
  rad <- sqrt(qchisq(0.9999975, ncol(df)))
  ellipse(center = robustcenter, shape = robustcov, radius = rad, col = "red")
  
  # get the 2000 points with the farthest Mahalanobis distances
  farthest_idx <- order(mahalanobis_dist, decreasing = TRUE)[1:2000]
  farthest_df <- data.frame(
    row_id = rownames(ins)[farthest_idx],
    distance = mahalanobis_dist[farthest_idx]
  )
  
  # add the farthest points for this combination to the overall data frame
  farthest_points <- rbind(farthest_points, farthest_df)
}

# sort the farthest points by distance in descending order
farthest_points <- farthest_points[order(farthest_points$distance, decreasing = TRUE), ]

# group by row_id and calculate average distance
farthest_points_avg <- aggregate(distance ~ row_id, data = farthest_points, FUN = mean)

# get top 3000 distances (in case there are duplicate customers)
top_3000 <- farthest_points_avg[order(farthest_points_avg$distance, 
                                      decreasing = TRUE), ][1:3000, ]

# subset the rows from ins
ins_subset <- ins[top_3000$row_id, ]

#add a column of the original row_ids for mapping back to original data
ins_subset$orig_row_id <- top_3000$row_id

#add the distance column
ins_subset$MD_distance <- top_3000$distance

#get unique customer ids
unique_customer_ids <- unique(ins_subset$Cust_ID)

#get the ordered customer ids
ordered_cust_ids <- ins_subset[order((ins_subset$MD_distance), 
                                     decreasing = TRUE), "Cust_ID"][1:2000]

# save as csv
write.table(ordered_cust_ids, "robustMD.csv", row.names = FALSE, col.names = "Cust_ID")


