###### imports
library(random)
library(dplyr)
library(sna)
library(igraph)
library(glmnet)
library(randomForest)

setwd("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case")

###### get data
nodes <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/nodes.csv")
links <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/links.csv")
names <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/random_names.csv")
zipcode_data <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/zip_codes_data.csv")

###### build out zipcode subgraphs
zipcode_results <- matrix(, ncol=5)
zips <- unique(zipcode_data$zip)
for (i in zips) {

  print(paste("Working on zipcode: ", i, sep=""))
  
  ### create subgraph
  # check size
  if (table(links$zip)[as.character(i)] < 50) {
    print(paste(i, "zipcode has too few observations (less than 50). Onto the next...", sep=" "))
    next
  }
  # get links
  subset_links <- links[links$zip==i, ]
  # get graph
  new_edges <- as.matrix(subset_links[, c(1, 2)])
  subset_g <- graph_from_edgelist(new_edges, directed=TRUE)
  # get nodes
  new_nodes <- c(unique(subset_links$fromNode), unique(subset_links$toNode))
  subset_nodes <- nodes[nodes$node %in% new_nodes, ]
  
  ### calculate Average Zipcode Revenue
  avg_rev <- mean(subset_nodes$revenueYear)
  ### calculate density
  den <- edge_density(subset_g)
  ### calculate connectedness
  con <- connectedness(get.edgelist(subset_g))
  ### calculate average strength of edges
  avg_str <- mean(subset_links$strength)
  
  ### build zipcode entry
  new_row <- matrix(c(i, avg_rev, den, con, avg_str), ncol=5)
  zipcode_results <- rbind(zipcode_results, new_row)
}
### cleanup zipcode_results
zipcode_results <- as.data.frame(zipcode_results[-1, ])
colnames(zipcode_results) <- c("zip", "avg_revenue", "density", "connectedness", "avg_edge_str")
zipcode_results <- zipcode_results[, -c(4)] # drop connectedness

###### join data
zipcode_data <- zipcode_data %>%
  left_join(zipcode_results, by="zip")
zipcode_data <- na.omit(zipcode_data)
row.names(zipcode_data) <- zipcode_data$zip
zipcode_data <- zipcode_data[, -c(1)]

###### plot data
for (i in colnames(zipcode_data)) {
  par(mfrow=c(2, 2))
  hist(zipcode_data[, i], main=i)
  hist(log(zipcode_data[, i]), main=paste("log", i, sep="_"))
  qqnorm(zipcode_data[, i], main=i); qqline(zipcode_data[, i])
  qqnorm(log(zipcode_data[, i]), main=paste("log", i, sep="_")); qqline(log(zipcode_data[, i]))
}
### make the following transformations
# avg_edge_str - None
# log_density
zipcode_data$density <- log(zipcode_data$density)
# log_avg_revenue
zipcode_data$avg_revenue <- log(zipcode_data$avg_revenue)
# high_income_households - None
# per_capita_income - None
# avg_household_income - None
# median_household_income - None
# log_popden
zipcode_data$popden <- log(zipcode_data$popden)
# log_age
zipcode_data$age <- log(zipcode_data$age)

###### lasso regression
zipcode_data <- as.matrix(zipcode_data)
col <- which(colnames(zipcode_data) == "avg_revenue")
age_selected <- c()
popden_selected <- c()
Median.Household.Income_selected <- c()
Average.Household.Income_selected <- c()
Per.Capita.Income_selected <- c()
High.Income.Households_selected <- c()
density_selected <- c()  
avg_edge_str_selected <- c()
features <- colnames(zipcode_data[, -col])
lasso_features_coefficents <- matrix(, nrow=length(features))
for (i in 1:1000) {
  
  if (i == 500) {
    print("50% done...")
  }
  set.seed(i)
  fit_lasso <- cv.glmnet(zipcode_data[, -col], zipcode_data[, col],
                         type.measure="mse", alpha=1, family="gaussian",
                         nfolds=5, standardize=TRUE)
  preds <- predict(fit_lasso, s="lambda.1se", newx=zipcode_data[, -col])
  lasso_trainmse <- mean((preds - zipcode_data[, col])^2)
  lasso_coef <- coef(fit_lasso, s="lambda.1se")
  
  # keep track of frequency selected by LASSO
  age_selected <- c(age_selected, lasso_coef["age", ] != 0)
  popden_selected <- c(popden_selected, lasso_coef["popden", ] != 0)
  Median.Household.Income_selected <- c(Median.Household.Income_selected, lasso_coef["Median.Household.Income", ] != 0)
  Average.Household.Income_selected <- c(Average.Household.Income_selected, lasso_coef["Average.Household.Income", ] != 0)
  Per.Capita.Income_selected <- c(Per.Capita.Income_selected, lasso_coef["Per.Capita.Income", ] != 0)
  High.Income.Households_selected <- c(High.Income.Households_selected, lasso_coef["High.Income.Households", ] != 0)
  density_selected <- c(density_selected, lasso_coef["density", ] != 0)
  avg_edge_str_selected <- c(avg_edge_str_selected, lasso_coef["avg_edge_str", ] != 0)
  # keep track of coefficients
  lasso_features_coefficents <- cbind(lasso_features_coefficents, lasso_coef[-1, , drop=FALSE])
  
}

### lasso regression results
# frequency selected by LASSO
sum(age_selected) / 1000 # 0.695
sum(popden_selected) / 1000 # 0.752
sum(Median.Household.Income_selected) / 1000 # 0.602
sum(Average.Household.Income_selected) / 1000 # 0.001
sum(Per.Capita.Income_selected) / 1000 # 0
sum(High.Income.Households_selected) / 1000 # 0
sum(density_selected) / 1000 # 0
sum(avg_edge_str_selected) / 1000 # 0
# coefficients
features_coefficients <- as.data.frame(rowSums(lasso_features_coefficents[, -c(1), drop=FALSE], na.rm=TRUE) / 1000)
colnames(features_coefficients) <- c("lasso_coefficients")

###### random forest regression
set.seed(100)
rf <- randomForest(x=zipcode_data[, -col], y=zipcode_data[, col],
                   ntree=500, mtry=sqrt(dim(zipcode_data[, -col])[2]),
                   replace=TRUE, importance=TRUE)
### random forest results
rf$importance[, 1, drop=FALSE]
