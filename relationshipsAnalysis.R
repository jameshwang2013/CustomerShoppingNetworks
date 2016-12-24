###### imports
library(random)
library(dplyr)
library(sna)
library(igraph)

###### setwd
setwd("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case")

###### get data
nodes <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/nodes.csv")
links <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/links.csv")
names <- read.csv("~/Documents/Dropbox/Career/2. Data Science Interviews/BCG Gamma F16/2nd Round Technical Case/random_names.csv")

###### rename nodes
node_name_id_map <- cbind(names, nodes)[, c(1, 2)]
nodes <- cbind(names, nodes)[, c(1, 3)]
names(nodes) <- c("node", "revenueYear")
for (i in node_name_id_map$node) {
  links[links$fromNode==i, 1] <- as.character(node_name_id_map[node_name_id_map$node==i,1])
  links[links$toNode==i, 2] <- as.character(node_name_id_map[node_name_id_map$node==i,1])
}

###### Create graph
edges <- as.matrix(links[, c(1, 2)])
g <- graph_from_edgelist(edges, directed=TRUE)

###############################################################################
###### ANALYSIS ###############################################################
###############################################################################

###### Question: Are customers often paired with others who have similar number
###### of friends, or is it typically one sided?
### create degree dataframe
# calculate degrees
out_degrees <- degree(g, mode="out")
in_degrees <- degree(g, mode="in")
tot_degrees <- degree(g, mode="total")
# build dataframes for each degree type
out_deg_matrix <- as.matrix(degree(g, mode="out"))
out_deg_matrix <- data.frame(row.names(out_deg_matrix), out_deg_matrix, row.names=NULL)
colnames(out_deg_matrix) <- c("node", "out_degree")
in_deg_matrix <- as.matrix(degree(g, mode="in"))
in_deg_matrix <- data.frame(row.names(in_deg_matrix), in_deg_matrix, row.names=NULL)
colnames(in_deg_matrix) <- c("node", "in_degree")
tot_deg_matrix <- as.matrix(degree(g, mode="total"))
tot_deg_matrix <- data.frame(row.names(tot_deg_matrix), tot_deg_matrix, row.names=NULL)
colnames(tot_deg_matrix) <- c("node", "tot_degree")
# combine dataframes
degrees <- out_deg_matrix %>%
  left_join(in_deg_matrix, by="node") %>%
  left_join(tot_deg_matrix, by="node")
### calculate degree similarity
# build dataframe
fromNode_degrees <- links %>%
  left_join(degrees, by=c("fromNode" = "node")) %>%
  select(out_degree, in_degree, tot_degree)
toNode_degrees <- links %>%
  left_join(degrees, by=c("toNode" = "node")) %>%
  select(out_degree, in_degree, tot_degree)
diff_degrees <- fromNode_degrees - toNode_degrees
# plot histogram of differences
par(mfrow=c(1, 1))
hist(diff_degrees$out_degree)
hist(diff_degrees$in_degree)
hist(hist(diff_degrees$tot_degree))
# perform bootstrapped t-tests on differences
t_stat_matrix <- matrix(, ncol=3)
for (i in 1:1000) {
  print(i)
  ix <- sample(c(1:nrow(diff_degrees)), size=0.7*nrow(diff_degrees), replace=TRUE)
  boot <- diff_degrees[ix, ]
  row <- c(t.test(x=boot$out_degree, mu=0)$estimate,
           t.test(x=boot$in_degree, mu=0)$estimate,
           t.test(x=boot$tot_degree, mu=0)$estimate)
  t_stat_matrix <- rbind(t_stat_matrix, row)
}
t_statistics <- as.data.frame(t_stat_matrix[-1, ])
colnames(t_statistics) <- c("out_degree", "in_degree", "tot_degree")
row.names(t_statistics) <- NULL
# plot histogram of bootstrapped mean estimates
hist(t_statistics$out_degree, main="Bootstrapped Estimates of Mean Difference in Degrees (Out)", xlab="Mean Difference in Degrees (Out)")
hist(t_statistics$in_degree, main="Bootstrapped Estimates of Mean Difference in Degrees (In)", xlab="Mean Difference in Degrees (In)")
hist(t_statistics$tot_degree, main="Bootstrapped Estimates of Mean Difference in Degrees (Tot)", xlab="Mean Difference in Degrees (Tot)")
# 95% CI of bootstrapped mean estimates
se_out <- sd(t_statistics$out_degree) / sqrt(length(t_statistics$out_degree))
ci_out <- c(mean(t_statistics$out_degree) - 1.96 * se_out, mean(t_statistics$out_degree) + 1.96 * se_out)
  # 1.888827 1.893311
se_in <- sd(t_statistics$in_degree) / sqrt(length(t_statistics$in_degree))
ci_in <- c(mean(t_statistics$in_degree) - 1.96 * se_in, mean(t_statistics$in_degree) + 1.96 * se_in)
 # -0.6694263 -0.6676228
se_tot <- sd(t_statistics$tot_degree) / sqrt(length(t_statistics$tot_degree))
ci_tot <- c(mean(t_statistics$tot_degree) - 1.96 * se_tot, mean(t_statistics$tot_degree) + 1.96 * se_tot)
  # 1.219882 1.225207


###### Question: Are customers often paired with others who have similar spending
###### of friends, or is it typically one sided?
### calculate differences in spending (all)
spending <- links %>%
  left_join(nodes, by=c("fromNode" = "node")) %>%
  left_join(nodes, by=c("toNode" = "node")) %>%
  mutate(diff = revenueYear.x - revenueYear.y) %>%
  select(diff)
# plot histogram of differences
hist(spending$diff)
# perform t-test on differences
t_stat_matrix <- matrix(, ncol=1)
for (i in 1:1000) {
  print(i)
  ix <- sample(c(1:nrow(spending)), size=0.7*nrow(spending), replace=TRUE)
  boot <- spending[ix, ]
  row <- c(t.test(x=boot, mu=0, alternative="two.sided")$estimate)
  t_stat_matrix <- rbind(t_stat_matrix, row)
}
t_statistics <- as.data.frame(t_stat_matrix[-1, ])
colnames(t_statistics) <- c("diff")
row.names(t_statistics) <- NULL
# plot histogram of bootstrapped mean estimates
hist(t_statistics$diff, main="Bootstrapped Estimates of Mean Difference in Spending (All Relationships)", xlab="Mean Difference in Spending ($)")

### calculate differences in spending (high spenders)
high_quantiles <- c(0.75, 0.9, 0.95, 0.99)
low_quantiles <- c(0.01, 0.05, 0.1, 0.25)
middle_quantiles <- c(0.5)
range = 0.2
# build dataframe of quantiles
quantiles <- low_quantiles
diff_spending_quantiles <- list()
for (i in quantiles) {
  # !!! "<" (low_quantiles), ">" (high_quantiles)
  q <- quantile(nodes$revenueYear, probs=i)
  # !!! ">" 0.20 and "<" 0.80 (middle_quantiles)
  subset_nodes <- nodes$node[nodes$revenueYear < q]
  #q_low <- quantile(nodes$revenueYear, probs=(i - range))
  #q_high <- quantile(nodes$revenueYear, probs=(i + range))
  #subset_nodes <- nodes$node[(nodes$revenueYear > q_low) & (nodes$revenueYear < q_high)]
  subset_links <- links[links$fromNode %in% subset_nodes,]
  spending <- subset_links %>%
    left_join(nodes, by=c("fromNode" = "node")) %>%
    left_join(nodes, by=c("toNode" = "node")) %>%
    mutate(diff = revenueYear.x - revenueYear.y) %>%
    select(diff)
  diff_spending_quantiles[[as.character(i)]] = spending$diff
}
# perform t-test on differences
ind <- 1
t_statistics_lst <- list()
for (i in names(diff_spending_quantiles)) {
  
  q <- diff_spending_quantiles[[i]]
  t_stat_matrix <- matrix(, ncol=1)
  for (j in 1:1000) {
    
    ix <- sample(c(1:length(q)), size=0.7*length(q), replace=TRUE)
    boot <- spending[ix, ]
    row <- c(t.test(x=boot, mu=0, alternative="two.sided")$estimate)
    t_stat_matrix <- rbind(t_stat_matrix, row)
  }
  t_statistics <- as.data.frame(t_stat_matrix[-1, ])
  colnames(t_statistics) <- c("diff")
  row.names(t_statistics) <- NULL
  
  t_statistics_lst[[i]] <- t_statistics
  ind <- ind + 1
}
# plot histogram of bootstrapped mean estimates at each quantile
par(mfrow=c(2,2))
hist(t_statistics_lst$`0.75`$diff, main="Bootstrapped Mean Diff in Spending (Top 25%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.9`$diff, main="Bootstrapped Mean Diff in Spending (Top 10%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.95`$diff, main="Bootstrapped Mean Diff in Spending (Top 5%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.99`$diff, main="Bootstrapped Mean Diff in Spending (Top 1%)", xlab="Mean Differences in Spending")

par(mfrow=c(2,2))
hist(t_statistics_lst$`0.01`$diff, main="Bootstrapped Mean Diff in Spending (Lower 1%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.05`$diff, main="Bootstrapped Mean Diff in Spending (Lower 5%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.1`$diff, main="Bootstrapped Mean Diff in Spending (Lower 10%)", xlab="Mean Differences in Spending")
hist(t_statistics_lst$`0.25`$diff, main="Bootstrapped Mean Diff in Spending (Lower 25%)", xlab="Mean Differences in Spending")

par(mfrow=c(1, 1))
hist(t_statistics_lst$`0.5`$diff, main="Bootstrapped Mean Differences in Spending (Middle 40%)", xlab="Mean Differences in Spending") # middle 40%
