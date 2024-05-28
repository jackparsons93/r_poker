library(pokerstoveR)
getwd()
poker = read.csv("poker.csv")
head(poker)
current_path <- Sys.getenv("PATH")
new_path <- paste(current_path, "/home/jackparsons210/pokerstove/build/bin/", sep=":")
Sys.setenv(PATH = new_path)


list1 <- c("AcAs", "KdKs", "QhQs")
list2 <- c("2h7d", "JcTc", "8s8h")

# Initialize the results data frame
results <- data.frame(hand = character(), equity = numeric(), wins = numeric(), ties = numeric(), list_group = character())

# Loop over each pair of hands from both lists and collect results
for (hand1 in list1) {
  for (hand2 in list2) {
    res <- ps_eval(hand1, hand2)  # Get results from the evaluation function
    res$list_group <- ifelse(res$hand == hand1, "list1", "list2")  # Add group information
    results <- rbind(results, res)  # Store the results
  }
}

# Calculate the number of occurrences for each hand in each group to compute the average equity
hand_counts <- table(results$hand, results$list_group)

# Aggregate results by hand and group
aggregate_results <- aggregate(cbind(equity, wins, ties) ~ hand + list_group, data = results, FUN = sum)

# Calculate the average equity by dividing the sum by the number of occurrences
aggregate_results$equity <- mapply(function(equity, hand, group) equity / hand_counts[hand, group], 
                                   aggregate_results$equity, 
                                   aggregate_results$hand, 
                                   aggregate_results$list_group)

# Output the aggregated results
print(aggregate_results)


# Calculate total equity for each list
average_equity_list1 <- sum(aggregate_results$equity[aggregate_results$list_group == "list1"]) / length(list1)
average_equity_list2 <- sum(aggregate_results$equity[aggregate_results$list_group == "list2"]) / length(list2)

# Output the average equity for each list
cat("Average equity for list1:", average_equity_list1, "\n")
cat("Average equity for list2:", average_equity_list2, "\n")

# Compare the average equities
if (average_equity_list1 > average_equity_list2) {
  cat("List1 has a higher average equity.\n")
} else if (average_equity_list1 < average_equity_list2) {
  cat("List2 has a higher average equity.\n")
} else {
  cat("Both lists have equal average equity.\n")
}

