# read in both Idaho CSVs
id_fund <- read.csv("12-Idaho_Fundable.csv")
id_comp <- read.csv("12-Idaho_PPL.csv")

# generate new fundable column indicating overlap between 
# comprehensive and fundable tables

id_comp$Fundable <- 
  if_else(id_comp$Rank %in% id_fund$Rank, 'Yes', 'No')

write.csv(id_comp, "12-Idaho-Overlap.csv", row.names=FALSE)
unlink(c("12-Idaho_Fundable.csv","12-Idaho_PPL.csv"))
