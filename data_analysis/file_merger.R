### master df
master_df <- data.frame(
  id = c("gg_1", "gg_2", "gg_3", "gg_4", "gg_5", "gg_6", "gg_7", "gg_8", "gg_9",
         "gg_10", "gg_11", "gg_12","gg_13", "gg_14", "gg_15", "gg_16","gg_17",
         "gg_18", "gg_19","gg_20", "gg_21", "gg_22", "gg_23", "gg_24", "gg_25", 
         "gg_26", "gg_27", "gg_28", "gg_29", "gg_30", "gg_31", "gg_32", "gg_33", 
         "gg_34", "gg_35", "gg_36", "gg_37", "gg_38", "gg_39", "gg_40")
)
master_df
length(master_df$id) 

### adding other data frames
vqmax_24 <- read.csv("vqmax24_df.csv")
vqmax_30 <- read.csv("vqmax30_df.csv")
vqmax_36 <- read.csv("vqmax36_df.csv")

### cleanup
vqmax_24 <- vqmax_24[, !names(vqmax_24) %in% "X"]
vqmax_30 <- vqmax_30[, !names(vqmax_30) %in% "X"]
vqmax_36 <- vqmax_36[, !names(vqmax_36) %in% "X"]

### merging
merged_df <- merge(
  master_df,
  vqmax_24,
  by = "id",
  all.x = TRUE
)
master_df
