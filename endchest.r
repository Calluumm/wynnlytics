library(dplyr)
library(tidyr)

item_map <- c(
  "eb" = "Emerald Block",
  "le" = "Liquid Emerald",
  "tmarathon2" = "Unidentified Fleetfooted Tome of the Marathon II",
  "tcloud2" = "Unidentified Clouded Tome of Defensive Mastery II",
  "tpulse2" = "Unidentified Pulsing Tome of Defensive Mastery II",
  "toceanic2" = "Unidentified Oceanic Tome of Defensive Mastery II",
  "mtfaerie" = "Unidentified Farie's Tome of Mysticism II",
  "mtdragon" = "Unidentified Dragon's Tome of Mysticism II",
  "mtvamp" = "Unidentified Vampiric Tome of Defensive Mastery II",
  "mteverlast" = "Unidentified Everlasting Tome of Defensive Mastery II",
  "mtgolem" = "Unidentified Golem's Tome of Mysticism II",
  "mtpegasus" = "Unidentified Pegasus' Tome of Mysticism II",
  "tbloom2" = "Unidentified Blooming Tome of Defensive Mastery II",
  "tcourage2" = "Unidentified Courageous Tome of Defensive Mastery II",
  "tephemeral2" = "Unidentified Ephemeral Tome of Mysticism II",
  "tharvest2" = "Unidentified Harvester's Tome of Mysticism II",
  "tsurefoot2" = "Unidentified Surefooted Tome of the Marathon II",
  "ucb" = "Varied Crafter Bag [1/1]",
  "pcb" = "Packed Crafter Bag [1/1]",
  "scb" = "Stuffed Crafter Bag [1/1]",
  "cots" = "Unidentified Charm of the Stone",
  "as" = "Ability Shard",
  "a1" = "Corkian Amplifier I",
  "a2" = "Corkian Amplifier II",
  "a3" = "Corkian Amplifier III",
  "ep4" = "Earth Powder IV", 
  "ep5" = "Earth Powder V",
  "tp4" = "Thunder Powder IV",
  "tp5" = "Thunder Powder V",
  "wp4" = "Water Powder IV",
  "wp5" = "Water Powder V",
  "fp4" = "Fire Powder IV",
  "fp5" = "Fire Powder V",
  "ap4" = "Air Powder IV",
  "ap5" = "Air Powder V",
  "titano" = "Unidentified Titanomachia",
  "insig" = "Unidentified Insignia",
  "Maze" = "Unidentified Mazeweaver",
  "Canno" = "Unidentified Cannonade",
  "Reso" = "Unidentified Resolution",
  "Ante" = "Unidentified Antebellum",
  "Wborne" = "Unidentified Windborne",
  "Succ" = "Unidentified Succession",
  "Disp" = "Unidentified Dispersion",
  "Recal" = "Unidentified Recalcintrance",
  "Obs" = "Unidentified Obstinance",
  "Comp" = "Unidentified Compliance"

)

expand_items <- function(x) {
  if (length(x) == 0) return(character(0))
  misses <- setdiff(x, names(item_map))
  if (length(misses) > 0) warning("Unmapped acronyms: ", paste(misses, collapse = ", "))
  vapply(x, function(v) if (v %in% names(item_map)) item_map[[v]] else v, character(1))
}

raid_pools_acr <- list(
  NOTG = c(),
  NOL  = c(),
  TNA  = c(),
  TCC  = c("eb", "le", "tmarathon2", "tcloud2", "tpulse2", "toceanic2", 
           "mtfaerie", "mtdragon", "mtvamp", "mteverlast", "mtgolem", "mtpegasus", 
           "tbloom2", "ucb", "pcb", "scb", "cots", "as", "a1", "a2", "a3", 
           "ep4", "ep5", "tp4", "tp5", "wp4", "wp5", "fp4", "fp5", "ap4", "ap5", 
           "titano", "insig", "Maze", "Canno", "Reso", "Ante", "Wborne", 
           "Succ", "Disp", "Recal", "Obs", "Comp", "tharvest2", "tsurefoot2", "tephemeral2", "tcourage2" )
)
raid_pools <- lapply(raid_pools_acr, expand_items)

raid_amount_pools <- list(
  NOTG = list(),
  NOL  = list(),
  TNA  = list(),
  TCC  = list(
    "Emerald Block"  = c(18, 12, 24, 16, 8, 16, 12, 8),
    "Liquid Emerald" = c(1, 1, 1),
    "Abilitiy Shard" = c(1, 2),
    "Thunder Powder IV" = c(2),
    "Corkian Amplifier I" = c(3, 2, 2),
    "Corkian Amplifier II" = c(2, 1),
    "Corkian Amplifier III" = c(1, 1)
  )
)
#csv format
#Raid	Timestamp	Total Pulls	Pull 1 Item	Pull 1 Count	Pull 2 Item	Pull 2 Count	Pull 3 Item	Pull 3 Count	Pull 4 Item	Pull 4 Count	Pull 5 Item	Pull 5 Count	Pull 6 Item	Pull 6 Count	Pull 7 Item	Pull 7 Count	Pull 8 Item	Pull 8 Count	Pull 9 Item	Pull 9 Count	Pull 10 Item	Pull 10 Count	Pull 11 Item	Pull 11 Count	Pull 12 Item	Pull 12 Count	Pull 13 Item	Pull 13 Count	Pull 14 Item	Pull 14 Count	Pull 15 Item	Pull 15 Count	Pull 16 Item	Pull 16 Count	Pull 17 Item	Pull 17 Count	Pull 18 Item	Pull 18 Count	Pull 19 Item	Pull 19 Count	Pull 20 Item	Pull 20 Count	Pull 21 Item	Pull 21 Count	Pull 22 Item	Pull 22 Count	Pull 23 Item	Pull 23 Count	Pull 24 Item	Pull 24 Count	Pull 25 Item	Pull 25 Count	Pull 26 Item	Pull 26 Count	Pull 27 Item	Pull 27 Count
raidr <- read.csv("raiddays/endchest/raid_rewards_data.csv", check.names = TRUE)
raidr <- raidr |> 
  mutate(
    run_id = row_number(),
    Raid = case_when(
      Raid %in% c("tcc", "TCC", "The Canyon Colossus") ~ "TCC",
      Raid %in% c("notg", "NOTG", "Nest of the Grootslangs") ~ "NOTG",
      Raid %in% c("nol", "NOL", "Nexus of Light") ~ "NOL",
      Raid %in% c("tna", "TNA", "The Nameless Anomaly") ~ "TNA",
      TRUE ~ Raid
    )
  )

pull_cols <- grep("^Pull\\.\\d+\\.(Item|Count)$", names(raidr), value = TRUE)

raid_long <- raidr |>
  select(Raid, Total.Pulls, run_id, all_of(pull_cols)) |>
  pivot_longer(
    cols = all_of(pull_cols),
    names_to = c("pull", "field"),
    names_pattern = "Pull\\.(\\d+)\\.(Item|Count)",
    values_to = "value",
    values_transform = as.character
  ) |>
  pivot_wider(names_from = field, values_from = value) |>
  mutate(
    pull = as.integer(pull),
    Item = trimws(as.character(Item)),
    Item = ifelse(Item %in% names(item_map), item_map[Item], Item),
    Count = suppressWarnings(as.integer(Count))
  ) |>
  filter(!is.na(Item) & Item != "" & Item != "Air" & !is.na(Count) & Count > 0)

prob_table <- raid_long |>
  group_by(Raid, Item) |>
  summarise(
    total_count = sum(Count, na.rm = TRUE),
    occurrences = sum(Count > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    raidr |>
      group_by(Raid) |>
      summarise(
        total_pulls = sum(Total.Pulls, na.rm = TRUE),
        runs = n(),
        .groups = "drop"
      ),
    by = "Raid"
  ) |>
  mutate(
    prob_per_pull = total_count / total_pulls,
    prob_presence = occurrences / runs
  ) |>
  arrange(Raid, desc(prob_per_pull))

count_dist <- raid_long |>
  group_by(Raid, Item, Count) |>
  summarise(pulls = n(), .groups = "drop_last") |>
  mutate(pct_within_item = pulls / sum(pulls)) |>
  arrange(Raid, Item, Count)

eb_le_run_totals <- raid_long |>
  filter(Item %in% c("Emerald Block", "Liquid Emerald")) |>
  group_by(Raid, run_id, Item) |>
  summarise(run_total = sum(Count, na.rm = TRUE), .groups = "drop") |>
  group_by(Raid, Item, run_total) |>
  summarise(runs = n(), .groups = "drop_last") |>
  mutate(pct_runs = runs / sum(runs)) |>
  arrange(Raid, Item, run_total)

items_wide <- raid_long |>
  select(Raid, run_id, Item, Count) |>
  group_by(Raid, run_id, Item) |>
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Item, values_from = Count, values_fill = 0)

emerald_factors <- items_wide |>
  mutate(
    total_emerald_count = `Emerald Block` + `Liquid Emerald`,
    total_non_emerald = rowSums(select(items_wide, -Raid, -run_id, -`Emerald Block`, -`Liquid Emerald`)),
    emerald_prop = ifelse(total_non_emerald + total_emerald_count > 0, 
                          total_emerald_count / (total_non_emerald + total_emerald_count), 
                          0),
    emerald_count_groups = cut(total_emerald_count, 
                               breaks = c(-Inf, 10, 20, 30, Inf), 
                               labels = c("Low", "Medium", "High", "Very High"))
  ) |>
  select(Raid, run_id, total_emerald_count, emerald_prop, emerald_count_groups)

emerald_model <- lm(total_emerald_count ~ emerald_prop + emerald_count_groups, 
                    data = emerald_factors)

pca_res <- NULL
pca_input <- items_wide |> select(-Raid, -run_id)
if (nrow(pca_input) > 1 && ncol(pca_input) > 1) {
  non_zero_var <- apply(pca_input, 2, var) > 0                                  #add PCA elipses n shit when i get actual data and see that we dont have like 90% variance in pc1
  if (sum(non_zero_var) > 1) {
    pca_res <- prcomp(pca_input[, non_zero_var], center = TRUE, scale. = TRUE)
  }
}

coverage_table <- bind_rows(lapply(names(raid_pools), function(r) {
  obs <- prob_table$Item[prob_table$Raid == r]
  data.frame(
    Raid = r,
    observed_items = length(unique(obs)),
    possible_items = length(raid_pools[[r]]),
    missing_items = paste(setdiff(raid_pools[[r]], unique(obs)), collapse = ", "),
    stringsAsFactors = FALSE
  )
}))

pool_amounts_table <- bind_rows(lapply(names(raid_amount_pools), function(r) {
  if (length(raid_amount_pools[[r]]) == 0) return(NULL)
  bind_rows(lapply(names(raid_amount_pools[[r]]), function(it) {
    data.frame(
      Raid = r,
      Item = it,
      allowed_counts = paste(raid_amount_pools[[r]][[it]], collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}))

#prints n stuff
cat("\nsummarys\n\n")

cat("by raid\n")
print(coverage_table)

cat("\n top items\n")
print(prob_table |> group_by(Raid) |> slice_head(n = 50))

cat("\nquant distribute\n")
print(count_dist |> filter(n() > 1))

cat("\njust eb/le\n")
print(eb_le_run_totals)

cat("\nexpected\n")
print(pool_amounts_table)

if (!is.null(pca_res)) {
  cat("\npca\n")
  print(summary(pca_res))
  cat("\nPC1 & PC2 Loadings:\n")
  print(pca_res$rotation[, 1:min(2, ncol(pca_res$rotation))])
}

cat("\nemerald factor regression\n")
print(summary(emerald_model))
cat("\nem fac summary\n")
print(summary(emerald_factors))

