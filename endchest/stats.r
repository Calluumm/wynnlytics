library(dplyr)
library(tidyr)

item_map <- c(
  # Tomes

  "meda" = "Seismic Tome of Combat Mastery III",
  "mtda" = "Voltaic Tome of Combat Mastery III",
  "mwda" = "Abyssal Tome of Combat Mastery III",
  "mfda" = "Infernal Tome of Combat Mastery III",
  "mada" = "Cyclonic Tome of Combat Mastery III",

  "mhef" = "Tome of Remedial Expertise III",

  "m1st" = "Faerie's Tome of Mysticism II",
  "m2nd" = "Pegasus' Tome of Mysticism II",
  "m3rd" = "Dragon's Tome of Mysticism II",
  "m4th" = "Golem's Tome of Mysticism II",

  "mhpr" = "Everlasting Tome of Defensive Mastery II",
  "mlst" = "Vampiric Tome of Defensive Mastery II",

  "fsdr" = "Esoteric Tome of Combat Mastery III",
  "fsdp" = "Sorcerer's Tome of Combat Mastery III",
  "fmdr" = "Nimble Tome of Combat Mastery III",
  "fmdp" = "Fighter's Tome of Combat Mastery III",

  "ftho" = "Tome of Countering Expertise III",
  "fref" = "Tome of Parrying Expertise III",
  "fste" = "Tome of Scavenging Expertise III",
  "fhrp" = "Tome of Triage Expertise III",

  "fedf" = "Blooming Tome of Defensive Mastery II",
  "ftdf" = "Pulsing Tome of Defensive Mastery II",
  "fwdf" = "Oceanic Tome of Defensive Mastery II",
  "ffdf" = "Courageous Tome of Defensive Mastery II",
  "fadf" = "Clouded Tome of Defensive Mastery II",

  "fmr" = "Ephemeral Tome of Mysticism II",
  "fms" = "Harvester's Tome of Mysticism II",
  "fws" = "Fleetfooted Tome of the Marathon II",
  "fspr" = "Surefooted Tome of the Marathon II",

  # Misc Rewards

  "eb" = "Emerald Block",
  "le" = "Liquid Emerald",
  "vcb" = "Varied Crafter Bag [1/1]",
  "pcb" = "Packed Crafter Bag [1/1]",
  "scb" = "Stuffed Crafter Bag [1/1]",
  "as" = "Ability Shard",
  "amp1" = "Corkian Amplifier I",
  "amp2" = "Corkian Amplifier II",
  "amp3" = "Corkian Amplifier III",

  # Powders

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

  # Short powder acronyms seen in data (map to same long names)
  "e5" = "Earth Powder V",
  "t5" = "Thunder Powder V",
  "w5" = "Water Powder V",
  "f5" = "Fire Powder V",
  "a5" = "Air Powder V",
  "e4" = "Earth Powder IV",
  "t4" = "Thunder Powder IV",
  "w4" = "Water Powder IV",
  "f4" = "Fire Powder IV",
  "a4" = "Air Powder IV",

  # Unid Raid Items

  "cnog" = "Unidentified Charm of the Worm",
  "cnol" = "Unidentified Charm of the Light",
  "ctcc" = "Unidentified Charm of the Stone",
  "ctna" = "Unidentified Charm of the Void",

  "exti" = "Unidentified Extinction",
  "symb" = "Unidentified Symbiont",
  "home" = "Unidentified Homeorhesis",
  "atav" = "Unidentified Atavistic",
  "apex" = "Unidentified Apex Predator",
  "eyes" = "Unidentified Eyes on All",

  "atha" = "Unidentified Athanasia",
  "caco" = "Unidentified Cacophany",
  "prov" = "Unidentified Provenance",
  "vene" = "Unidentified Veneration",
  "meta" = "Unidentified Metamorphosis",
  "reck" = "Unidentified Reckoning",

  "tita" = "Unidentified Titanomachia",
  "insi" = "Unidentified Insignia",
  "reso" = "Unidentified Resolution",
  "cann" = "Unidentified Cannonade",
  "ante" = "Unidentified Antebellum",
  "wind" = "Unidentified Windborne",
  "maze" = "Unidentified Mazeweaver",
  "disp" = "Unidentified Dispersion",
  "obst" = "Unidentified Obstinance",
  "succ" = "Unidentified Succession",
  "comp" = "Unidentified Compliance",
  "reca" = "Unidentified Recalcitrance",
  "abra" = "Unidentified Abrasion",

  # TNA IS ####### STUPID AND THE SET ITEMS ARE BOXED
  "none" = "Unidentified Helmet", # "Unidentified Nonexistence"
  "alep" = "Unidentified Leggings",  # "Unidentified Aleph Null",
  "infi" = "Unidentified Dagger", # "Unidentified Infinitesmal",
  "dive" = "Unidentified Spear", # "Unidentified Divergence",
  "cont" = "Unidentified Wand", # "Unidentified Continuum",
  "recu" = "Unidentified Bow", # "Unidentified Recursion",
  "frac" = "Unidentified Relik", # "Unidentified Fractal",
  "fore" = "Unidentified Forebearance",
  "ingr" = "Unidentified Ingress",
  "brea" = "Unidentified Breakthrough",
  "deta" = "Unidentified Detachment",
  "coll" = "Unidentified Collection",
  "simu" = "Unidentified Simulacrum"
)

expand_items <- function(x) {
  if (length(x) == 0) return(character(0))
  misses <- setdiff(x, names(item_map))
  if (length(misses) > 0) warning("Unmapped acronyms: ", paste(misses, collapse = ", "))
  vapply(x, function(v) if (v %in% names(item_map)) item_map[[v]] else v, character(1))
}
######CHANGES WEEKLY###########
raid_pools_acr <- list(
  NOTG = c(
    "mhef", "mada", "mtda", "mlst", "m4th", "m3rd",
    "ftdf", "fsdp", "fedf", "fadf", "fwdf", "fws", "fms", "fste", "fmr",
    "eb", "le", "vcb", "pcb", "scb",
    "cnog", "exti", "symb", "home", "atav", "apex", "eyes",
    "amp1", "amp2", "amp3", "as",
    "e5", "t5", "w5", "f5", "a5", "e4", "t4", "w4", "f4", "a4"
  ),
  NOL  = c(
    "mhef", "mfda", "mtda", "mwda", "meda", "mada",
    "fsdp", "fmdr", "fsdr", "fmdp", "fste", "ftho", "fref", "fhrp",
    "eb", "le", "vcb", "pcb", "scb",
    "cnol", "atha", "caco", "prov", "vene", "meta", "reck",
    "amp1", "amp2", "amp3", "as",
    "e5", "t5", "w5", "f5", "a5", "e4", "t4", "w4", "f4", "a4"
  ),
  TCC  = c(
    "mhpr", "mlst", "m1st", "m2nd", "m4th", "m3rd",
    "fedf", "ftdf", "fwdf", "ffdf", "fadf", "fmr", "fms", "fws", "fspr",
    "eb", "le", "vcb", "pcb", "scb",
    "ctcc", "tita", "insi", "reso", "cann", "ante", "wind",
    "maze", "disp", "obst", "succ", "comp", "reca", "abra",
    "amp1", "amp2", "amp3", "as",
    "e5", "t5", "w5", "f5", "a5", "e4", "t4", "w4", "f4", "a4"
  ),
  TNA  = c(
    "meda", "mtda", "mwda", "mfda", "mada", "mhef",
    "fsdr", "fsdp", "fmdr", "fmdp", "fhrp", "fref", "ftho", "fste",
    "eb", "le", "vcb", "pcb", "scb",
    "ctna", "none", "alep", "infi", "dive", "recu", "frac",
    "fore" , "ingr", "brea", "deta", "coll", "simu",
    "amp1", "amp2", "amp3", "as",
    "e5", "t5", "w5", "f5", "a5", "e4", "t4", "w4", "f4", "a4"
  )
)
raid_pools <- lapply(raid_pools_acr, expand_items)

raid_amount_pools <- list(
  NOTG = list(
    "Emerald Block"  = c(18, 15, 24, 12, 15, 8, 8, 8, 16, 12),
    "Liquid Emerald" = c(1, 1, 1),
    "Ability Shard" = c(1, 2),
    "Air Powder V" = c(2),
    "Earth Powder IV" = c(2),
    "Air Powder IV" = c(2),
    "Corkian Amplifier I" = c(1, 2, 2),
    "Corkian Amplifier II" = c(2, 3),
    "Corkian Amplifier III" = c(1, 1)
  ),
  NOL  = list(
    "Emerald Block"  = c(16, 32, 16, 24, 12, 16, 12, 16, 16),
    "Liquid Emerald" = c(1, 2, 1),
    "Ability Shard" = c(1, 2),
    "Earth Powder V" = c(2),
    "Fire Powder V" = c(2),
    "Thunder Powder IV" = c(3, 1),
    "Corkian Amplifier I" = c(1, 2, 1),
    "Corkian Amplifier II" = c(1, 2),
    "Corkian Amplifier III" = c(1, 1)
  ),
  TCC  = list(
    "Emerald Block"  = c(18, 12, 24, 16, 8, 16, 12, 8),
    "Liquid Emerald" = c(1, 1, 1),
    "Ability Shard" = c(1, 2),
    "Thunder Powder IV" = c(2),
    "Corkian Amplifier I" = c(3, 2, 2),
    "Corkian Amplifier II" = c(2, 1),
    "Corkian Amplifier III" = c(1, 1)
  ),
  TNA  = list(
    "Emerald Block"  = c(12, 12, 12, 12, 15, 18, 8, 16),
    "Liquid Emerald" = c(1, 1, 1, 1),
    "Ability Shard" = c(1, 2),
    "Earth Powder V" = c(2),
    "Fire Powder V" = c(2),
    "Air Powder V" = c(2),
    "Water Powder V" = c(2),
    "Air Powder IV" = c(2),
    "Corkian Amplifier I" = c(2, 2, 2),
    "Corkian Amplifier II" = c(1, 1),
    "Corkian Amplifier III" = c(1, 1)
  )
)
##########END OF CHANGES WEEKLY###########
         
#csv format
#Raid	Timestamp	Total Pulls	Pull 1 Item	Pull 1 Count	Pull 2 Item	Pull 2 Count	Pull 3 Item	Pull 3 Count	Pull 4 Item	Pull 4 Count	Pull 5 Item	Pull 5 Count	Pull 6 Item	Pull 6 Count	Pull 7 Item	Pull 7 Count	Pull 8 Item	Pull 8 Count	Pull 9 Item	Pull 9 Count	Pull 10 Item	Pull 10 Count	Pull 11 Item	Pull 11 Count	Pull 12 Item	Pull 12 Count	Pull 13 Item	Pull 13 Count	Pull 14 Item	Pull 14 Count	Pull 15 Item	Pull 15 Count	Pull 16 Item	Pull 16 Count	Pull 17 Item	Pull 17 Count	Pull 18 Item	Pull 18 Count	Pull 19 Item	Pull 19 Count	Pull 20 Item	Pull 20 Count	Pull 21 Item	Pull 21 Count	Pull 22 Item	Pull 22 Count	Pull 23 Item	Pull 23 Count	Pull 24 Item	Pull 24 Count	Pull 25 Item	Pull 25 Count	Pull 26 Item	Pull 26 Count	Pull 27 Item	Pull 27 Count
raidr <- read.csv("raiddays/endchest/raid_rewards_data.csv", check.names = TRUE)
raidr <- raidr |> distinct()
raidr <- raidr |> filter(Total.Pulls <= 40)
#raidr <- raidr |> filter(Timestamp ><><><>) set some inequality to a date range

raidr <- raidr |> 
  mutate(
    run_id = row_number(),
    Raid = case_when(
      Raid %in% c("tcc", "TCC", "The Canyon Colossus") ~ "TCC",
      Raid %in% c("notg", "NOTG", "Nest of the Grootslangs") ~ "NOTG",
      Raid %in% c("nol", "NOL", "Orphion's Nexus of Light") ~ "NOL",
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

raid_long_agg <- raid_long |>
  mutate(
    Item_clean = gsub("^Unidentified ", "", Item),
    acronym = names(item_map)[match(Item_clean, unname(item_map))],
    Item_Agg = case_when(
      !is.na(acronym) & grepl("^m", acronym) & grepl("Tome", Item) ~ "Mythic Tome",
      !is.na(acronym) & grepl("^f", acronym) & grepl("Tome", Item) ~ "Fabled Tome",
      TRUE ~ Item
    )
  )

prob_table_agg <- raid_long_agg |>
  group_by(Raid, Item_Agg) |>
  summarise(
    total_count = sum(Count, na.rm = TRUE),
    occurrences = n(),
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
    prob_per_pull = occurrences / total_pulls,
    prob_presence = occurrences / (runs * total_pulls / runs)
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

eb_long <- raid_long |> filter(Item == "Emerald Block")

eb_per_pull_prob <- raidr |>
  select(Raid, run_id, Total.Pulls) |>
  left_join(
    eb_long |> group_by(Raid, run_id) |> summarise(eb_pulls = n(), .groups = "drop"),
    by = c("Raid", "run_id")
  ) |>
  replace_na(list(eb_pulls = 0)) |>
  group_by(Raid) |>
  summarise(
    total_pulls = sum(Total.Pulls),
    total_eb_pulls = sum(eb_pulls),
    prob_any_eb_stack = total_eb_pulls / total_pulls,
    .groups = "drop"
  )

eb_stack_dist <- eb_long |>
  group_by(Raid, Count) |>
  summarise(stacks = n(), .groups = "drop_last") |>
  mutate(prob_this_stack = stacks / sum(stacks)) |>
  arrange(Raid, Count)

eb_stacks_per_run <- eb_long |>
  group_by(Raid, run_id) |>
  summarise(num_eb_stacks = n(), .groups = "drop")

eb_stacks_summary <- eb_stacks_per_run |>
  group_by(Raid, num_eb_stacks) |>
  summarise(runs = n(), .groups = "drop_last") |>
  mutate(prob_n_stacks = runs / sum(runs))

eb_conditional <- eb_long |>
  left_join(eb_stacks_per_run, by = c("Raid", "run_id")) |>
  group_by(Raid, num_eb_stacks, Count) |>
  summarise(stacks = n(), .groups = "drop_last") |>
  mutate(prob_stack_size_given_n = stacks / sum(stacks)) |>
  arrange(Raid, num_eb_stacks, Count)

eb_chisq_tests <- lapply(split(eb_long |> left_join(eb_stacks_per_run, by = c("Raid", "run_id")), 
                                eb_long$Raid), function(df) {
  tab <- table(df$num_eb_stacks, df$Count)
  if (min(dim(tab)) < 2 || sum(tab) < 20) return(NULL)
  tryCatch(chisq.test(tab), error = function(e) NULL)
})

items_wide_agg <- raid_long_agg |>
  select(Raid, run_id, Item_Agg, Count) |>
  group_by(Raid, run_id, Item_Agg) |>
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Item_Agg, values_from = Count, values_fill = 0)

pca_res_agg <- NULL
pca_input_agg <- items_wide_agg |> select(-Raid, -run_id)
if (nrow(pca_input_agg) > 1 && ncol(pca_input_agg) > 1) {
  non_zero_var_agg <- apply(pca_input_agg, 2, var) > 0
  if (sum(non_zero_var_agg) > 1) {
    pca_res_agg <- prcomp(pca_input_agg[, non_zero_var_agg], center = TRUE, scale. = TRUE)
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

cat("\nProbability of ANY EB stack per pull:\n")
print(eb_per_pull_prob)

cat("\nEB stack size distribution (prob of each stack size among EB stacks):\n")
print(eb_stack_dist)

cat("\nNumber of EB stacks per run:\n")
print(eb_stacks_summary)

cat("\nConditional EB stack size given number of EB stacks in run:\n")
print(eb_conditional)

cat("\nChi-square tests (EB stack size vs number of EB stacks in run):\n")
invisible(lapply(names(eb_chisq_tests), function(r) {
  cat(paste0("\nRaid ", r, ":\n"))
  test <- eb_chisq_tests[[r]]
  if (is.null(test)) {
    cat("Insufficient data for test\n")
  } else {
    print(test)
  }
}))

if (!is.null(pca_res_agg)) {
  cat("Variance explained:\n")
  print(summary(pca_res_agg))
  cat("\nPC1 & PC2 Loadings (Aggregated):\n")
  print(pca_res_agg$rotation[, 1:min(2, ncol(pca_res_agg$rotation))])
  cat("\nInterpretation: Items loading together suggest they appear in same runs.\n")
} else {
  cat("Insufficient data for PCA on aggregated items.\n")
}

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

slot_analysis <- eb_stacks_per_run |>
  left_join(raidr |> select(run_id, Total.Pulls), by = "run_id") |>
  group_by(Raid) |>
  summarise(
    avg_eb_stacks = mean(num_eb_stacks),
    avg_total_pulls = mean(Total.Pulls),
    avg_other_pulls = avg_total_pulls - avg_eb_stacks,
    pct_eb = avg_eb_stacks / avg_total_pulls,
    .groups = "drop"
  )

print(slot_analysis)

for (r in unique(slot_analysis$Raid)) {
  row <- slot_analysis |> filter(Raid == r)
  cat(sprintf("%s: %.1f EB slots + %.1f other slots = %.1f total (%.1f%% EB)\n",
              r, row$avg_eb_stacks, row$avg_other_pulls, row$avg_total_pulls, row$pct_eb * 100))
}

eb_pool_sizes <- data.frame(
  Raid = c("NOTG", "NOL", "TCC", "TNA"),
  pool_size = c(
    length(raid_amount_pools$NOTG$`Emerald Block`),
    length(raid_amount_pools$NOL$`Emerald Block`),
    length(raid_amount_pools$TCC$`Emerald Block`),
    length(raid_amount_pools$TNA$`Emerald Block`)
  )
)

pool_vs_prob <- eb_per_pull_prob |>
  left_join(eb_pool_sizes, by = "Raid") |>
  mutate(
    prob_per_stack = prob_any_eb_stack / pool_size,
    expected_prob_if_independent = 1 - (1 - prob_per_stack)^pool_size
  )

cat("\nIndependant chance per eb stack\n")
print(pool_vs_prob)

cat("\ncorr between pool count and eb chance\n")
cor_test <- cor.test(pool_vs_prob$pool_size, pool_vs_prob$prob_any_eb_stack)
print(cor_test)

cat("\nfinal summs per raid\n")

for (raid_name in c("NOTG", "NOL", "TCC", "TNA")) {
  cat(sprintf("\n--- %s ---\n", raid_name))
  
  raid_summary <- prob_table_agg |>
    filter(Raid == raid_name) |>
    select(Item_Agg, prob_per_pull) |>
    arrange(desc(prob_per_pull))
  
  key_items <- raid_summary |>
    filter(Item_Agg %in% c("Mythic Tome", "Fabled Tome", "Emerald Block", "Liquid Emerald"))
  
  if (nrow(key_items) > 0) {
    cat("\nCore Rewards:\n")
    print(key_items, n = Inf)
  }
  
  other_items <- raid_summary |>
    filter(!Item_Agg %in% c("Mythic Tome", "Fabled Tome", "Emerald Block", "Liquid Emerald"))
  
  if (nrow(other_items) > 0) {
    cat("\nOther Items:\n")
    print(other_items, n = Inf)
  }
}

cat("\nend of final sum\n\n")
