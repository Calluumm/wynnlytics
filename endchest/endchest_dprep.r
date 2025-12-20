library(dplyr)
library(tidyr)

source("endchest_chests.r")
setwd("C:/Users/Student/Desktop/wynn programs/raiddays/endchest")
loadRaidData <- function(csvPath = "raid_rewards_data.csv") {
  raidr <- read.csv(csvPath, check.names = TRUE)
  raidr <- raidr |> distinct()
  raidr <- raidr |> filter(Total.Pulls <= 50)
  raidr <- raidr |> filter(Timestamp >= 1766167200 & Timestamp <= 1766772000) #week of the event
  
  raidr <- raidr |> 
    mutate(
      runId = row_number(),
      Raid = case_when(
        Raid %in% c("tcc", "TCC", "The Canyon Colossus") ~ "TCC",
        Raid %in% c("notg", "NOTG", "Nest of the Grootslangs") ~ "NOTG",
        Raid %in% c("nol", "NOL", "Orphion's Nexus of Light") ~ "NOL",
        Raid %in% c("tna", "TNA", "The Nameless Anomaly") ~ "TNA",
        TRUE ~ Raid
      )
    )
  
  return(raidr)
}

prepRaidData <- function(raidr) {
  raidr <- applyChestCapacity(raidr)
  
  pullCols <- grep("^Pull\\.\\d+\\.(Item|Count)$", names(raidr), value = TRUE)
  
  raidLong <- raidr |>
    select(Raid, Total.Pulls, runId, all_of(pullCols)) |>
    pivot_longer(
      cols = all_of(pullCols),
      names_to = c("pull", "field"),
      names_pattern = "Pull\\.(\\d+)\\.(Item|Count)",
      values_to = "value",
      values_transform = as.character
    ) |>
    pivot_wider(names_from = field, values_from = value) |>
    mutate(
      pull = as.integer(pull),
      Item = trimws(as.character(Item)),
      Item = ifelse(Item %in% names(itemMap), itemMap[Item], Item),
      Count = suppressWarnings(as.integer(Count))
    ) |>
    filter(!is.na(Item) & Item != "" & Item != "Air" & !is.na(Count) & Count > 0) |>
    select(Raid, Total.Pulls, runId, pull, Item, Count)
  
  return(raidLong)
}

prepAggData <- function(raidLong) {
  raidLongAgg <- raidLong |>
    mutate(
      itemClean = gsub("^Unidentified ", "", Item),
      acronym = names(itemMap)[match(itemClean, unname(itemMap))],
      ItemAgg = case_when(
        !is.na(acronym) & grepl("^m", acronym) & grepl("Tome", Item) ~ "Mythic Tome",
        !is.na(acronym) & grepl("^f", acronym) & grepl("Tome", Item) ~ "Fabled Tome",
        TRUE ~ Item
      )
    )
  
  return(raidLongAgg)
}

prepWideItems <- function(raidLong) {
  itemsWide <- raidLong |>
    select(Raid, runId, Item, Count) |>
    group_by(Raid, runId, Item) |>
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = Item, values_from = Count, values_fill = 0)
  
  return(itemsWide)
}

prepWideItemsAgg <- function(raidLongAgg) {
  itemsWideAgg <- raidLongAgg |>
    select(Raid, runId, ItemAgg, Count) |>
    group_by(Raid, runId, ItemAgg) |>
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = ItemAgg, values_from = Count, values_fill = 0)
  
  return(itemsWideAgg)
}
