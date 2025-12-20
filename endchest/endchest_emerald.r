getEbStacksRun <- function(raidLong) {
  ebLong <- raidLong |> filter(Item == "Emerald Block")
  
  ebStacksRun <- ebLong |>
    group_by(Raid, runId) |>
    summarise(numEbStacks = n(), .groups = "drop")
  
  return(ebStacksRun)
}

calcEbPullProb <- function(raidr, raidLong) {
  ebLong <- raidLong |> filter(Item == "Emerald Block")
  
  ebPullProb <- raidr |>
    select(Raid, runId, Total.Pulls) |>
    left_join(
      ebLong |> group_by(Raid, runId) |> summarise(ebPulls = n(), .groups = "drop"),
      by = c("Raid", "runId")
    ) |>
    replace_na(list(ebPulls = 0)) |>
    group_by(Raid) |>
    summarise(
      totalPulls = sum(Total.Pulls),
      totalEbPulls = sum(ebPulls),
      probAnyEb = totalEbPulls / totalPulls,
      .groups = "drop"
    )
  
  return(ebPullProb)
}

calcEbStackDist <- function(raidLong) {
  ebLong <- raidLong |> filter(Item == "Emerald Block")
  
  ebStackDist <- ebLong |>
    group_by(Raid, Count) |>
    summarise(stacks = n(), .groups = "drop_last") |>
    mutate(probThisStack = stacks / sum(stacks)) |>
    arrange(Raid, Count)
  
  return(ebStackDist)
}

summarizeEbStacks <- function(ebStacksRun) {
  ebStacksSummary <- ebStacksRun |>
    group_by(Raid, numEbStacks) |>
    summarise(runs = n(), .groups = "drop_last") |>
    mutate(probNStacks = runs / sum(runs))
  
  return(ebStacksSummary)
}

calcEbConditional <- function(raidLong, ebStacksRun) {
  ebLong <- raidLong |> filter(Item == "Emerald Block")
  
  ebConditional <- ebLong |>
    left_join(ebStacksRun, by = c("Raid", "runId")) |>
    group_by(Raid, numEbStacks, Count) |>
    summarise(stacks = n(), .groups = "drop_last") |>
    mutate(probStackGiven = stacks / sum(stacks)) |>
    arrange(Raid, numEbStacks, Count)
  
  return(ebConditional)
}

analyzeUnifiedEmeralds <- function(raidLong, raidr, ebStacksRun) {
  emeraldLong <- raidLong |> 
    filter(Item %in% c("Emerald Block", "Liquid Emerald")) |>
    mutate(itemType = if_else(Item == "Liquid Emerald", "LE", "EB"))
  
  emeraldPerRun <- emeraldLong |>
    group_by(Raid, runId, itemType) |>
    summarise(count = sum(Count, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = itemType, values_from = count, values_fill = 0)
  
  emeraldRunSummary <- emeraldPerRun |>
    mutate(
      emeraldType = case_when(
        EB > 0 & LE == 0 ~ "EB Only",
        EB == 0 & LE > 0 ~ "LE Only",
        EB > 0 & LE > 0 ~ "Both",
        TRUE ~ "Neither"
      )
    ) |>
    group_by(Raid, emeraldType) |>
    summarise(runs = n(), .groups = "drop_last") |>
    mutate(pctRuns = runs / sum(runs))
  
  emeraldValue <- emeraldPerRun |>
    left_join(ebStacksRun |> select(Raid, runId, numEbStacks), by = c("Raid", "runId")) |>
    mutate(
      totalEmeraldValue = (EB * 1) + (LE * 8),
      hasEmerald = (EB > 0 | LE > 0)
    ) |>
    group_by(Raid) |>
    summarise(
      avgEb = mean(EB, na.rm = TRUE),
      avgLe = mean(LE, na.rm = TRUE),
      avgTotalValue = mean(totalEmeraldValue, na.rm = TRUE),
      pctRunsWithEmerald = sum(hasEmerald) / n(),
      .groups = "drop"
    )
  
  emeraldPullProb <- raidr |>
    select(Raid, runId, Total.Pulls) |>
    left_join(
      emeraldLong |> 
        group_by(Raid, runId) |> 
        summarise(emeraldPulls = n(), .groups = "drop"),
      by = c("Raid", "runId")
    ) |>
    replace_na(list(emeraldPulls = 0)) |>
    group_by(Raid) |>
    summarise(
      totalPulls = sum(Total.Pulls),
      totalEmeraldPulls = sum(emeraldPulls),
      probAnyEmerald = totalEmeraldPulls / totalPulls,
      .groups = "drop"
    )
  
  ebVsLe <- raidLong |>
    filter(Item %in% c("Emerald Block", "Liquid Emerald")) |>
    group_by(Raid, Item) |>
    summarise(occurrences = n(), .groups = "drop") |>
    pivot_wider(names_from = Item, values_from = occurrences, values_fill = 0) |>
    mutate(
      totalEmeraldPulls = `Emerald Block` + `Liquid Emerald`,
      pctEb = `Emerald Block` / totalEmeraldPulls,
      pctLe = `Liquid Emerald` / totalEmeraldPulls
    )
  
  return(list(
    emeraldValue = emeraldValue,
    emeraldRunSummary = emeraldRunSummary,
    emeraldPullProb = emeraldPullProb,
    ebVsLe = ebVsLe
  ))
}

printUnifiedEmeralds <- function(emeraldAnal) {
  cat("\nEmerald per run summary:\n")
  print(emeraldAnal$emeraldValue)
  
  cat("\nDistribution: EB only vs LE only vs Both vs Neither\n")
  print(emeraldAnal$emeraldRunSummary)
  
  cat("\nProbability of ANY emerald reward per pull:\n")
  print(emeraldAnal$emeraldPullProb)
  
  cat("\nEB vs LE split per raid:\n")
  print(emeraldAnal$ebVsLe)
}
