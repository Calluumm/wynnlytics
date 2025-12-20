calcProbTable <- function(raidLong, raidr) {
  probTable <- raidLong |>
    group_by(Raid, Item) |>
    summarise(
      totalCnt = sum(Count, na.rm = TRUE),
      occurrences = n(),
      .groups = "drop"
    ) |>
    left_join(
      raidr |>
        group_by(Raid) |>
        summarise(
          totalPulls = sum(Total.Pulls, na.rm = TRUE),
          runs = n(),
          .groups = "drop"
        ),
      by = "Raid"
    ) |>
    mutate(
      probPerPull = totalCnt / totalPulls,
      probPresence = occurrences / runs
    ) |>
    arrange(Raid, desc(probPerPull))
  
  return(probTable)
}

calcProbTableAgg <- function(raidLongAgg, raidr) {
  probTableAgg <- raidLongAgg |>
    group_by(Raid, ItemAgg) |>
    summarise(
      totalCnt = sum(Count, na.rm = TRUE),
      occurrences = n(),
      .groups = "drop"
    ) |>
    left_join(
      raidr |>
        group_by(Raid) |>
        summarise(
          totalPulls = sum(Total.Pulls, na.rm = TRUE),
          runs = n(),
          .groups = "drop"
        ),
      by = "Raid"
    ) |>
    mutate(
      probPerPull = occurrences / totalPulls,
      probPresence = occurrences / (runs * totalPulls / runs)
    ) |>
    arrange(Raid, desc(probPerPull))
  
  return(probTableAgg)
}

calcCountDist <- function(raidLong) {
  countDist <- raidLong |>
    group_by(Raid, Item, Count) |>
    summarise(pulls = n(), .groups = "drop_last") |>
    mutate(pctWithinItem = pulls / sum(pulls)) |>
    arrange(Raid, Item, Count)
  
  return(countDist)
}

printTopItems <- function(probTable, n = 50) {
  topItems <- probTable |> group_by(Raid) |> slice_head(n = n)
  print(topItems)
}

printCountDist <- function(countDist) {
  summary <- countDist |> filter(n() > 1)
  print(summary)
}
