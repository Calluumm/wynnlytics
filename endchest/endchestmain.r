library(dplyr)
library(tidyr)
library(mclust)
setwd("C:/Users/Student/Desktop/wynn programs/raiddays/endchest")
source("endchest_chests.r")
source("endchest_dprep.r")
source("endchest_probab.r")
source("endchest_emerald.r")
source("endchest_pattern.r")

# set the wd's appropriately
# use the raid_rewards_data.csv's people get and compile into a master list to use
# put that path in the dprep file
# I do just comment out the prints i dont want so do- do that

raidr <- loadRaidData()
raidLong <- prepRaidData(raidr)
raidLongAgg <- prepAggData(raidLong)
itemsWide <- prepWideItems(raidLong)
itemsWideAgg <- prepWideItemsAgg(raidLongAgg)

probTable <- calcProbTable(raidLong, raidr)
probTableAgg <- calcProbTableAgg(raidLongAgg, raidr)
countDist <- calcCountDist(raidLong)

cat("\nConverage Analysis\n")
covTable <- calcCoverageTable(probTable)
print(covTable)

cat("\nPool Amounts (Expected)\n")
poolAmts <- calcPoolAmounts()
print(poolAmts)

printTopItems(probTable, n = 50)
printCountDist(countDist)

cat("\nEB Analysis\n")
ebStacksRun <- getEbStacksRun(raidLong)
ebPullProb <- calcEbPullProb(raidr, raidLong)
ebStackDist <- calcEbStackDist(raidLong)
ebStacksSummary <- summarizeEbStacks(ebStacksRun)
ebConditional <- calcEbConditional(raidLong, ebStacksRun)

cat("\nProbability of ANY EB stack per pull:\n")
print(ebPullProb)

cat("\nEB stack size distribution:\n")
print(ebStackDist)

cat("\nNumber of EB stacks per run:\n")
print(ebStacksSummary)

cat("\nConditional EB stack size given number of EB stacks:\n")
print(ebConditional)

cat("\nAnalysis as if le/eb are unified:\n")
emeraldAnal <- analyzeUnifiedEmeralds(raidLong, raidr, ebStacksRun)
printUnifiedEmeralds(emeraldAnal)

cat("\nPCA + Chi squared\n")
pcaAgg <- performPcaAgg(itemsWideAgg)
pcaRes <- performPca(itemsWide)
printPcaResults(pcaAgg, pcaRes)

ebChisq <- performEbChisq(raidLong, ebStacksRun)
printChisqResults(ebChisq)

cat("\nFurther branch checks\n")

hclustAnal <- performHierarchicalClustering(itemsWideAgg)
printHierarchicalResults(hclustAnal)

gmmAnal <- performGaussianMixture(itemsWideAgg)
printGmmResults(gmmAnal)

cat("\nChest overflow assessment\n")
overflowAnal <- analyzeOverflow(raidr, raidLong)
print(overflowAnal)

cat("\nItem category bias in overflow runs\n")
biasCategory <- analyzeBias(raidLong, raidr)
print(biasCategory)

cat("\nSlot utilisation analysis\n")
slotAnal <- analyzeSlots(ebStacksRun, raidr)
print(slotAnal)

for (r in unique(slotAnal$Raid)) {
  row <- slotAnal |> filter(Raid == r)
  cat(sprintf("%s: %.1f EB slots + %.1f other slots = %.1f total (%.1f%% EB)\n",
              r, row$avgEb, row$avgOther, row$avgTotal, row$pctEb * 100))
}

cat("\nPer raid summaries\n")
for (raid in c("NOTG", "NOL", "TCC", "TNA")) {
  cat(sprintf("\n--- %s ---\n", raid))
  
  summary <- probTableAgg |>
    filter(Raid == raid) |>
    select(ItemAgg, probPerPull) |>
    arrange(desc(probPerPull))
  
  keyItems <- summary |>
    filter(ItemAgg %in% c("Mythic Tome", "Fabled Tome", "Emerald Block", "Liquid Emerald"))
  
  if (nrow(keyItems) > 0) {
    cat("\nCore Rewards:\n")
    print(keyItems, n = Inf)
  }
  
  otherItems <- summary |>
    filter(!ItemAgg %in% c("Mythic Tome", "Fabled Tome", "Emerald Block", "Liquid Emerald"))
  
  if (nrow(otherItems) > 0) {
    cat("\nOther Items:\n")
    print(otherItems, n = Inf)
  }
}

cat("\nend\n")
