library(tidyverse) #install.packages("lib_name") for packages not already on system
library(vegan)
library(lme4)
library(lmerTest)
library(car)
library(effsize)
library(randomForest)

wd <- "c:/Users/Student/Desktop/wynn programs/raiddays"
setwd(wd)

overallGambit <- read.csv("ova_gambitd.csv")
archetypeRaid <- read.csv("archetype_gam_raid.csv")
archetypeShare <- read.csv("archetype_share_gam.csv")
archetypeOverall <- read.csv("archetype_ovasense.csv")
archetypeRaidGambit <- read.csv("archtype_gambit_sens.csv")

#set thresholds
alphaLevel <- 0.05
negativeThreshold <- -0.01
largeEffectThreshold <- 0.5
nPermutations <- 10000
minSampleSize <- 5

cat("\nAnalysing negative impact on play\n")

overallGambit <- overallGambit %>% arrange(impact)

negativeGambits <- overallGambit %>% filter(impact < 0)
positiveGambits <- overallGambit %>% filter(impact > 0)

cat(sprintf("\nGambits reducing raids per hour: %d\n", nrow(negativeGambits)))
cat(sprintf("Gambits increasing raids per hour: %d\n", nrow(positiveGambits)))
cat(sprintf("Net effect (mean across all gambits): %.6f raids/hour\n", mean(overallGambit$impact)))

cat("\nTop 10 most damaging gambits:\n")
topNegative <- head(negativeGambits, 10)
for(i in 1:nrow(topNegative)) {
  cat(sprintf("%d. %s: %.6f raids/hour reduction\n", 
              i, topNegative$gambit[i], topNegative$impact[i]))
}

cat("\nTop 5 most beneficial gambits:\n")
topPositive <- head(arrange(positiveGambits, desc(impact)), 5)
for(i in 1:nrow(topPositive)) {
  cat(sprintf("%d. %s: %.6f raids/hour increase\n", 
              i, topPositive$gambit[i], topPositive$impact[i]))
}

tTestNegative <- t.test(negativeGambits$impact, mu = 0, alternative = "less")
cat(sprintf("\nTest if negative gambits significantly reduce play:\n"))
cat(sprintf("t = %.4f, df = %d, p = %.4e\n", 
            tTestNegative$statistic, tTestNegative$parameter, tTestNegative$p.value))
cat(sprintf("Mean reduction: %.6f raids/hour\n", mean(negativeGambits$impact)))

tTestPositive <- t.test(positiveGambits$impact, mu = 0, alternative = "greater")
cat(sprintf("\nTest if positive gambits significantly increase play:\n"))
cat(sprintf("t = %.4f, df = %d, p = %.4e\n", 
            tTestPositive$statistic, tTestPositive$parameter, tTestPositive$p.value))

medianImpact <- median(overallGambit$impact)
signTest <- binom.test(sum(overallGambit$impact < 0), nrow(overallGambit), p = 0.5)
cat(sprintf("\nSign test (do more gambits reduce vs increase play):\n"))
cat(sprintf("Negative impacts: %d, Positive impacts: %d\n", 
            sum(overallGambit$impact < 0), sum(overallGambit$impact > 0)))
cat(sprintf("p = %.4f\n", signTest$p.value))

largeNegativeEffects <- overallGambit %>% filter(impact < negativeThreshold)
cat(sprintf("\n%d gambits with substantial negative effect (< %.3f):\n", 
            nrow(largeNegativeEffects), negativeThreshold))
for(i in 1:nrow(largeNegativeEffects)) {
  cat(sprintf("  %s: %.6f\n", largeNegativeEffects$gambit[i], largeNegativeEffects$impact[i]))
}

cat("\nAnalysing bidirectional data\n")

shareWide <- archetypeShare %>%
  filter(archetype != "N/A") %>%
  pivot_wider(names_from = archetype, values_from = impact, values_fill = 0)

shareMatrix <- as.matrix(shareWide[, -1])
rownames(shareMatrix) <- shareWide$gambit

archetypeNames <- colnames(shareMatrix)
cat(sprintf("\nAnalysing %d archetypes across %d gambits\n", 
            ncol(shareMatrix), nrow(shareMatrix)))

zeroSumTest <- rowSums(shareMatrix)
cat(sprintf("\nMean row sum (should be ~0 for perfect substitution): %.6f\n", 
            mean(zeroSumTest)))
cat(sprintf("SD of row sums: %.6f\n", sd(zeroSumTest)))

tTestZeroSum <- t.test(zeroSumTest, mu = 0)
cat(sprintf("Test if changes are zero-sum (compositional): t = %.4f, p = %.4f\n",
            tTestZeroSum$statistic, tTestZeroSum$p.value))

archetypeTotals <- colSums(shareMatrix)
archetypeRanking <- sort(archetypeTotals, decreasing = FALSE)

cat("\nArchetypes by cumulative share loss across all gambits:\n")
for(i in 1:length(archetypeRanking)) {
  cat(sprintf("%d. %s: %.6f cumulative share change\n", 
              i, names(archetypeRanking)[i], archetypeRanking[i]))
}

winners <- names(archetypeRanking)[archetypeRanking > 0]
losers <- names(archetypeRanking)[archetypeRanking < 0]

cat(sprintf("\nArchetypes gaining from gambits: %d\n", length(winners)))
cat(sprintf("Archetypes losing from gambits: %d\n", length(losers)))

cat("\nanalysis of per archetype vulnerability\n")

archetypeRaidData <- archetypeRaid %>%
  filter(archetype != "N/A")

archetypeVulnerability <- archetypeRaidData %>%
  group_by(archetype) %>%
  summarise(
    meanImpact = mean(impact),
    minImpact = min(impact),
    maxImpact = max(impact),
    sdImpact = sd(impact),
    negativeCount = sum(impact < 0),
    severeNegative = sum(impact < negativeThreshold),
    totalGambits = n()
  ) %>%
  mutate(vulnerabilityScore = negativeCount / totalGambits) %>%
  arrange(desc(vulnerabilityScore))

cat("\nArchetype vulnerability ranking (% of gambits with negative impact):\n")
cat(sprintf("(Note: %d total gambits, %d archetypes)\n", 
            length(unique(archetypeRaidData$gambit)), 
            length(unique(archetypeRaidData$archetype))))
for(i in 1:nrow(archetypeVulnerability)) {
  cat(sprintf("%d. %s: %.1f%% negative (%d/%d), worst: %.6f, mean: %.6f\n",
              i, 
              archetypeVulnerability$archetype[i],
              archetypeVulnerability$vulnerabilityScore[i] * 100,
              archetypeVulnerability$negativeCount[i],
              archetypeVulnerability$totalGambits[i],
              archetypeVulnerability$minImpact[i],
              archetypeVulnerability$meanImpact[i]))
}

mostVulnerable <- archetypeVulnerability %>% filter(vulnerabilityScore > 0.5)
cat(sprintf("\n%d archetypes negatively affected by majority of gambits\n", 
            nrow(mostVulnerable)))

resilientArchetypes <- archetypeVulnerability %>% filter(vulnerabilityScore < 0.3)
cat(sprintf("\n%d resilient archetypes (< 30%% negative impacts):\n", 
            nrow(resilientArchetypes)))
for(i in 1:nrow(resilientArchetypes)) {
  cat(sprintf("  %s\n", resilientArchetypes$archetype[i]))
}

cat("\ngreatest archetype threats\n")

worstCombinations <- archetypeRaidData %>%
  arrange(impact) %>%
  head(20)

cat("\nhead of 20 worst combos\n")
for(i in 1:nrow(worstCombinations)) {
  cat(sprintf("%d. %s playing under %s: %.6f raids/hour\n",
              i,
              worstCombinations$archetype[i],
              worstCombinations$gambit[i],
              worstCombinations$impact[i]))
}

gambitArchetypeMatrix <- archetypeRaidData %>%
  pivot_wider(names_from = archetype, values_from = impact, values_fill = 0) %>%
  select(-gambit) %>%
  as.matrix()

gambitWorstHit <- apply(gambitArchetypeMatrix, 1, min)
gambitMeanHit <- rowMeans(gambitArchetypeMatrix)

gambitSeverity <- data.frame(
  gambit = archetypeRaidData %>%
    pivot_wider(names_from = archetype, values_from = impact) %>%
    pull(gambit),
  worstHit = gambitWorstHit,
  meanHit = gambitMeanHit,
  archetypesNegative = rowSums(gambitArchetypeMatrix < 0)
) %>%
  arrange(worstHit)

cat("\npredictive impact\n")

rfData <- archetypeRaidData %>%
  mutate(isNegative = as.factor(ifelse(impact < 0, "Negative", "Positive")))

if(length(unique(rfData$gambit)) > 3 && nrow(rfData) > 50) {
  gambitDummy <- model.matrix(~ gambit - 1, data = rfData)
  archetypeDummy <- model.matrix(~ archetype - 1, data = rfData)
  
  rfFeatures <- cbind(gambitDummy, archetypeDummy)
  
  set.seed(42)
  rfModel <- randomForest(
    x = rfFeatures,
    y = rfData$isNegative,
    importance = TRUE,
    ntree = 500
  )
  
  cat(sprintf("\nRandom Forest OOB error: %.2f%%\n", 
              rfModel$err.rate[nrow(rfModel$err.rate), 1] * 100))
  
  importance <- importance(rfModel)
  gambitImportance <- importance[grep("^gambit", rownames(importance)), ]
  
  if(is.matrix(gambitImportance)) {
    gambitImpDF <- data.frame(
      gambit = gsub("^gambit", "", rownames(gambitImportance)),
      meanDecreaseGini = gambitImportance[, "MeanDecreaseGini"]
    ) %>%
      arrange(desc(meanDecreaseGini)) %>%
      head(10)
    
    cat("\npredictive gambit danger head 10\n")
    for(i in 1:nrow(gambitImpDF)) {
      cat(sprintf("%d. %s (Gini: %.4f)\n",
                  i, gambitImpDF$gambit[i], gambitImpDF$meanDecreaseGini[i]))
    }
  }
}

cat("\nregression assessment for loss of play\n")

raidGambitAgg <- archetypeRaidGambit %>%
  filter(archetype != "N/A") %>%
  group_by(gambit, raid) %>%
  summarise(totalImpact = sum(impact, na.rm = TRUE), .groups = 'drop')

gambitTotalEffect <- raidGambitAgg %>%
  group_by(gambit) %>%
  summarise(
    meanTotalImpact = mean(totalImpact),
    sdTotalImpact = sd(totalImpact),
    minTotalImpact = min(totalImpact),
    maxTotalImpact = max(totalImpact)
  ) %>%
  arrange(meanTotalImpact)

cat("\ntotal summed archetype loss\n")
for(i in 1:min(15, nrow(gambitTotalEffect))) {
  cat(sprintf("%d. %s: total %.6f (range: %.6f to %.6f)\n",
              i,
              gambitTotalEffect$gambit[i],
              gambitTotalEffect$meanTotalImpact[i],
              gambitTotalEffect$minTotalImpact[i],
              gambitTotalEffect$maxTotalImpact[i]))
}

netNegativeGambits <- gambitTotalEffect %>% filter(meanTotalImpact < 0)
cat(sprintf("\n%d gambits cause NET reduction in play (players quit rather than switch)\n",
            nrow(netNegativeGambits)))

cat("\nMost damaging for total engagement:\n")
for(i in 1:min(5, nrow(netNegativeGambits))) {
  cat(sprintf("  %s: %.6f net loss\n",
              netNegativeGambits$gambit[i],
              netNegativeGambits$meanTotalImpact[i]))
}

tTestNetEffect <- t.test(gambitTotalEffect$meanTotalImpact, mu = 0)
cat(sprintf("\nTest if gambits reduce total play on average:\n"))
cat(sprintf("Mean total effect: %.6f\n", mean(gambitTotalEffect$meanTotalImpact)))
cat(sprintf("t = %.4f, p = %.4f\n", tTestNetEffect$statistic, tTestNetEffect$p.value))

cat("\nRaid specific vulnerabilities\n")

raidVulnerability <- archetypeRaidGambit %>%
  filter(archetype != "N/A") %>%
  group_by(raid) %>%
  summarise(
    meanImpact = mean(impact, na.rm = TRUE),
    medianImpact = median(impact, na.rm = TRUE),
    minImpact = min(impact, na.rm = TRUE),
    negativeRate = sum(impact < 0) / n(),
    severeNegativeCount = sum(impact < negativeThreshold)
  ) %>%
  arrange(meanImpact)

cat("\nRaids by gambit vulnerability:\n")
for(i in 1:nrow(raidVulnerability)) {
  cat(sprintf("%d. %s: mean %.6f, %.1f%% negative impacts, %d severe\n",
              i,
              raidVulnerability$raid[i],
              raidVulnerability$meanImpact[i],
              raidVulnerability$negativeRate[i] * 100,
              raidVulnerability$severeNegativeCount[i]))
}

cat("\nMixed model accounting for archetype and raid structure\n")

mixedData <- archetypeRaidGambit %>%
  filter(archetype != "N/A" & !is.na(impact))

mixedData$gambitNegative <- ifelse(mixedData$gambit %in% negativeGambits$gambit, 1, 0)

nullModel <- lmer(impact ~ 1 + (1|archetype) + (1|raid), data = mixedData, REML = FALSE)
gambitModel <- lmer(impact ~ gambitNegative + (1|archetype) + (1|raid), 
                    data = mixedData, REML = FALSE)

lrtTest <- anova(nullModel, gambitModel)

cat("\nLikelihood ratio\n")
cat(sprintf("Chi-squared = %.4f, df = %d, p = %.4e\n",
            lrtTest$Chisq[2], lrtTest$Df[2], lrtTest$`Pr(>Chisq)`[2]))

modelSummary <- summary(gambitModel)
fixedEffects <- coef(modelSummary)

cat("\nFixed effects:\n")
cat(sprintf("Intercept: %.6f (SE = %.6f, p = %.4e)\n",
            fixedEffects[1, 1], fixedEffects[1, 2], fixedEffects[1, 5]))
cat(sprintf("Gambit negative effect: %.6f (SE = %.6f, p = %.4e)\n",
            fixedEffects[2, 1], fixedEffects[2, 2], fixedEffects[2, 5]))

randEffects <- VarCorr(gambitModel)
cat("\nRandom effects SD:\n")
cat(sprintf("Archetype: %.6f\n", attr(randEffects$archetype, "stddev")))
cat(sprintf("Raid: %.6f\n", attr(randEffects$raid, "stddev")))
cat(sprintf("Residual: %.6f\n", attr(randEffects, "sc")))

cat("\nPermutation test for negative gambit clustering\n")

gambitImpacts <- overallGambit %>% arrange(gambit)
observedNegativeRun <- max(rle(gambitImpacts$impact < 0)$lengths)

permutedRuns <- replicate(nPermutations, {
  shuffled <- sample(gambitImpacts$impact)
  max(rle(shuffled < 0)$lengths)
})

permPvalueCluster <- mean(permutedRuns >= observedNegativeRun)
cat(sprintf("\nLongest run of consecutive negative gambits: %d\n", observedNegativeRun))
cat(sprintf("Permutation test p-value: %.4f\n", permPvalueCluster))

cat("\nCompositional data analysis archetype switching\n")

shareWideClean <- archetypeShare %>%
  filter(archetype != "N/A") %>%
  pivot_wider(names_from = archetype, values_from = impact, values_fill = 0)

shareMatrix <- as.matrix(shareWideClean[, -1])
rownames(shareMatrix) <- shareWideClean$gambit

archetypeCorrelations <- cor(shareMatrix, use = "pairwise.complete.obs")

negativeCorrelations <- which(archetypeCorrelations < -0.5 & 
                                archetypeCorrelations != 1, arr.ind = TRUE)

cat("\nStrong negative correlations:\n") #remember to come back here for bidirectional, need to re-check how I've done this
processed <- c()
for(i in 1:nrow(negativeCorrelations)) {
  arch1 <- rownames(archetypeCorrelations)[negativeCorrelations[i, 1]]
  arch2 <- colnames(archetypeCorrelations)[negativeCorrelations[i, 2]]
  pair <- paste(sort(c(arch1, arch2)), collapse = "-")
  if(!(pair %in% processed)) {
    cat(sprintf("  %s <-> %s: r = %.4f\n",
                arch1, arch2,
                archetypeCorrelations[negativeCorrelations[i, 1], 
                                     negativeCorrelations[i, 2]]))
    processed <- c(processed, pair)
  }
}


cat("\nKey stuff")
cat("\n1. Gambits causing overall play reduction:\n")
severeGambits <- overallGambit %>%
  filter(impact < negativeThreshold) %>%
  arrange(impact) %>%
  head(10)

for(i in 1:nrow(severeGambits)) {
  cat(sprintf("   %s: %.6f raids/hour reduction\n",
              severeGambits$gambit[i], severeGambits$impact[i]))
}

cat("\n2. Most vulnerable archetypes:\n")
topVulnerable <- head(archetypeVulnerability, 5)
for(i in 1:nrow(topVulnerable)) {
  cat(sprintf("   %s: %.1f%% of gambits cause negative impact\n",
              topVulnerable$archetype[i],
              topVulnerable$vulnerabilityScore[i] * 100))
}

cat("\n3. Net engagement effect:\n")
cat(sprintf("   Average total impact: %.6f\n", mean(gambitTotalEffect$meanTotalImpact)))

cat("\n4. Statistical significance:\n")
cat(sprintf("   Negative gambits reduce play (p = %.4e)\n", tTestNegative$p.value))
cat(sprintf("   Net gambit effect (p = %.4f)\n", tTestNetEffect$p.value))