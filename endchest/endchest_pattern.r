performPcaAgg <- function(itemsWideAgg) {
  pcaAgg <- NULL
  pcaInput <- itemsWideAgg |> select(-Raid, -runId)
  
  if (nrow(pcaInput) > 1 && ncol(pcaInput) > 1) {
    nonZeroVar <- apply(pcaInput, 2, var) > 0
    if (sum(nonZeroVar) > 1) {
      pcaAgg <- prcomp(pcaInput[, nonZeroVar], center = TRUE, scale. = TRUE)
    }
  }
  
  return(pcaAgg)
}

performPca <- function(itemsWide) {
  pcaRes <- NULL
  pcaInput <- itemsWide |> select(-Raid, -runId)
  
  if (nrow(pcaInput) > 1 && ncol(pcaInput) > 1) {
    nonZeroVar <- apply(pcaInput, 2, var) > 0
    if (sum(nonZeroVar) > 1) {
      pcaRes <- prcomp(pcaInput[, nonZeroVar], center = TRUE, scale. = TRUE)
    }
  }
  
  return(pcaRes)
}

performEbChisq <- function(raidLong, ebStacksRun) {
  ebLong <- raidLong |> filter(Item == "Emerald Block")
  
  ebChisq <- lapply(
    split(ebLong |> left_join(ebStacksRun, by = c("Raid", "runId")), ebLong$Raid),
    function(df) {
      tab <- table(df$numEbStacks, df$Count)
      if (min(dim(tab)) < 2 || sum(tab) < 20) return(NULL)
      tryCatch(chisq.test(tab), error = function(e) NULL)
    }
  )
  
  return(ebChisq)
}

calcCoverageTable <- function(probTable) {
  covTable <- bind_rows(lapply(names(raidPools), function(r) {
    obs <- probTable$Item[probTable$Raid == r]
    data.frame(
      Raid = r,
      obsItems = length(unique(obs)),
      possItems = length(raidPools[[r]]),
      missingItems = paste(setdiff(raidPools[[r]], unique(obs)), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  
  return(covTable)
}

calcPoolAmounts <- function() {
  poolAmts <- bind_rows(lapply(names(raidAmountPools), function(r) {
    if (length(raidAmountPools[[r]]) == 0) return(NULL)
    bind_rows(lapply(names(raidAmountPools[[r]]), function(it) {
      data.frame(
        Raid = r,
        Item = it,
        allowedCounts = paste(raidAmountPools[[r]][[it]], collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  return(poolAmts)
}

analyzeOverflow <- function(raidr, raidLong) {
  overflowAnal <- raidLong |>
    filter(!is.na(Total.Pulls) & Total.Pulls > 27) |>
    group_by(Raid) |>
    summarise(
      runsOver27 = n_distinct(runId),
      totalItemsOverflow = sum(Count, na.rm = TRUE),
      avgOverflowAmt = mean(Total.Pulls - 27),
      .groups = "drop"
    ) |>
    left_join(
      raidr |> group_by(Raid) |> summarise(totalRuns = n(), .groups = "drop"),
      by = "Raid"
    ) |>
    mutate(
      pctOverflowing = (runsOver27 / totalRuns) * 100
    )
  
  return(overflowAnal)
}

analyzeBias <- function(raidLong, raidr) {
  biasCategory <- raidLong |>
    mutate(
      itemCat = sapply(Item, itemCat),
      catName = case_when(
        itemCat == 1 ~ "Tomes (Protected)",
        itemCat == 2 ~ "Emeralds (Protected)",
        itemCat == 3 ~ "Armour/Weapons (Medium Risk)",
        itemCat == 4 ~ "Accessories (High Risk)",
        itemCat == 5 ~ "Powders/Shards/Amps (Highest Risk)",
        TRUE ~ "Other"
      ),
      isOverflow = Total.Pulls > 27
    ) |>
    group_by(Raid, catName) |>
    summarise(
      obsPulls = n(),
      totalCnt = sum(Count, na.rm = TRUE),
      runsWithOverflow = n_distinct(runId[isOverflow]),
      .groups = "drop"
    )
  
  return(biasCategory)
}

analyzeSlots <- function(ebStacksRun, raidr) {
  slotAnal <- ebStacksRun |>
    left_join(raidr |> select(runId, Total.Pulls), by = "runId") |>
    group_by(Raid) |>
    summarise(
      avgEb = mean(numEbStacks),
      avgTotal = mean(Total.Pulls),
      avgOther = avgTotal - avgEb,
      pctEb = avgEb / avgTotal,
      .groups = "drop"
    )
  
  return(slotAnal)
}

performHierarchicalClustering <- function(itemsWideAgg) {
  clusterInput <- itemsWideAgg |> select(-Raid, -runId)
  
  if (nrow(clusterInput) < 2) {
    return(NULL)
  }
  
  scaledInput <- scale(clusterInput)
  
  distMatrix <- dist(scaledInput, method = "euclidean")
  hclustRes <- hclust(distMatrix, method = "ward.D2")
  
  wss <- sapply(1:min(10, nrow(clusterInput)-1), function(k) {
    sum(kmeans(scaledInput, k, nstart = 10)$withinss)
  })
  
  return(list(
    hclust = hclustRes,
    scaledData = scaledInput,
    wss = wss,
    originalData = itemsWideAgg
  ))
}

performGaussianMixture <- function(itemsWideAgg) {
  gmInput <- itemsWideAgg |> select(-Raid, -runId)
  
  if (nrow(gmInput) < 3) {
    return(NULL)
  }
  
  scaledInput <- scale(gmInput)
  
  tryCatch({
    gmRes <- mclust::Mclust(scaledInput, G = 2:9, modelNames = "VVV")
    
    clusterAssign <- gmRes$classification
    clusterProbs <- gmRes$uncertainty
    
    resultDf <- itemsWideAgg |>
      mutate(
        clusterGMM = clusterAssign,
        clusterUncertainty = clusterProbs
      )
    
    return(list(
      gmmFit = gmRes,
      clusterAssignments = resultDf,
      scaledData = scaledInput,
      originalData = itemsWideAgg
    ))
  }, error = function(e) {
    cat("GMM fitting failed:", conditionMessage(e), "\n")
    return(NULL)
  })
}

analyzeClusterBranches <- function(hclustRes, itemsWideAgg, numClusters = 3) {
  if (is.null(hclustRes)) return(NULL)
  
  clusterAssign <- cutree(hclustRes$hclust, k = numClusters)
  
  analysisData <- itemsWideAgg |>
    mutate(clusterHC = clusterAssign)
  clusterVec <- numeric()
  numRunsVec <- numeric()
  raidsVec <- character()
  tomeVec <- numeric()
  emberVec <- numeric()
  liquidVec <- numeric()
  
  for (k in 1:numClusters) {
    clusterData <- analysisData |> filter(clusterHC == k)
    
    if (nrow(clusterData) == 0) next
    
    means <- colMeans(clusterData |> select(-Raid, -runId, -clusterHC), na.rm = TRUE)
    
    tomeVal <- 0
    if ("Mythic Tome" %in% names(means)) {
      val <- as.numeric(means["Mythic Tome"])
      if (!is.na(val)) tomeVal <- val
    }
    if (tomeVal == 0 && "Fabled Tome" %in% names(means)) {
      val <- as.numeric(means["Fabled Tome"])
      if (!is.na(val)) tomeVal <- val
    }
    
    emeraldVal <- 0
    if ("Emerald Block" %in% names(means)) {
      val <- as.numeric(means["Emerald Block"])
      if (!is.na(val)) emeraldVal <- val
    }
    
    liquidVal <- 0
    if ("Liquid Emerald" %in% names(means)) {
      val <- as.numeric(means["Liquid Emerald"])
      if (!is.na(val)) liquidVal <- val
    }
    
    clusterVec <- c(clusterVec, k)
    numRunsVec <- c(numRunsVec, nrow(clusterData))
    raidsVec <- c(raidsVec, paste(unique(clusterData$Raid), collapse = ","))
    tomeVec <- c(tomeVec, round(tomeVal, 2))
    emberVec <- c(emberVec, round(emeraldVal, 2))
    liquidVec <- c(liquidVec, round(liquidVal, 2))
  }
  
  clusterChar <- data.frame(
    cluster = clusterVec,
    numRuns = numRunsVec,
    raids = raidsVec,
    avgTome = tomeVec,
    avgEmber = emberVec,
    avgLiquidEb = liquidVec,
    stringsAsFactors = FALSE
  )
  
  return(list(
    assignments = analysisData,
    characteristics = clusterChar
  ))
}

analyzeGmmBranches <- function(gmmRes) {
  if (is.null(gmmRes)) return(NULL)
  
  assignData <- gmmRes$clusterAssignments
  
  if (is.null(assignData) || nrow(assignData) == 0) {
    return(list(
      assignments = assignData,
      characteristics = data.frame(),
      gmmFit = gmmRes$gmmFit
    ))
  }
  
  numClusters <- length(unique(assignData$clusterGMM))
  clusterVec <- numeric()
  numRunsVec <- numeric()
  raidsVec <- character()
  tomeVec <- numeric()
  emberVec <- numeric()
  liquidVec <- numeric()
  uncertaintyVec <- numeric()
  
  for (k in 1:numClusters) {
    clusterData <- assignData |> filter(clusterGMM == k)
    
    if (nrow(clusterData) == 0) next
    
    means <- colMeans(clusterData |> select(-Raid, -runId, -clusterGMM, -clusterUncertainty), na.rm = TRUE)
    
    tomeVal <- 0
    if ("Mythic Tome" %in% names(means)) {
      val <- as.numeric(means["Mythic Tome"])
      if (!is.na(val)) tomeVal <- val
    }
    if (tomeVal == 0 && "Fabled Tome" %in% names(means)) {
      val <- as.numeric(means["Fabled Tome"])
      if (!is.na(val)) tomeVal <- val
    }
    
    emeraldVal <- 0
    if ("Emerald Block" %in% names(means)) {
      val <- as.numeric(means["Emerald Block"])
      if (!is.na(val)) emeraldVal <- val
    }
    
    liquidVal <- 0
    if ("Liquid Emerald" %in% names(means)) {
      val <- as.numeric(means["Liquid Emerald"])
      if (!is.na(val)) liquidVal <- val
    }
    
    uncertaintyVal <- mean(clusterData$clusterUncertainty, na.rm = TRUE)
    if (is.na(uncertaintyVal)) uncertaintyVal <- 0
    
    clusterVec <- c(clusterVec, k)
    numRunsVec <- c(numRunsVec, nrow(clusterData))
    raidsVec <- c(raidsVec, paste(unique(clusterData$Raid), collapse = ","))
    tomeVec <- c(tomeVec, round(tomeVal, 2))
    emberVec <- c(emberVec, round(emeraldVal, 2))
    liquidVec <- c(liquidVec, round(liquidVal, 2))
    uncertaintyVec <- c(uncertaintyVec, round(uncertaintyVal, 3))
  }
  
  clusterChar <- data.frame(
    cluster = clusterVec,
    numRuns = numRunsVec,
    raids = raidsVec,
    avgTome = tomeVec,
    avgEmber = emberVec,
    avgLiquidEb = liquidVec,
    avgUncertainty = uncertaintyVec,
    stringsAsFactors = FALSE
  )
  
  return(list(
    assignments = assignData,
    characteristics = clusterChar,
    gmmFit = gmmRes$gmmFit
  ))
}

printPcaResults <- function(pcaAgg, pcaRes) {
  if (!is.null(pcaAgg)) {
    cat("\npca results (aggregated items)\n")
    cat("Variance explained:\n")
    print(summary(pcaAgg))
    cat("\nPC1 & PC2 Loadings:\n")
    print(pcaAgg$rotation[, 1:min(2, ncol(pcaAgg$rotation))])
  }
  
  if (!is.null(pcaRes)) {
    cat("\npca results (detailed items)\n")
    print(summary(pcaRes))
    cat("\nPC1 & PC2 Loadings:\n")
    print(pcaRes$rotation[, 1:min(2, ncol(pcaRes$rotation))])
  }
}

printChisqResults <- function(ebChisq) {
  cat("\nchi squared results\n")
  invisible(lapply(names(ebChisq), function(r) {
    cat(paste0("\nRaid ", r, ":\n"))
    test <- ebChisq[[r]]
    if (is.null(test)) {
      cat("Insufficient data\n")
    } else {
      print(test)
    }
  }))
}

printHierarchicalResults <- function(hclustAnal) {
  if (is.null(hclustAnal)) {
    cat("\nClustering?\n")
    cat("No clustering results available\n")
    return(invisible(NULL))
  }
  
  cat("\nHierarchical clustering (Ward method):\n")
  cat("Elbow method WSS values:\n")
  print(hclustAnal$wss)
  
  cat("\nTrying 3-cluster solution:\n")
  branches <- analyzeClusterBranches(hclustAnal, hclustAnal$originalData, numClusters = 3)
  
  if (!is.null(branches)) {
    cat("\nBranch Characteristics:\n")
    branchChar <- branches$characteristics
    for (i in 1:nrow(branchChar)) {
      cat(sprintf("Cluster %d: %d runs (raids: %s) | Avg Tome: %.2f, Ember: %.2f, Liquid EB: %.2f\n",
                  branchChar$cluster[i], branchChar$numRuns[i], branchChar$raids[i],
                  branchChar$avgTome[i], branchChar$avgEmber[i], branchChar$avgLiquidEb[i]))
    }
    
    cat("\n\nBranch Assignments (first 10 runs):\n")
    print(head(branches$assignments |> select(Raid, runId, clusterHC), 10))
  }
}

printGmmResults <- function(gmmAnal) {
  if (is.null(gmmAnal)) {
    cat("\ngaussian mixture models\n")
    cat("GMM analysis not available (mclust package may need installation)\n")
    return(invisible(NULL))
  }
  
  cat("\ngaussian mixture models\n")
  cat("Model selection (AIC/BIC):\n")
  if (!is.null(gmmAnal$gmmFit)) {
    cat(sprintf("Optimal clusters: %d\n", gmmAnal$gmmFit$G))
    cat(sprintf("Model type: %s\n", gmmAnal$gmmFit$modelName))
    cat(sprintf("BIC: %.2f\n", gmmAnal$gmmFit$bic))
  }
  
  cat("\nBranch Characteristics:\n")
  branchChar <- gmmAnal$characteristics
  
  if (is.null(branchChar) || nrow(branchChar) == 0) {
    cat("No branch characteristics available\n")
    return(invisible(NULL))
  }
  
  for (i in 1:nrow(branchChar)) {
    cat(sprintf("Cluster %d: %d runs (raids: %s) | Avg Tome: %.2f, Ember: %.2f, Liquid EB: %.2f, Uncertainty: %.3f\n",
                branchChar$cluster[i], branchChar$numRuns[i], branchChar$raids[i],
                branchChar$avgTome[i], branchChar$avgEmber[i], branchChar$avgLiquidEb[i], branchChar$avgUncertainty[i]))}
  
  cat("\n\nBranch Assignments with Uncertainty (first 10 runs):\n")
  assignData <- gmmAnal$assignments
  if (!is.null(assignData) && nrow(assignData) > 0) {
    print(head(assignData |> select(Raid, runId, clusterGMM, clusterUncertainty), 10))
  } else {
    cat("No assignment data available\n")
  }
}