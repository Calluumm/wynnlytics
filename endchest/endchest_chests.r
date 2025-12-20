itemMap <- c(
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

  "eb" = "Emerald Block",
  "le" = "Liquid Emerald",
  "vcb" = "Varied Crafter Bag [1/1]",
  "pcb" = "Packed Crafter Bag [1/1]",
  "scb" = "Stuffed Crafter Bag [1/1]",
  "as" = "Ability Shard",
  "amp1" = "Corkian Amplifier I",
  "amp2" = "Corkian Amplifier II",
  "amp3" = "Corkian Amplifier III",

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

  # TNA set items (boxed)
  "none" = "Unidentified Helmet",
  "alep" = "Unidentified Leggings",
  "infi" = "Unidentified Dagger",
  "dive" = "Unidentified Spear",
  "cont" = "Unidentified Wand",
  "recu" = "Unidentified Bow",
  "frac" = "Unidentified Relik",
  "fore" = "Unidentified Forebearance",
  "ingr" = "Unidentified Ingress",
  "brea" = "Unidentified Breakthrough",
  "deta" = "Unidentified Detachment",
  "coll" = "Unidentified Collection",
  "simu" = "Unidentified Simulacrum"
)

raidPoolsAcr <- list(
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

raidAmountPools <- list(
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

raidPools <- lapply(raidPoolsAcr, function(pool) {
  vapply(pool, function(v) if (v %in% names(itemMap)) itemMap[[v]] else v, character(1))
})

itemCat <- function(itemName) {
  if (grepl("Tome", itemName)) return(1)
  if (itemName %in% c("Emerald Block", "Liquid Emerald")) return(2)
  if (grepl("Unidentified", itemName) && 
      (grepl("Helmet|Leggings|Dagger|Spear|Wand|Bow|Relik", itemName) ||
       grepl("Titanomachia|Insignia|Resolution|Cannonade|Antebellum|Windborne|Mazeweaver", itemName))) {
    return(3)
  }
  if (grepl("Unidentified", itemName)) return(4)
  if (grepl("Powder|Ability Shard|Corkian Amplifier", itemName)) return(5)
  return(5)
}

ampPrio <- function(itemName) {
  if (grepl("Amplifier III", itemName)) return(1)
  if (grepl("Amplifier II", itemName)) return(2)
  if (grepl("Amplifier I", itemName)) return(3)
  return(4)
}

isFable <- function(itemName) {
  grepl("Titanomachia|Insignia|Resolution|Cannonade", itemName)
}

packer <- function(itemNames, itemCounts) {
  if (length(itemNames) == 0 || length(itemCounts) == 0) {
    return(data.frame(
      item = character(0),
      count = numeric(0),
      category = numeric(0),
      ampPriority = numeric(0),
      isFable = logical(0),
      slotIndex = numeric(0),
      dropped = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  itemsDf <- data.frame(
    item = itemNames,
    count = itemCounts,
    category = sapply(itemNames, itemCat),
    ampPriority = sapply(itemNames, ampPrio),
    isFable = sapply(itemNames, isFable),
    stringsAsFactors = FALSE
  )
  
  itemsDf <- itemsDf[
    with(itemsDf, order(
      category,
      ifelse(category == 3, !isFable, TRUE),
      ifelse(category == 5, ampPriority, 0),
      item
    )),
  ]
  
  slotCount <- 0
  maxSlots <- 27
  itemsDf$dropped <- FALSE
  itemsDf$slotIndex <- NA_integer_
  
  for (i in 1:nrow(itemsDf)) {
    slotCount <- slotCount + 1
    itemsDf$slotIndex[i] <- slotCount
    if (slotCount > maxSlots) {
      itemsDf$dropped[i] <- TRUE
    }
  }
  
  return(itemsDf)
}

expandItems <- function(x) {
  if (length(x) == 0) return(character(0))
  misses <- setdiff(x, names(itemMap))
  if (length(misses) > 0) warning("Unmapped acronyms: ", paste(misses, collapse = ", "))
  vapply(x, function(v) if (v %in% names(itemMap)) itemMap[[v]] else v, character(1))
}

applyChestCapacity <- function(raiderData) {
  raiderAdj <- raiderData
  
  for (idx in 1:nrow(raiderAdj)) {
    pullColsPattern <- grep("^Pull\\.\\d+", names(raiderAdj), value = TRUE)
    itemCols <- grep("Item$", pullColsPattern, value = TRUE)
    countCols <- grep("Count$", pullColsPattern, value = TRUE)
    
    itemsRaw <- as.character(as.vector(raiderAdj[idx, itemCols]))
    countsRaw <- suppressWarnings(as.numeric(as.vector(raiderAdj[idx, countCols])))
    
    validMask <- !is.na(itemsRaw) & itemsRaw != "" & itemsRaw != "Air" & 
                  !is.na(countsRaw) & countsRaw > 0
    
    items <- itemsRaw[validMask]
    counts <- countsRaw[validMask]
    validi <- which(validMask)
    
    items <- expandItems(items)
    
    if (length(items) > 0) {
      packedOut <- packer(items, counts)
      
      droppedM <- packedOut$dropped
      if (any(droppedM)) {
        droppedSlot <- which(droppedM)
        for (dPos in droppedSlot) {
          colIdx <- validi[dPos]
          if (colIdx <= length(countCols)) {
            colName <- countCols[colIdx]
            raiderAdj[idx, colName] <- 0
          }
        }
      }
    }
  }
  
  return(raiderAdj)
}
