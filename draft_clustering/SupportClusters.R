#'Support Clusters
#'
#'This function cluster maintenance events based on related resources in the context of a support structure.
#'
#' @param MAAPResults Extracted R object using the \code{MAAPResults} from a *.tfd file.
#'
#' @param NumberofClusters Desired number of clusters
#'
#' @return A list with SupportClusters dataframe, Maintenance skills cluster and the kMeans object (kn), Model matrix (mm).
#'
#' @details Maintenance is clustered based on all the resources linked to the maintenance event.
#' The clusters is presented within the context of the support structure, that is the support and the supported unit.
#' Use to identify improvements in the use of resources.
#'
#' @section Breakeven: A break-even point is also calculated, but not not used in the clustering,
#' breakeven is based on the projection of the number of events and when cost of maintenance exceed the cost of the item.
#' Breakeven points are categorised on _short_ < 2 years; _medium_ < 5 years; and _long_ > 5 years.
#'
#' @export

SupportClusters <- function(MAAPResults,
                            NumberofClusters) {
  require(dplyr)
  # MAAPResults <- R1
  #Make the cluster from matrix
  df <- MAAPResults$MaintResources[,c("meid", "ResID")]

  mm <- model.matrix(meid ~., data = df)[,-1]

  mm.grp <-  as.data.frame(mm) %>%
    cbind(meid = df$meid)%>%
    group_by(meid) %>%
    summarise_all(sum)

  #Cluster and dimension reduction
  set.seed(1234)
  kmeansObj <- kmeans(mm.grp[,-1], NumberofClusters)
  mm.grp$meCluster <- kmeansObj$cluster
  #Cluster info
  # kmeansObj$size #The number of points in each cluster
  # kmeansObj$centers #A matrix of cluster centres.

  event.maint.cluster <- left_join(x = MAAPResults$MaintenanceEvents,
                                   y = mm.grp[,c("meid","meCluster")],
                                   by = c("MEID" = "meid"))
  #Support Structure
  ss <- MAAPResults$SupportStructure %>%
    rename(MERefNum = RefNum, RefNum = RefNum.1) %>%
    select(MERefNum,
           SupportedUnit, SupportedUnitRef,
           SupportingUnit, SupportingUnitRef,
           EchelonSupportingUnit,
           ADTfwd, ADTretro,
           OSTfwd, OSTretro,
           ShippingCostfwd, ShippingCostretro,
           RCT) %>%
    unique()

  maint.support.cluster <- left_join(x = ss,
                                     y = unique(event.maint.cluster[,c("MEID",
                                                                       "RefNum",
                                                                       "meCluster")]),
                                     by = c("MERefNum" = "RefNum"))

#Expand cluster related data to include support structure data

  library(reshape2)
  #TODO Filter on the corrective only
  mta <- melt(MAAPResults$MaintenanceAnnual,
              id.vars = c("MType", "PartName", "RefNum", "MEName", "MERefNum", "UnitNameRepair", "UnitNameRaising"),
              variable.name = "Year",
              value.name = "NbrEvents")

  mta$Year <- as.numeric(levels(mta$Year))[mta$Year]

  #Find system level
  SysLvl <- MAAPResults$SysStructure %>%
    group_by(RefNum) %>%
    summarise(Level = round(mean(as.numeric(Level),0)))

  mta <- left_join(x = mta,
                   y = SysLvl,
                   by = c("RefNum" = "RefNum"))

  #Support Structure
  ss <- MAAPResults$SupportStructure %>%
    rename(MERefNum = RefNum, RefNum = RefNum.1) %>%
    select(MERefNum,
           SupportedUnit, SupportedUnitRef,
           SupportingUnit, SupportingUnitRef,
           EchelonSupportingUnit,
           ADTfwd, ADTretro,
           OSTfwd, OSTretro,
           ShippingCostfwd, ShippingCostretro,
           RCT) %>%
    unique()

  #Support Unit Information
  sui <- MAAPResults$UnitInfo %>%
    select( OrgType, OrgRefNbr, ShopHrsWk, ShopDaysWk,
            ShopWksYr, HoldingCostRate, RprLvl, Am, Essent,
            WtSclr, VolSclr, InitialOpDate,DurationOfOp,
            InitialSparesYears, UnitHrsWk, UnitDaysWk,
            UnitWksYr, ConfidenceLevel, SupportPriority,
            SupportPriorityMult) %>%
    mutate(OrgRefNbr = as.character(OrgRefNbr))

  #Support Structure combined with Unit Info
  ss_ui <- left_join(ss, sui, by=c("SupportingUnitRef"="OrgRefNbr"))

  #Annual tasks and add the SupportedUnit and SupportingUnit info
  mta.ss <- left_join(x = mta,
                      y = ss_ui,
                      by = c("MERefNum"="MERefNum",
                             "UnitNameRaising" = "SupportedUnit",
                             "UnitNameRepair" = "SupportingUnit"))

  mta.px <- left_join(x = mta.ss,
                      y = MAAPResults$px[,c("RefNum", "ItemPrice")],
                      by = c("RefNum" = "RefNum"))

  #Add resource cost breakdown (if exist)
  if (is.null(MAAPResults$MaintResourceCost)){
    mta.ss.res <- mta.px %>%
      mutate(TotalCost =0,
             DataCost =0,
             EnergyCost =0,
             FacilityCost =0,
             MiscCost =0,
             PartCost =0,
             SkillCost =0,
             ToolCost =0)
  } else{
    MaintResourceCost <- MAAPResults$MaintResourceCost
    mta.ss.res <- left_join(x = mta.px,
                            y = MaintResourceCost,
                            by = c("MERefNum"="RefNum"))
  }

  #Org Levels
  SupportUnits <- unique(MAAPResults$SupportStructure$SupportingUnitRef[order(
    MAAPResults$SupportStructure$SupportingUnitRef)])
  OperatingUnits <- unique(MAAPResults$SupportStructure$SupportedUnitRef[order(
    MAAPResults$SupportStructure$SupportedUnitRef)])
  #LOR options
  policies <- c(SupportUnits, "Discard", "MixedCost", "MixedPolicy")

  #Add columns for each support unit repair cost, and rows
  su <- data.frame(matrix(ncol = length(policies), nrow = nrow(mta.ss.res)))

  colnames(su) <- policies

  #Last bind before tidying
  lor.untidy <- mta.ss.res %>%
    cbind(su)

  #Get Contractor info
  contractor <- MAAPResults$contractor %>%
    filter(Name %in% unique(lor.untidy$UnitNameRepair))

  # add org units
  unitref <-  lor.untidy %>%
    select(UnitNameRaising, SupportedUnitRef) %>%
    filter(!is.na(SupportedUnitRef)) %>%
    unique()

  #setup org ref numbers
  lor.untidy <- left_join(x = lor.untidy,
                          y = contractor,
                          by = c("UnitNameRepair" = "Name")) %>%
    transform(SupportingUnitRef.x = ifelse(!is.na(SupportingUnitRef.x),
                                           SupportingUnitRef.x,
                                           SupportingUnitRef.y)) %>%
    rename(SupportingUnitRef = SupportingUnitRef.x) %>%
    select(-SupportingUnitRef.y)

  lor.untidy <- left_join(x = lor.untidy,
                          y = unitref,
                          by = c("UnitNameRaising" = "UnitNameRaising")) %>%
    transform(SupportedUnitRef.x = ifelse(!is.na(SupportedUnitRef.x),
                                          SupportedUnitRef.x,
                                          SupportedUnitRef.y)) %>%
    rename(SupportedUnitRef = SupportedUnitRef.x) %>%
    select(-SupportedUnitRef.y)

  #add contractor repair cost
  #Assumptions for splitting costs where no values available.
  aDataCost <- 0.01
  aEnergyCost <- 0.02
  aFacilityCost <- 0.06
  aMiscCost <- 0.01
  aPartCost <- 0.2
  aSkillCost <- 0.6
  aToolCost <- 0.1

  if(nrow(MAAPResults$M207)>0){
    d = MAAPResults$M207[MAAPResults$M207$Repairs > 0,c("OrgName", "Repair", "ItemName", "Name", "Reference", "RefNum", "Cost", "RepairYear")] %>%
      rename(UnitNameRaising = OrgName, UnitNameRepair = Repair,
             PartName = ItemName, MEName = Name, MERefNum = Reference, MCost = Cost,
             Year = RepairYear) %>%
      #Weights based on assumptions
      mutate(mDataCost = MCost*aDataCost,
             mEnergyCost = MCost*aEnergyCost,
             mFacilityCost = MCost*aFacilityCost,
             mMiscCost = MCost*aMiscCost,
             mPartCost = MCost*aPartCost,
             mSkillCost = MCost*aSkillCost,
             mToolCost = MCost*aToolCost)

    lor.untidy <- left_join(x = lor.untidy,
                            y = d,
                            by = c("UnitNameRaising" = "UnitNameRaising",
                                   "UnitNameRepair" = "UnitNameRepair",
                                   "PartName" = "PartName",
                                   "MEName" = "MEName",
                                   "MERefNum" = "MERefNum",
                                   "RefNum" = "RefNum",
                                   "Year" = "Year")) %>%
      mutate(TotalCost = ifelse(!is.na(TotalCost),TotalCost, MCost),
             DataCost = ifelse(!is.na(DataCost),DataCost, mDataCost),
             EnergyCost = ifelse(!is.na(EnergyCost),EnergyCost, mEnergyCost),
             FacilityCost = ifelse(!is.na(FacilityCost),FacilityCost, mFacilityCost),
             MiscCost = ifelse(!is.na(MiscCost),MiscCost, mMiscCost),
             PartCost = ifelse(!is.na(PartCost),PartCost, mPartCost),
             SkillCost = ifelse(!is.na(SkillCost),SkillCost, mSkillCost),
             ToolCost = ifelse(!is.na(ToolCost),ToolCost, mToolCost)) %>%
      select(-c(MCost, mDataCost, mEnergyCost, mFacilityCost,
                mMiscCost, mMiscCost, mPartCost, mSkillCost,mToolCost))
#TotalCost, DataCost, EnergyCost, FacilityCost, MiscCost, PartCost, SkillCost, ToolCost
    rm(d)
  }

  #Clean up to avoid errors (good for Tableau too)
  # lor <- lor.untidy %>%
  #   mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #   mutate_at("EchelonSupportingUnit",
  #             funs(replace(., is.na(.),"Subcontractor"))) %>%#Echelon = Subcontractor
  #   mutate_at("OrgType",
  #             funs(replace(., is.na(.),"Support"))) %>%
  #   #TODO add unit ref numbers
  #   mutate_if(is.character, .funs = as.factor)

  # update due to funs deprecated in dplyr 0.8.0
  lor <- lor.untidy %>%
    mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>%
    mutate_at("EchelonSupportingUnit",
              list(~replace(., is.na(.),"Subcontractor"))) %>%#Echelon = Subcontractor
    mutate_at("OrgType",
              list(~replace(., is.na(.),"Support"))) %>%
    #TODO add unit ref numbers
    mutate_if(is.character, .funs = as.factor)

  rm(SysLvl, mta.px, mta, ss, sui, ss_ui, mta.ss, MaintResourceCost, mta.ss.res, SupportUnits, OperatingUnits, policies, su, lor.untidy, contractor, unitref)

  # names(lor)

  #Breakeven point calculation
  library(tidyr)
  library(purrr)
  part.be <- lor %>%
    arrange(PartName, RefNum, Year) %>%
    group_by(PartName, RefNum, MERefNum, ItemPrice) %>%
    mutate(CTotalCost = cumsum(TotalCost),
           CEvents = cumsum(NbrEvents))%>%
    nest()%>%
    mutate(model = purrr::map(data, ~ lm(Year ~ CTotalCost, data = .)),
           mf.Events = purrr::map(data, ~ lm(CEvents ~ Year, data = .))) %>%
    mutate(BEYear = map2_dbl(.x = model,
                             .y = ItemPrice,
                             .f = ~predict.lm(.x,
                                              newdata = data.frame(
                                                CTotalCost = .y))),
           BEEvents = map2_dbl(.x = mf.Events,
                               .y = BEYear,
                               .f = ~predict.lm(.x,
                                                newdata = data.frame(
                                                  Year = .y)))
    ) %>%
    mutate(BEYear = floor(BEYear),
           BEEvents = floor(BEEvents)) %>%
    select(RefNum, MERefNum, BEYear, BEEvents)

  #join breakeven with lor
  lor.be <- left_join(x = lor,
                      y = part.be,
                      by = c("RefNum" = "RefNum",
                             "MERefNum" = "MERefNum",
                             "PartName" = "PartName"))

  #Set BEPeriod
  StartYear <- MAAPResults$Environment$ProgramInitDate
  lor.be <- lor.be %>%
    mutate(BEPeriod = as.factor(if_else(BEYear - StartYear <= 2,
                                        "Short",
                                        if_else(BEYear - StartYear <= 5,
                                                "Medium",
                                                "Long"))))

#Final tidying and export
    skills.unit.summary <- MAAPResults$M219 %>%
    select(OrgRefNbr, SName, NumReqd,
           Utilization,
           AnnualManhoursSchMnt,
           AnnualManhoursUnSchMnt) %>%
    group_by(OrgRefNbr, SName) %>%
    summarise(NumReqd = mean(NumReqd), Utilization = mean(Utilization),
              AnnualManhoursSchMnt = mean(AnnualManhoursSchMnt),
              AnnualManhoursUnSchMnt = mean(AnnualManhoursUnSchMnt))

  #Add skill to the dataset
  maint.cluster.skill <- left_join(x = maint.support.cluster,
                                   y = skills.unit.summary,
                                   by = c("SupportingUnitRef" = "OrgRefNbr"))

  # save(maint.cluster.skill, file = SaveSkills)

  #Select a subset of columns to join with the cluster data
  breakeven <- lor.be %>%
    select(PartName, RefNum, MERefNum, MEName, BEYear, BEEvents, BEPeriod) %>%
    unique()

  SupportClusters <- left_join(x = maint.support.cluster,
                               y = breakeven,
                               by = c("MERefNum" = "MERefNum"))
  # save(SupportClusters, file = SaveSupport)

  #----

  #Create data for heatmap
  m <- kmeansObj$centers
  selectedData <- melt(m)
  # heatmap(m)

  #Remove ResID from the matrix data
  pxm <- melt(m) %>%
    mutate(ipn = gsub("ResID", "", Var2),
           Cluster = Var1,
           ClusCentre = value) %>%
    select(ipn, Cluster, ClusCentre)

  #Add ResName as rownames to show possible heatmap or decision tree
  mr <- MAAPResults$MaintResources %>%
    select(ResID, ResName) %>%
    unique()
  pxm2 <- left_join(x = pxm,
                    y = mr,
                    by = c("ipn" = "ResID")) %>%
    select(-ipn)

  hm <- acast(pxm2, ResName ~ Cluster, value.var = "ClusCentre")

#--------------------------------------
  ClusterData <- list(MaintClusters = maint.cluster.skill,
                      SupportClusters = SupportClusters,
                      mm = mm.grp,
                      kn = kmeansObj,
                      heatmapdata = hm)

  return(ClusterData)

  #cleanup data
  rm(breakeven, lor, lor.be, maint.cluster.skill, maint.support.cluster, part.be, skills.unit.summary)
  #cleanup values
  rm(df, mm, mm.grp, aDataCost, aEnergyCost, aFacilityCost, aMiscCost, aPartCost, aSkillCost, aToolCost, StartYear, tfd.file)
}
