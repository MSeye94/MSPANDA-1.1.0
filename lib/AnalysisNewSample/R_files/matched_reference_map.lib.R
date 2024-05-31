
matchRtMzMassif<-function(x,
                          table,
                          nomatch = NA_integer_,
                          ppm.tolerance = 0, 
                          mass.tolerance = 0.150, 
                          rt_tolerance = 180){
  
  
  library(stringr)
  library(progressr)
  
  library(shiny)
  library(shinyjs)
  library(shinythemes)
  library(shinyWidgets)
  library(shinycssloaders)
  library(shinyalert)
  library(shinydashboard)
  library(shinyFiles)
  
  if(!require(MsCoreUtils))
    stop("R package \"MsCoreUtils\" is not found, please install R package \"MsCoreUtils\" ")
  
  
  if (!is.numeric(nomatch) || length(nomatch) != 1L)
    stop("'nomatch' has to be a 'numeric' of length one.")
  
  if (length(dim(x)) != 2 || length(dim(table)) != 2)
    stop("'x' and 'table' have to be two data frames")
  
  if(nrow(x)<2)
    stop("'x' must have two or more rows!")
  
  
  ## Oreder tables 
  table<-table[order(table[,'M+H']), ]
  x<-x[order(x[,'M+H']), ]
  
  # create Id for features new sample 
  table$IdTab<-createID(ref = "NewSample", nrow(table))
  table$Match<-"Matched"
  
  ## iso.mass.link for ref
  iso.mass.link1<-x[,"iso.mass.link"]
  mass1<-x[,"M+H"]
  rt1<-x[,"CE-time"]
  
  ## iso.mass.link for new sample
  iso.mass.link2<-table[,"iso.mass.link"]
  mass2<-table[,"M+H"]
  rt2<-table[,"CE-time"]
  
  
  names(iso.mass.link2)<-createID(ref = "NewSample", nrow(table))
  names(mass2)<-createID(ref = "NewSample", nrow(table))
  names(rt2)<-createID(ref = "NewSample", nrow(table))
  rownames(table)<-createID(ref = "NewSample", nrow(table))
  tab_IDNames<-createID(ref = "NewSample", nrow(table))
  
  
  
  idxl <- vector("list", length = nrow(x))
  
  pb_matchMassif <- txtProgressBar(min=1, max = length(seq_along(idxl)), style = 3)
  
  cat("matching massif...\n")
  withProgress(message = paste("Matching sample:",unique(table$sample)), value = 0, {
    
    for (i in seq_along(idxl)) {
      
      incProgress(1/length(idxl), detail =paste(unique(table$sample),"..."))
      
      setTxtProgressBar(pb_matchMassif, i)
      matches <-tab_IDNames[which(abs(rt2 - rt1[i]) <= rt_tolerance)] 
      
      ## Mass filter
      matches<-matches[which(abs(mass2[matches]-mass1[i])<=5)]
      
      if (length(matches)) {
        
        iso.mass.link.matchedRT<-iso.mass.link2[matches]
        
        
        massifMatched<-list()
        massifMatchedBool<-c()
        
        for (j in 1:length(iso.mass.link.matchedRT)) {
          
          iso.mass.link.ref<-iso.mass.link1[i]
          
          N_Ref_Massif<-length(unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ","))))))
          N_Candidate_Massif<-length(unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ","))))))
          
          
          if(N_Candidate_Massif == 2 | N_Candidate_Massif == 3){
            
            if(N_Ref_Massif == 2 | N_Ref_Massif == 3){
              
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ","))))),
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif >3){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:3],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            }
            
            
          } else if(N_Candidate_Massif == 4 | N_Candidate_Massif == 5){
            
            if(N_Ref_Massif == 2 | N_Ref_Massif == 3){
              
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ","))))),
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif  == 4){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:3],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif > 4){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:3],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:4],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            }
          } else if(N_Candidate_Massif == 6 | N_Candidate_Massif == 7){
            
            if(N_Ref_Massif == 2 | N_Ref_Massif == 3){
              
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ","))))),
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif  == 4){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:3],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif == 5){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:3],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:4],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif > 5){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:4],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:5],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            }
            
          } else if(N_Candidate_Massif >= 8){
            
            if(N_Ref_Massif == 2 | N_Ref_Massif == 3){
              
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ","))))),
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif  == 4){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:2],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:3],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif  == 5){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:3],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:4],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif == 6){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:4],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:5],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            } else if(N_Ref_Massif > 6){
              massifMatched[[j]]<-MsCoreUtils::closest(
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.matchedRT[j], pattern = ",")))))[1:5],
                unique(sort(as.numeric(unlist(str_split(iso.mass.link.ref, pattern = ",")))))[1:6],
                tolerance = mass.tolerance, ppm = ppm.tolerance,
                duplicates="closest")
              massifMatchedBool[j]<-is.element(NA, massifMatched[[j]])
            }
          }
          
          
          
        }
        
        
        matchesj<-matches[which(!massifMatchedBool)]
        
        if(length(matchesj)!=0){
          
          if(length(matchesj)>1){
            ## If matched is multiple
            #idx_matchedjSelected<-matchesj[which(table[matchesj,]$intensity==max(table[matchesj,]$intensity))][1]
            idx_matchedjSelected<-matchesj[which(abs(table[matchesj,]$`CE-time`-rt1[i])==min(abs(table[matchesj,]$`CE-time`-rt1[i])))][1]
            
            idxl[[i]]<-idx_matchedjSelected
            
            idxl[[i]]<-which(tab_IDNames %in% idxl[[i]])
       
            
          }else{
            idxl[[i]] <- matchesj
            idxl[[i]]<-which(tab_IDNames %in% idxl[[i]])
            
            } 
        
          

          } else{
          idxl[[i]] <- nomatch
        }
        
      } else{
        idxl[[i]] <- nomatch
      }
      
    
      
    }
    close(pb_matchMassif)
    
  })
  

  cat("OK...!\n")
  rownames(table)<-1:nrow(table)
  colnames(x)<-paste0(colnames(x),"1")
  
  MatchTable = cbind(x[seq_along(x[,"M+H1"]),],table[as.integer(idxl),])

  
  rownames(MatchTable)<-rownames(x)

  
  # percentageMatch = round((nrow(MatchTable)-sum(is.na(MatchTable$`M+H`)))*100/nrow(table), digits = 2)
  # numberMatch = (nrow(MatchTable)-sum(is.na(MatchTable$`M+H`)))

  nomatchTable<-table[table$IdTab %ni% MatchTable$IdTab[!is.na(MatchTable$IdTab)],]
  nomatchTable$Match<-"No match"
  
  rownames(nomatchTable)<-1:nrow(nomatchTable)
  nomatchTable$ID<-NA
  
  matched<-MatchTable[!is.na(MatchTable$IdTab),9:ncol(MatchTable)]
  matched$ID<-rownames(matched)
  

  nomatchTable<-rbind(nomatchTable, matched)
  
  ### Removes duplicated rows
  idx_Duplicates<-nomatchTable$IdTab[duplicated(nomatchTable$IdTab)]
  idx_Duplicates_ref<-nomatchTable[duplicated(nomatchTable$IdTab),]$ID
  
  if(!is.null(idx_Duplicates_ref)){
   
    nomatchTable[nomatchTable$ID %in% idx_Duplicates_ref,"Match"]<-"Duplicates"
    MatchTable[rownames(MatchTable) %in% idx_Duplicates_ref,"Match"]<-"Duplicates" #new
  }
  
  
  
  #write.table(x = MatchTable, file ="MatchTable.csv", sep = ",")
  
  # View(MatchTable)
  # View(nomatchTable)

  
  percentageMatch = round(sum(MatchTable$Match=="Matched", na.rm = TRUE)*100/nrow(table), digits = 2)
  numberMatch = sum(MatchTable$Match=="Matched", na.rm = TRUE)
  
  
  #   write.table(MatchTable, file = "MatchTable.csv")
  
  return(ResMatch = list(idxl = as.integer(idxl),
                         percentageMatch = percentageMatch,
                         numberMatch = numberMatch,
                         MatchTable = MatchTable,
                         nomatchTable = nomatchTable,
                         idx_Duplicates_ref = idx_Duplicates_ref))  
  
  
}


#####################################################################################################################
####################################################################################################################
###################################################################################################################
##################################################################################################################


matchNewSample_ref<-function(ms_features_MSDIAL_list,
                             map_ref,
                             rt_tolerance = 180,
                             mass_tolerance = 0.150
                             # mzcol = "mz",
                             # rtcol = "rt"
){
  mzRt_map_ref<-map_ref[,c(2:3,c(5:ncol(map_ref)))]
  rownames(mzRt_map_ref)<-map_ref$ID
  colnames(mzRt_map_ref)[1:2]<-c("M+H","CE-time")
  
  ms_features_MSDIAL<-do.call("rbind", ms_features_MSDIAL_list)
  
  ##### match news samples to ref
  idx_Duplicates_ref<-list()
  match_ref_list<-list()
  matrix_new_samples<-list()
  noMatch_newSample<-list()
  matchedTable_newSample<-list()
  pct_match_ref<-c()
  number_match<-c()
  
  
  
  # matchParallel<-function(ms_features, ref , rt_tolerance){
  # 
  #   res<-matchRtMzMassif(ref, ms_features, rt_tolerance)
  # 
  #   return(res)
  # }
  # 
  # workers = ceiling((detectCores())-1)
  # param <- SnowParam(workers = workers, type = "SOCK")
  # 
  # system.time(match_ref_list<-bplapply(ms_features_MSDIAL_list,
  #                                     matchParallel,
  #                                     ref = mzRt_map_ref,
  #                                     rt_tolerance = rt_tolerance,
  #                                     BPPARAM = param))
  # 
  # names(match_ref_list)<-1:length(match_ref_list)
  # 
  # for (i in 1:length(match_ref_list)) {
  # 
  #   matrix_new_samples[[i]]<-match_ref_list[[i]]$MatchTable[,"intensity"]
  #   matchedTable_newSample[[i]]<-match_ref_list[[i]]$MatchTable
  # 
  #   noMatch_newSample[[i]]<-match_ref_list[[i]]$nomatchTable
  #   pct_match_ref[i]<-match_ref_list[[i]]$percentageMatch
  #   number_match[i]<-match_ref_list[[i]]$numberMatch
  # 
  # }
  # 
  # names(matrix_new_samples)<-1:length(matrix_new_samples)
  # names(noMatch_newSample)<-unique(ms_features_MSDIAL$sample)
  # names(matchedTable_newSample)<-unique(ms_features_MSDIAL$sample)
  
  
  withProgress(message = 'Matching', value = 0, {
    for (i in 1:length(ms_features_MSDIAL_list)) {
      
      ########################################################################
      # Reset the timebar for later
      updateShinyProgressBar(
        shinyProgressData=list(
          session=session,
          progressId="matchProgressBar",
          progressTotal=length(ms_features_MSDIAL_list),
          textId="match_pre"
        ),
        pbValue=i,
        headerMsg= "",
        footerMsg=paste("Matching sample ",unique(ms_features_MSDIAL_list[[i]]$sample),"...")
      )
      ########################################################################
      #incProgress(1/length(ms_features_MSDIAL_list), detail = paste(unique(ms_features_MSDIAL_list[[i]]$sample),"..."))
      incProgress(1/length(ms_features_MSDIAL_list), detail = "...")
      
      # match_ref_list[[i]]<-matchRtMz(mzRt_map_ref,
      #                                ms_features_MSDIAL_list[[i]],
      #                                rt_tolerance = rt_tolerance,
      #                                mzcol = mzcol,
      #                                rtcol = rtcol)
      
      
      match_ref_list[[i]]<-matchRtMzMassif(mzRt_map_ref,
                                           ms_features_MSDIAL_list[[i]],
                                           rt_tolerance = rt_tolerance,
                                           mass.tolerance  = mass_tolerance)
      
      
      
      names(match_ref_list)<-1:length(match_ref_list)
      matrix_new_samples[[i]]<-match_ref_list[[i]]$MatchTable[,"intensity"]
      matchedTable_newSample[[i]]<-match_ref_list[[i]]$MatchTable
      
      noMatch_newSample[[i]]<-match_ref_list[[i]]$nomatchTable
      pct_match_ref[i]<-match_ref_list[[i]]$percentageMatch
      number_match[i]<-match_ref_list[[i]]$numberMatch
      idx_Duplicates_ref[[i]]<-match_ref_list[[i]]$idx_Duplicates_ref
      
    }
    
    names(matrix_new_samples)<-1:length(matrix_new_samples)
    names(noMatch_newSample)<-unique(ms_features_MSDIAL$sample)
    names(matchedTable_newSample)<-unique(ms_features_MSDIAL$sample)
    names(idx_Duplicates_ref)<-unique(ms_features_MSDIAL$sample)
  })
  
  
  ########################################################################
  # Reset the timebar for later
  updateShinyProgressBar(
    shinyProgressData=list(
      session=session,
      progressId="matchProgressBar",
      progressTotal=length(ms_features_MSDIAL_list),
      textId="match_pre"
    ),
    pbValue=length(ms_features_MSDIAL_list),
    headerMsg= "",
    footerMsg=""
  )
  ########################################################################
  matrix_new_samples<-do.call("cbind",matrix_new_samples)
  
  matrix_new_samples<-matrix_new_samples
  rownames(matrix_new_samples)<-rownames(mzRt_map_ref)
  colnames(matrix_new_samples)<-unique(ms_features_MSDIAL$sample)
  
  res_newSample<-cbind(map_ref[,1:3],matrix_new_samples)
  
  pct_match_ref<-data.frame(sample = unique(ms_features_MSDIAL$sample),
                            percent_matched = pct_match_ref,
                            number_match = number_match)
  #names(pct_match_ref)<-pData(raw_align_Data)$Filename[-length(pData(raw_align_Data)$Filename)]
  
  
  res<-list(res_newSample = res_newSample,
            pct_match_ref = pct_match_ref, 
            noMatch_newSample = noMatch_newSample,
            matchedTable_newSample = matchedTable_newSample,
            idx_Duplicates_ref = idx_Duplicates_ref)
  
  return(res)
  
}



