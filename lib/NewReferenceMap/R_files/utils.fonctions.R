


##### Function to choose the reference sample
choose_refSample_alignment <- function(cutOff_PeakList){
  message("Selectting the reference sample...\n")
  cutOff_PeakList <- as.data.frame(cutOff_PeakList)
  
  min_value <- cutOff_PeakList %>%
    group_by(sample) %>%
    summarise(MaxRT = max(rt,na.rm=T),MinRT=min(rt,na.rm=T)) %>%
    arrange(sample) %>%
    mutate(diff=MaxRT-MinRT) %>%
    mutate(median_shift = diff - median(diff))

  mean_rt <- min_value %>% 
    arrange(abs(diff))%>%
    dplyr::filter(diff >= quantile(min_value$diff)[[2]] & diff <= quantile(min_value$diff)[[4]])
  
  
  mean_rt <- mean_rt %>% 
    arrange(abs(median_shift)) %>% 
    ungroup()
  
  
  value_intensity <- as.data.frame(cutOff_PeakList) %>%
    group_by(sample) %>%
    mutate(nb=length(rt)) %>% 
    distinct(sample, nb)
  
  centerSample <- 0
  withProgress(message = 'Selectting the reference sample......', value = 0, {
  for (j in seq(4,1,-1)){
    incProgress(1/4, detail = paste("in progress... ",j/4*100, "%",collapse=""))
    max_peaks <- value_intensity %>% 
      arrange(desc(nb))%>% ungroup() %>%
      slice(1:(nrow(value_intensity)/j))
    
    for (i in mean_rt$sample){
      
      if (i %in% max_peaks$sample){
        centerSample <- i
        break
      }
    }
    if (centerSample!=0){
      break
    }
    
  }
  })
  
  if(centerSample==0){
    centerSample<-value_intensity[value_intensity$nb == max(value_intensity$nb),]$sample[1]
    
    message(paste0("Warning! : the algorithm can’t find a median sample. The sample with the most peaks is selected.")) 
    ask_confirmation(
      inputId = "warning_ref_sample",
      type = "warning",
      title = "Warning!",
      text = tags$b(
        icon("frown"),
        "The algorithm can’t find a median sample.",
        br(),
        "The sample with the most peaks is selected.",
        
        style = "color: #FA5858;"
      ),
      btn_labels = c("Yep"),
      btn_colors = c("#FE642E"),
      html = TRUE,
      closeOnClickOutside = TRUE
    )
  }
  message(paste0("Sample : ",centerSample, " is selected as a reference"))  
  return(centerSample)
}


