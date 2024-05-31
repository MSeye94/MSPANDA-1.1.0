##" Initialize reactive values
RvarsInternalStandard<-allReactiveVarsNewRefMap$InternalStandard

## Controle some button

### Return generate reference map page
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$PreviousPageGenerateRefMap
  }
  ,
  handlerExpr = {
    updateRadioButtons(session = session, 
                       inputId = "RefMapStep", 
                       selected = "4")
    
    updateNavbarPage(
      session = session,
      inputId = "analysisNavbar",
      selected = "Generate the reference map"
    )
    
  })

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$nextPageIS
  }
  ,
  handlerExpr = {
    updateRadioButtons(session = session, 
                       inputId = "InternalStandardsStep", 
                       selected = "2")
    
    updateNavbarPage(
      session = session,
      inputId = "analysisNavbar",
      selected = "Identification internal standards"
    )
    
  })


observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$PreviousPage1
  }
  ,
  handlerExpr = {
    updateRadioButtons(session = session, 
                       inputId = "InternalStandardsStep", 
                       selected = "1")
    
    updateNavbarPage(
      session = session,
      inputId = "analysisNavbar",
      selected = "Identification internal standards"
    )
    
  })



observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$IdentifyNormalizers
  },
  handlerExpr = {
    
    if(!is.null(RvarsGrouping$RefereanceMap_selected) & !is.null(RvarsGrouping$MatrixAbundance_selected)){
      
      ### Disable some buttons when prepossessing start
      shinyjs::disable(selector = '.navbar-nav a[data-value="Match reference map"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Analysis new samples"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Peak detection"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Generate the reference map"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Database"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Statistical analysis"')
      shinyjs::disable(selector = '.navbar-nav a[data-value="Help"')
      
      disable("id_InternalStandardsParametersPanel")
      disable("IdentifyNormalizers")
      disable("ValidRefrenceMap")
      disable("PreviousPageGenerateRefMap")
      
      
      ### Verify Min features and Min sample
      
      
      #source("lib/NewRefrenceMap.lib/ClassRererenceMap_Function/ReferenceMap.R", local=TRUE)
      source("lib/NewReferenceMap/R_files/InternalStandard.lib_NewRefMap.R", local=TRUE)
      
      #### Method ratios
      
      input_Matrix<-as.data.frame(RvarsGrouping$MatrixAbundance_selected)[,4:ncol(RvarsGrouping$MatrixAbundance_selected)]
      
      colnames(input_Matrix)<-RvarsCorrectionTime$pheno_Data_mzML$Filenames
      
      
      RvarsInternalStandard$OjectNormalizers<-Search_normalizers(Matrix = input_Matrix,
                                                            pFeatures = input$pFeatures,
                                                            pSample = input$pSample,
                                                            minNormalizersParam = input$minNormalizers)
      
      

      
      
      output$DistibutionVar<-renderPlot({
        
        
        plot_Variability<-RvarsInternalStandard$OjectNormalizers$varCalcul
        Features_selected<-RvarsInternalStandard$OjectNormalizers$Features_selected
        nbrNormalizers<-length(Features_selected)
        nTick_x<-length(plot_Variability)/20
        nMarge_x<-ceiling(length(plot_Variability)/6.15) 
        
        
        print(plot_Variability)
        
        par(mar=c(4,1,1,5))
        plot(0:(length(plot_Variability)-1), plot_Variability, pch=16, axes=F, 
             ylim=c(-max(plot_Variability)/8,max(plot_Variability)+max(plot_Variability)/5), 
             xlim = c(-nMarge_x,length(plot_Variability)),
             xlab="", ylab="", type="o",col="black", main="")
        # Légende de l'axe Y de gauchehttp://127.0.0.1:24891/graphics/plot_zoom_png?width=994&height=889
        
        axis(2, ylim=c(0,max(plot_Variability)), yaxp=c(0,max(plot_Variability),7), 
             at = seq(0,max(plot_Variability), length.out = 7),
             labels = format(seq(0,max(plot_Variability), length.out = 7), scientific=TRUE, digits=2),
             col="black", cex.axis = 0.9, pos = 0, font = 2)
        text(0,max(plot_Variability)+max(plot_Variability)/10, "Variability between samples ", font = 2, cex = 1)
        arrows(-ceiling(nMarge_x-nMarge_x/2),min(plot_Variability),0,min(plot_Variability),lwd=2,col="blue", length = 0.1,angle=20)
        text(-ceiling(nMarge_x-nMarge_x/10),min(plot_Variability),format(min(plot_Variability), scientific=TRUE, digits=2), 
             font = 2, cex = 0.9, col = "blue")
        text(-ceiling(nMarge_x-nMarge_x/4.5), min(plot_Variability)-0.005, labels = "Variability minimum", col = "black", cex = 0.65, font = 2)
        segments(0, min(plot_Variability), length(plot_Variability)+50, min(plot_Variability), 
                 col = "green", lty = "dashed", lwd = 2)
        # segments(which(plot_Variability==min(plot_Variability))[1], 0, 
        #          which(plot_Variability==min(plot_Variability))[1],max(plot_Variability), 
        #          col = "green", lty = "dashed", lwd = 2)
        arrows(which(plot_Variability==min(plot_Variability))[1]-1,-max(plot_Variability)/8,
               which(plot_Variability==min(plot_Variability))[1]-1,0,lwd=2,col="blue", length = 0.1,angle=20)
        text(which(plot_Variability==min(plot_Variability))[1]-1, -max(plot_Variability)/7, labels = paste(nbrNormalizers-which(plot_Variability==min(plot_Variability))[1]+1," normalizers"), 
             col = "blue", cex = 0.9, font = 2)
        # text(which(plot_Variability==min(plot_Variability))[1], max(plot_Variability),
        #      labels = paste(nidxSampleNormalized[which(plot_Variability==min(plot_Variability))[1]],"samples normalized"), 
        #      col = "blue", cex = 0.8, font = 2)
        
        axis(1, xlim = c(0,length(plot_Variability)), pos = 0, cex.axis = 0.9,  font = 2)
        text(ceiling(length(plot_Variability)/2),-max(plot_Variability)/8 , "Number leave out", font = 2, cex = 0.9)
        # mtext("Number leave out",side=1,col="black",line=2.5)
        grid(nx = 14)
        
        
      })
      
      
      
      
      RvarsInternalStandard$names_normalizers<-RvarsInternalStandard$OjectNormalizers$ID_normalizres
      
      ID_Normalizers<-RvarsInternalStandard$names_normalizers
      ref_intensity<-log2(RvarsInternalStandard$OjectNormalizers$ref_intensity)
      
      map_ref<-data.frame(RvarsGrouping$MatrixAbundance_selected[1:3],
                          intensity = ref_intensity,
                          Type = "Not a Normalizer"
                          )
      
      map_ref[ID_Normalizers,"Type"]<-"Normalizer"
      
      map_ref$Type<-factor(map_ref$Type, levels = c("Not a Normalizer", "Normalizer"))
      colnames(map_ref)<-c("ID","M+H","CE-time","intensity", "Type")
      
      normalizers_ref<-data.frame(
        ID = ID_Normalizers,
        mzmed = map_ref[ID_Normalizers,]$`M+H`,
        rtmed = map_ref[ID_Normalizers,]$`CE-time`,
        maxo = map_ref[ID_Normalizers,]$intensity
      )
      
      
      # ## plot the distribution of the normalizers
      output$DistibutionNormalizers<-renderPlot({
        ### Distribution of normalizers
        #par(fig = c(0,0.80,0,1),mar=c(4,4,3,1))
        par(fig = c(0,1,0,1),mar=c(4,4,5,0))
        plot(
             # x = map_ref[map_ref$ID %ni% ID_Normalizers,]$`CE-time`,
             # y = map_ref[map_ref$ID %ni% ID_Normalizers,]$`M+H`,
             x = map_ref$`CE-time`,
             y = map_ref$`M+H`,
             axes = F, pch=20, col= "gray", cex=1.2, #0.8
             # xlab = "Ce-time(second)",
             # ylab = "M+H(Da)",
             xlab = "",
             ylab = "",
             main = paste("Distribution of the nomalizers (Mass ~ CE-time ) ","\n",
                          "Number of normalizers :",length(ID_Normalizers)),
             col.main = "#760001",
             
             font.main = 2,
             cex.lab = 1.3, #1.3
             font.lab = 2,
             cex.axis = 1.2,
             font.axis = 2,
             cex.main = 1.2)
        
        axis(1,col="black",col.axis="black",ylim=c(min(map_ref$`CE-time`), max(map_ref$`CE-time`)),
             font = 2, cex.lab = 1.3, cex.axis = 1.2, lwd = 2, line = 0, pos = 0)
        mtext("Ce-time(second)",side=1,line=3,col="black", font = 2, cex=1.2)
        
        axis(2,col="black",col.axis="black",ylim=c(min(map_ref$`M+H`), max(map_ref$`M+H`), pos = 0),
             font = 2, cex.lab = 1.3, cex.axis = 1.2, lwd = 2, line = 0)
        mtext("M+H(Da)",side=2,line=3,col="black", font = 2, cex=1.2)
        
        
        points(x = map_ref[ID_Normalizers,]$`CE-time` ,
               y = map_ref[ID_Normalizers,]$`M+H`, pch=20, col= "blue", cex=1.2, #0.8
               # cex.lab = 1.3,
               # cex.axis = 1.2,
               # font.axis = 2
        )
        
        legend("topleft", pch = 20, pt.cex = 2, inset = 0, text.font = 2.3,
               legend = levels(map_ref$Type),  bty = "n", xpd = NA, cex = 1.2,
               col = c("gray", "blue"), horiz=FALSE)
        grid(nx = 14)
        
      })
      
      
      ###Density for stable features
      if(length(RvarsInternalStandard$names_normalizers)>=1){
        
        stable_refMap<-map_ref[ID_Normalizers,]
        stable_refMap$Type<-"Not a Normalizer"
        
        Plot_Density<-as.data.frame(rbind(map_ref,
                                          stable_refMap))
        
        Plot_Density$Type<-as.character(Plot_Density$Type)
        
        Plot_Density[Plot_Density$Type=="Not a Normalizer",]$Type<-"Reference map"
        Plot_Density[Plot_Density$Type=="Normalizer",]$Type<-"Normalizers"
        
        Plot_Density$Type<-as.factor(Plot_Density$Type)
        
        Plot_Density$Type<-factor(Plot_Density$Type, levels = c("Reference map", "Normalizers"))
        colnames(Plot_Density)<-c("ID","M+H","CE-time","intensity", "Type")
        
        output$DensityIntensity<-renderPlot({
          
          ggplot(data=Plot_Density, aes(x=intensity, group=Type, fill=Type)) +
            geom_density(adjust=1.5, alpha=.4) +
            scale_fill_manual(values = c("gray", "blue"))+
            ggtitle("Intensities distribution")+
            #theme_ipsum()
            
            theme_ben()+
            theme(plot.title = element_text(size = rel(1),
                                            face = "bold",
                                            color = "#760001",
                                            margin = margin(0,0,5,0), hjust = 0.5),
                  legend.title = element_text(size = rel(1), face = "bold.italic", hjust = 0.5),
                  legend.text = element_text(size = rel(0.95), face = "bold"))
          
        })
      } else {
        output$DensityIntensity<-renderPlot({})
        
      }
      
      
      ## Data to save
  
      RvarsInternalStandard$map_ref_ToSave<-data.frame(ID = map_ref$ID,
                                                  M.H = map_ref$`M+H`,
                                                  rt = map_ref$`CE-time`,
                                                  maxo = map_ref$intensity,
                                                  RvarsGrouping$RefereanceMap_selected[,5:10]
      )
      
      
      
      
      #### Matrix before normalization
      matrix_Before<-RvarsInternalStandard$OjectNormalizers$Matrix_Before
      matrix_Before_with_ID<-data.frame(RvarsInternalStandard$map_ref_ToSave$ID,
                                        matrix_Before)
      
      colnames(matrix_Before_with_ID)<-c("ID",
                                         colnames(matrix_Before))
      
      
      RvarsInternalStandard$MatrixAbundance_Before<-matrix_Before_with_ID
      
      
      #### Matrix after normalization
      Matrix_After<-RvarsInternalStandard$OjectNormalizers$Matrix_After
      matrix_After_with_ID<-data.frame(RvarsInternalStandard$map_ref_ToSave$ID,
                                       Matrix_After)
      
      colnames(matrix_After_with_ID)<-c("ID",
                                        colnames(Matrix_After))
      
      RvarsInternalStandard$MatrixAbundance_After<-matrix_After_with_ID
      
      
      RvarsInternalStandard$normalizers_ref<-normalizers_ref
      
      #peaksList_run_ref<-RvarsInternalStandard$raw_peaks_List[RvarsInternalStandard$raw_peaks_List$sample==rValuesNewRefMap$ref_sample_sampleName,]
      
      
      peaksList_run_ref<-RvarsCorrectionTime$ref_sample_samplePeaks
      RvarsInternalStandard$peaksList_run_ref<-peaksList_run_ref
      
      rt_ref<-data.frame(Rtmin = range(RvarsGrouping$RefereanceMap_selected$rt)[1] ,
                         Rtmax = range(RvarsGrouping$RefereanceMap_selected$rt)[2])
      
      RvarsInternalStandard$rt_ref<-rt_ref
      
      
      ### Enable some buttons when prepossessing start
      shinyjs::enable(selector = '.navbar-nav a[data-value="Match reference map"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Analysis new samples"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Peak detection"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Generate the reference map"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Database"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Statistical analysis"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Help"')
      
      enable("id_InternalStandardsParametersPanel")
      enable("IdentifyNormalizers")
      enable("ValidRefrenceMap")
      enable("PreviousPageGenerateRefMap")
      
      # #Go to the next page
      # updateRadioButtons(session = session, 
      #                    inputId = "InternalStandardsStep", 
      #                    selected = "2")
      # 
      # updateNavbarPage(
      #   session = session,
      #   inputId = "analysisNavbar",
      #   selected = "Identification internal standards"
      # )
      # 
      sendSweetAlert(
        session = session,
        title = "Preprocess complete !",
        type = "success",
        closeOnClickOutside = TRUE,
        html = TRUE
      )
      
      
    } 
    
  })


observe({
  if(is.null(RvarsGrouping$RefereanceMap_selected)){
    output$DistibutionVar<-renderPlot({})
    output$DensityIntensity<-renderPlot({})
    output$DistibutionNormalizers<-renderPlot({})
  }
  
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Validate the reference map~~~~~~~~~~~~~~~~~~~~~#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$ValidRefrenceMap
  },
  handlerExpr = {
    
    ask_confirmation(
      inputId = "ValidRefrenceMap_confirmation",
      title = NULL,
      text = tags$b(
        #icon("info"),
        "Do you really want to register this newly created reference map ?",
        style = "color: #FA5858;"
      ),
      btn_labels = c("Cancel", "Confirm"),
      btn_colors = c("#00BFFF", "#FE2E2E"),
      html = TRUE
    )
    
  }) 



observeEvent(
  ignoreNULL = TRUE,
  
  eventExpr = {
    input$ValidRefrenceMap_confirmation
    
  },
  
  handlerExpr = {
    
    if(!is.null(input$ValidRefrenceMap_confirmation)){
      
      if(input$ValidRefrenceMap_confirmation==TRUE){ 
        
        
        if(!is.null(RvarsCorrectionTime$rawData_mzML_path) && !is.null(RvarsInternalStandard$map_ref_ToSave) && 
           !is.null(RvarsInternalStandard$normalizers_ref) && !is.null(RvarsInternalStandard$peaksList_run_ref) &&
           !is.null(RvarsInternalStandard$rt_ref)){
          
          # ### Disable some buttons when prepossessing start
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Match reference map"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Analysis new samples"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Peak detection"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Generate the reference map"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Database"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Statistical analysis"')
          # shinyjs::disable(selector = '.navbar-nav a[data-value="Help"')
          # 
          # disable("id_InternalStandardsParametersPanel")
          # disable("IdentifyNormalizers")
          # disable("ValidRefrenceMap")
          # disable("PreviousPageGenerateRefMap")
          
          withProgress(message = 'Saving the reference map...', value = 0, {
            
            incProgress(1/5, detail = paste(round(1/5*100,0)))
            source_python('lib/NewReferenceMap/Python_files/exportDefaultParam.py')
            ## Create the folder for the reference
            ## creer un dossier contenant le nouveau map de referene (reference map)
            if(dir.exists(file.path("data/References",req(input$Name_NewRefMap)))) {
              unlink(file.path("data/References",req(input$Name_NewRefMap)), recursive = TRUE)
              dir.create(file.path("data/References",req(input$Name_NewRefMap)))
              RvarsInternalStandard$Output_NewRefMaps<-file.path("data/References",req(input$Name_NewRefMap))
              
              
            } else{
              dir.create(file.path("data/References", req(input$Name_NewRefMap)))
              RvarsInternalStandard$Output_NewRefMaps<-file.path("data/References", req(input$Name_NewRefMap))
              
              
            }
            
            incProgress(1/5, detail = paste(round(2/5*100,0)))
            ### Copy files param
            paramMsdial<-readLines(file.path("lib/NewReferenceMap/parameters","paramMsdial.txt")) 
            Parameters_Used.txt<-readLines(file.path("lib/NewReferenceMap/parameters","Parameters_Used.txt"))
            DefaultParam_path<-file.path("lib/NewReferenceMap/parameters","defaultParam.txt")
            
            
            
            exportDefaultParam(
              Input_DefaultParam = DefaultParam_path,
              output_DefaultParam = file.path(RvarsInternalStandard$Output_NewRefMaps,"defaultParam.txt"),
              rt_begin = req(as.character(input$rt_begin_NewRefMap)),                 
              rt_end = req(as.character(input$rt_end_NewRefMap)),                   
              mz_range_begin = req(as.character(input$mz_range_begin_NewRefMap)),            
              mz_range_end = req(as.character(input$mz_range_end_NewRefMap)),              
              mz_tolerance_centroid_MS1 = req(as.character(input$mz_tolerance_centroid_MS1_NewRefMap)), 
              mz_tolerance_centroid_MS2 = req(as.character(input$mz_tolerance_centroid_MS2_NewRefMap)),
              maxCharge = req(as.character(input$maxCharge_NewRefMap)),                 
              min_Peakwidth = req(as.character(input$min_Peakwidth_NewRefMap)),             
              min_PeakHeight = req(as.character(input$min_PeakHeight_NewRefMap)),            
              mass_slice_width = req(as.character(input$mass_slice_width_NewRefMap)),   
              min_PeaksMassif = req(as.character(input$minPeaksMassif_NewRefMap)),
              Adduct_list = req(as.list(input$Adduct_list_NewRefMap)),                     
              bw = req(as.character(input$rt_tolGroupingRefMap)),
              mass_tolerance = req(as.character(input$mzToleranceID))
            )
            
            incProgress(1/5, detail = paste(round(3/5*100,0)))
            
            
            writeLines(paramMsdial, file.path(RvarsInternalStandard$Output_NewRefMaps,"paramMsdial.txt"))
            writeLines(Parameters_Used.txt, file.path(RvarsInternalStandard$Output_NewRefMaps,"Parameters_Used.txt"))
            
            incProgress(1/5, detail = paste(round(4/5*100,0)))
            ## Copy the mzML reference sample
            copy_files(from =RvarsCorrectionTime$rawData_mzML_path[which(RvarsCorrectionTime$pheno_Data_mzML$Filenames==RvarsCorrectionTime$ref_sample_sampleName)],
                       to = file.path(RvarsInternalStandard$Output_NewRefMaps,"run_ref.mzML"))
            
            ## copy the csv files
            
            write.table(RvarsInternalStandard$map_ref_ToSave, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"map_ref.csv"), 
                        sep = ",", row.names =  FALSE)
            
            write.table(RvarsInternalStandard$MatrixAbundance_Before,
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"MatrixAbundance_Before.csv"), 
                        sep = ",", row.names = FALSE)
            
            write.table(RvarsInternalStandard$MatrixAbundance_After,
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"MatrixAbundance_After.csv"), 
                        sep = ",", row.names = FALSE)
            
            write.table(RvarsCorrectionTime$peakListBefore, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"peaklistBefore.csv"),
                        sep = ",", row.names = FALSE)
            
            write.table(RvarsCorrectionTime$peakListAligned_KernelDensity, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"peaklistAligned.csv"), 
                        sep = ",", row.names = FALSE)
            
            write.table(RvarsInternalStandard$normalizers_ref, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"normalizers_ref.csv"), 
                        sep = ",", row.names =  FALSE)
            
            write.table(RvarsInternalStandard$peaksList_run_ref, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"peaksList_run_ref.csv"), 
                        sep = ",", row.names =  FALSE)
            
            write.table(RvarsInternalStandard$rt_ref, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"rt_ref.csv"), 
                        sep = ",", row.names =  FALSE)
            
            write.table(RvarsInternalStandard$OjectNormalizers$varCalcul, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"VarCacul.csv"), 
                        sep = ",", row.names =  FALSE)
            
            
            Variability<-data.frame(w.metric.before = RvarsInternalStandard$OjectNormalizers$w.metric.before,
                                    w.metric.after = RvarsInternalStandard$OjectNormalizers$w.metric.after)
            
            write.table(Variability, 
                        file = file.path(RvarsInternalStandard$Output_NewRefMaps,"Variability.csv"), 
                        sep = ",", row.names =  FALSE)
            
            ## Export data into database
            
            listTables<-list(map_ref = RvarsInternalStandard$map_ref_ToSave,
                             normalizers_ref = RvarsInternalStandard$normalizers_ref)
            
            path_to_database<-file.path("data/ReferencesDatabases",paste0(req(input$Name_NewRefMap),".sqlite"))
            
            source(file = "lib/NewReferenceMap/R_files/DatabaseManageNewReferenceMap.lib.R", local = TRUE)
            
            CreateDatabaseReferenceMap(path_to_database = path_to_database, 
                                       listTables = listTables)
            
            
            ### Enable some buttons when prepossessing start
            shinyjs::enable(selector = '.navbar-nav a[data-value="Match reference map"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Analysis new samples"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Peak detection"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Generate the reference map"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Database"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Statistical analysis"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Help"')
            
            enable("id_InternalStandardsParametersPanel")
            enable("IdentifyNormalizers")
            enable("ValidRefrenceMap")
            enable("PreviousPageGenerateRefMap")
            
            incProgress(1/5, detail = paste(round(5/5*100,0)))
            
            
            updateRadioButtons(session = session, 
                               inputId = "IdAnalysisStep", 
                               selected = "1")
            
            updateNavbarPage(
              inputId = "analysisNavbar",
              session = session,
              selected = "Peak detection and grouping"
            )
            
            sendSweetAlert(
              session = session,
              title = "Reference map successfully created !",
              text = HTML("Thank for using this application ! <br/>
                     Now you can do analysis new sample by using this reference map.
                     "),
              type = "success",
              width = "70%",
              closeOnClickOutside = TRUE,
              html = TRUE
            )
          })
          
          
        }
        
        
      }
      
    }  
    
    
  })


observe({
  
  if(length(input$InternalStandardsParameters)!=0) {
    if(input$InternalStandardsParameters=="defaults") {
      
      updateNumericInput(session = session,
                         inputId = "pFeatures",
                         value = 10)
      updateNumericInput(session = session,
                         inputId = "pSample",
                         value = 50)
      updateNumericInput(session = session,
                         inputId = "minNormalizers",
                         value = 100)
    }
    
  }
})
