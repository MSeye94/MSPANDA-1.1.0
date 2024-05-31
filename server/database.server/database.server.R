#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~ getting database reference map ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
observe({
  print(input$ChoosereferncesMapDatabase)
  
  if(file.exists(file.path(
    ref_map_databases_directory,
    paste0(input$ChoosereferncesMapDatabase, ".sqlite")
  ))){
    path_to_database <-
      file.path(
        ref_map_databases_directory,
        paste0(input$ChoosereferncesMapDatabase, ".sqlite")
      )
    print(path_to_database)
    if (length(input$ChoosereferncesMapDatabase) != 0 & 
        file.exists(file.path(
          ref_map_databases_directory,
          paste0(input$ChoosereferncesMapDatabase, ".sqlite"))
          )
        ) {
      path_to_database <-
        file.path(
          ref_map_databases_directory,
          paste0(input$ChoosereferncesMapDatabase, ".sqlite")
        )
      con <- dbConnect(RSQLite::SQLite(), dbname = path_to_database)
      
      
      map_ref_toShow <-
        dbGetQuery(con, "select ID, `M.H`, rt, maxo from map_ref")
      
      normalizers_ref_toShow <-
        dbGetQuery(con, "select ID, mzmed, rtmed, maxo from normalizers_ref")
      
      dbDisconnect(con)
      
      colnames(map_ref_toShow) <-
        c("ID", "M+H", "CE-time", "Intensity")
      
      colnames(normalizers_ref_toShow) <-
        c("ID", "M+H", "CE-time", "Intensity")
      
      map_ref_toShow$`M+H` <- round(map_ref_toShow$`M+H`, 4)
      map_ref_toShow$`CE-time` <- round(map_ref_toShow$`CE-time`, 0)
      map_ref_toShow$Intensity <- round(map_ref_toShow$Intensity, 3)
      
      normalizers_ref_toShow$`M+H` <- round(normalizers_ref_toShow$`M+H`, 4)
      normalizers_ref_toShow$`CE-time` <- round(normalizers_ref_toShow$`CE-time`, 0)
      normalizers_ref_toShow$Intensity <- round(normalizers_ref_toShow$Intensity, 3)
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~ Filter data ~~~~~~~~~~~~~~~~~~~~~~##
      if (is.na(input$MFrom) &
          is.na(input$MTo) &
          is.na(input$CEFrom) &
          is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow
        
        normalizers_ref_toShow <- normalizers_ref_toShow
        
      } else if (!is.na(input$MFrom) &
                 is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom)
        
      } else if (is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` <= input$MTo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` <= input$MTo)
        
      } else if (!is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <= input$MTo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <= input$MTo)
        
      } else if (is.na(input$MFrom) &
                 is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`CE-time` >= input$CEFrom)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`CE-time` >= input$CEFrom)
        
      } else if (is.na(input$MFrom) &
                 is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`CE-time` <= input$CETo)
        
      } else if (is.na(input$MFrom) &
                 is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
      } else if (!is.na(input$MFrom) &
                 is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` >= input$CEFrom)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` >= input$CEFrom)
        
      } else if (!is.na(input$MFrom) &
                 is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` <= input$CETo)
        
      } else if (is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom)
        
      } else if (is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` <= input$CETo)
        
      } else if (!is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom)
        
      } else if (!is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <=  input$MTo,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `M+H` <=  input$MTo,
                        `CE-time` <= input$CETo)
        
      } else if (!is.na(input$MFrom) &
                 is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` >= input$MFrom,
                        `CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
      } else if (is.na(input$MFrom) &
                 !is.na(input$MTo) &
                 !is.na(input$CEFrom) &
                 !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(`M+H` <=  input$MTo,
                        `CE-time` >= input$CEFrom,
                        `CE-time` <= input$CETo)
        
      }  else if (!is.na(input$MFrom) &
                  !is.na(input$MTo) &
                  !is.na(input$CEFrom) &
                  !is.na(input$CETo)) {
        map_ref_toShow <- map_ref_toShow %>%
          dplyr::filter(
            `M+H` >= input$MFrom,
            `M+H` <=  input$MTo,
            `CE-time` >= input$CEFrom,
            `CE-time` <= input$CETo
          )
        
        normalizers_ref_toShow <- normalizers_ref_toShow %>%
          dplyr::filter(
            `M+H` >= input$MFrom,
            `M+H` <=  input$MTo,
            `CE-time` >= input$CEFrom,
            `CE-time` <= input$CETo
          )
        
      }
      
      RvarsDatabaseReferenceMap$map_ref_toShow<-map_ref_toShow
      
      RvarsDatabaseReferenceMap$normalizers_ref_toShow<-normalizers_ref_toShow
      
    } else {
      sendSweetAlert(
        session = session,
        title = "Warning !",
        text = HTML(
          "No reference map available in the application. <br>",
          "Please create a new reference map! <b> "
        ),
        type = "warning",
        html = TRUE
      )
    }
  } else {
    sendSweetAlert(
      session = session,
      title = "Warning !",
      text = HTML(
        "No reference map available in the application. <br>",
        "Please create a new reference map! <b> "
      ),
      type = "warning",
      html = TRUE
    )
  }
  
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~ Data table refernce map  ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
output$ReferenceMapShow <- renderDT({
  

  if(!is.null(RvarsDatabaseReferenceMap$map_ref_toShow)){
    datatable(RvarsDatabaseReferenceMap$map_ref_toShow,
              rownames = FALSE,
              options = list(scrollX = TRUE))
  }
  
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~ Data table Normalizers  ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
output$normalizersRefShow <- renderDT({
  
  
  if(!is.null(RvarsDatabaseReferenceMap$normalizers_ref_toShow)){
    datatable(RvarsDatabaseReferenceMap$normalizers_ref_toShow,
              rownames = FALSE,
              options = list(scrollX = TRUE))
  }
  
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~ Plotting the reference map ~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rangesZoomDatabaseReferenceMap <-
  reactiveValues(x = NULL, y = NULL)

# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent({
  input$DatabaseReferenceMap_dblclick
}, {
  brush <- input$DatabaseReferenceMap_brush
  if (!is.null(brush)) {
    rangesZoomDatabaseReferenceMap$x <-
      c(brush$xmin, brush$xmax)
    rangesZoomDatabaseReferenceMap$y <-
      c(brush$ymin, brush$ymax)

  } else {
    ## reset coord_cartesian
    rangesZoomDatabaseReferenceMap$x <- NULL
    rangesZoomDatabaseReferenceMap$y <- NULL
  }
})

output$PlotDatabaseReferenceMap <- renderPlot({
  if(!is.null(RvarsDatabaseReferenceMap$map_ref_toShow)){
    RvarsDatabaseReferenceMap$map_ref_toShow %>%
      ggplot() +
      aes(x = `CE-time`,
          y = `M+H`,
          colour = Intensity) +
      geom_point(size = 1) +
      
      scale_color_viridis_c(option = "inferno", direction = -1) +
      ylab("Mass (M+H) (Da)") +
      xlab("CE-time (Seconde)") +
      coord_cartesian(xlim = rangesZoomDatabaseReferenceMap$x,
                      ylim = rangesZoomDatabaseReferenceMap$y,
                      expand = TRUE) +
      ggtitle(paste(
        "Reference map name :",
        req(input$ChoosereferncesMapDatabase),
        "\n"
      )) +
      
      scale_x_continuous(n.breaks = 14) +
      scale_y_continuous(n.breaks = 14) +
      
      theme_ben() +
      labs(colour = "log2 Intensity") +
      theme(
        plot.title = element_text(
          size = rel(0.9),
          face = "bold",
          color = "#760001",
          margin = margin(0, 0, 5, 0),
          hjust = 0.5
        ),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }

})


# #~~~~~~~~~~~~~~~~~~ info-bulle ~~~~~~~~~~~~~~~~~~~~~#
# output$DatabaseReferenceMap_hover_info <-
#   renderUI({
#     hover <- req(input$DatabaseReferenceMap_hover)
# 
#     if (!is.null(
#       RvarsDatabaseReferenceMap$map_ref_toShow
#     )) {
#       point <-
#         nearPoints(
#           req(
#             RvarsDatabaseReferenceMap$map_ref_toShow
#           ),
#           hover,
#           threshold = 5,
#           maxpoints = 1,
#           addDist = TRUE
#         )
#       if (nrow(point) == 0)
#         return(NULL)
# 
#       # calculate point position INSIDE the image as percent of total dimensions
#       # from left (horizontal) and from top (vertical)
#       left_pct <-
#         (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
#       top_pct <-
#         (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
# 
#       # calculate distance from left and bottom side of the picture in pixels
#       left_px <-
#         hover$range$left + left_pct * (hover$range$right - hover$range$left)
#       top_px <-
#         hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
# 
#       # create style property fot tooltip
#       # background color is set so tooltip is a bit transparent
#       # z-index is set so we are sure are tooltip will be on top
#       style <-
#         paste0(
#           "position:absolute; z-index:100; background-color: #760001; color: white;",
#           "left:",
#           left_px + 2,
#           "px; top:",
#           top_px + 2,
#           "px;"
#         )
# 
#       # actual tooltip created as wellPanel
#       div(class = "well well-sm",
#           style = style,
#           p(HTML(
#             paste0(
#               "<span class='bullText'> M+H: </span>",
#               point$`M+H`,
#               "<br/>",
#               "<span class='bullText'> CE-time: </span>",
#               point$`CE-time`,
#               "<br/>",
#               "<span class='bullText'> log2 Intensity: </span>",
#               point$Intensity,
#               "<br/>",
#               "<span class='bullText'> Intensity: </span>",
#               2^(point$Intensity),
#               "<br/>"
#             )
#           )))
#     }
#   })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~ Manage some ction button ~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Button IdDeleteReferenceMap ~~~~~~~~~~~~~~~~~~~~~~~~#*
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$IdDeleteReferenceMap
             },
             handlerExpr = {
               ask_confirmation(
                 inputId = "Comfirm_IdDeleteReferenceMap",
                 title = NULL,
                 text = tags$b(
                   #icon("info"),
                   paste0(
                     "Are you sure you want to delete this reference map ? \n ",
                     "Note that, this action is irreversible and you will lose all data for this reference map."
                   ),
                   style = "color: #FA5858;"
                 ),
                 btn_labels = c("Cancel", "OK"),
                 btn_colors = c("#00BFFF", "#FE2E2E"),
                 html = TRUE
               )
               
               
             })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Button Comfirm_IdDeleteReferenceMap ~~~~~~~~~~~~~~~~~~~~~~~~#
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$Comfirm_IdDeleteReferenceMap
             },
             handlerExpr = {
               if (input$Comfirm_IdDeleteReferenceMap == TRUE) {
                 
                 ##-----------Delete database file-------------#
                 path_to_database <-
                   file.path(
                     ref_map_databases_directory,
                     paste0(input$ChoosereferncesMapDatabase, ".sqlite")
                   )
                 
                 if(file.exists(path_to_database)){
                   file.remove(path_to_database)
                 }
                 
                 
                 ##-----------Delete the reference map folder-------------#
                 path_to_reference_map <-
                   file.path(
                     ref_map_directory,
                     paste0(input$ChoosereferncesMapDatabase)
                   )
                 
                 if(dir.exists(path_to_reference_map)){
                   unlink(path_to_reference_map, recursive = TRUE)
                 }
                 
                 sendSweetAlert(
                   session = session,
                   title = "Preprocess complete !",
                   text = HTML(paste(
                     "The reference map: ",input$ChoosereferncesMapDatabase, " has been removed !<br>"
                   )),
                   type = "success",
                   closeOnClickOutside = FALSE,
                   html = TRUE
                 )
                 
               }
             })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~ Saving the reference map database ~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$IDSaveReferenceMapDatabase
             }
             ,
             handlerExpr = {
               shinyFileSave(input,
                             id = "IDSaveReferenceMapDatabase",
                             roots = volumes,
                             session = session)
               
               
               
               path_Save_Files_origine <-
                 parseSavePath(volumes, input$IDSaveReferenceMapDatabase)$datapath
               
               if (length(path_Save_Files_origine) > 0) {
                 path_Save_Files <- strsplit(path_Save_Files_origine, split = "/")[[1]]
                 directory_Save_Files <-
                   paste0(path_Save_Files[-length(path_Save_Files)], collapse = "/")
                 
                 if (!is.null(RvarsDatabaseReferenceMap$map_ref_toShow)) {
                   map_ref_toSave <- RvarsDatabaseReferenceMap$map_ref_toShow
                   

                   
                   # Format csv
                   write.table(map_ref_toSave,
                               file = paste0(c(directory_Save_Files,
                                               paste(
                                                 path_Save_Files[length(path_Save_Files)],
                                                 collapse = "", sep = "-")), collapse = "/"),
                               sep = ",", row.names = FALSE)
                   
                   print("Save ok")
                   
                 }
                 
               }
               
               
             })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~ Saving the set of normalizers database ~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$IDSaveNormalizersRefDatabase
             }
             ,
             handlerExpr = {
               shinyFileSave(input,
                             id = "IDSaveNormalizersRefDatabase",
                             roots = volumes,
                             session = session)
               
               
               
               path_Save_Files_origine <-
                 parseSavePath(volumes, input$IDSaveNormalizersRefDatabase)$datapath
               
               if (length(path_Save_Files_origine) > 0) {
                 path_Save_Files <- strsplit(path_Save_Files_origine, split = "/")[[1]]
                 directory_Save_Files <-
                   paste0(path_Save_Files[-length(path_Save_Files)], collapse = "/")
                 
                 if (!is.null(RvarsDatabaseReferenceMap$normalizers_ref_toShow)) {
                   normalizers_ref_toSave <- RvarsDatabaseReferenceMap$normalizers_ref_toShow
                   

                   
                   # Format csv
                   write.table(normalizers_ref_toSave,
                               file = paste0(c(directory_Save_Files,
                                               paste(
                                                 path_Save_Files[length(path_Save_Files)],
                                                 collapse = "", sep = "-")), collapse = "/"),
                               sep = ",", row.names = FALSE)
                   
                   print("Save ok")
                   
                 }
                 
               }
               
               
             })