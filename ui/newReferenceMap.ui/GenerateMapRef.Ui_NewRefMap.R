GenerateRefMapTabPanelNewRefrenceMap <- function() {
  fluidRow(
    #### Analysis step
    hidden(div(
      radioButtons(
        inputId = "GenerateRefMapStep",
        label = "",
        inline = TRUE,
        choices = c("1", "2", "3", "4"),
        selected = "1"
      )
    )),
    
    conditionalPanel(condition = "input.GenerateRefMapStep=='1'",
                     GroupingMassif_NewRefMap_Ui(),),
    
    conditionalPanel(condition = "input.GenerateRefMapStep=='2'",
                     ViewMapNewRefrenceMap_Ui(),),
    
    conditionalPanel(condition = "input.GenerateRefMapStep=='3'",),
    
    conditionalPanel(condition = "input.GenerateRefMapStep=='4'",),
    
    
  )
  
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ functions ~~~~~~~~~~~~~~~~~~~~~####
###~~~~~~~~~~~~~~~~~~~~~~~~ Grouping massif between samples Ui ~~~~~~~~~~~~~~~~~~~~~~~~~~~###

GroupingMassif_NewRefMap_Ui <- function() {
  fluidRow(column(
    12,
    
    HTML('<h4>Step 6 : Generate the refereance map </h4>'),
    hr(),
    fluidRow(column(
      6,
      
      div(
        class = "well well-sm",
        
        h4("Parameters (grouping massif within and between samples):"),
        
        fluidRow(column(
          12,
          
          awesomeRadio(
            inputId = "groupingMassifChoice_NewRefMap",
            label = '',
            inline = TRUE,
            checkbox = TRUE,
            choices = list("Use defaults" =
                             "defaults",
                           "Customize" = "custom")
          )
        )),
        
        conditionalPanel(condition = "input.groupingMassifChoice_NewRefMap=='custom'",
                         fluidRow(column(
                           12,
                           fluidRow(column(
                             6,
                             numericInput(
                               inputId = "rt_tolGroupingRefMap",
                               label =
                                 "CE-time tolerence (Second)",
                               value =
                                 3 * 60,
                               min = 30,
                               max = 5 *
                                 60,
                               step = 5
                             )
                           ),
                           column(
                             6,
                             
                             numericInput(
                               inputId = "mzToleranceID",
                               label = "mass tolerance (Da)",
                               value = 0.150,
                               min = 0.1,
                               max = 0.5,
                               step = 0.001
                             )
                           ))
                         ))),
        
        fluidRow(br()),
        fluidRow(column(
          6,
          div(
            class = "pull-left",
            style = "display:inline-block",
            actionButton(
              inputId = "GroupingButtonID",
              label = "Start grouping",
              icon = icon("rocket"),
              class = "btn-primary"
            )
          )
        )),
        hr(),
        h4("Parameters reference map:"),
        fluidRow(column(
          12,
          awesomeRadio(
            inputId = "generateParameters_NewRefMap",
            label = '',
            inline = TRUE,
            checkbox = TRUE,
            choices = list("Use defaults" =
                             "defaults",
                           "Customize" = "custom")
          )
        )),
        
        conditionalPanel(condition = "input.generateParameters_NewRefMap=='custom'",
                         fluidRow(column(
                           12,
                           fluidRow(column(
                             6,
                             numericInput(
                               inputId = "minFraction_RefMap",
                               label =
                                 "Min fraction (%)",
                               value =
                                 10,
                               min = 0,
                               max =
                                 100,
                               step = 1
                             )
                           ),
                           
                           column(
                             6,
                             textInput(
                               inputId = "prefixID_NewRefMap",
                               label =
                                 "Feature ID prefix",
                               value =
                                 "FT",
                               placeholder =
                                 "FT"
                             )
                           )),
                           
                         ))),
        
        fluidRow(br()),
        
        fluidRow(column(
          6,
          div(
            class = "pull-left",
            style = "display:inline-block",
            disabled(
              actionButton(
                inputId = "GenerateMapRefButtonID",
                label = "Generate reference map",
                icon = icon("rocket"),
                class = "btn-primary"
              )
            )
          )
        ))
      ),
      
      fluidRow(column(
        6,
        div(
          class = "pull-left",
          style = "display:inline-block",
          actionButton(
            inputId = "ReurnToCorrectionTime",
            label = "Return",
            class = "btn-primary",
            icon = icon("arrow-left")
          )
          
        )
      ),
      column(
        6,
        div(
          class = "pull-right",
          style = "display:inline-block",
          disabled(
            actionButton(
              inputId = "GroupingMassifNexPage",
              label = "Next",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
          
          
          
        )
      ))
    ),
    
    
    
    column(6, ))
  ))
}



ViewMapNewRefrenceMap_Ui <- function() {
  fluidRow(column(
    12,
    HTML('<h4>Visualization of the reference map </h4>'),
    
    tabsetPanel(
      tabPanel("Reference Map",
               RefMapViewer()),
      tabPanel(
        "Data table",
        fluidRow(br()),
        
        div(
          class = "small",
          
          shinycssloaders::withSpinner(
            DTOutput("dataTableRefernceMapViewer",
                     width = "100%"),
            type = 1,
            size = 0.8,
            id = "dataTableReferenceMpaViwerID"
          ),
          style = "overflow: auto;"
        )
        
        
      )
    )
    
    
    
  ))
  
}


RefMapViewer <- function() {
  fluidRow(column(
    12,
    
    div(
      style = "position:relative",
      shinycssloaders::withSpinner(
        plotOutput(
          "ReferenceMapViewer",
          # width = "100%",
          height = "450px",
          dblclick = "RefMapPlot_dblclick",
          click = "RefMapPlot_click",
          brush = brushOpts(id = "RefMapPlot_brush",
                            resetOnNew = TRUE),
          hover = hoverOpts("RefMap_hover", delay = 100, delayType = "debounce")
          
        ),
        type = 1,
        size = 0.8
      ),
      ## bule info
      uiOutput("RefMapPlot_hover_info"),
      
      
    ),
    
    fluidRow(
      column(
        6,
        
        fluidRow(br()),
        fluidRow(column(12,
                        p(
                          paste("CE-time filter")
                        ))),
        fluidRow(column(4,
                        div(
                          style = paste(
                            "font-weight:bold;",
                            "border-style: none none dashed none;",
                            "border-width: 2px;"
                          ),
                          "CE-time min"
                        )), column(4,
                                   div(
                                     style = paste(
                                       "font-weight:bold;",
                                       "border-style: none none dashed none;",
                                       "border-width: 2px;"
                                     ),
                                     "CE-time max"
                                   ))),
        fluidRow(column(12,
                        div(
                          class = "small",
                          
                          fluidRow(
                            column(
                              4,
                              align = "center",
                              numericInput(
                                inputId = "rt_min_RefMap",
                                label =
                                  "",
                                value =
                                  NULL,
                                min = 0
                              )
                            ),
                            column(
                              4,
                              align = "center",
                              numericInput(
                                inputId = "rt_max_RefMap",
                                label =
                                  "",
                                value =
                                  NULL,
                                min = 0
                              )
                            ),
                            column(4, div(
                              style = "position:relative;top:20px",
                              actionButton(
                                inputId = "FilterRefmap",
                                label =
                                  "Cut-off",
                                class =
                                  "btn-primary",
                                icon = icon("scissors")
                              )
                            ))
                          )
                          
                          
                        ))),
        
        
        fluidRow(br()),
        ## Buttons next and previous
        fluidRow(column(
          6,
          div(
            class = "pull-left",
            style = "display:inline-block",
            actionButton(
              inputId = "ReturnToCreateMap",
              label = "Return",
              class = "btn-primary",
              icon = icon("arrow-left")
            )
            
          )
        ),
        column(
          6,
          div(
            class = "pull-right",
            style = "display:inline-block",
            actionButton(
              inputId = "ViewRefMapNextPage",
              label = "Next",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
            
            
          )
          
        ))
      ),
      column(
        6,
        hidden(
          div(
            id = "RefMapPlotInfo_id",
            class = "pull-right",
            style = "display:inline-block;font-weight:bold;color: #760001;",
            #disabled(
            verbatimTextOutput("RefMapPlot_info")
            
            #)
          )
          
        ),
        
        ### Sauvegarde de la carte de reference
        
        fluidRow(br()),
        div(
          class = "pull-right",
          style = "display:inline-block",
          
          shinySaveButton(
            id = "SaveMatrixBeforeNormalize",
            label = "Save matrix abundance before normalization",
            title = "Save file as...",
            filename = "Matrix-abundance-Before",
            
            
            filetype = list(text = "xlsx"),
            viewtype = "icon",
            icon = icon("save", lib = "glyphicon"),
            class = "btn-primary"
          )
          
        )
        
        
      )
    )
    
  ))
  
  
  
}

