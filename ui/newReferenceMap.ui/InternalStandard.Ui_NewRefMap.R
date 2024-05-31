InternalStandardsTabPanelNewRefrenceMap<-function(){
  
  fluidRow(
    
    #### Analysis step
    hidden(
      div(
        radioButtons(
          inputId="InternalStandardsStep",
          label="",
          inline=TRUE,
          choices=c("1","2"),
          selected = "1")
      )
    ),
    
    conditionalPanel(
      condition = "input.InternalStandardsStep=='1'",
      IndentificationInternalStandards()
    ),
    
    conditionalPanel(
      condition = "input.InternalStandardsStep=='2'",
      IndentificationInternalStandards_Plot()
    )
    
  )
  
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
IndentificationInternalStandards<-function(){
  
  fluidRow(column(12,
                  
                  HTML('<h4>Step 4 : Identification internal standards </h4>'),
                  hr(),
                  
                  fluidRow(
                    column(5,
                           
                           #hr(),
                           div(
                             id = "id_InternalStandardsParametersPanel",
                             class = "well well-sm",
                             h4("Internal standards parameters"),
                             fluidRow(column(12,
                                             
                                             awesomeRadio(
                                               inputId="InternalStandardsParameters",
                                               label = '',
                                               inline=TRUE,
                                               checkbox = TRUE,
                                               choices=list(
                                                 "Use defaults"="defaults",
                                                 "Customize"="custom"
                                               )
                                             )
                             )),
                             
                             conditionalPanel(
                               condition="input.InternalStandardsParameters=='custom'",
                               fluidRow(column(12,
                                               fluidRow(column(6,
                                                               numericInput(
                                                                 inputId="pSample",
                                                                 label="Min sample (%)",
                                                                 value=50,
                                                                 min = 0,
                                                                 max = 100,
                                                                 step = 1
                                                               ),
                                                               
                                                               numericInput(
                                                                 inputId="minNormalizers",
                                                                 label="Min normalizers",
                                                                 value=100,
                                                                 min = 10,
                                                                 step = 1
                                                               )
                                                               
                                                               
                                                               
                                               ),
                                               column(6,
                                                      numericInput(
                                                        inputId="pFeatures",
                                                        label="Min features (%)",
                                                        value=10,
                                                        min = 0,
                                                        max = 100,
                                                        step = 1
                                                      )
                                                      
                                                      
                                               )
                                               )
                                               
                               ))
                               
                               
                             )
                           ),
                           hr(),
                           
                           fluidRow(
                             column(6,
                                    div(
                                      class="pull-left",
                                      style="display:inline-block",
                                      #disabled(
                                      actionButton(
                                        inputId="IdentifyNormalizers",
                                        label="Start!",
                                        icon=icon("rocket"),
                                        class="btn-primary"
                                      )
                                      #)
                                      
                                    )
                             )
                             
                           )
                           
                           
                           
                    ),
                    
                    column(7,
                           plotOutput("DistibutionVar",
                                      height = "550px")   
                    )),
                  
                  fluidRow(
                    column(5,
                           hr(),
                           div(
                             fluidRow(
                               column(6,
                                      div(
                                        class="pull-left",
                                        style="display:inline-block",
                                        #disabled(
                                        actionButton(
                                          inputId="PreviousPageGenerateRefMap",
                                          label="Return",
                                          icon=icon("arrow-left"),
                                          class="btn-primary"
                                        )
                                        #)
                                        
                                      )),
                               column(6,
                                      div(
                                        class="pull-right",
                                        style="display:inline-block",
                                        #disabled(
                                        actionButton(
                                          inputId="nextPageIS",
                                          label="Next",
                                          icon=icon("arrow-right"),
                                          class="btn-primary"
                                        )
                                        #)
                                        
                                      ))
                             )
                           )
                    )
                  )
  )
  
  
  )
  
  
}

### nomalization viewer
IndentificationInternalStandards_Plot<-function(){
  
  fluidRow(column(12,
                  
                  HTML('<h4>Step 4 : Identification internal standards </h4>'),
                  hr(),
                  
                  fluidRow(column(7,
                                  
                                  shinycssloaders::withSpinner(plotOutput("DistibutionNormalizers",
                                                                          height = "550px"), type = 1, size = 0.8),
                  ),
                  column(5,
                         
                         
                         shinycssloaders::withSpinner(plotOutput("DensityIntensity",
                                                                 height = "400px"), type = 1, size = 0.8)
                         
                  )),
                  
                  
                  
                  
                  fluidRow(
                    column(12,
                           hr(),
                           div(
                             fluidRow(
                               column(6,
                                      div(
                                        class="pull-left",
                                        style="display:inline-block",
                                        #disabled(
                                        actionButton(
                                          inputId="PreviousPage1",
                                          label="Return",
                                          icon=icon("arrow-left"),
                                          class="btn-primary"
                                        )
                                        #)
                                        
                                      )),
                               column(6,
                                      div(
                                        class="pull-right",
                                        style="display:inline-block",
                                        disabled(
                                          actionButton(
                                            inputId="ValidRefrenceMap",
                                            label="Validate the reference map",
                                            icon=icon("save", lib = "glyphicon"),
                                            class="btn-primary"
                                          )
                                        )
                                        
                                      )
                               )
                               
                             )
                           )
                    )
                  )
                  
                  
                  
  )
  
  
  )
  
  
}
