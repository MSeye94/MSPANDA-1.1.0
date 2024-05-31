databaseTabPanel <- function() {
  fluidRow(databaseTabPanelContainer(), )
}



databaseTabPanelContainer <- function() {
  fluidRow(column(
    12,
    
    HTML('<h4>References map manager </h4>'),
    
    hr(),
    fluidRow(
      column(
        5,
        
        div(
          id = "IdPanelDatabase",
          class = "well well-sm",
          h4("Available reference map:"),
          hr(),
          pickerInput(
            inputId = "ChoosereferncesMapDatabase",
            label = "Select a reference map:",
            choices = ref_map_databases,
            options = pickerOptions(
              size = 10,
              showTick = TRUE,
              style = "btn-primary"
            ),
            width = "auto"
          ),
          
          fluidRow(br()),
          fluidRow(div(column(
            6,
            div(class = "pull-left",
                style = "display:inline-block", )
          ),
          column(
            6,
            div(
              class = "pull-right",
              style = "display:inline-block",
              actionButton(
                inputId = "IdDeleteReferenceMap",
                label =
                  "Delete",
                class =
                  "btn-primary",
                icon = icon("trash")
              )
            )
          )))
        ),
        
        div(
          class = "well well-sm",
          h4("Search Feature"),
          hr(),
          h5("Search feature by: "),
          hr(),
          
          ##~~~~~~~ M+H range ~~~~~~~~~#
          h6("M+H range: "),
          fluidRow(
            column(
              4,
              align = "center",
              numericInput(
                inputId = "MFrom",
                label = "From",
                value = NULL,
                min = 0
              )
            ),
            column(
              4,
              align = "center",
              numericInput(
                inputId = "MTo",
                label = "To",
                value = NULL,
                min = 0
              )
            )
          ),
          
          ##~~~~~~~ CE-time range ~~~~~~~~~#
          h6("CE-time range: "),
          fluidRow(
            column(
              4,
              align = "center",
              numericInput(
                inputId = "CEFrom",
                label = "From",
                value = NULL,
                min = 0
              )
            ),
            column(
              4,
              align = "center",
              numericInput(
                inputId = "CETo",
                label = "To",
                value = NULL,
                min = 0
              )
            )
          ),
          
          
        )
        
      ),
      column(
        7,
        
        tabsetPanel(
          id = "IdTabsetPanelDatabase",
          
          tabPanel(fluidRow(br()),
                   title = paste("Reference map"),
                   
                   fluidRow(column(
                     6,
                     div(
                       class = "pull-left",
                       style = "display:inline-block",
                       shinySaveButton(
                         id =  "IDSaveReferenceMapDatabase",
                         label = "Download the reference map",
                         title = "Save file as...",
                         filename = "Reference-Map",
                         filetype = list(text = "csv"),
                         viewtype = "icon",
                         icon = icon("save", lib = "glyphicon"),
                         class =
                           "btn-primary"
                       )                     
                     )
                   ),
                   column(
                     6,
                   )),
                   
                   hr(),
                   
                   div(
                     class = "well well-sm",
                     shinycssloaders::withSpinner(
                       DTOutput("ReferenceMapShow"),
                       type = 1,
                       size = 0.8,
                       id = "IDReferenceMapShow"
                     )
                   )
                   
                   ),
          
          tabPanel(fluidRow(br()),
                   title = "Set of normalizers",
                   
                   fluidRow(column(
                     6,
                     div(
                       class = "pull-left",
                       style = "display:inline-block",
                       shinySaveButton(
                         id =  "IDSaveNormalizersRefDatabase",
                         label = "Download the set of normalizers",
                         title = "Save file as...",
                         filename = "Normalizers",
                         filetype = list(text = "csv"),
                         viewtype = "icon",
                         icon = icon("save", lib = "glyphicon"),
                         class =
                           "btn-primary"
                       )                     
                     )
                   ),
                   column(
                     6,
                   )),
                   
                   hr(),
                   
                   
                   div(
                     class = "well well-sm",
                     shinycssloaders::withSpinner(
                       DTOutput("normalizersRefShow"),
                       type = 1,
                       size = 0.8,
                       id = "IDnormalizersRefShow"
                     )
                   )
                   
                   ),
          
          tabPanel(fluidRow(br()),
                   title = "Plot M+H~CE-time",
                   
                   div(
                     class = "well well-sm",
                     shinycssloaders::withSpinner(
                       plotOutput(
                         "PlotDatabaseReferenceMap",
                         width = "100%",
                         height = "400px",
                         dblclick = "DatabaseReferenceMap_dblclick",
                         click = "DatabaseReferenceMap_click",
                         brush = brushOpts(id = "DatabaseReferenceMap_brush",
                                           resetOnNew = TRUE),
                         hover = hoverOpts(
                           "DatabaseReferenceMap_hover",
                           delay = 100,
                           delayType = "debounce"
                         )
                         
                       ),
                       type = 1,
                       size = 0.8
                     ),
                     uiOutput("DatabaseReferenceMap_hover_info")
                     
                     
                     )
                   )
          )
        
        
        
      )
    )
  ))
}