# Author: Jishnu S
# Email : jishnu.s@k2analytics.co.in


# please load atleast one df in environment
#install.packages("qpcR")

#' BinaryTargetViz
#' Visualization code in shiny app.
#' It takes the dataframes from current memory and visualize the graph according to the selected columns
#'@author Jishnu S
#'@return A graph
#'@example example.R
#'@export

BinaryTargetViz <- function(var) {
  library(shiny)
  m = var
  library(plotly)
  if (interactive()) {
    ui <- navbarPage(theme = shinytheme("cosmo")
                     ,header =
                       absolutePanel(
                         wellPanel(style = "background-color: lightblue;",
                                   textOutput("text"),
                                   #textOutput("text233"),
                                   tags$head(
                                     tags$style(
                                       "#text{color: white;\n                font-size: 12px;\n         }"
                                     )
                                   ))
                         ,top = "8%",left =  "75%",width="250px",height ="10%",draggable=TRUE)
                     ,
                     "Visualization Dashboard", id="VD",
                     tabPanel(
                       "Visualization",
                       fluidPage(
                         titlePanel("Visualization"),
                         column(
                           4,
                           selectInput(
                             label = "Select Dataframe",
                             choices = m$names.x...x....TRUE..,
                             selected = NULL,
                             inputId = "Table2"
                           ),
                           uiOutput("factor"),
                           uiOutput("numeric"),
                           radioButtons(
                             inputId = "ones",
                             label = "Please choose one",
                             choices = c(
                               `Factor / Character Variable` = "fac",
                               `Numeric Variable` = "nam"
                             ),
                             selected = NULL,
                             inline = FALSE,
                             width = NULL,
                             choiceNames = NULL,
                             choiceValues = NULL
                           ),
                           uiOutput("all"),
                           actionButton("plot",
                                        "Click for plot"))


                         ,
                         column(8, plotlyOutput(
                           "trendPlot",
                           width = "auto", height = "auto"
                         ))
                       )
                     ),
                     tabPanel("Cross Table", fluidPage(
                       titlePanel("Cross Table"),
                       column(
                         4,
                         selectInput(
                           label = "Select Dataframe",
                           choices = m$names.x...x....TRUE..,
                           selected = NULL,
                           inputId = "Table266"
                         ),
                         uiOutput("factor266"),
                         uiOutput("numeric266"),
                         uiOutput("all266"),
                         actionButton("crossTable", "Click for Cross Table")
                         # ,wellPanel(HTML(text = "Company:  K2 Analytics Finishing School Pvt. Ltd.")),
                         # wellPanel(HTML(text = "website: http://www.k2analytics.co.in"))
                       ),
                       column(
                         7,
                         textOutput("crosstab2299"),
                         tags$head(
                           tags$style(
                             "#crosstab2299{color: red;\n                font-size: 20px;\n                font-style: italic;\n  }"
                           )
                         ),
                         tableOutput("crosstab"),
                         tableOutput("count")
                       )
                     )),
                     tabPanel(
                       "Descriptive Analysis",
                       fluidPage(
                         titlePanel("Descriptive Analysis"),
                         column(
                           4,
                           selectInput(
                             label = "Select Dataframe",
                             choices = m$names.x...x....TRUE..,
                             selected = NULL,
                             inputId = "Table987"
                           ),
                           uiOutput("factor987"),
                           helpText("Factor variable will produce a Frequency"),
                           uiOutput("numeric987"),
                           helpText("Numeric variable will produce a Quantile"),
                           selectInput(
                             "num987",
                             "Please Enter Number(Optional for quantile)",
                             0:100,
                             multiple = T,
                             selected = c(0, 1,
                                          5, 10, 25, 50, 75, 90, 95, 99, 100)
                           ),
                           radioButtons(
                             inputId = "ones987",
                             label = "Please choose one",
                             choices = c(
                               `Factor / Character Variable` = "fac987",
                               `Numeric Variable` = "nam987"
                             )
                           ),
                           actionButton("analysis",
                                        "Get Analysis")
                           # , wellPanel(
                           #    HTML(text =
                           #           "Company:  K2 Analytics Finishing School Pvt. Ltd."
                           #         ))
                           #  ,
                           #  wellPanel(HTML(text = "website: http://www.k2analytics.co.in"))
                         ),
                         column(7, tableOutput("crosstab987"), tableOutput("count987"))
                       )
                     )
    )

    server <- function(input, output) {
      {


        out_factore <-
          reactive({
            #for reactive factore output based on selected object
            if (is.null(input$Table2))
              return(NULL)
            op <- data.frame(get((input$Table2)))
            j <- data.frame(names(Filter(is.factor, op)))
            k <- data.frame(names(Filter(is.character, op)))
            colnames(j) <- "factor"
            colnames(k) <- "factor"

            j <- rbind(j, k)

            j
          })
        output$factor <-
          #reactive input factore
          renderUI({
            selectInput(
              label = "Factor / Character Variable" ,
              choices = out_factore() ,
              selected = NULL,
              inputId = "factor"
            )
          })
      }

      {
        out_numeric <-
          reactive({
            #for reactive numeric output based on selected object
            if (is.null(input$Table2))
              return(NULL)
            op <- data.frame(get((input$Table2)))
            j <- data.frame(names(Filter(is.numeric, op)))
            colnames(j) <- "numeric"
            j
          })
        output$numeric <-
          #reactive input numeric
          renderUI({
            selectInput(
              label = "Numeric Variable" ,
              choices = out_numeric() ,
              selected = NULL,
              multiple = F,
              inputId = "numeric"
            )
          })
      }


      {
        out_all <-
          reactive({
            #for reactive all output based on selected object
            if (is.null(input$Table2))
              return(NULL)
            op <- data.frame(get((input$Table2)))
            j <- data.frame(names(op))
            colnames(j) <- "all"
            j
          })
        output$all <-
          #reactive input all
          renderUI({
            selectInput(
              label = "Select Target Variable" ,
              choices = out_all() ,
              selected = NULL,
              multiple = F,
              inputId = "all"
            )
          })
      }

      {
        ## List of Libraries
        library(data.table)
        library(scales)

        ## deciling code
        decile <- function(x) {
          deciles <- vector(length = 10)
          for (i in seq(0.1, 1, .1)) {
            deciles[i * 10] <- quantile(x, i, na.rm = T)
          }
          return (ifelse(x < deciles[1], 1,
                         ifelse(
                           x < deciles[2], 2,
                           ifelse(x < deciles[3], 3,
                                  ifelse(
                                    x < deciles[4], 4,
                                    ifelse(x < deciles[5], 5,
                                           ifelse(
                                             x < deciles[6], 6,
                                             ifelse(x < deciles[7], 7,
                                                    ifelse(
                                                      x < deciles[8], 8,
                                                      ifelse(x <
                                                               deciles[9], 9, 10)
                                                    ))
                                           ))
                                  ))
                         )))
        }

        ## set the working directory of folder to dump the output
        ## compile the function


        fn_biz_viz <- function(df, target, var)
        {
          tmp <- df[, c(var , target)]
          colnames(tmp)[1] = "Xvar"
          colnames(tmp)[2] = "Target"


          tmp$deciles <- decile(tmp$Xvar)

          library(data.table)
          tmp_DT = data.table(tmp)

          RRate <- tmp_DT[, list(
            min_ = min(Xvar),
            max_ = max(Xvar),
            avg_ = mean(Xvar),
            cnt = length(Target),
            cnt_resp = sum(Target),
            cnt_non_resp = sum(Target == 0)
          ) ,
          by = deciles][order(deciles)]

          RRate$range = paste(RRate$min_ , RRate$max_ , sep = " to ")

          RRate$prob <- round(RRate$cnt_resp / RRate$cnt, 3)


          setcolorder(RRate, c(1, 8, 2:7, 9))


          RRate$cum_tot <- cumsum(RRate$cnt)
          RRate$cum_resp <- cumsum(RRate$cnt_resp)
          RRate$cum_non_resp <- cumsum(RRate$cnt_non_resp)
          RRate$cum_tot_pct <-
            round(RRate$cum_tot / sum(RRate$cnt), 2)

          RRate$cum_resp_pct <-
            round(RRate$cum_resp / sum(RRate$cnt_resp), 2)

          RRate$cum_non_resp_pct <-
            round(RRate$cum_non_resp / sum(RRate$cnt_non_resp), 2)

          RRate$ks <-
            abs(RRate$cum_resp_pct - RRate$cum_non_resp_pct)


          RRate$prob = 100 * (RRate$prob)
          RRate$cum_tot_pct = 100 * (RRate$cum_tot_pct)
          RRate$cum_resp_pct = 100 * (RRate$cum_resp_pct)
          RRate$cum_non_resp_pct = 100 * (RRate$cum_non_resp_pct)
          RRate$ordered_range <-
            factor(RRate$range, levels = RRate$range)
          ## Output the RRate table to csv file
          ## you should ensure the setwd -  working directory
          #write.csv(RRate, file = paste0(output_folder, var, ".csv"),
          #         row.names = FALSE)
          RRate
        }
      }
      {
        kpp <- reactive({
          y <- data.frame(get((input$Table2)))
          jp <- fn_biz_viz(df = y,
                           target = input$all,
                           var = input$numeric)


        })


      }
      {
        header <- reactive({
          target = input$all
          var = input$numeric
          paste(target, "Vs", var)
        })

        x_var_n <- reactive({
          var <- input$numeric
          var
        })

        kpp1 <- reactive({
          jp <- kpp()
          ay <- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Target Rate",
            range = c(0, max(jp$prob + 2, by = 2))
          )
          p <- plot_ly() %>%
            add_bars (x = jp$ordered_range,
                      y = jp$cnt,
                      name = "# Customers") %>%
            add_lines(
              x = jp$ordered_range,
              y = jp$prob,
              name = "Target Rate",
              yaxis = "y2"
            ) %>%
            add_text(
              x =  jp$ordered_range,
              y = jp$prob,
              text = jp$prob,
              inherit = FALSE,
              name = "Target Rate",
              yaxis = "y2",
              textposition = 'top',
              textfont = list(color = '#000000', size = 16),
              showlegend = FALSE
            ) %>%
            layout(
              title = header(),
              yaxis2 = ay,
              xaxis = list(title = x_var_n()),
              yaxis = list(title = "# Customers"),
              legend = list(
                x = 0.3,
                y = 1.1,
                orientation = 'h'
              ),
              margin = 10
            )



          p

        })

        header123 <- reactive({
          in1 <- input$factor
          in2 <- input$all
          paste(in2, "Vs", in1)
        })

        x_var <- reactive({
          in1 <- input$factor
          in1
        })
        chr1 <- reactive({
          if (input$factor == input$all)
            return(NULL)
          y <- data.frame(get((input$Table2)))
          in1 <- input$factor
          in2 <- input$all
          kop <- y[, c(in1, in2)]
          colnames(kop) <- c("group_V", "Target")
          pp <- as.data.frame.matrix(table(kop$group_V, kop$Target))
          jq <- pp
          jq$all_c <- (jq$`0` + jq$`1`)
          jq$prob <- round(jq$`1` / (jq$`0` + jq$`1`), 3)
          jq$prob <- 100 * jq$prob

          ay <- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Target Rate",
            range = c(0, max(jq$prob + 2, by = 2))
          )
          p <- plot_ly() %>%
            add_bars (x = row.names(jq),
                      y = jq$all_c,
                      name = "# Customers") %>%
            add_lines (
              x = row.names(jq),
              y = jq$prob,
              name = "Target Rate",
              yaxis = "y2"
            ) %>%
            add_text(
              x =  row.names(jq),
              y = jq$prob,
              text = jq$prob,
              inherit = FALSE,
              yaxis = "y2",
              name = "Target Rate",
              textposition = 'top',
              textfont = list(color = '#000000', size = 16),
              showlegend = FALSE
            ) %>%
            layout(
              title = header123(),
              yaxis2 = ay,
              xaxis = list(title = x_var()),
              yaxis = list(title = "# Customers"),
              legend = list(
                x = 0.3,
                y = 1.1,
                orientation = 'h'
              ),
              margin = 10
            )

          p

        })


        ntext <- eventReactive(input$plot, {
          if (input$ones == "fac")
            return(chr1())

          kpp1()

        })
      }
      {
        output$trendPlot <- renderPlotly({
          options(warn = -1)
          ntext()

        })



        out_factore266 <-
          reactive({
            #for reactive factore output based on selected object
            if (is.null(input$Table266))
              return(NULL)
            op <- data.frame(get((input$Table266)))
            j <- data.frame(names(op))
            colnames(j) <- "all"
            j
          })
        output$factor266 <-
          #reactive input factore
          renderUI({
            selectInput(
              label = "Dimension-1" ,
              choices = out_factore266() ,
              selected = NULL,
              inputId = "factor266"
            )
          })
      }

      {
        out_numeric266 <-
          reactive({
            #for reactive numeric output based on selected object
            if (is.null(input$Table266))
              return(NULL)
            op <- data.frame(get((input$Table266)))
            j <- data.frame(names(op))
            colnames(j) <- "all"
            j
          })
        output$numeric266 <-
          #reactive input numeric
          renderUI({
            selectInput(
              label = "Dimension-2" ,
              choices = out_numeric266() ,
              selected = NULL,
              multiple = F,
              inputId = "numeric266"
            )
          })
      }


      {
        out_all266 <-
          reactive({
            #for reactive all output based on selected object
            if (is.null(input$Table266))
              return(NULL)
            op <- data.frame(get((input$Table266)))
            j <- data.frame(names(op))
            colnames(j) <- "all"
            j
          })
        output$all266 <-
          #reactive input all
          renderUI({
            selectInput(
              label = "Select Target Variable" ,
              choices = out_all266() ,
              selected = NULL,
              multiple = F,
              inputId = "all266"
            )
          })


        header99 <- eventReactive(input$crossTable, {
          in1 <- input$factor266
          in2 <- input$numeric266
          paste(c(in1, "Vs", in2))
        })


        output$crosstab2299 <- renderText({
          header99()
        })

        output$text<-renderText({
          k<- c("K2 Analytics Finishing School Pvt. Ltd.","     Website: http://www.k2analytics.co.in")
          k
        })
        output$text233<-renderText({
          k<- "website: http://www.k2analytics.co.in"
          k
        })



        crstbl <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")


          tb <- as.data.frame.matrix(xtabs(~ kop$x + kop$y))
          tb2 <- as.data.frame.matrix(xtabs(kop$Ta ~ kop$x + kop$y))
          per <- tb2 * 100 / tb

          library(data.table)
          setDT(per, keep.rownames = TRUE)[]

          names(per)[names(per) == "rn"] = "Target Rate"
          per

        })


        crstbl99 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")


          tb <- as.data.frame.matrix(xtabs(~ kop$x + kop$y))

          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb

        })

        crstbl2_dia_1 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      ))
                                             ))
                                    ))
                           )))
          }

          kop$decile = decile(kop$x)
          library(data.table)
          tmp_DT = data.table(kop)

          RRatet <- tmp_DT[, list(min_ = min(x),
                                  max_ = max(x)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          kop <- join(kop, RRatet, by = "decile")


          kop
        })

        crstbl2 <- reactive({
          kop <- crstbl2_dia_1()


          tb <- as.data.frame.matrix(xtabs(~ kop$range + kop$y))
          tb2 <- as.data.frame.matrix(xtabs(kop$Ta ~ kop$range + kop$y))
          per <- tb2 * 100 / tb

          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per

        })


        crstbl299 <- reactive({
          kop <- crstbl2_dia_1()


          tb <- as.data.frame.matrix(xtabs(~ kop$range + kop$y))

          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb

        })



        crstbl2_dia_2 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      ))
                                             ))
                                    ))
                           )))
          }

          kop$decile = decile(kop$y)
          library(data.table)
          tmp_DT = data.table(kop)

          RRatet <- tmp_DT[, list(min_ = min(y),
                                  max_ = max(y)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          kop <- join(kop, RRatet, by = "decile")


          kop
        })

        crstbl3 <- reactive({
          kop <- crstbl2_dia_2()


          tb <- as.data.frame.matrix(xtabs(~ kop$x + kop$range))
          tb2 <- as.data.frame.matrix(xtabs(kop$Ta ~ kop$x + kop$range))
          per <- tb2 * 100 / tb

          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per

        })


        crstbl399 <- reactive({
          kop <- crstbl2_dia_2()


          tb <- as.data.frame.matrix(xtabs(~ kop$x + kop$range))

          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb

        })



        crstbl2_dia_bth <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      ))
                                             ))
                                    ))
                           )))
          }

          kop$decile1 = decile(kop$y)
          kop$decile = decile(kop$x)
          library(data.table)
          tmp_DT = data.table(kop)

          RRatet <- tmp_DT[, list(min_ = min(x),
                                  max_ = max(x)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          kop <- join(kop, RRatet, by = "decile")


          tmp_DT = data.table(kop)

          RRatet <- tmp_DT[, list(min_ = min(y),
                                  max_ = max(y)) ,
                           by = decile1][order(decile1)]
          RRatet$range1 = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          kop <- join(kop, RRatet, by = "decile1")

          kop
        })

        crstbl4 <- reactive({
          kop <- crstbl2_dia_bth()


          tb <- as.data.frame.matrix(xtabs(~ kop$range1 + kop$range))
          tb2 <-
            as.data.frame.matrix(xtabs(kop$Ta ~ kop$range1 + kop$range))
          per <- tb2 * 100 / tb

          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per

        })



        crstbl499 <- reactive({
          kop <- crstbl2_dia_bth()


          tb <- as.data.frame.matrix(xtabs(~ kop$range1 + kop$range))

          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb


        })

        ntext266 <- eventReactive(input$crossTable, {
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")


          ifelse(
            class(kop$x) %in% c("numeric", "integer") == "TRUE"
            &
              class(kop$y) %in% c("numeric", "integer") == "TRUE" ,
            return(crstbl4()),

            ifelse(
              class(kop$x) %in% c("numeric", "integer") == "FALSE"
              &
                class(kop$y) %in% c("numeric", "integer") == "TRUE" ,
              return(crstbl3()),

              ifelse(
                class(kop$x) %in% c("numeric", "integer") == "TRUE"
                &
                  class(kop$y) %in% c("numeric", "integer") == "FALSE" ,
                return(crstbl2()),
                return(crstbl())
              )
            )
          )

        })


        ntext26699 <- eventReactive(input$crossTable, {
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          kop <- y[, c(in1, in2, in3)]
          colnames(kop) <- c("x", "y", "Ta")

          ifelse(
            class(kop$x) %in% c("numeric", "integer") == "TRUE"
            &
              class(kop$y) %in% c("numeric", "integer") == "TRUE" ,
            return(crstbl499()),

            ifelse(
              class(kop$x) %in% c("numeric", "integer") == "FALSE"
              &
                class(kop$y) %in% c("numeric", "integer") == "TRUE" ,
              return(crstbl399()),

              ifelse(
                class(kop$x) %in% c("numeric", "integer") == "TRUE"
                &
                  class(kop$y) %in% c("numeric", "integer") == "FALSE" ,
                return(crstbl299()),
                return(crstbl99())
              )
            )
          )

        })


        output$crosstab <- renderTable({
          ntext266()
        },
        bordered = TRUE)

        output$count <- renderTable({
          ntext26699()
        },
        bordered = TRUE)


      }


      out_factore987 <-
        reactive({
          #for reactive factore output based on selected object
          if (is.null(input$Table987))
            return(NULL)
          op <- data.frame(get((input$Table987)))
          j <- data.frame(names(Filter(is.factor, op)))
          k <- data.frame(names(Filter(is.character, op)))
          colnames(j) <- "factor"
          colnames(k) <- "factor"

          j <- rbind(j, k)

          j

        })
      output$factor987 <-
        #reactive input factore
        renderUI({
          selectInput(
            label =  "Factor / Character Variable" ,
            choices = out_factore987() ,
            selected = NULL,
            inputId = "factor987"
          )
        })

      out_numeric987 <-
        reactive({
          #for reactive factore output based on selected object
          if (is.null(input$Table987))
            return(NULL)
          op <- data.frame(get((input$Table987)))
          j <- data.frame(names(Filter(is.numeric, op)))
          colnames(j) <- "numeric"
          j

        })
      output$numeric987 <-
        #reactive input factore
        renderUI({
          selectInput(
            label = "Numeric Variable" ,
            choices = out_numeric987() ,
            selected = NULL,
            multiple = T,
            inputId = "numeric987"
          )
        })



      head987<-reactive({
        in1<-input$factor987
        in1
      })

      freq<-reactive({
        y <- data.frame(get((input$Table987)))
        in1 <- input$factor987
        kop <- y[ in1]
        colnames(kop)="x"

        FREQ<-table(kop$x)
        PER<-FREQ/nrow(kop)
        FREQ<-data.frame(FREQ)

        PER<-data.frame(PER)
        FREQ<-merge(FREQ,PER,by="Var1")
        library(scales)
        FREQ$Freq.y<-percent(FREQ$Freq.y)
        kob<-list(Var1="Total",Freq.x=nrow(kop),Freq.y=percent(nrow(kop)/nrow(kop)))
        FREQ<-rbind(FREQ,data.frame(kob))

        colnames(FREQ)<-c(head987(),"FREQ","PER")


        FREQ
      })

      qunt<-reactive({
        y <- data.frame(get((input$Table987)))
        in1 <- input$numeric987
        kop <- y[ in1]

        in3<-paste(input$num987,sep=",")
        in4<-as.numeric(in3)
        in4<-sort(in4)
        QUANTILE<- apply( kop[c(1:ncol(kop))] , 2 ,quantile ,
                          probs=in4/100,na.rm=TRUE)
        QUANTILE<-as.data.frame(QUANTILE)
        setDT(QUANTILE, keep.rownames = TRUE)[]
        names(QUANTILE)[names(QUANTILE) == "rn"] = "PER"
        QUANTILE



        # QUANTILE<-quantile(kop$x,probs = in4/100)
        # QUANTILE<-as.data.frame(QUANTILE)
        # library(data.table)
        # setDT(QUANTILE, keep.rownames = TRUE)[]
        # colnames(QUANTILE)=c("PER","QUANTILE")
        # QUANTILE
      })


      ntext987 <- eventReactive(input$analysis, {
        if (input$ones987 == "fac987")
          return(freq())

        qunt()
      })

      output$crosstab987 <- renderTable({
        ntext987()
      },
      bordered = TRUE)



    }


    shinyApp(ui, server)
  }
}
