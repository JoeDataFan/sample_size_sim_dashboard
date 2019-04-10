#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/


################################################################################
#Todo:
# - explore ways to improve speed of app
#       - try other means to create 100 replicate sample data, use time 
# - clean up code and add lots of comments and build better outline
# - review format of app with Kasey and see what I can do to improve
# - perhaps in another tab have many plots that show how variable change with 
# relation to other variables:
# - how sample size changes with increasing standard deviation
# - how p-vlaue changes with increasing sample size
# - how sample size changes with increased differences between treatment means

################################################################################    

library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(pwr)
library(dashboardthemes)


ui <- dashboardPage(
    dashboardHeader(title = "Sample Size Simulation",
                    titleWidth = 250),
    dashboardSidebar(
        width = 150,
        sidebarMenu(
            menuItem("Use simulator", tabName = "simulator"),
            menuItem("Instructions", tabName = "instructions")
        )
    ),
    dashboardBody(
        ### changing theme
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        #tags$head(tags$style(HTML('
        #        /* navbar (rest of the header) */
        #         .skin-blue .main-header .navbar {
        #         background-color: #1C4E80;
        #                                        }        
        #                          
        #        /* logo */
        #        .skin-blue .main-header .logo {
        #        background-color: #0091D5;
        #                                    }'
        #)
        #)
        #),
        tabItems(
            tabItem(tabName = "instructions",
                    tabPanel("How to use this tool", 
                             h4(tags$b("Motivation for the tool:")),
                             h5("The act of collecting data is often time consuming and expensive. 
                                However, not collecting enough data can lead to decisions (sometimes 
                                very important ones) based on false conclusions from sample data that
                                did not accurately represent the entire population. This shiny app is
                                intended to help demonstrate why sample size is important for good 
                                statistical inference and how standard deviation, effect size and 
                                significance level all influence sample size decisions."),
                             
                             h4(tags$b("To get started:")),
                             h5("On the Use simulator page you will see a series of input boxes and slider bars that will
                                be manipulated to simluate different experimental conditions."),
                             h5("1. Choose a starting mean for treatment A"),
                             h5("2. Choose a starting standard deviation for treatment A"),
                             h5("3. Choose a starting sample size"),
                             h5("4. Choose the % difference between treatment means that you would like to detect. 
                                Are you trying to detect a small or large difference between treatments?"),
                             h5("5. Choose the desired significance level or alpha; the probability of falsely concluding a 
                                difference in means when there is no difference (falsely rejecting the null 
                                hypothesis)."),
                             h5("6. Choose the desired power or beta; the probability of falsely concluding no difference 
                                between means when there is a difference (falsely accepting the null hypothesis)."),
                             
                             h4(tags$b("Simulating experiments and plotting results:")),
                             h5(tags$li("At the top right you see a button labeled", tags$b("Run experiment!"), ". This button will take 
                                        a random sample (using the sample size you have defined) from the populations A and B
                                        that you have defined. The result of this random sampling is then plotted in horizontal 
                                        boxplots seen on the", tags$b("Plots"), "tab. Each time you click this button a new random sample is taken.")),
                             h5(tags$li("Alternatively you can use the Replicate slider bar to chose a new replicate. This 
                                        slider can also be animated by pressing the play button in the bottom righthand corner.
                                        The slider will loop until it is paused.")),
                             h5(tags$li("After each random sampling a t-test is ran between treatments A and B and the
                                            resulting p-value is reported at the top of the plot. If the p-value is lower than 
                                            the choosen significance level then the p-vlaue is labeled", tags$b("Statistically significant"))),
                             h5(tags$li("At the top left you will see the recommended sample size based on currently
                                            selected conditions.")),
                             
                             h4(tags$b("Go and have fun:")),
                             h5("Now adjust everything you can think of and observe the effect."),
                             h5(tags$li("Observe what happens to estimated sample size with changes in each parameter?")),
                             h5(tags$li("What happens to the p-value with changes in each parameter?")),
                             h5(tags$li("Observe what happens to the position of the boxplots as experiments are repeated with increasing 
                                            sample sizes and or decreasing standard deviations.")),
                             h5(tags$li("Finally, click the checkbox below the boxplots to see the true distributions of treatments A and B.
                                            This graph has the same x-axis as the boxplot. Compare the two plots to evaluate if the sample was a 
                                            good estimation of the population."))
                    )
            ),
            tabItem(tabName = "simulator",
                    #tags$style(".content {background-color:#F1F1F1;}"),
                    tags$style(HTML("
                                    .box.box-solid.box-primary>.box-header {
                                    background:#7E909A
                                    }")),
                    tags$style(HTML("
                                    .box.box-solid.box-primary>.box-header>.box-title {
                                    color:#FFF
                                    }")),
                    
                    
                    # change color of recomended sample size valuebox
                    tags$style(".small-box.bg-blue { background-color: #A5D8DD !important; color: #1C4E80 !important; }"),
                    # change color of t-test pvlaue box
                    tags$style(".small-box.bg-red { background-color: #EA6A47 !important; color: #1C4E80 !important; }"),
                    tags$style(".small-box.bg-green { background-color: #A5D8DD !important; color: #1C4E80 !important; }"),
                    # change color of slider bars
                    #.irs-max,
                    #.irs-grid-pol
                    tags$style("
                                .irs-bar,
                                .irs-bar-edge,
                                .irs-single
                                 {
                                  background: #EA6A47;
                                  border-color: #EA6A47;
                                }"),
                    # change color of min and max values on slider bars
                    tags$style("
                               .irs-min,
                               .irs-max {
                               background: #FFF;
                               border-color: #FFF;
                               }"),
                    
                    tags$style(HTML("
                                    .box.box-solid.box-primary{
                                    #border-bottom-color:#FFF;
                                    #border-left-color:#FFF;
                                    #border-right-color:#FFF;
                                    #border-top-color:#FFF;
                                    }
                                    "
                    )),
                    fluidRow(
                        column(
                            width = 3,
                            valueBoxOutput("best.sample.size",
                                           width = NULL),
                            box(
                                title = HTML(paste("<b>","Mean","</b>")),
                                status = "primary",
                                width = NULL,
                                solidHeader = TRUE,
                                numericInput("mu.val", "1. Enter a starting mean", 100),
                                uiOutput("mu.slider")
                            ),
                            
                            box(
                                title = HTML(paste("<b>","Standard Deviation","</b>")),
                                status = "primary",
                                width = NULL,
                                solidHeader = TRUE,
                                numericInput("sd.val", "2. Enter a starting standard deviation", 20),
                                uiOutput("sd.slider")
                            ),
                            box(
                                title = HTML(paste("<b>", "Effect Size (Difference Bewteen Treatments)", "</b>")),
                                status = "primary",
                                solidHeader = TRUE,
                                width = NULL,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                sliderInput(inputId = "diff.mu",
                                            label = "% difference between treatment means to be detected",
                                            min = 0,
                                            max = 100,
                                            value = 10)
                                ),
                            box(
                                title = HTML(paste("<b>","Test Parameters","</b>")),
                                status = "primary",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                sliderInput(inputId = "alpha",
                                            label = "Type I error (false positive) - Significance level (alpha)",
                                            min = 0,
                                            max = 1,
                                            value = 0.05),
                                sliderInput(inputId = "beta",
                                            label = "Type II error (false negative) - Power (beta)",
                                            min = 0,
                                            max = 1,
                                            value = 0.80)
                            )
                        ),
                        column(
                            width = 9,
                            fluidRow(
                                box(
                                    title = HTML(paste("<b>","Sample Size","</b>")),
                                    status = "primary",
                                    width = 4,
                                    solidHeader = TRUE,
                                    sliderInput(inputId = "sample.size",
                                                label = "Choose a sample size",
                                                min = 3,
                                                max = 200,
                                                value = 3,
                                                step = 1,
                                                animate =
                                                    animationOptions(interval = 1000, loop = TRUE))
                                ),
                                box(
                                    title = HTML(paste("<b>","Errorbars (display of variation)","</b>")),
                                    width = 3,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    selectInput("errorbars", "Choose type of errorbar:",
                                                list("Confidence Interval" = "confidence.int",
                                                     "Standard Deviation" = "standard.dev")
                                    )
                                ),
                                box(
                                    title = HTML(paste("<b>","Repeat Test","</b>")),
                                    width = 5,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    fluidRow(
                                        box(
                                            width = 5,
                                            title = "Run Experiment",
                                            actionButton("s1","Start"),
                                            actionButton("s2","Stop")
                                            
                                        ),
                                        valueBoxOutput("p.value",
                                                       width = 7)
                                    )
                                    )
                                ),
                         tabBox(
                            width = NULL,
                            height = 600,
                            tabPanel(HTML(paste("<b>","Single Test","</b>")),
                                     fluidRow(
                                         box(
                                             title = HTML(paste("<b>","True Treatment Distributions","</b>")),
                                             status = "primary",
                                             solidHeader = TRUE,
                                             width = 6,
                                             collapsible = TRUE,
                                             collapsed = TRUE,
                                             plotOutput("density")
                                         ),
                                         box(
                                             title = HTML(paste("<b>", "Single Test", "</b>")),
                                             status = "primary",
                                             solidHeader = TRUE,
                                             width = 6,
                                             collapsible = TRUE,
                                             collapsed = FALSE,
                                             plotOutput("sample.box")
                                         )
                                     )
                            ),
                            #tabPanel("True Distributions", plotOutput("density")),
                            tabPanel(HTML(paste("<b>","50 Replicates","</b>")),
                                     box(
                                         title = HTML(paste("<b>", "50 Replicate Tests Shaded by Significant Difference", "</b>")),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         plotOutput("replicate.box")
                                     )
                            )
                        )
                    )
            )
        )
    )
)
)



