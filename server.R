#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# required libraries----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(pwr)
library(scales)
library(RcppZiggurat)

# color theme pallet ----
#HEX color codes used:  
# light gray - #F1F1F1,
# black - #202020,
# gray - #7E909A,
# navy blue - #1C4E80,
# aqua - #A5D8DD,
# orange red - #EA6A47,
# blue - #0091D5

# plot theme----
my_plot_theme <- theme_classic()+
    theme(text = element_text(size = 16),
          legend.position = "top",
          legend.justification = "left")

# alpha for plots----
alpha.val <- 0.4

# size for edges of plots-----
size.val <- 1.25

# Define server logic 
server <- (function(input, output) {
    
    output$mu.slider <- renderUI({
        sliderInput("slider.out.mu", "Mean", min=input$mu.val * 0.5, max=input$mu.val * 1.5, value = input$mu.val)
    })
    output$sd.slider <- renderUI({
        sliderInput("slider.out.sd", "Standard Deviation", min=input$sd.val * 0.2,  max=input$sd.val * 1.8, value = input$sd.val)
    })
    
    #create a function for alpha to be used in other places
    alpha <- reactive({
        input$alpha
    })
    
    # density distribution plot ----
    output$density <- renderPlot({
        pop.n <- 1000
        data.pop <- tibble(treatment = c(rep("A", times = pop.n),
                                         rep("B", times = pop.n)
        ),                                                                          
        property = c((zrnorm(n = pop.n) * input$slider.out.sd + input$slider.out.mu),
                     (zrnorm(n = pop.n) * input$slider.out.sd + 
                          (input$slider.out.mu + input$slider.out.mu * input$diff.mu/100)
                      ))
        )
        
        # data to define vertical mean lines for treatments
        mean.data <- tibble(treatment = c("A", "B"),
                            means = c(input$slider.out.mu, input$slider.out.mu + 
                                          input$slider.out.mu * input$diff.mu/100))
        
        ggplot(data = data.pop,
               aes(x = property,
                   fill = treatment,
                   color = treatment)
        ) + 
            geom_density(alpha = alpha.val,
                         size = size.val)+
            my_plot_theme +
            guides(fill = guide_legend(title = "Treatment"),
                   color = FALSE)+
            labs(x = "Property",
                 y = "Frequency",
                 title = paste(pop.n, "Observations from Treatments A and B"))+
            scale_x_continuous(limits = c(input$slider.out.mu - 3* (input$sd.val * 1.5),
                                          input$slider.out.mu * (1 + input$diff.mu / 100) + 3* (input$sd.val * 1.5)
            )
            )+
            geom_vline(data = mean.data,
                       aes(xintercept = means,
                           color = treatment),
                       linetype = 2,
                       size = size.val)+
            scale_fill_manual(values = c('#EA6A47', '#0091D5'))+
            scale_color_manual(values = c('#EA6A47', '#0091D5'))
    })
    
    # create function to observer timer and render plot at time intervals
    # based on start and stop buttons
    my<-reactiveValues(timer=reactiveTimer(800), started=FALSE)
    observeEvent(input$s1, {my$started<-TRUE})
    observeEvent(input$s2, {my$started<-FALSE})
    
    observe({
        my$timer()
        if(isolate(my$started))
    
    # Single test plot----
    output$sample.box <- renderPlot({
        # Take a dependency on input$goButton. This will run once initially,
        # because the value changes from NULL to 0.
        #input$goButton
        
        # create data
        data.sample <- tibble(treatment = c(rep("A", times = input$sample.size),
                                            rep("B", times = input$sample.size)
        ),
        #replicate = c(rep(1:100, each = input$sample.size),
        #              rep(1:100, each = input$sample.size)
        #),
        property = c(rnorm(n = input$sample.size,
                           mean = input$slider.out.mu,
                           sd = input$slider.out.sd),
                     rnorm(n = input$sample.size,
                           mean = input$slider.out.mu + input$slider.out.mu * input$diff.mu/100,
                           sd = input$slider.out.sd)
        )
        )
        
        # p.value from randomly sampled data                        
        p.value <- tidy(t.test(data.sample$property[data.sample$treatment == "A"], #& data.sample$replicate == input$replicate],
                               data.sample$property[data.sample$treatment == "B"], #& data.sample$replicate == input$replicate],
                               alternative = "two.sided",
                               paired = FALSE,
                               var.equal = FALSE,
                               conf.level = 1-input$alpha))$p.value
        
        difference <- ifelse(p.value > input$alpha, "", "Statistically significant")
        
        output$p.value <- renderValueBox({
            if (p.value  < input$alpha)
            {
                valueBox(value = tags$p(paste("P-value =", round(p.value, 6)),
                                        style = "font-size: 75%;"),
                         width = 2,
                         subtitle = 'Significant difference',
                         color = 'green')
            }
            else if (p.value  > input$alpha)
            {
                valueBox(value = tags$p(paste("P-value =", round(p.value, 6)),
                                        style = "font-size: 75%;"),
                         width = 2,
                         subtitle = 'Not significantly different', 
                         color = 'red')
            }
        })
        
        # prepare data for plotting
        data.sample.stats <- data.sample %>% 
            #filter(replicate %in% input$replicate) %>%
            group_by(treatment) %>% 
            summarise(mean.prop = mean(property, na.rm = TRUE),
                      confidence.int = qnorm(1-(input$alpha/2))*sd(property, na.rm=TRUE)/sqrt(input$sample.size),
                      standard.dev = sd(property, na.rm = TRUE))
        
        #grpah to single result of each sampling
        ggplot(data = data.sample.stats,
               aes(x = as.factor(treatment),
                   y = mean.prop, 
                   fill = as.factor(treatment),
                   color = as.factor(treatment))
        ) +
            geom_bar(stat = "identity",
                     width = 0.75,
                     alpha = alpha.val,
                     size = size.val)+
            labs(x = "Treatment",
                 y = "Property",
                 title = paste(input$sample.size, "Samples from Each Treatment"))+
            geom_errorbar(aes(ymin = mean.prop - !!as.symbol(input$errorbars),
                              ymax = mean.prop + !!as.symbol(input$errorbars)),
                          position = "dodge",
                          width = 0.25,
                          size = size.val)+
            my_plot_theme +
            guides(fill = guide_legend(title = "Treatment"))+
            guides(color = FALSE)+
            guides(size = FALSE) +
            scale_fill_manual(values = c('#EA6A47', '#0091D5'))+
            coord_cartesian(ylim = c(input$slider.out.mu - 3* (input$sd.val * 1.5),
                                          input$slider.out.mu * (1 + input$diff.mu / 100) + 3* (input$sd.val * 1.5)
            )
        )
    })
    })
    
    # Current sample size----
    output$act.sample.size <- renderValueBox({
        valueBox(tags$p(input$sample.size,
                        style = "font-size: 75%;"),
                 width = 3,
                 subtitle = 'Sample size',
                 color = 'aqua')
    })
    
    # Sample size slider
    best.sample.size <- reactive({
        n.samples <- pwr.t.test(d = ((input$slider.out.mu * (1 + (input$diff.mu / 100)))
                                     - input$slider.out.mu) / input$slider.out.sd,
                                sig.level = input$alpha,
                                power = input$beta,
                                type = "two.sample",
                                alternative="two.sided"
        )
        round(as.numeric(n.samples[[1]]), 0)
    })
    
    # Recommended sample size----
    output$best.sample.size <- renderValueBox({
        valueBox(tags$p(paste(best.sample.size(), "samples"),
                        style = "font-size: 75%;"),
                 width = 3,
                 subtitle = 'Recommended Sample size',
                 color = "blue")
    })
    
    observe({
        my$timer()
        if(isolate(my$started))
    
    # n replicates plot----
    output$replicate.box <- renderPlot({
        # Take a dependency on input$goButton. This will run once initially,
        # because the value changes from NULL to 0.
        
        # create data
        n.reps <- 50
        data.sample <- tibble(treatment = c(rep("A", times = input$sample.size * n.reps),
                                            rep("B", times = input$sample.size * n.reps)
        ),
        replicate = c(rep(1:n.reps, each = input$sample.size),
                      rep(1:n.reps, each = input$sample.size)
        ),
        property = c(zrnorm(n = input$sample.size * n.reps) *
                         input$slider.out.sd + input$slider.out.mu,
                     zrnorm(n = input$sample.size * n.reps) *
                         input$slider.out.sd + 
                         (input$slider.out.mu + input$slider.out.mu * input$diff.mu/100)
                     )
        )
        
        # create data frame of t.test results for each replicate
        t.test.results <- data.sample %>%
            group_by(replicate) %>%
            do(tidy(t.test(.$property[.$treatment == "A" ],
                           .$property[.$treatment == "B" ],
                           alternative = "two.sided",
                           paired = FALSE,
                           var.equal = FALSE,
                           conf.level = 1-0.05))) 
        
        # merge t.test results with sample.data
        data.sample <- merge(data.sample, t.test.results, by = "replicate") %>% 
            mutate(significance = as.factor(if_else(p.value < input$alpha, "significant", "not significant")))
        
        #create grpah to single result of each sampling replicate
        ggplot(data = data.sample %>% 
                   group_by(treatment, replicate) %>% 
                   summarise(mean.prop = mean(property, na.rm = TRUE),
                             significance = unique(significance),
                             confidence.int = qnorm(1-(input$alpha/2))*sd(property, na.rm=TRUE)/sqrt(input$sample.size),
                             standard.dev = sd(property, na.rm = TRUE)),
               aes(x = as.factor(replicate),
                   y = mean.prop, 
                   fill = as.factor(treatment),
                   alpha = significance)
        ) +
            geom_bar(stat = "identity",
                     position = position_dodge())+
            labs(x = "Treatment",
                 y = "Property",
                 title = paste("Each Test Performed Using", input$sample.size, "Samples from Each Treatment"))+
            geom_errorbar(aes(ymin = mean.prop - !!as.symbol(input$errorbars),
                              ymax = mean.prop + !!as.symbol(input$errorbars))
                          , position = position_dodge(width=0.9),
                          width = 0.4)+
            my_plot_theme +
            guides(fill = guide_legend(title = "Treatment"))+
            guides(color = FALSE)+
            scale_fill_manual(values = c('#EA6A47', '#0091D5'))+
            coord_cartesian(ylim = c(input$slider.out.mu - 3* (input$sd.val * 1.5),
                                     input$slider.out.mu * (1 + input$diff.mu / 100) + 3* (input$sd.val * 1.5)
            )
            )
    })
    })
}
)
