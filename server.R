library(shiny)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(shinyWidgets)
library(rsconnect)
library(gt)
library(tidyverse)
library(latex2exp) #TeX function
library(patchwork)
library(kableExtra)
library(grid)
# Define the server
function(input, output) {
  # Create the first plot
  output$graph1 <- renderPlot({
    x <- seq(-3.5, 3.5, by = 0.01)
    y <- dnorm(x)
    df <- data.frame(x = x, y = y)
    tick_values <- seq(-3, 3, by = 1)
    sigma <- "\u03C3"
    mu <- "\u03BC"
    lower_labels <- paste0(mu, tick_values[1:3], sigma)
    upper_labels <- paste0(mu, "+", tick_values[5:7], sigma)
    tick_labels <- c(lower_labels, mu, upper_labels)
    ggplot(df, aes(x, y)) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      labs(title = "Standard Normal Distribution",
           x = "", y = "") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold"))
  })
  
  # Create the second plot
  output$graph2 <- renderPlot({
    x <- seq(-15, 15, length.out = 1000)
    y <- dnorm(x, mean = 0, sd = input$sd)
    data <- data.frame(x = x, y = y)
    ggplot(data, aes(x = x, y = y))+
      geom_line(color = "black") +
      geom_area(fill = "#478978") +
      labs(x = "X", y = "Count") +
      theme_minimal()
  })
  
  #Mean Distribution graph 3
  output$graph3 <- renderPlot({
    x <- seq(-15, 15, length.out = 100)
    dynamic_density_data <- data.frame(x, density = dnorm(x, mean = input$mean))
    static_density_data <- data.frame(x, density = dnorm(x, mean = 0))
    ggplot() +
      geom_line(data = static_density_data, aes(x, density), color = "skyblue4") +
      geom_line(data = dynamic_density_data, aes(x, density), color = "orchid4") +
      labs(x = "X", y = "Density") +
      theme_minimal()
  })
  
  #shape this should be two graphs nest to each other graph 4
  output$graph4 <- renderPlot({
    mu = 140
    sigma = 20
    x = seq(70, 210, 1)
    y = dnorm(x, mean = mu, sd = sigma)
    df <- data.frame(x = x, y = y)
    tick_values <- seq(80, 200, sigma)
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    pop_ldl <- ggplot(df, aes(x, y)) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "longdash", color = "red") +
      scale_x_continuous(breaks = tick_values) +
      labs(title = "Population Distribution of LDL",
           x = "LDL (mmol/L)", y = "",
           subtitle = paste("LDL ~ N(",mu_char, "=140, ", sigma_char, "=20)", sep = "")) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    x2 <- rnorm(350, mu, sigma)
    y2 <- dnorm(x2, mu, sigma)
    df2 <- cbind.data.frame(x2, y2)
    samp_ldl <- ggplot(df2, aes(x = x2)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, 
                     fill = "lightskyblue2", color = "black") +
      geom_line(aes(y = y2), color = "royalblue4", linewidth = 1.1) + 
      geom_vline(xintercept = mu,
                 color = "#95190C", linewidth = 1.1) +
      labs(title = "Sample Distribution of LDL",
           y ="", x = "LDL (mmol/L)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    graph4p <- grid.arrange(pop_ldl)
    print(graph4p)
  })
  
  #graph 5 changes in mean and sd 
  output$graph5 <- renderPlot({
    x <- seq(-15, 15, length.out = 1000)
    y <- dnorm(x, mean = input$mean1, sd = input$sd1)
    data <- data.frame(x = x, y = y)
    ggplot(data, aes(x = x, y = y)) +
      geom_line(color = "orchid4") +
      labs(x = "X", y = "Count") +
      theme_minimal()
  })
  
  #graph 6
  output$graph6 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    dof = 5
    sd <- sqrt(dof*2)
    
    x <- seq(0, 20, by = 0.1)
    y <- dchisq(x, df = dof)
    df <- data.frame(x = x, y = y)
    
    
    tick_values <- c(-0.5, dof - sd, dof, dof + sd,
                     dof + 2*sd, dof + 3*sd, dof + 4*sd, 
                     dof + 5*sd)
    sigma <- "\u03C3"
    mu <- "\u03BC"
    plus_minus <- "\u00B1"
    lower_labels <- paste(mu, "-", c(2, ""), sigma, sep = "")
    upper_labels <- paste0(mu, "+", c("", 2:5), sigma)
    tick_labels <- c(lower_labels, mu, upper_labels)
    
    
    
    sd2_plot <- ggplot(df, aes(x, y)) +
      geom_line() +
      geom_ribbon(data = subset(df, (x >= dof - 2*sd) & (x <= dof + 2*sd)),
                  aes(ymin = 0, ymax = y), 
                  alpha = 0.75, fill = "lightblue1") +
      geom_vline(xintercept = c(dof - sd, dof + 3*sd, 
                                dof + 2*sd, dof + sd, dof + 4*sd,
                                dof + 5*sd),
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = dof,
                 linetype = "dashed", color = "red") +
      labs(x = "", y = "", 
           subtitle = paste("75% of data within ",  
                            mu_char, plus_minus, "2", sigma_char,
                            sep = "")) +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      theme_classic() +
      theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold")) 
    
    
    sd3_plot <- ggplot(df, aes(x, y)) +
      geom_line() +
      geom_ribbon(data = subset(df, (x >= dof - 3*sd) & (x <= dof + 3*sd)),
                  aes(ymin = 0, ymax = y), 
                  alpha = 0.75, fill = "lightblue1") +
      geom_vline(xintercept = c(dof - sd, dof + 3*sd, 
                                dof + 2*sd, dof + sd, dof + 4*sd,
                                dof + 5*sd),
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = dof,
                 linetype = "dashed", color = "red") +
      labs(x = "", y = "", 
           subtitle = paste("89% of data within ",  
                            mu_char, plus_minus, "3", sigma_char,
                            sep = "")) +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      theme_classic() +
      theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold")) 
    
    
    
    graph6p <- grid.arrange(sd2_plot,
                            top = "Chebychev's Inequality for Non-Normal Data")
    print(graph6p)
  })
  #graph 7
  output$graph7 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    mu = 140
    sigma = 20
    
    x = seq(60, 220, 1)
    y = dnorm(x, mean = mu, sd = sigma)
    
    df <- data.frame(x = x, y = y)
    
    tick_values <- seq(80, 200, sigma)
    
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    plus_minus <- "\u00B1"
    
    # 2SD
    sd2_plot <- ggplot(df, aes(x, y)) +
      geom_area(stat = 'function',
                fun = dnorm,
                args=list(mean=mu, sd=sigma),
                fill = 'goldenrod',
                xlim = c(100, 180),
                alpha = 0.6) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values) +
      labs(title = paste(mu_char, " ", plus_minus, " 2", sigma_char, sep =""),
           x = "LDL (mmol/L)", y = "",
           subtitle = "75% of Data") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    # 3 SD
    sd3_plot <- ggplot(df, aes(x, y)) +
      geom_area(stat = 'function',
                fun = dnorm,
                args=list(mean=mu, sd=sigma),
                fill = 'goldenrod',
                xlim = c(80, 200),
                alpha = 0.6) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values) +
      labs(title = paste(mu_char, " ", plus_minus, " 3", sigma_char, sep =""),
           x = "LDL (mmol/L)", y = "",
           subtitle = "87% of Data") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    graph7p <- grid.arrange(sd2_plot, sd3_plot, nrow = 1, ncol = 2,
                            top = paste("Distribution of LDL Cholesterol (by Chebychev's Inequality)", "\n",
                                        paste("LDL ~ N(",mu_char, "=140, ", sigma_char, "=20)", sep = "")))
    print(graph7p)
  })
  # graph 8 
  output$graph8 <- renderPlot({
    x <- seq(-3.5, 3.5, by = 0.01)
    y <- dnorm(x)
    
    df <- data.frame(x = x, y = y)
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    
    
    tick_values <- seq(-3, 3, by = 1)
    sigma <- "\u03C3"
    mu <- "\u03BC"
    plus_minus <- "\u00B1"
    lower_labels <- paste0(mu, tick_values[1:3], sigma)
    upper_labels <- paste0(mu, "+", tick_values[5:7], sigma)
    tick_labels <- c(lower_labels, mu, upper_labels)
    
    sd1_plot <- ggplot(df, aes(x, y)) +
      geom_area(stat = 'function',
                fun = dnorm,
                fill = 'lightblue1',
                xlim = c(-1, 1),
                alpha = 0.75) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      labs(x = "", y = "",
           subtitle = paste("68% of data within", "\n",
                            mu_char, plus_minus, "1", sigma_char, 
                            sep = "")) +
      theme_classic() +
      theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold")) 
    
    sd2_plot <- ggplot(df, aes(x, y)) +
      geom_area(stat = 'function',
                fun = dnorm,
                fill = 'lightblue1',
                xlim = c(-2, 2),
                alpha = 0.75) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      labs(x = "", y = "",
           subtitle = paste("95% of data within", "\n",
                            mu_char, plus_minus, "2", sigma_char, 
                            sep = "")) +
      theme_classic() +
      theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold")) 
    
    sd3_plot <- ggplot(df, aes(x, y)) +
      geom_area(stat = 'function',
                fun = dnorm,
                fill = 'lightblue1',
                xlim = c(-3, 3),
                alpha = 0.75) +
      geom_line() +
      geom_vline(xintercept = tick_values[-4], 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = tick_values[4], 
                 linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = tick_values, labels = tick_labels) +
      labs(x = "", y = "",
           subtitle = paste("99.7% of data within", "\n",
                            mu_char, plus_minus, "3", sigma_char, 
                            sep = "")) +
      theme_classic() +
      theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(face = "bold"))
    
    
    graph8p <- grid.arrange(sd2_plot, sd3_plot,
                            nrow = 1, ncol = 2,
                            top = "The Empirical Rule with the Standard Normal Distribution")
    print(graph8p)
  })
  # graph 9 
  output$graph9 <- renderPlot({
    mon_x <- rnorm(100, mean = 140, sd = 20)
    samp_mean <- mean(mon_x)
    error_lines_added <- FALSE
    if (!error_lines_added) {
      graph9p <- ggplot(data = NULL, aes(mon_x)) +
        geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
        theme_minimal() +
        labs(x = "LDL (mmol/L)", y = "Number of Patients",
             title = "LDL Values from Monday",
             subtitle = "from 100 patients in clinic") + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5)) +
        scale_x_continuous(limits = c(70,220))
    } else {
      graph9p <- ggplot(data = NULL, aes(mon_x)) +
        geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = c(100, 180), color = "red", linewidth = 1.05, alpha = 0.6) +
        labs(x = "LDL (mmol/L)", y = "Number of Patients",
             title = "LDL Values from Monday",
             subtitle = "from 100 patients in clinic") + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5)) +
        scale_x_continuous(limits = c(70,220))
    }
    print(graph9p)
    
    observeEvent(input$add_lines_button, {
      if (!error_lines_added) {
        error_lines_added <<- TRUE
        output$graph9 <- renderPlot({
          graph9p <- ggplot(data = NULL, aes(mon_x)) +
            geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
            theme_minimal() +
            geom_vline(xintercept = c(100, 180), color = "red", linewidth = 1.05, alpha = 0.6) +
            labs(x = "LDL (mmol/L)", y = "Number of Patients",
                 title = "LDL Values from Monday",
                 subtitle = "from 100 patients in clinic") + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5)) +
            scale_x_continuous(limits = c(70,220))
          print(graph9p)
        })
      }
    })
    # Create an observer for the "Revert to Original Graph" button
    observeEvent(input$revert, {
      error_lines_added <<- FALSE
      output$graph9 <- renderPlot({
        # Original code for the graph without error_lines_added
        graph9p <- ggplot(data = NULL, aes(mon_x)) +
          geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
          theme_minimal() +
          labs(x = "LDL (mmol/L)", y = "Number of Patients",
               title = "LDL Values from Monday",
               subtitle = "from 100 patients in clinic") + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5)) +
          scale_x_continuous(limits = c(70, 220))
        print(graph9p)
      })
    })
    
  }) 
  # graph 10
  output$graph10 <- renderPlot({
    
    set.seed(123)
    mon_x <- rnorm(100, 140, 20)
    
    
    samp_mean <- mean(mon_x)
    
    Monday <- ggplot(data = NULL, aes(mon_x)) +
      geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "Number of Patients",
           title = "LDL Values from Monday",
           subtitle = "from 100 patients in clinic") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)) +
      scale_x_continuous(limits = c(70,220))
    Monday2 <- Monday +
      geom_vline(xintercept = c(100, 180),
                 color = "red", linewidth = 1.05, alpha = 0.6) 
    Monday2 +
      geom_vline(aes(xintercept = 140, linetype = "Population"),
                 color = "black") + 
      geom_vline(aes(xintercept = samp_mean, linetype = "Sample"), 
                 color = "red") +
      annotate(geom = "label",
               x = 80, y = 6.5, 
               label = paste("Mean LDL:", round(samp_mean, 2),
                             sep = "\n"),
               color = "red") +
      scale_linetype_manual(name = "Mean:",
                            values = c(5,1),
                            breaks = c("Population", "Sample"),
                            guide = guide_legend(override.aes = list(color = c("black", "red")))) +
      geom_vline(aes(xintercept = samp_mean), linewidth = 1.1,
                 color = "red") +
      theme(legend.title = element_text(face = "bold", hjust = 0.5))
  })
  # graph 11
  output$graph11 <- renderPlot({
    set.seed(123)
    mon_x <- rnorm(100, 140, 20)
    samp_mean <- mean(mon_x)
    Monday <- ggplot(data = NULL, aes(mon_x)) +
      geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "Number of Patients",
           title = "LDL Values from Monday",
           subtitle = "from 100 patients in clinic") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)) +
      scale_x_continuous(limits = c(70,220))
    Monday_simple <- ggplot(data = NULL, aes(mon_x)) +
      geom_histogram(fill = "#BBDEF0", col = "black", alpha = 0.8) +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = NULL,
           title = "Monday") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_x_continuous(limits = c(70,220)) +
      geom_vline(aes(xintercept = 140), 
                 color = "black", linetype = "longdash") + 
      geom_vline(aes(xintercept = samp_mean),
                 color = "red", linewidth = 1.1) 
    
    Monday_annotated <- Monday_simple +
      annotate(geom = "label",
               x = 80, y = 6.5, 
               label = paste("Mean LDL:", round(samp_mean, 2),
                             sep = "\n"))
    set.seed(11)
    tues_x <- rnorm(100, 140, 20)
    samp_mean_tues <- mean(tues_x)
    
    Tuesday_base <- ggplot(data = NULL, aes(tues_x)) +
      geom_histogram(fill = "#00A6A6", col = "black", alpha = 0.8) +
      theme_minimal() +
      scale_x_continuous(limits = c(70,220))
    
    Tuesday_annotated <- Tuesday_base +
      annotate(geom = "label",
               x = 80, y = 8, 
               label = paste("Mean LDL:", round(samp_mean_tues, 2),
                             sep = "\n")) +
      labs(x = "LDL (mmol/L)", y = "Number of Patients",
           title = "LDL Values from Tuesday",
           subtitle = "from 100 patients in clinic") + 
      geom_vline(aes(xintercept = samp_mean_tues, 
                     linetype = "Sample"),
                 color = "red") + 
      geom_vline(aes(xintercept = 140, linetype = "Population"),
                 color = "black") + 
      scale_linetype_manual(name = element_text("Mean:"),
                            values = c(5,1),
                            breaks = c("Population", "Sample"),
                            guide = guide_legend(override.aes = 
                                                   list(color = c("black", "red")))) + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(face = "bold"),
            legend.background = element_rect(color = "black"),
            legend.position = "bottom") +
      geom_vline(aes(xintercept = samp_mean_tues), linewidth = 1.1,
                 color = "red")
    
    
    
    
    Tuesday <- Tuesday_base +
      labs(x = "LDL (mmol/L)", y = NULL,
           title = "Tuesday") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      geom_vline(xintercept = 140, linetype = "longdash",
                 color = "black") + 
      geom_vline(xintercept = samp_mean_tues, linewidth = 1.1,
                 color = "red", linetype = "dashed")  
    
    
    
    Mon_Tues <- ggarrange(Monday_annotated, Tuesday_annotated,
                          common.legend = TRUE, legend = "bottom",
                          nrow = 2)
    
    Mon_Tues
  })
  # graph 12 
  output$graph12 <- renderPlot({
    observeEvent(input$button1, {
      output$graph12 <- renderPlot({
        data121 <- data.frame(x=rnorm(100, mean = 140, sd = 30))
        ggplot(data = data121, aes(x=x))+
          geom_histogram(col = "black", fill = "#EFCA08", bins = 20) +
          labs(x = "Values", title = "Monday", y = "Frequency")+
          xlim(50,250)+
          geom_vline(aes(xintercept = mean(data121$x)), col = "red")+
          geom_vline(aes(xintercept = 140), col = "black", linetype = "dashed")+
          annotate(geom = "label",
                   x = 80, y = 6.5, 
                   label = paste("Mean LDL:", round(mean(data121$x), 2),
                                 sep = "\n"))+
          theme_minimal()
        #hist(data, col = "#EFCA08", xlab = "Values", main = "Monday")
        #abline(v = 142.71, col = "red", lwd = 3)
        #abline(v = 140, col = "black", lwd = 4, lty = "dashed")
        #text(x = 143, y = 10, labels = "142.71", col = "red", pos = 4)
        #text(x = 125, y = 10, labels = "140", col = "black", pos = 4)
      })
    })
    
    observeEvent(input$button2, {
      output$graph12 <- renderPlot({
        data122 <- data.frame(x = rnorm(100, mean = 140, sd = 30))
        ggplot(data = data122, aes(x=x))+
          geom_histogram(col = "black", fill = "#F49F0A", bins = 20) +
          labs(x = "Values", title = "Tuesday", y = "Frequency")+
          xlim(50,250)+
          geom_vline(aes(xintercept = mean(data122$x)), col = "red")+
          geom_vline(aes(xintercept = 140), col = "black", linetype = "dashed")+
          annotate(geom = "label",
                   x = 80, y = 6.5, 
                   label = paste("Mean LDL:", round(mean(data122$x), 2),
                                 sep = "\n"))+
          theme_minimal()
        #hist(data, col = "#F49F0A", xlab = "Values", main = "Tuesday")
        #abline(v = 136.29, col = "red", lwd = 3)
        #abline(v = 140, col = "black", lwd = 4, lty = "dashed")
        #text(x = 115, y = 10, labels = "136.29", col = "red", pos = 4)
        #text(x = 140, y = 10, labels = "140", col = "black", pos = 4)
      })
    })
    
    observeEvent(input$button3, {
      output$graph12 <- renderPlot({
        data123 <- data.frame(x = rnorm(100, mean = 140, sd = 30))
        ggplot(data = data123, aes(x=x))+
          geom_histogram(col = "black", fill = "#F08700", bins = 20) +
          labs(x = "Values", title = "Wednesday", y = "Frequency")+
          xlim(50,250)+
          geom_vline(aes(xintercept = mean(data123$x)), col = "red")+
          geom_vline(aes(xintercept = 140), col = "black", linetype = "dashed")+
          annotate(geom = "label",
                   x = 80, y = 6.5, 
                   label = paste("Mean LDL:", round(mean(data123$x), 2),
                                 sep = "\n"))+
          theme_minimal()
      })
    })
    
    observeEvent(input$button4, {
      output$graph12 <- renderPlot({
        data124 <- data.frame(x = rnorm(100, mean = 140, sd = 30))
        ggplot(data = data124, aes(x=x))+
          geom_histogram(col = "black", fill = "#00A6A6", bins = 20) +
          labs(x = "Values", title = "Thursday", y = "Frequency")+
          xlim(50,250)+
          geom_vline(aes(xintercept = mean(data124$x)), col = "red")+
          geom_vline(aes(xintercept = 140), col = "black", linetype = "dashed")+
          annotate(geom = "label",
                   x = 80, y = 6.5, 
                   label = paste("Mean LDL:", round(mean(data124$x), 2),
                                 sep = "\n"))+
          theme_minimal()
      })
    })
    
    observeEvent(input$button5, {
      output$graph12 <- renderPlot({
        data125 <- data.frame(x = rnorm(100, mean = 140, sd = 30))
        ggplot(data = data125, aes(x=x))+
          geom_histogram(col = "black", fill = "#BBDEF0", bins = 20) +
          labs(x = "Values", title = "Friday", y = "Frequency")+
          xlim(50,250)+
          geom_vline(aes(xintercept = mean(data125$x)), col = "red")+
          geom_vline(aes(xintercept = 140), col = "black", linetype = "dashed", )+
          annotate(geom = "label",
                   x = 80, y = 6.5, 
                   label = paste("Mean LDL:", round(mean(data125$x), 2),
                                 sep = "\n"))+
          theme_minimal()
        #hist(data, col = "#BBDEF0", xlab = "Values", main = "Friday")
        #abline(v = 136.75, col = "red", lwd = 3)
        #abline(v = 140, col = "black", lwd = 4, lty = "dashed")
        #text(x = 143, y = 10, labels = "136.75", col = "red", pos = 4)
        #text(x = 125, y = 10, labels = "140", col = "black", pos = 4)
      })
    })
    
  })
  # graph 13 #need to add a line that is static at 140 and then make a dynamic line that updates for the mean of what is entered
  #also need to add a reset button
  data_sets <- reactiveValues(
    "Monday" = rnorm(100, 140, 20),
    "Tuesday" = rnorm(100, 140, 20),
    "Wednesday" = rnorm(100, 140, 20),
    "Thursday" = rnorm(100, 140, 20),
    "Friday" = rnorm(100, 140, 20)
  )
  # Initialize an empty reactiveVal to store selected days
  selected_days <- reactiveVal(character(0))
  
  output$graph13 <- renderPlot({
    # Get the selected days
    selected_data_sets <- input$data_sets
    
    # Update the selected days when the input changes
    observeEvent(selected_data_sets, {
      selected_days(selected_data_sets)
    })
    
    if (length(selected_days()) > 0) {
      df <- data.frame(values = NULL)
      
      for (data_set in selected_days()) {
        data <- data_sets[[as.character(data_set)]]
        df <- rbind(df, data.frame(values = data))
      }
      static_x <- 140  # Static x-axis value
      
      graph13p <- ggplot(df, aes(x = values)) +
        geom_histogram(fill = "lightblue", color = "black") +
        scale_x_continuous(limits = c(70, 220)) +
        labs(x = "Values", y = "Frequency", title = "Weekday Histogram") +
        geom_vline(xintercept = static_x, color = "black", linetype = "dashed", size = 2) +
        geom_vline(xintercept = mean(df$values), color = "red", linetype = "solid") +
        annotate(geom = "label",
                 x = 80, y = 6.5, 
                 label = paste("Mean LDL:", round(mean(df$values), 2), sep = "\n"))
      
      print(graph13p)
    }
  })
  # graph 14
  output$graph14 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    pop <- rnorm(10000, 140, 20)
    pop_y <- dnorm(pop, 140, 20)
    
    pop_plot_base <- ggplot(data = NULL, aes(pop, pop_y)) + 
      geom_line() +
      geom_ribbon(data = NULL,
                  aes(ymin = 0, ymax = pop_y), 
                  alpha = 0.8, fill = "#F2D0A4") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Population LDL") +
      geom_vline(xintercept = 140, color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(limits = c(70, 210),
                         breaks = c(80, 100, 120, 140, 160,
                                    180, 200))
    
    pop_plot <- pop_plot_base +
      annotate(geom = "label", x = 140, y = 0.002,
               label = mu_char, color = "darkred")
    
    set.seed(13454)
    samp <- rnorm(100, 140,2)
    
    
    samp_plot <- ggplot(data = NULL, aes(samp)) +
      geom_histogram(color = "black", alpha = 0.8,
                     fill = "#FFECCC") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Sample LDL",
           subtitle = "From 100 Patients") +
      geom_vline(xintercept = mean(samp), color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = c(134, 137, 140, 143, 146),
                         limits =c(134, 146)) +
      annotate(geom = "label", x = mean(samp), y = 1,
               color = "darkred", label ="bar(x)",
               parse = TRUE)
    
    
    
    graph14p <- grid.arrange(pop_plot, samp_plot, ncol = 2)
    print(graph14p)
  })
  # graph 15
  output$graph15 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    pop <- rnorm(10000, 140, 20)
    pop_y <- dnorm(pop, 140, 20)
    
    pop_plot_base <- ggplot(data = NULL, aes(pop, pop_y)) + 
      geom_line() +
      geom_ribbon(data = NULL,
                  aes(ymin = 0, ymax = pop_y), 
                  alpha = 0.8, fill = "#F2D0A4") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Population LDL") +
      geom_vline(xintercept = 140, color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(limits = c(70, 210),
                         breaks = c(80, 100, 120, 140, 160,
                                    180, 200))
    
    pop_plot <- pop_plot_base +
      annotate(geom = "label", x = 140, y = 0.002,
               label = mu_char, color = "darkred")
    
    set.seed(13454)
    samp <- rnorm(100, 140,2)
    set.seed(3)  
    random_numbers <- rnorm(100, mean = 140, sd = 2)
    samp_mean <- round(mean(random_numbers), 1)
    
    samp_plot2 <- ggplot(data = NULL, aes(random_numbers)) +
      geom_histogram(color = "black", alpha = 0.8,
                     fill = "#FFECCC") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Sample LDL",
           subtitle = "From 100 Patients") +
      geom_vline(xintercept = mean(samp), color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = c(134, 137, 140, 143, 146),
                         limits =c(134, 146)) +
      annotate(geom = "label",
               x = 136, y = 7,
               label = paste("bar(x) ~`=`~ ",samp_mean,
                             sep = ""),
               parse = TRUE)
    
    pop_plot2 <- pop_plot_base +
      annotate(geom = "label",
               x = 137, y = 0.01,
               label = paste(mu_char, " = 140",
                             sep = ""))
    
    graph15p <- grid.arrange(pop_plot2, samp_plot2, ncol = 2)
    print(graph15p)
  })
  # graph 16
  output$graph16 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    pop <- rnorm(10000, 140, 20)
    pop_y <- dnorm(pop, 140, 20)
    
    pop_plot_base <- ggplot(data = NULL, aes(pop, pop_y)) + 
      geom_line() +
      geom_ribbon(data = NULL,
                  aes(ymin = 0, ymax = pop_y), 
                  alpha = 0.8, fill = "#F2D0A4") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Population LDL") +
      geom_vline(xintercept = 140, color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(limits = c(70, 210),
                         breaks = c(80, 100, 120, 140, 160,
                                    180, 200))
    
    pop_plot <- pop_plot_base +
      annotate(geom = "label", x = 140, y = 0.002,
               label = mu_char, color = "darkred")
    
    set.seed(13454)
    samp <- rnorm(100, 140,2)
    
    
    samp_plot <- ggplot(data = NULL, aes(samp)) +
      geom_histogram(color = "black", alpha = 0.8,
                     fill = "#FFECCC") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Sample LDL",
           subtitle = "From 100 Patients") +
      geom_vline(xintercept = mean(samp), color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = c(134, 137, 140, 143, 146),
                         limits =c(134, 146)) +
      annotate(geom = "label", x = mean(samp), y = 1,
               color = "darkred", label ="bar(x)",
               parse = TRUE)
    
    set.seed(3)  
    random_numbers <- rnorm(100, mean = 140, sd = 2)
    
    samp_mean <- round(mean(random_numbers), 1)
    
    samp_plot2 <- ggplot(data = NULL, aes(random_numbers)) +
      geom_histogram(color = "black", alpha = 0.8,
                     fill = "#FFECCC") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Sample LDL",
           subtitle = "From 100 Patients") +
      geom_vline(xintercept = mean(samp), color = "darkred",
                 linewidth = 1.05) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = c(134, 137, 140, 143, 146),
                         limits =c(134, 146)) +
      annotate(geom = "label",
               x = 136, y = 7,
               label = paste("bar(x) ~`=`~ ",samp_mean,
                             sep = ""),
               parse = TRUE)
    
    pop_plot2 <- pop_plot_base +
      annotate(geom = "label",
               x = 137, y = 0.01,
               label = paste(mu_char, " = 140",
                             sep = ""))
    sd_pop <- ggplot(data = NULL, aes(pop, pop_y)) + 
      geom_line() +
      geom_ribbon(data = NULL,
                  aes(ymin = 0, ymax = pop_y), 
                  alpha = 0.8, fill = "#F2D0A4") +
      theme_minimal() +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Population LDL") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) + 
      geom_segment(aes(x = 120,xend = 160,
                       y=dnorm(120, 140, 20),
                       yend=dnorm(120, 140, 20)),
                   color = "darkred", linewidth = 1.05) +
      annotate(geom = "label",
               x = 140, y = 0.008,
               label = paste("sigma[LDL] ~`=`~ ", "20",
                             sep = ""),
               parse = TRUE) +
      scale_x_continuous(limits = c(70, 210),
                         breaks = c(80, 100, 120, 140, 160,
                                    180, 200))
    
    set.seed(123)
    rand_samp <- rnorm(1000, 140, 2.1)
    rand_samp_y <- dnorm(rand_samp, 140, 2.1)
    
    sd_samp <- ggplot(data = NULL, aes(rand_samp)) +
      theme_minimal() +
      geom_histogram(alpha = 0.8, fill = "#FFECCC", 
                     color = "black") +
      labs(x = "LDL (mmol/L)", y = "",
           title = "Distribution of Sample LDL",
           subtitle = "From 100 Patients") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y=element_blank(),  
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = c(134, 137, 140, 143, 146),
                         limits =c(134, 146)) +
      geom_segment(aes(x = mean(rand_samp) - sd(rand_samp),
                       xend = mean(rand_samp) + sd(rand_samp),
                       y=58,
                       yend=58),
                   color = "darkred", linewidth = 1.5) +
      annotate(geom = "label", x = 140, y = 32,
               label = paste("sigma[LDL[100]] ~`=`~ ", "2.1",
                             sep = ""),
               parse = TRUE) +
      geom_segment(aes(x = c(mean(rand_samp)-sd(rand_samp),
                             mean(rand_samp)+sd(rand_samp)),
                       xend = c(mean(rand_samp)-sd(rand_samp),
                                mean(rand_samp)+sd(rand_samp)),
                       y=54.5,
                       yend=61.5),
                   color = "darkred", linewidth = 1.5) 
    
    
    graph16p <- grid.arrange(sd_pop, sd_samp, ncol = 2)
    print(graph16p)
  })
  #graph16.5
  output$graph165 <- renderPlot({
    set.seed(123456789)
    mean_vec <- rep(NA, 100)
    for (i in 1:100) {
      mean_vec[i] <- mean(rnorm(100, 140, 20))
    }
    
    set.seed(3)
    x <- rnorm(1000, 140, 20)
    y <- dnorm(x, 140, 20)
    df <- data.frame(x, y)
    
    
    
    mean_plot <- ggplot(data = NULL, aes(mean_vec)) +
      geom_histogram(color = "black", fill = "#478978",
                     alpha = 0.8) +
      labs(x = "Daily Mean LDL (mmol/L)", y = "Frequency",
           caption = "Population Mean: 140") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, 
                                        color = "#95190C",
                                        size = 12)) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      scale_x_continuous(limits = c(132, 148)) 
    
    
    bottom = text_grob("The above figure uses 100 clinic days, each of which has 100 patients", 
                       size = 11, face = "bold")
    
    overlay <- ggplot(NULL, aes(mean_vec)) +
      geom_histogram(aes(y=after_stat(density)), 
                     fill = "#478978",
                     color = "black",
                     alpha = 0.8) +
      geom_line(aes(y=after_stat(density)), stat = "density",
                linewidth = 1.1) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      scale_x_continuous(limits = c(132, 148)) +
      labs(x = "Daily Mean LDL (mmol/L)", y = NULL,
           caption = "Population Mean: 140") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, 
                                        color = "#95190C",
                                        size = 12),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    tg <- textGrob('Distribution of Mean LDL Values', 
                   gp = gpar(fontsize = 13, fontface = 'bold'))
    sg <- textGrob('from 100 Clinic Days', 
                   gp = gpar(fontsize = 10, fontface = "bold"))
    margin <- unit(0.5, "line")
    
    grided <- gridExtra::grid.arrange(mean_plot,
                                      overlay, ncol = 2)
    gridExtra::grid.arrange(tg, sg, grided,
                            heights = unit.c(grobHeight(tg) + 1.2*margin, 
                                             grobHeight(sg) + margin, 
                                             unit(1,"null")),
                            bottom = bottom)
    
  })
  output$graph1651 <- renderPlot({
    set.seed(123456789)
    mean_vec <- rep(NA, 100)
    for (i in 1:100) {
      mean_vec[i] <- mean(rnorm(100, 140, 20))
    }
    
    set.seed(3)
    x <- rnorm(1000, 140, 20)
    y <- dnorm(x, 140, 20)
    df <- data.frame(x, y)
    
    
    
    mean_plot <- ggplot(data = NULL, aes(mean_vec)) +
      geom_histogram(color = "black", fill = "#478978",
                     alpha = 0.8) +
      labs(x = "Daily Mean LDL (mmol/L)", y = "Frequency",
           caption = "Population Mean: 140") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, 
                                        color = "#95190C",
                                        size = 12)) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      scale_x_continuous(limits = c(132, 148)) 
    
    
    bottom = text_grob("The above figure uses 100 clinic days, each of which has 100 patients", 
                       size = 11, face = "bold")
    
    overlay <- ggplot(NULL, aes(mean_vec)) +
      geom_histogram(aes(y=after_stat(density)), 
                     fill = "#478978",
                     color = "black",
                     alpha = 0.8) +
      geom_line(aes(y=after_stat(density)), stat = "density",
                linewidth = 1.1) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      scale_x_continuous(limits = c(132, 148)) +
      labs(x = "Daily Mean LDL (mmol/L)", y = NULL,
           caption = "Population Mean: 140") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, 
                                        color = "#95190C",
                                        size = 12),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    tg <- textGrob('Distribution of Mean LDL Values', 
                   gp = gpar(fontsize = 13, fontface = 'bold'))
    sg <- textGrob('from 100 Clinic Days', 
                   gp = gpar(fontsize = 10, fontface = "bold"))
    margin <- unit(0.5, "line")
    
    grided <- gridExtra::grid.arrange(mean_plot,
                                      overlay, ncol = 2)
    gridExtra::grid.arrange(tg, sg, grided,
                            heights = unit.c(grobHeight(tg) + 1.2*margin, 
                                             grobHeight(sg) + margin, 
                                             unit(1,"null")),
                            bottom = bottom)
    total <- ggplot(data = NULL, aes(x = mean_vec)) +
      geom_histogram(aes(y=after_stat(density)), 
                     fill = "#478978",
                     color = "black",
                     alpha = 0.8, bins = 155) +
      geom_line(data = df, aes(x = x, y = y), 
                color = "#01231B",
                linewidth = 1.1) +
      scale_y_continuous(limits = c(0, 0.18)) +
      scale_x_continuous(limits = c(90, 190),
                         breaks = seq(100, 180, 20)) +
      theme_minimal() +
      labs(y = "Density", x = "LDL (mmol/L)",
           title = paste("Distribution of Mean LDL Values", 
                         "vs. Population LDL", 
                         sep = "\n"),
           caption = "Population Mean: 140") +
      theme(plot.title = element_text(hjust = 0.5, 
                                      face = "bold"),
            plot.caption = element_text(hjust = 0.5, 
                                        color = "#95190C",
                                        size = 11)) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      annotate("segment", x = 110, xend = 120, 
               y = 0.04, yend = 0.02,
               colour = "#01231B", size = 1, 
               arrow = arrow(type = "closed", 
                             length = unit(0.02, "npc"))) +
      annotate("segment", x = 155, xend = 145, 
               y = 0.12, yend = 0.1,
               colour = "#478978", size = 1, 
               arrow = arrow(type = "closed", 
                             length = unit(0.02, "npc"))) +
      annotate("label", x = 100, y = 0.05,
               label = paste("Distribution of", "Population LDL",
                             "for individual patients",
                             sep = "\n"),
               color = "#01231B") +
      annotate("label", x = 167, y = 0.13,
               label = paste("Distribution of", 
                             "Mean LDL Values",
                             "across 100 samples",
                             sep = "\n"),
               color = "#478978")
    total
  })
  # graph 17
  output$graph17 <- renderPlot({
    mean_vec <- rep(NA, 100)
    
    
    for (i in 1:100) {
      mean_vec[i] <- mean(rnorm(100, 140, 30))
    }
    
    #Slider Input:
    ggplot(data = NULL, aes(mean_vec[1:input$mu])) +
      geom_dotplot(color = "black", fill = "#478978",
                   alpha = 0.8, method = "histodot") +
      labs(x = "Daily Mean LDL (mmol/L)", y = "Frequency",
           title = paste("Distribution of Mean LDL Values from",
                         input$mu, "Clinic Days")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, 
                                      face = "bold")) +
      geom_vline(xintercept = 140,
                 color = "#95190C", linewidth = 1.1) +
      scale_x_continuous(limits = c(132, 148))
  })
  # graph 18
  output$graph18 <- renderPlot({
    sigma_char <- "\u03C3"
    mu_char <- "\u03BC"
    x <- seq(65, 215, by = 0.1)
    y <- dnorm(x, mean = 140, sd = 20)
    y2 <- dnorm(x, mean = 140, sd = 20/sqrt(input$n))
    
    df <- data.frame(x = x, y = y, y2 = y2)
    
    
    tick_values <- seq(80, 200, by = 20)
    
    
    ggplot(df, aes(x, y)) +
      geom_line(aes(color = "#618B25"), linewidth = 1.05) +
      geom_line(aes(y = y2, color = "#3993DD"), linewidth = 1.05) +
      scale_color_identity(guide = "legend",
                           name = "Distribution",
                           breaks = c("#618B25", "#3993DD"),
                           labels = c("Population", "Sample")) +
      geom_vline(xintercept = c(80, 100, 120, 160, 180, 200), 
                 linetype = "dashed", color = "gray") +
      geom_vline(xintercept = 140, 
                 linetype = "longdash", color = "darkred") +
      scale_x_continuous(breaks = tick_values) +
      labs(title = "Population vs. Sampling Distributions",
           x = "LDL (mmol/L)", y = "",
           subtitle = paste("Population: N(",
                            mu_char,"=140, ", sigma_char,
                            "=20)", "\n",
                            "Sample: N(", mu_char,
                            "=140, ", sigma_char, 
                            "=20/", round(sqrt(input$n), 3), ")", sep="")) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.background = element_rect(color = "black"))
  })
  # table symbol key
  output$symbol_key <- renderImage({
    list(src = "www/symbol_key.png",
         width = "100%")
  }, deleteFile = FALSE)
  
}
