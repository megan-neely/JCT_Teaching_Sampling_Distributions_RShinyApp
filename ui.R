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

# Define the UI
fluidPage(
  #allow Latex in App
  tags$head(
    tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
  ),
  #setting the background color
  setBackgroundColor("ivory"),
  # Application title
  titlePanel(strong("Sampling Distribution")),
  #Creating Instruction tab
  # Create the tabs
  tabsetPanel(
    #creating an instructions tab
    tabPanel( "Instructions",
              h2("Instructions"),
              p("This is instructions on how to go about this website/app."),
              p("Please go through this from left to right in order of the tabs."),
              p("When there are options/sliding bars please go ahead and mess around with them to help gather a better understanding of the concept."),
              p("Happy Learning!")
    ),
    # First tab content
    tabPanel("Fixed Characteristics", #this is the name of the tab
             h2("Central Tendency and Variability"), #this is a subheading 
             #p() is the writting before the plot
             p("Central tendency and variability are fixed characteristics of a population. 
               Central tendency is often quantified by the mean, \u03BC, and the variability is often quantified by the standard deviation, \u03C3."),
             fluidRow(
               #tags$h1("title", style = "font-size: 20px; text-align: center;"),
               
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph1")
                 )
               )
             ),
             #p() is the writting below the plot
             h2("Standard Deviation (Variability)"),
             p("As an intuitive demonstration that \u03C3 quantifies variability, plugging identical LDL values into the formula for \u03C3 leads to a value of 0 - any other data will lead to positive values of \u03C3. 
               The greater the amount of variation, the larger will be the value of \u03C3. Here, \u03BC = 0"),
             fluidRow(
               #tags$h1("title", style = "font-size: 20px; text-align: center;"),
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("sd", "Standard Deviation:", min = 0.1, max = 5, value = 1, step = 0.1)
                 ),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph2")
                   )
                 )
               ),
               h2("Mean (Central Tendency)"),
               p("Graphically, increasing the mean of a normal distribution shifts it to the right, but without changing the amount of spread. Vice versa if you decrease the mean shifts the distribution to the left."),
               fluidRow(
                 #tags$h1("title", style = "font-size: 20px; text-align: center;"),
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("mean", "Mean:", min = -10, max = 10, value = 0, step = 0.1)
                   ),
                   mainPanel(
                     div(style = "text-align: center;",
                         plotOutput("graph3")
                     )
                   )
                 )
               ),
               h2("Distribution (Shape)"),
               p("Because they are fixed characteristics of a population, neither \u03BC or \u03C3 are related to sample size. For example, the LDL values of a population of patients might have \u03BC = 140 and \u03C3 = 20. Another fixed characteristic of a population is its shape, which is often operationalized through a statistical distribution. For example, although this is a simplifying assumption because of the lack of a perfect bell shape, in some circumstances, it might be reasonable to assume that LDL has a normal (i.e., bell-shaped) distribution. 
                 For our purposes, we will use histograms to visualize our sample and smooth curves (i.e., density curves) to visualize our population."),
               fluidRow(
                 #tags$h1("title", style = "font-size: 20px; text-align: center;"),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph4")
                   )
                 )
               ),
               h2("Changes in SD and Mean"),
               p("Now see what happens to the spread when you change the mean and the standard deviation.
                 Notice that changing the mean of a normal distribution shifts it along the x-axis without changing the spread.
                 Then changing the standard deviation either increases or decreases the spread of the normal distribution without changing its center."),
               fluidRow(
                 #tags$h1("title", style = "font-size: 20px; text-align: center;"),
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("sd1", "Standard Deviation:", min = 0.1, max = 5, value = 1, step = 0.1),
                     sliderInput("mean1", "Mean", min = 0, max = 5, value = 1, step = 1)
                   ),
                   mainPanel(
                     div(style = "text-align: center;",
                         plotOutput("graph5")
                     )
                   )
                 )
               )
             )
    ),#tabpanel
    #creating the second panel
    tabPanel("Why Does It Matter",
             h2("Why Does It Matter"),
             p("One reason we care about \u03C3 is that, in conjunction with \u03BC, it helps to predict the likely range of LDL values you are likely to see in your clinic. More specifically, regardless of the shape of the data, 75% of the values should fall within 2 standard deviations of the mean (i.e., between \u03BC - 2 \u03C3 and \u03BC + 2 \u03C3) and 89% of values should fall within 3 standard deviations of the mean (Chebyshev's inequality). So, for example, if \u03BC = 140 and \u03C3 = 20, then 95% of LDL values should fall between 100 and 180."),
             fluidRow(
               #tags$h1("title", style = "font-size: 20px; text-align: center;"),
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph6")
                 )
               )
             ), #fluid Row 
             #fluidRow(
               #tags$h1("title", style = "font-size: 20px; text-align: center;"),
             #  mainPanel(
             #    div(style = "text-align: center;",
             #        plotOutput("graph7")
             #    )
             #  )
             #), #fluid Row
             #h2("Gaussian Distribution"),
             p("This range is even tighter if the distribution is normal (i.e., bell-shaped): 95% of observations should fall within 2 standard deviations of the mean and 99+% of observations should fall within 3 standard deviations. So, for example, if \u03BC=140 and \u03C3=20, then 95% of LDL values for your patients should fall between 100 and 180."),
             fluidRow(
               #tags$h1("title", style = "font-size: 20px; text-align: center;"),
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph8")
                 )
               )
             ) #fluid Row
    ),#tabpannel
    tabPanel("LDL Example",
             h2("LDL Clinical Week Example"),
             p("Suppose that LDL is normal with \u03BC=140 and \u03C3=20, and also that your clinic sees 100 patients per day. On Monday, you expect that approximately 95% of patients will have LDL values between 100 and 180 (i.e., the 2.5th and 97.5th percentiles, respectively). For any patient, though, the LDL value cannot be predicted ahead of time."),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   actionButton("add_lines_button", "Add Error Lines"),
                   actionButton("revert", "Reset")
                 ),
                 mainPanel(
                   div(style = "tex-align: center;",
                       plotOutput("graph9"))
                 )
               )
             ),#fluidRow
             p("You are not only interested in the characteristics of individual patients, but also in the average (i.e., mean) LDL value for any particular day. Perhaps calculating mean LDL is part of your standard clinic reporting -- for example, to verify that your clinic population as a whole is at high risk for cardiovascular disease. At the close of business on Monday, you take the 100 LDL values, add them, then divide by 100 to calculate the mean LDL for the day. It ought to be near the population mean of 140 (annotated below with the black dotted line), but probably will not equal 140 exactly. In our visual below, the sample mean is marked by the red line."),
             fluidRow(
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph10"))
               )
             ),#fluidrow
             p("You can do the same thing for the 100 patients seen on Tuesday. The mean LDL value for Tuesday will also probably be near 140, but not equal to 140 exactly, nor will it exactly equal Monday's value. As above, the population mean is marked by the black dotted line while the sample mean is marked by the red line."),
             fluidRow(
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph11"))
               )
             ),#fluidRow
             p("This process could be continued across multiple days. The empirical results could be summarized by a histogram, the first entry to which is Monday's mean LDL value, the second entry to which is Tuesday's mean LDL value, etc. As above, the population mean is marked by the black dotted line while the sample mean is marked by the red line."),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   column(12, actionButton("button1", "Monday")),
                   column(12, actionButton("button2", "Tuesday")),
                   column(12, actionButton("button3", "Wednesday")),
                   column(12, actionButton("button4", "Thursday")),
                   column(12, actionButton("button5", "Friday"))
                 ),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph12"))
                 )
               )
             ),#fluidRow
             p("It is intuitively reasonable (and also true) that the distribution of daily mean LDL values should be centered around 140. The histogram of mean LDL values, the first five values of which are generated from Monday through Friday, is a visual representation of a “sampling distribution”."),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("data_sets", "Select Data Sets", choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
                 ),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph13"))
                 )
               )
             ),
             p("The more days we observe, the more values we can add to the histogram, which might be termed the 
             “empirical sampling distribution” or “estimated sampling distribution,” and the closer we come 
             to the actual (but unobservable) sampling distribution."),
             #fluidRow
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("mu", "Number of Means:", min = 1, max = 100, value = 10, step = 1)
                 ),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph17"))
                 )
               )
             ),#fluidrow
    ),#tabpanel
    #add in the dot growth plot here
    tabPanel("More on Distributions",
             h2("More on Distributions"), #Need to re-do this whole section
             #p("We previously described the distribution of patient-specific LDL values. The unit of reporting (and analysis) was the LDL value for an individual patient. For example, we used information about this distribution to predict how variable LDL values will be across patients, the ranges within which LDL values are likely to fall, etc. Because LDL values for specific patients can't be precisely predicted ahead of time, patient-specific LDL values are a random variable. We can denote this random variable using X. Random variables have distributions, such as the bell-shaped normal distribution assumed here. Random variables also have parameters such as the mean, \u03BC, and the standard deviation, \u03C3. A parameter is a characteristic of a distribution, such as its center and spread."),
             #p("The parameters \u03BC and \u03C3 refer to the mean and spread of the distribution of LDL values in the population. As such, they aren't directly observable to the analyst. What is observable is the sample mean and sample standard deviation, which are denoted by verline{X} and S, respectively. Sometimes, for clarity, we will add the sample size to our notation. For example, we would use verline{X}_{100} and S_{100} to emphasize that what we've observed is based on a sample size of 100. See the symbol key at the end of this text for further clarification on notation."),
             #p("We also described the distribution of mean LDL values for samples (of size 100) taken at different clinic days. The unit of reporting (and analysis) is the observed mean LDL (of a sample of size 100) for a clinic day. The mean LDL values for specific clinic days can't be precisely predicted ahead of time, and so these are also a random variable. As a random variable, it has a distribution, a population mean, and a population standard deviation. To emphasize that the unit of analysis is the clinic day rather than the individual patient, we term it a sampling distribution."),
             #p("In summary, we have described two distributions which are distinct but related: (1) the (underlying) distribution of patient-specific LDL values; and (2) the distribution of the mean LDL values of samples of 100 patients (i.e., the sampling distribution).Next, we will describe some notation which allows us to speak precisely about these distributions --- for example, to distinguish between the mean of the underlying distribution and the mean of the sampling distribution, to distinguish between the standard deviation of the underlying distribution and the standard deviation of the sampling distribution, etc."),
             #fluidRow(
             #  mainPanel(
             #    div(style = "text-align: center;",
             #        plotOutput("graph14"))
             #  )
             #),#fluidrow
             #p("To comment: one reason clinicians might have difficulty conceptualizing sampling distributions is the difficulty in distinguishing μLDL 
             #from x¯¯¯LDL100 in that they happen to have the same value. As above, μLDL represents the mean LDL value for the population as a whole 
             #while x¯¯¯LDL100 represents the mean LDL for our sample of 100 patients."),
             #fluidRow(
             #  mainPanel(
             #    div(style = "text-align: center;",
             #        plotOutput("graph15"))
             #  )
             #),#fluidrow
             #p("The distribution of daily mean LDL values also has a standard deviation, which we will denote as σLDL100, in order to distinguish it from σLDL 
             #(which was previously denoted by σ). As terminology, σLDL100 is called the “standard error of the sample mean (from 100 patients per day),” 
             #or just the “standard error” for short. Unlike the sample mean, σLDL100 is a function of both the sample size and the population standard deviation, 
             #σLDL, rather than sample size alone."),
             #p("Moreover, as the number of patients per clinic day increases, σLDL100 decreases. In fact σLDL100=σLDL10, 
             #where 10 is the square root of 100, the number of clinic patients per day. σn=σ2n−−−√=σn−−√⟹σLDL100=σ2LDL100−−−−−√=σLDL10"),
             #fluidRow(
             #  mainPanel(
             #    div(style = "text-align: center;",
             #        plotOutput("graph16"))
             #  )
             #),#fluidrow
             p("The left plot illustrates the histogram (of the sample means, each of size $100$), for $100$ clinic days. 
               It is an empirical estimate of the sampling distribution (of sample means, each of size $100$), which can't be directly observed. 
               The plot on the right overlays this distribution (i.e., the smooth curve) on the empirical estimate of the sampling distribution. 
               We refer to the smooth curve as the theoretical sampling distribution."),
             fluidRow(
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph165"))
               )
             ),
             p("The bottom plot above combines the top two plots to highlight that there are two different distributions being considered: 
             the distribution of individual LDL values (i.e. $X$, the distribution of population LDL) and the distribution of 
             sample mean LDL values (each of size 100, i.e. $\\bar{X}_{100}$). To recapitulate, the distribution of individual LDL values uses the 
               patient as the unit of analysis, whereas the distribution of sample mean LDL values uses the sample as the unit of analysis. 
               The distribution of sample mean LDL values has less spread than the distribution of individual LDL values."),
             #fluidrow
             fluidRow(
               mainPanel(
                 div(style = "text-align: center;",
                     plotOutput("graph1651"))
               )
             ),#fluidrow
             p("We previously described the distribution of patient-specific LDL values. The unit of reporting (and analysis) was the LDL value 
             for an individual patient. For example, we used information about this distribution to predict how variable LDL values will be across 
             patients, the ranges within which LDL values are likely to fall, etc. Because LDL values for specific patients can’t be precisely 
             predicted ahead of time, patient-specific LDL values are a “random variable.” We can denote this random variable using $X$. 
             Random variables have distributions, such as the bell-shaped normal distribution assumed here. Random variables also have parameters such 
             as the mean, $\\mu$, and the standard deviation, $\\sigma$. A parameter is a characteristic of a distribution, such as its center and spread. 
             The parameters $\\mu$ and $\\sigma$ refer to the mean and spread of the distribution of LDL values in the population. As such, they aren’t directly 
             observable to the analyst. What is observable is the sample mean and sample standard deviation, which are denoted by $\\bar{X}$ and $S$, respectively. 
             Sometimes, for clarity, we will add the sample size to our notation. For example, we would use $\\bar{X}_{100}$ and $S_{100}$ to emphasize that what we’ve 
             observed is based on a sample size of $100$. See the symbol key at the end of this text for further clarification on notation. We also described 
             the distribution of mean LDL values for samples (of size $100$) taken at different clinic days. The unit of reporting (and analysis) is the 
             observed mean LDL (of a sample of size $100$) for a clinic day. The mean LDL values for specific clinic days can’t be precisely predicted ahead of time, 
             and so these are also a random variable. As a random variable, it has a distribution, a population mean, and a population standard deviation. 
             To emphasize that the unit of analysis is the clinic day rather than the individual patient, we term it a “sampling distribution.” 
             In summary, we have described two distributions which are distinct but related: (1) the (underlying) distribution of patient-specific LDL values; 
             and (2) the distribution of the mean LDL values of samples of $100$ patients (i.e., the sampling distribution). Next, we will describe some 
             notation which allows us to speak precisely about these distributions - for example, to distinguish between the mean of the underlying distribution 
             and the mean of the sampling distribution, to distinguish between the standard deviation of the underlying distribution and the standard deviation 
             of the sampling distribution, etc."),
             p("The mean LDL value for any particular day is: $\\bar{X}_{n} = \\frac{\\sum_{i=1}^{n} X_{i}}{n}$. Since you see $100$ patients per day, $n=100$, and this formula becomes:
             $\\bar{X}_{100} = \\frac{X_{1} + X_{2} + ... + X_{99} + X_{100}}{100}$. The standard deviation for a particular clinic day is: $S =$ $ \\sqrt{\\frac{(X_{1} - \\bar{X}_{100})^{2} + (X_{2} - \\bar{X}_{100})^2 + ... + (X_{100} - \\bar{X}_{100})^{2}}{99}}$ 
             Here, $\\bar{X}_{n}$ estimates $\\mu$ and $S$ estimates $\\sigma$. $\\bar{X}$ and $S$ can be calculated using data from a single clinic day."),
             p(HTML("Much of statistical inference is concerned with properties of the sampling distribution of $\\bar{X}$ - especially, 
             how to use $\\bar{X}_{100}$ to estimate the unknown population mean, $\\mu$. As previously mentioned, since $\\bar{X}_{100}$ will differ 
             from one clinic day to another, it is a random variable, and, thus, has its own population mean and standard deviation. 
             We’ll denote this by $\\mu_{\\bar{x}_{100}}$ and $\\sigma_{\\bar{X}_{100}}$.")),
             p("Moreover, as the number of patients per clinic day increases, $\\sigma_{\\bar{X}_{100}}$ decreases. In fact 
             $\\sigma_{\\bar{X}_{100}} = \\frac{\\sigma}{10}$,
             where $10$ is the square root of $100$, the number of clinic patients per day. However, since $\\sigma$ is an unknown population parameter, we can use $S$
             to estimate the standard deviation of individual LDL values as in 16 above. Therefore we have:  
             $\\sigma = \\sqrt{\\frac{\\sigma^{2}}{n}} = \\frac{\\sigma}{\\sqrt{n}} \\Rightarrow \\sigma_{\\bar{X}_{100}} = \\sqrt{\\frac{\\sigma^{2}}{100}} = \\frac{\\sigma}{10} \\Rightarrow \\sigma_{\\bar{X}_{100}} = \\sqrt{\\frac{S^{2}}{100}} = \\frac{S}{10}$
             This formula provides the crucial link from a single clinic day to the sampling distribution. Our best guess about $\\mu_{\\bar{X}_{100}}$ is $\\bar{X}$. 
             Our best guess about $\\sigma_{\\bar{X}_{100}}$ is $\\frac{S}{\\sqrt{n}}$. $\\bar{X}$, $S$, and $n$ can all be observed using data from a single clinic day, 
             and so we can use data from a single clinic day to make an educated guess about the parameters of the sampling distribution."),
             p("It also turns out that the mean LDL per clinic day (calculated using $100$ patients per day) has a normal shape, 
             and so it can be fully described as normal with mean $140$ and standard deviation $2$. Because any particular day’s mean LDL 
             can’t be precisely predicted ahead of time, it is a random variable. Moreover, it turns out that this sampling distribution 
             (i.e., of the means of repeated samples, each taking $100$ patients within a single clinic day) has a normal shape, and so we 
             can apply the result that $95%$ of daily values will fall within $\\mu_{ \\bar{X}_{100}} \\plusmn 2* \\sigma_{ \\bar{X}_{100}}$."),
             p("In fact, for $95%$ of days, the mean LDL per clinic day should fall within $136$ and $144$."),
             p("o recapitulate, what if you only had data from Monday and want to derive the above sampling distribution? On first blush, this seems impossible, 
             since all you have is a single data point. Suppose that value is $139$, which we anticipate will be near but not exactly equal to the unknown mean value 
             of $\\mu_{LDL} = 140$. We can also calculate a standard deviation from the $100$ patients, which estimates $\\sigma_{LDL}$. Suppose that value happens to be $21$, 
             which we anticipate will be near but not exactly equal to the unknown value of $\\sigma_{LDL} = 20$."),
             p("This is where statistical theory comes to the rescue with $3$ crucial results."),
             p("The first crucial result was mentioned before – namely that $\\mu = \\mu_{ \\bar{X}_{100}}$. So, based on the data from Monday, we estimate $\\mu_{ \\bar{X}_{100}}$ to be $139$."),
             p("The second crucial result is that $S_{ \\bar{X}_{100}} = \\frac{ \\sigma}{ \\sqrt{100}}$, where $100$ is the sample size. So, based on the data form Monday, 
               we estimate that $S_{ \\bar{X}_{100}} = \\frac{21}{ \\sqrt{100}} = 2.1$"),
             p("The third critical result is that, under fairly general conditions (i.e. applying the Central Limit Theorem), 
               sample means based on $100$ patients per day have a normal distribution."),
             p("Putting these $3$ results together: the true sampling distribution in question is normal with mean $140$ and standard error $2.0$."),
             
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("n", "Sample Size:", min = 0, max = 100, value = 10, step = 1)
                 ),
                 mainPanel(
                   div(style = "text-align: center;",
                       plotOutput("graph18"))#div
                 )#mainpanel
               )
             ),#fluidrow
             p("In our example above, we observed that LDL values from a single sample size of 100 is normally distributed with mean 139 and 
             standard deviation of 21 (i.e., $N(139,21)$). From this, we conclude using the above results that the estimated sampling distribution 
             for a sample of size 100 is normal with mean 139 and standard error $2.1$ (i.e., $N(139,2.1)$), which we obtained from our observed LDL values.
             We will use the theoretical sampling distribution, $N(140,2)$, to obtain a point estimate and measure around $\\mu$, the true population mean 
             LDL value (rather than $ \\mu_{ \\bar{X}_{100}}$). We can estimate a confidence interval for $\\mu$ using our observed sampling distribution: 
             $139\\plusmn z*(2.1)$ where $z$ is the critical value determined by our confidence level."),
             p("At this point, the concept of a sampling distribution should be clear to the student, and, indeed, we are but a single step from 
               introducing related concepts such as estimation, confidence intervals, p-values, etc."),
             p("To help connect this presentation with more mathematically-based treatments of this information, we will close by noting that we’ve embedded 
               some mathematical assumptions into our analysis. For example, we implicitly assumed that LDL values for one patient are unrelated to LDL 
               values for other patients, and that each patient is similar, in that they represent an independent sample from an underlying population of LDL values. 
               In other words, we’ve assumed that the LDL values for individual patients are independently and identically distributed. We’ve also assumed that the mean 
               LDL values for each day were similarly representative - that is, independently and identically distributed. Indeed, a single clinic day was used as a metaphor 
               for the more general construct of taking a single sample."),
             p("Roughly speaking, the CLT states that if the distribution of LDL values (i.e., across individual patients) is normal (bell-shaped), 
             then the sampling distribution of sample means will be normal as well, with the mean and standard deviation as previously provided. 
             Moreover, if the distribution of LDL values isn’t normal, as the sample size (i.e., the number of clinic patients per day) increases, 
             the shape of the sampling distribution will more and more closely approximate normality, with the mean and standard deviation as above. 
             The speed at which this occurs depends on both the shape of the underlying distribution and the sample size - for LDL, $n=30$ is probably sufficient."),
             ),#tabpanel
    tabPanel("Symbol Key",
             fluidRow(
               mainPanel(
                 imageOutput("symbol_key")
               )#mainpanel
             )#fluidrow
    )#tabpanel
  )
)
