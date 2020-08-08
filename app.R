library(shiny)
library(rethinking)
library(tidyverse)
# devtools::install_github("aloy/qqplotr")
library("qqplotr")
library(car)
library(agricolae)
library(brms)
library(tidybayes)
library(ggridges)
library(cowplot)
library(ggbeeswarm)

# User interface ----
ui <- tagList(
  navbarPage(
    "BioSci 3700 Statisics Tool",
    
    ## Tab 1 ====
    tabPanel(
      "File Upload",
      titlePanel("Upload and select data"),
      sidebarLayout(
        sidebarPanel(
          checkboxInput("DemoMode", label = "Demo Mode", value = FALSE),
          
          #tags$hr(),
          
          fileInput("file1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
          ),
          "If dependent variables are in multiple columns, specify the first and last columns.",
          numericInput("FirstGather", label = "First data column", value = 2),
          numericInput("LastGather", label = "Last data column", value = 3),
          # textInput("KeyName", label = ("Independent Variable Name"), value = ""),
          # textInput("ValueName", label = ("Dependent Variable Name"), value = ""),
          checkboxInput("RunGather", label = "Convert to long format", value = FALSE),          
          
          
          # textInput("ivar", label = ("Independent Variable"), value = ""),
          # textInput("dvar", label = ("Dependent Variable"), value = ""),
          
          # dept.var = "Value"
          # indept.var = "Key"
          # color.var = "Cult"
          
          textInput("dept.var", label = ("Dependent Variable"), value = "Value"),
          textInput("indept.var", label = ("Independent Variable"), value = "Key"),
          checkboxInput("ColorBy", label = "Color by Variable?", value = FALSE),
          textInput("color.var", label = ("Coloring Variable"), value = "")
          
        ),
        
        mainPanel(
          "Data Preview:",
          tableOutput("contents"),
          
          "Visualized:",
          # plotOutput("user_plot"),
          plotOutput("user_plot2")
          
        )
      )
    ),
    
    
    ## Tab 2 Freq Methods ==== 
    tabPanel(
      "Frequentist Methods",
      sidebarPanel(
        
        checkboxInput("RunAssumptions", label = "Check Assumptions?", value = F),
        checkboxInput("RunFreqModel", label = "Run linear model?", value = F),
        checkboxInput("RunPostHoc", label = "Run Tukey's post-hoc?", value = F)
        
      ),
      mainPanel(
        
        "Normality of Residuals:",
        plotOutput("user_qqplots"),
        tableOutput("user_shap.wilk"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),
        
        "Equal Variance:",
        tableOutput("user_levene"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),
        
        "Main Effect:",
        tableOutput("MainTable"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),
        
        "Post-Hoc:",
        tableOutput("PostHoc"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        )
      )
    ),
    
    ## Tab 3 NonP Methods ==== 
    tabPanel(
      "Non-Parametric Methods",
      sidebarPanel(
        checkboxInput("RunNonPModel", label = "Run model?", value = F)
        
      ),
      mainPanel(
        
        "Summary Table:",
        tableOutput("NonPTable"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        )
      )
    ),
    
    ## Tab 4 brms Methods ====
    tabPanel(
      "Bayesian Method (MCMC)",
      sidebarPanel(

        "This model can take a little while to run since it is running online. Please allow a few minutes to run.",
        checkboxInput("RunBrmsModel", label = "Run Bayesian model?", value = F)

      ),
      mainPanel(
        "Posterior Probabilites:",
        plotOutput("Posts"),
        tableOutput("PostsTable"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),

        "Subtracted Posteriors:",
        plotOutput("PostDifs"),
        tableOutput("PostDifsTable"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        )


      )
    ),
    
    
    ## Tab 5 MAP Methods ====
    tabPanel(
      "Bayesian Method (MAP)",
      sidebarPanel(
        "This is a simple linear model allowing a different intercept for each treatment in the provided data.",
        
        checkboxInput("RunModel", label = "Ready to run model?", value = F),
        numericInput("HPDIProb", label = "Highest posterior density interval:", value = 0.89),
        
        "To control for a catagorical variable, type the column name below:",
        textInput("CtrlEffect", label = "Control for a factor?", value = "No"),        
        "To manually specify the priors to be used, use the spaces below. For the default values, see the Notes tab.",
        checkboxInput("ManualPriors", label = "Manually specify priors?", value = F),
        numericInput("a_mu", label = "Treatment mu", value = 0),
        numericInput("a_sigma", label = "Treatment sigma", value = 0.1),
        numericInput("b_mu", label = "Statistically controled factor mu", value = 0),
        numericInput("b_sigma", label = "Statistically controled factor sigma", value = 0.1),
        numericInput("sigma_min", label = "Sigma min", value = 0),
        numericInput("sigma_max", label = "Sigma max", value = 40)
      ),
      
      mainPanel(
        # plots
        plotOutput("PosteriorDensityRidges"),
        tableOutput("PosteriorStats"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),
        plotOutput("SubtractedPosteriors"),
        tableOutput("SubtractedPosteriorsStats"),
        tags$style(
          type = "text/css",
          "#SubtractedPosteriorsStats {white-space: pre-wrap;}"
        )
        
      )
    ),
    
    
    ## Tab 6 Iter Methods ==== 
    # tabPanel(
    #   "Resampling Methods",
    #   sidebarPanel(
    #     
    #     "This model can take a little while to run since it is running online. Please allow a few minutes to run.",
    #     
    #     numericInput("Iter", label = "Number of Resampling Iterations", value = 1000),
    #     
    #     checkboxInput("RunIterModel", label = "Run Resampled model?", value = F)
    #     
    #   ),
    #   mainPanel(
    #     "Test Statistic Distribution", 
    #     plotOutput("TestStatDist"),
    #     tableOutput("EpTable"),
    #     tags$style(
    #       type = "text/css",
    #       "#PosteriorStats {white-space: pre-wrap;}"
    #     )
    # 
    #     
    #   )
    # ),
    
    
    ## Tab N ====
    tabPanel(
      "Notes",
      withMathJax(),
      # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with < 
      # and grater-than-sign with >
      tags$div(HTML("less-than-sign script type='text/x-mathjax-config' greater-than-sign
                    MathJax.Hub.Config({
                    tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                    });
                    less-than-sign /script greater-than-sign
                    ")),
      helpText('An irrational number $\\sqrt{2}$
               and a fraction $1-\\frac{1}{2}$'),
      helpText('and a fact about $\\pi$:$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
               \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
               \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$'),
      mainPanel(
        
        tags$h4("Automatic Priors"),
        
        "If left unchecked the following opperations will be performed on the full dataset (with all treatments lumped together) and the resultant values will be used as priors
        Treatment mu = median, Treatment sigma = standard deviation, Control mu = 0.5/median, Control sigma = 0.5*standard deviation, Sigma min = 0.001, Sigma max = 2*range",
        
        tags$h4("Model Templates"), 
        "Below are several example modeling statements. Since this app is designed to be used with multiple datasets, the treatments (and controls if used) will need to be adapted to one's specific experiment",
        
        tags$hr(),
        
        "Template model sans statistical control, where \"Trt\" refers to a level of a treatment (e.g. harvest date in the cabbage data set). ", 
        
        withMathJax(
          helpText(
            "$$Response \\sim \\mathcal{N}(\\mu, \\sigma)$$ \n
            $$\\mu_{Treatment} \\leftarrow \\alpha_{1}*[Trt_{1}] + \\alpha_2*[Trt_{2}] + \\alpha_3*[Trt_{3}] + etc..$$ \n
            
            $$\\alpha_{1} \\sim \\mathcal{N}(\\mu_{Trt_1}, \\sigma_{Trt_1}) $$ \n
            $$\\alpha_{2} \\sim \\mathcal{N}(\\mu_{Trt_2}, \\sigma_{Trt_2}) $$ \n
            $$\\alpha_{3} \\sim \\mathcal{N}(\\mu_{Trt_3}, \\sigma_{Trt_3}) $$ \n
            $$\\sigma \\sim \\mathcal{U}(min, max) $$
            "
          )),
        
        tags$hr(), 
        
        "Below are is alternative ways of describing the model, both without and with a factor which is controlled for.",
        "Template model sans statistical control", 
        
        withMathJax(
          #helpText(
          "$$Response \\sim \\mathcal{N}(\\mu_{Treatment}, \\sigma)$$ \n
          $$\\mu_{Treatment} \\leftarrow \\alpha[Treatment] $$ \n
          $$\\alpha \\sim \\mathcal{N}(\\mu_{\\alpha}, \\sigma_{\\alpha}) $$ \n
          $$\\sigma \\sim \\mathcal{U}(min, max) $$
          "
          #)
        ),
        
        tags$hr(), 
        
        "Template model with statistical control", 
        withMathJax(
          helpText(
            "$$Response \\sim \\mathcal{N}(\\mu_{Treatment}, \\sigma)$$ \n
            $$\\mu_{Treatment} \\leftarrow \\alpha[Treatment] + \\beta[Control]$$ \n
            $$\\alpha \\sim \\mathcal{N}(\\mu_{\\alpha}, \\sigma_{\\alpha}) $$ \n
            $$\\beta \\sim \\mathcal{N}(\\mu_{\\beta}, \\sigma_{\\beta}) $$ \n
            $$\\sigma \\sim \\mathcal{U}(min, max) $$
            "
          ))
        
        
        
      )
    )
    
    ## End ====
  )
)

# Server ----

server <- function(input, output) {
  
  observe({
    
    ## For Tab 1 Data ====  
    if (input$DemoMode == TRUE) {
      # This is a long format version of MASS' cabbage dataset (sans a col)
      df <- readRDS(file = "./DemoCabbage.RDS")
      
    } else if (is.null(input$file1) == FALSE) {
      req(input$file1) # redundent
      
      # Will err if semicolon separated files
      tryCatch({
        df <- read.csv(input$file1$datapath)
        df <- as.data.frame(df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
      )
    }
    
    #print(str(df))
    #print(head(df))
    req(df)
    
    # have to double check in case no file has been selected.
    if (class(df) == "data.frame") {
      output$contents <- renderTable({
        head(df)
      })
    }
    
    # Proceed as normal
    # print(class(df))
    
    # Gather if need be
    if (input$RunGather == TRUE &
        class(df) == "data.frame") {
      req(input$FirstGather)
      req(input$LastGather)
      req(df)
      
      df <- tidyr::gather(as.data.frame(df), "Key", "Value", seq(
        from = input$FirstGather,
        to = input$LastGather,
        by = 1
      ))
      
      # do renaming based on user params
      names(df)[which(names(df) == "Value")] <- input$dept.var
      names(df)[which(names(df) == "Key")] <- input$indept.var
    }
    
    
    if (class(df) == "data.frame") {
      
      dept.var = input$dept.var
      indept.var = input$indept.var
      color.var = input$color.var
      
      
      output$user_plot2 <- renderPlot({
        
        if(input$ColorBy == FALSE){
          print("out")
          ggplot(df, aes_string(x = indept.var, y = dept.var))+
            # geom_point(alpha = 0.3)+
            ggbeeswarm::geom_quasirandom()+
            theme_minimal()+
            scale_colour_brewer(type = "qual", palette = "Set1")
        } else {
          ggplot(df, aes_string(x = indept.var, y = dept.var, color = color.var))+
            # geom_point(alpha = 0.3)+
            ggbeeswarm::geom_quasirandom()+
            theme_minimal()+
            scale_colour_brewer(type = "qual", palette = "Set1")
        }
        
      })
    }
    ## For Tab 2 Freq ====
    ### Check Assumptions ####
    if (input$RunAssumptions == T){
      indept.var.levels.Fr <- unique(df[[indept.var]])
      ## 1. Independent cases
      
      ## 2. Normally distributed residuals
      output$user_qqplots <- renderPlot({
        cowplot::plot_grid(plotlist = purrr::map(indept.var.levels.Fr, function(i){
          temp = df[df[[indept.var]] == i, ]
          ggplot(data = temp, mapping = aes_string(sample = dept.var)) +
            stat_qq_band() +
            stat_qq_line() +
            stat_qq_point() +
            labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = i)+
            theme_minimal()
        }))
      })
      
      # Shapiro-Wilk Normality Test
      output$user_shap.wilk <- renderTable({
        purrr::map_df(indept.var.levels.Fr, function(i){
          temp = df[df[[indept.var]] == i, ]
          
          mm <- shapiro.test(temp[[dept.var]])
          
          data.frame(Group = i,
                     Method = mm$method,
                     pValue = mm$p.value)
        })
      })
      
      ## 3. Equal Variance (homoscedasticity)
      lev.result <- car::leveneTest(as.formula(paste(dept.var,
                                                     indept.var,
                                                     sep = " ~ ")), df)
      
      output$user_levene <- renderTable({
        data.frame(Method = "Levene's Test",
                   pValue = lev.result$`Pr(>F)`[1])
      })
      
    }
    
    ### Run Model ####
    if (input$RunFreqModel == T){
      # Re-run here in case student does not check assumptions
      indept.var.levels.Fr <- unique(df[[indept.var]])
      
      if (length(indept.var.levels.Fr) == 2){
        fm.Fr <- t.test(as.formula(paste(dept.var,
                                         indept.var,
                                         sep = " ~ ")), df)
        
        output$MainTable <- renderTable({
          data.frame(Method = fm.Fr$method,
                     t = fm.Fr$statistic,
                     df = fm.Fr$parameter,
                     pValue = fm.Fr$p.value)
        })
        
        
      } else {
        fm.Fr <- lm(as.formula(paste(dept.var,
                                     indept.var,
                                     sep = " ~ ")), df)
        
        temp <- as.data.frame(car::Anova(fm.Fr, type = "III") %>% rename(pValue = `Pr(>F)`))
        
        temp$. <- rownames(temp)
        temp <- temp[, c(".", "Sum Sq", "Df", "F value", "pValue")]
        
        output$MainTable <- renderTable({
          temp
        })
      }
    }
    
    ### Run Post Hoc ####
    if (input$RunPostHoc == T){
      
      # Post Hoc Test
      fm.PH <- lm(as.formula(paste(dept.var,
                                   indept.var,
                                   sep = " ~ ")), df)
      
      #aov(Centimeters ~ Location, data = M.point) %>% TukeyHSD()
      ph <- agricolae::HSD.test(fm.PH,
                                indept.var,
                                group=TRUE,
                                console = TRUE,
                                main="")
      
      
      ph.res <- ph$groups
      ph.res$. <- rownames(ph.res)
      ph.res <- as.data.frame(ph.res)
      
      ph.res <- ph.res[, c(".", names(ph.res)[names(ph.res) != "."])]
      # print(ph.res)
      
      output$PostHoc <- renderTable({
        as.data.frame(ph.res)
      })
    }
    
    ## For Tab 3 NonP ====
    if (input$RunNonPModel == T){
      # Re-run here in case student does not check assumptions
      indept.var.levels.NP <- unique(df[[indept.var]])
      
      # Added as using chars here prevented model from running.
      df[[indept.var]] <- as.factor(df[[indept.var]])
      
      print(str(df))
      print(str(indept.var.levels.NP))
      
      
      if (length(indept.var.levels.NP) == 2){
        ## 2 groups
        # Wilcoxon / Mann-Whitney U
        fm.NP <- wilcox.test(as.formula(paste(dept.var,
                                              indept.var,
                                              sep = " ~ ")), df)
        
        output$NonPTable <- renderTable({
          data.frame(Method = fm.NP$method,
                     `W` = fm.NP$statistic,
                     pValue = fm.NP$p.value)
        })
        
      } else {
        ## 3+ groups
        fm.NP <- kruskal.test(as.formula(paste(dept.var,
                                               indept.var,
                                               sep = " ~ ")), df)  # Built-in
        
        output$NonPTable <- renderTable({
          data.frame(Method = fm.NP$method,
                     `chi-sq` = fm.NP$statistic,
                     df = fm.NP$parameter, #df
                     pValue = fm.NP$p.value)
        })
      }
    }
    
    ## For Tab 4 brms ====


    if (input$RunBrmsModel == T){

      # Bayesian
      bfm <- brms::brm(data = df, family = gaussian,
                       as.formula(paste(dept.var, "~", #"1 +",
                                        indept.var,
                                        sep = " ")),
                       iter = 2000,
                       warmup = 500)
      # ,
      # chains = 4, cores = 1, seed =1)

      # library(tidybayes)


      post <- posterior_samples(bfm)

      # levels to move over
      exp.levels <- df[[indept.var]] %>% levels()
      # retain cols for the independent variables
      post <- post[, seq_along(exp.levels)]
      names(post) <- exp.levels


      #FIXME breaks with uploaded Cabbages Dataset
      print("3")
      #TODO confirm this works properly.
      # should match (more or less) MAP fitting.
      # Adding all but first col to get the variable level _with_ the intercept taken into account.

      # for (i in seq(2, ncol(post))){
      #   post[,i] <- post[,i] + post[,1]
      # }

      post2 <- post[,1]
      for (i in seq(2, ncol(post))){
        post2 <- rbind(post2, post[,i] + post[,1])
      }
      post <- post2

      print("2")

      # Summary tables
      post.table <-
        map_df(seq(1, ncol(post)), function(i){

          int.1 <- bayestestR::hdi(post[,i], ci = 0.67)
          int.2 <- bayestestR::hdi(post[,i], ci = 0.89)
          int.3 <- bayestestR::hdi(post[,i], ci = 0.97)

          data.frame("Distribution"=   names(post)[i],
                     "l 0.97" =   int.3$CI_low,
                     "l 0.89"=   int.2$CI_low,
                     "l 0.67"=   int.1$CI_low,
                     "Mode"=  rethinking::chainmode(post[,i]),
                     "u 0.67"=   int.1$CI_high,
                     "u 0.89"=  int.2$CI_high,
                     "u 0.97"=  int.3$CI_high)
        })


      output$PostsTable <- renderTable({
        post.table
      })

      # Summary Figure
      df.ridges <- post %>% gather(Condition, Value, seq(1, ncol(post)))

      # library(ggridges)
      output$Posts <- renderPlot({
        ggplot(df.ridges, aes(x =  Value, y = Condition, fill =  Condition))+
          ggridges::geom_density_ridges(alpha = 0.5)+
          theme_minimal()+
          scale_fill_brewer(type = "qual", palette = "Set1")
      })

      # Name all comparisons between groups
      Var1 <- c()
      Var2 <- c()

      for (i in seq_along(exp.levels)){
        for(j in seq(i, length(exp.levels))){
          # print(paste(i, j))
          if(j != i){
            Var1 <- c(Var1, exp.levels[i])
            Var2 <- c(Var2, exp.levels[j])
          }
        }
      }

      df.difs <- as.data.frame(matrix(nrow = nrow(post), ncol = length(Var1)))
      names(df.difs) <- paste(Var1, Var2, sep = "_m_")
      # Done separately to avoid ggplot2 parsing errors
      df.labs <- paste(Var1, Var2, sep = "-")

      # Make all comparisons
      for (i in seq_along(Var1)){
        df.difs[,i] <- post[, Var1[i]] - post[, Var2[i]]
      }

      # Difference Tables
      difs.table <-
        map_df(seq(1, ncol(df.difs)), function(i){

          int.1 <- bayestestR::hdi(df.difs[,i], ci = 0.67)
          int.2 <- bayestestR::hdi(df.difs[,i], ci = 0.89)
          int.3 <- bayestestR::hdi(df.difs[,i], ci = 0.97)

          data.frame("Comparison"=   df.labs[i],
                     "l 0.97" =   int.3$CI_low,
                     "l 0.89"=   int.2$CI_low,
                     "l 0.67"=   int.1$CI_low,
                     "Mode"=  rethinking::chainmode(df.difs[,i]),
                     "u 0.67"=   int.1$CI_high,
                     "u 0.89"=  int.2$CI_high,
                     "u 0.97"=  int.3$CI_high)
        })
      output$PostDifsTable <- renderTable({
        difs.table
      })

      # Difference Figure
      difs.plts <-
        purrr::map(seq_along(Var1), function(i){
          # i = 1
          data.col <- names(df.difs)[i]

          ggplot(df.difs, aes_string(x = data.col, y = 0)) +
            geom_halfeyeh(#fill = "steelblue",
              point_interval = median_qi,
              .width = c(.89)) +
            stat_intervalh(.width = c(.67, .89, .97)) +
            # scale_y_continuous(NULL, breaks = NULL) +
            # labs(subtitle = "Model-implied difference score",
            #      x = expression(alpha["male"] - alpha["female"])) +
            geom_vline(xintercept = rethinking::chainmode(df.difs[[data.col]]),
                       size = 1,
                       linetype = "dashed",
                       color = "steelblue") +
            geom_vline(xintercept = 0,
                       size = 1,
                       linetype = "dashed",
                       color = "black") +
            labs(title = df.labs[i], x = "", y = "Density")+
            theme_minimal()+
            theme(legend.position = "")+
            scale_color_brewer()
        })

      output$PostDifs <- renderPlot({
        cowplot::plot_grid(plotlist = difs.plts)
      })
    }
    
    
    ## For Tab 5 MAP ====
    ### ####Cont
    
    # use observe to pull the processing out of the assignment of ui objects
    #observe({
    req(input$CtrlEffect)
    req(input$RunModel)
    
    
    #TODO confirms this works. 
    # df[[]]
    # indept.var
    # dept.var
    # 
    df.map <- df
    # rename indept/dept to key/value
    names(df.map)[names(df.map) == indept.var] <- "Key"
    names(df.map)[names(df.map) == dept.var] <- "Value"
    
    
    if (input$ManualPriors == T) {
      a_mu <<- as.numeric(input$a_mu)
      a_sigma <<- as.numeric(input$a_sigma)
      b_mu <<- as.numeric(input$b_mu)
      b_sigma <<- as.numeric(input$b_sigma)
      sigma_min <<- as.numeric(input$sigma_min)
      sigma_max <<- as.numeric(input$sigma_max)
    } else {
      
      # calculate reasonable priors
      
      # a_mu <- 73
      # a_sigma <- 13
      a_mu <<- median(df.map$Value)
      a_sigma <<- sd(df.map$Value)
      
      if (input$CtrlEffect != "No") {
        # df.map[[as.character(input$CtrlEffect)]]
        
        # b_mu <- 0
        # b_sigma <- 5
        b_mu <<- as.numeric(median(df.map$Value) / 2) #5
        b_sigma <<- as.numeric(sd(df.map$Value) / 2)
      }
      
      # sigma_min <- 0
      # sigma_max <- 40
      sigma_min <<- as.numeric(0.001)
      sigma_max <<- as.numeric(2 * (max(df.map$Value) - min(df.map$Value)))
      
    }
    
    ### Fit model with MAP ####
    
    df.map$KEY_ID <- rethinking::coerce_index(df.map$Key)
    
    
    print(input$CtrlEffect)
    if (input$CtrlEffect != "No") {
      # FIXME if someone types in a non valid name this could break
      # Add an else that controls for individual effect
      # use coerce_index to make sure that we know what the controled factor is.
      df.map$CTRL_BY <- rethinking::coerce_index(df.map[[as.character(input$CtrlEffect)]])
      print("running model with factor controlled")
      model <- rethinking::map(
        alist(
          Value ~ dnorm(mu, sigma),
          mu <- a[KEY_ID] + b[CTRL_BY],
          a[KEY_ID] ~ dnorm(a_mu, a_sigma),
          b[CTRL_BY] ~ dnorm(b_mu, b_sigma),
          sigma ~ dunif(sigma_min, sigma_max)
          #sigma ~ dcauchy(0, 1)
        ),
        data = df.map
      )
    } else if (input$CtrlEffect == "No") {
      print("running model without factor controlled")
      model <- rethinking::map(
        alist(
          Value ~ dnorm(mu, sigma),
          mu <- a[KEY_ID],
          a[KEY_ID] ~ dnorm(a_mu, a_sigma),
          sigma ~ dunif(sigma_min, sigma_max)
          #sigma ~ dcauchy(0, 5)
        ),
        data = df.map
      )
    } else {
      warning("Controlled effect is neither 'No' nor a column name")
    }
    
    
    post <- rethinking::extract.samples(model)
    
    ## plots of posterior
    temp <- df.map[, c("Key", "KEY_ID")] %>% unique()
    ordered.names <- dplyr::arrange(temp, KEY_ID)[, 1]
    
    temp <- post$a %>% as.data.frame()
    names(temp) <- ordered.names
    ### First plot ####
    output$PosteriorDensityRidges <- renderPlot({
      ggplot(
        gather(temp, KEY, VALUE, seq(1, ncol(temp))),
        aes(x = VALUE, y = KEY, fill = KEY)
      ) +
        ggridges::geom_density_ridges(alpha = 0.5)+
        labs(x = dept.var, y = indept.var)+
        theme_minimal()+
        scale_fill_brewer(type = "qual", palette = "Set1")
    })
    
    ### Prep plots of differences of posteriors ####
    
    temp.diffs <- as.data.frame(matrix(NA,
                                       nrow = nrow(temp),
                                       ncol = sum(seq(1, (ncol(temp) - 1)))
    ))
    
    empty.col <- 1
    for (i in seq(1, length(ordered.names) - 1)) {
      for (j in seq(i + 1, length(ordered.names))) {
        temp.diffs[, empty.col] <- temp[, i] - temp[, j]
        names(temp.diffs)[empty.col] <- paste0(ordered.names[i], "-", ordered.names[j])
        # print(paste(i, j))
        empty.col <- empty.col + 1
      }
    }
    
    ### Make a nice figure that can be produced from one difference column.
    # Shade HDPI for 67, 89, 97 because they're all prime
    plt.list <- purrr::map(seq(1, ncol(temp.diffs)), function(i) {
      # i=1
      # ref : http://rstudio-pubs-static.s3.amazonaws.com/5475_d63ad1667701424c9a1292ee766b45bb.html
      
      temp.diffs.plt <- with(density(temp.diffs[, i]), data.frame(x, y))
      # names(temp.diffs.plt) <- c(names(temp.diffs)[i], "Density")
      HPDI.67 <- rethinking::HPDI(temp.diffs[, i], prob = 0.67)
      HPDI.89 <- rethinking::HPDI(temp.diffs[, i], prob = 0.89)
      HPDI.97 <- rethinking::HPDI(temp.diffs[, i], prob = 0.97)
      CMODE <- rethinking::chainmode(temp.diffs[, i])
      
      ggplot(temp.diffs.plt, aes_string(x = "x", y = "y")) +
        geom_segment(aes(x = HPDI.97[1],
                         xend = HPDI.97[2],
                         y= 0,
                         yend = 0,),
                     size = 3,
                     color = "deepskyblue1", alpha = 0.4)+
        
        geom_segment(aes(x = HPDI.89[1],
                         xend = HPDI.89[2],
                         y= 0,
                         yend = 0,), 
                     size = 3,
                     color = "deepskyblue3", alpha = 0.4)+
        
        geom_segment(aes(x = HPDI.67[1],
                         xend = HPDI.67[2],
                         y= 0,
                         yend = 0,), 
                     size = 3,
                     color = "deepskyblue4", alpha = 0.4)+
        
        # geom_area(aes(x = ifelse(x > HPDI.97[1] & x < HPDI.97[2], x, 0)),
        #   fill = "firebrick", alpha = 0.4
        # ) +
        # geom_area(aes(x = ifelse(x > HPDI.89[1] & x < HPDI.89[2], x, 0)),
        #   fill = "firebrick", alpha = 0.4
        # ) +
        # geom_area(aes(x = ifelse(x > HPDI.67[1] & x < HPDI.67[2], x, 0)),
        #   fill = "firebrick", alpha = 0.4
        # ) +
        geom_vline(xintercept = CMODE, size = 1, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 0, size = 1, linetype = "dashed", color = "steelblue") +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, max(temp.diffs.plt$y))) +
        labs(x = names(temp.diffs)[i], y = "Density")+
        theme_minimal()+
        scale_colour_brewer(type = "qual", palette = "Set1")
    })
    
    ### Multi-plot figure ####
    output$SubtractedPosteriors <- renderPlot({
      cowplot::plot_grid(plotlist = plt.list, ncol = 2)
    })
    
    # NOTE: If this function is defined earlier (beginning of file) it does not execute properly
    new_precis <- function(input.df.map = temp,
                           HDPI.prob = 0.89) {
      output.df.map <- as.data.frame(matrix(NA,
                                            nrow = ncol(input.df.map),
                                            ncol = 5
      ))
      
      for (i in seq(1, ncol(input.df.map))) {
        output.df.map[i, 1] <- names(input.df.map)[i]
        
        output.df.map[i, 2:5] <- c(
          round(mean(input.df.map[, i]), digits = 2),
          round(sd(input.df.map[, i]), digits = 2),
          rethinking::HPDI(input.df.map[, i], prob = HDPI.prob)
        )
      }
      
      names(output.df.map) <- c(
        "Comparison", "Mean", "StdDev",
        paste("|", as.character(HDPI.prob)),
        paste(as.character(HDPI.prob), "|")
      )
      
      return(output.df.map)
    }
    
    ### table of posteriors ####
    output$PosteriorStats <- renderTable({
      new_precis(temp, HDPI.prob = input$HPDIProb)
    })
    
    ### table  of differences of posteriors ####
    output$SubtractedPosteriorsStats <- renderTable({
      new_precis(temp.diffs, HDPI.prob = input$HPDIProb)
    })
    
    ## End ====
  })
}


shiny::shinyApp(ui = ui, server = server)
