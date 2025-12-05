library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)

if (interactive()) {

  ui <- fluidPage(

    titlePanel("Statistical Distribution Explorer & Sample Size Calculator"),
    br(),

    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Select a Distribution"),

          selectInput("distType", "Distribution",
                      c("Uniform" = "unif",
                        "Normal" = "norm",
                        "Exponential" = "exp",
                        "Gamma" = "gamma",
                        "Weibull" = "weibull",
                        "--- Discrete ---" = "",
                        "Binomial" = "binom",
                        "Poisson" = "pois",
                        "Geometric" = "geom",
                        "Negative Binomial" = "nbinom")
          ),

          hr(),
          h4("Parameters"),

          # Uniform
          conditionalPanel(
            condition = "input.distType == 'unif'",
            sliderInput("min_u", "Min (a)", min = -10, max = 5, value = -5),
            sliderInput("max_u", "Max (b)", min = -5, max = 10, value = 5)
          ),

          # Normal
          conditionalPanel(
            condition = "input.distType == 'norm'",
            sliderInput("mean_n", "Mean (µ)", min = -5, max = 5, value = 0),
            sliderInput("sd_n", "SD (σ)", min = 0.1, max = 3, value = 1)
          ),

          # Exponential
          conditionalPanel(
            condition = "input.distType == 'exp'",
            sliderInput("lambda_e", "Rate (λ)", min = 0.1, max = 5, value = 1)
          ),

          # Gamma
          conditionalPanel(
            condition = "input.distType == 'gamma'",
            sliderInput("alpha_g", "Shape (α)", min = 0.5, max = 10, value = 2),
            sliderInput("beta_g", "Scale (θ)", min = 0.5, max = 5, value = 1)
          ),

          # Weibull
          conditionalPanel(
            condition = "input.distType == 'weibull'",
            sliderInput("lambda_w", "Shape (k)", min = 0.5, max = 5, value = 1.5),
            sliderInput("beta_w", "Scale (λ)", min = 0.5, max = 5, value = 2)
          ),

          # Binomial
          conditionalPanel(
            condition = "input.distType == 'binom'",
            sliderInput("n_b", "Trials (n)", min = 1, max = 50, value = 10),
            sliderInput("p_b", "Prob (p)", min = 0.01, max = 0.99, value = 0.5)
          ),

          # Poisson
          conditionalPanel(
            condition = "input.distType == 'pois'",
            sliderInput("lambda_p", "Rate (λ)", min = 0.5, max = 10, value = 3)
          ),

          # Geometric
          conditionalPanel(
            condition = "input.distType == 'geom'",
            sliderInput("p_geom", "Prob (p)", min = 0.01, max = 0.99, value = 0.3)
          ),

          # Negative Binomial
          conditionalPanel(
            condition = "input.distType == 'nbinom'",
            sliderInput("r_n", "Successes (r)", min = 1, max = 20, value = 5),
            sliderInput("p_n", "Prob (p)", min = 0.01, max = 0.99, value = 0.5)
          ),

          hr(),

          # ========== DISTRIBUTION COMPARISON ==========
          wellPanel(
            h4("Distribution Comparison"),
            helpText("Select distributions to compare"),

            checkboxGroupInput("compare_dists", "Compare with:",
                               choices = list(
                                 "Uniform" = "unif",
                                 "Normal" = "norm",
                                 "Exponential" = "exp",
                                 "Gamma" = "gamma",
                                 "Weibull" = "weibull",
                                 "Binomial" = "binom",
                                 "Poisson" = "pois"
                               ),
                               selected = NULL)
          )
        )
      ),

      column(
        width = 8,
        tabsetPanel(

          # TAB 1 – Distribution Plots
          tabPanel("Distribution Plots",
                   br(),
                   fluidRow(
                     column(12, plotOutput("allPlots", height = "700px"))
                   )
          ),

          # TAB 2 – Properties
          tabPanel("Properties",
                   br(),
                   withMathJax(),
                   uiOutput("properties")
          ),

          # TAB 3 – Comparison Plots
          tabPanel("Distribution Comparison",
                   br(),
                   h4("PDF/PMF Comparison"),
                   plotOutput("compare_pdf_plot", height = "400px"),
                   br(),
                   h4("CDF Comparison"),
                   plotOutput("compare_cdf_plot", height = "400px")
          ),

          # TAB 4 – Sample Size Calculator (SIMPLIFIED)
          tabPanel("Sample Size Calculator",
                   br(),
                   tabsetPanel(

                     # Sub-tab 1: Cochran's Formula
                     tabPanel("Cochran's Formula",
                              br(),
                              wellPanel(
                                h4("Cochran's Sample Size Formula"),
                                helpText("For estimating proportions in large populations:"),

                                # Using numericInput for precise input
                                numericInput("cochran_p", "Estimated proportion (p, 0.01 to 0.99):",
                                             value = 0.5, min = 0.01, max = 0.99, step = 0.01),
                                numericInput("cochran_margin", "Margin of error (e, 0.01 to 0.2):",
                                             value = 0.05, min = 0.01, max = 0.2, step = 0.01),
                                numericInput("cochran_confidence", "Confidence level (0.8 to 0.99):",
                                             value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                                numericInput("cochran_population", "Population size (optional, for finite population correction):",
                                             value = NA, min = 1),

                                actionButton("calc_cochran", "Calculate Sample Size",
                                             class = "btn-primary"),

                                br(), br(),

                                verbatimTextOutput("cochran_result")
                              ),

                              plotOutput("cochran_plot", height = "350px")
                     ),

                     # Sub-tab 2: Power Analysis
                     tabPanel("Power Analysis",
                              br(),
                              wellPanel(
                                h4("Power Analysis Sample Size"),
                                helpText("For comparing two means (Cohen's d):"),

                                # Using numericInput for precise input
                                numericInput("power_alpha", "Significance level (α, 0.001 to 0.2):",
                                             value = 0.05, min = 0.001, max = 0.2, step = 0.001),
                                numericInput("power_beta", "Power (1-β, 0.5 to 0.99):",
                                             value = 0.8, min = 0.5, max = 0.99, step = 0.01),
                                numericInput("power_cohen", "Effect size (Cohen's d, 0.1 to 2.0):",
                                             value = 0.5, min = 0.1, max = 2.0, step = 0.1),
                                selectInput("power_test", "Test type:",
                                            choices = list("Two-sample t-test" = "two.sample",
                                                           "Paired t-test" = "paired",
                                                           "One-sample t-test" = "one.sample"),
                                            selected = "two.sample"),
                                numericInput("power_ratio", "Ratio (n2/n1, 0.5 to 2.0):",
                                             value = 1, min = 0.5, max = 2.0, step = 0.1),

                                actionButton("calc_power", "Calculate Sample Size",
                                             class = "btn-primary"),

                                br(), br(),

                                verbatimTextOutput("power_result")
                              ),

                              plotOutput("power_plot", height = "350px")
                     )
                   )
          )


        )
      )
    )
  )

  server <- function(input, output, session) {

    # Helper: Produce a sequence for continuous distributions
    get_x <- reactive({
      switch(input$distType,

             "unif" = seq(input$min_u - 1, input$max_u + 1, length.out = 400),
             "norm" = seq(input$mean_n - 4 * input$sd_n,
                          input$mean_n + 4 * input$sd_n, length.out = 400),
             "exp" = seq(0, 6 / input$lambda_e, length.out = 400),
             "gamma" = seq(0, input$alpha_g * (1/input$beta_g) * 4, length.out = 400),
             "weibull" = seq(0, input$beta_w * 4, length.out = 400),
             NULL
      )
    })

    # Get distribution color palette
    dist_colors <- reactive({
      all_dists <- c(input$distType, input$compare_dists)
      n <- length(all_dists)
      colors <- brewer.pal(max(3, n), "Set1")[1:n]
      setNames(colors, all_dists)
    })

    # Get distribution name for display
    get_dist_name <- function(dist_code) {
      switch(dist_code,
             "unif" = "Uniform",
             "norm" = "Normal",
             "exp" = "Exponential",
             "gamma" = "Gamma",
             "weibull" = "Weibull",
             "binom" = "Binomial",
             "pois" = "Poisson",
             "geom" = "Geometric",
             "nbinom" = "Negative Binomial",
             dist_code)
    }

    # --------------------- PDF / PMF PLOT -------------------------
    create_pdf_plot <- function() {
      dist <- input$distType

      # Continuous
      if (dist %in% c("unif", "norm", "exp", "gamma", "weibull")) {
        x <- get_x()

        y <- switch(dist,
                    "unif" = dunif(x, input$min_u, input$max_u),
                    "norm" = dnorm(x, input$mean_n, input$sd_n),
                    "exp" = dexp(x, input$lambda_e),
                    "gamma" = dgamma(x, input$alpha_g, scale = input$beta_g),
                    "weibull" = dweibull(x, input$lambda_w, scale = input$beta_w)
        )

        df <- data.frame(x, y, Distribution = get_dist_name(dist))

        ggplot(df, aes(x, y, color = Distribution)) +
          geom_line(size = 1.5) +
          scale_color_manual(values = dist_colors()[dist]) +
          labs(title = "Probability Density Function (PDF)",
               x = "x", y = "Density") +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(hjust = 0.5, size = 14),
                legend.position = "top")
      }

      # Discrete
      else if (dist %in% c("binom", "pois", "geom", "nbinom")) {
        x <- switch(dist,
                    "binom" = 0:input$n_b,
                    "pois"  = 0:(input$lambda_p + 4*sqrt(input$lambda_p)),
                    "geom"  = 0:10,
                    "nbinom" = 0:30
        )

        p <- switch(dist,
                    "binom" = dbinom(x, input$n_b, input$p_b),
                    "pois" = dpois(x, input$lambda_p),
                    "geom" = dgeom(x, input$p_geom),
                    "nbinom" = dnbinom(x, size = input$r_n, prob = input$p_n)
        )

        df <- data.frame(x, p, Distribution = get_dist_name(dist))

        ggplot(df, aes(x, p, fill = Distribution)) +
          geom_col(alpha = 0.8) +
          scale_fill_manual(values = dist_colors()[dist]) +
          geom_text(aes(label = round(p, 3)), vjust = -0.3, size = 3) +
          labs(title = "Probability Mass Function (PMF)",
               x = "k", y = "Probability") +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(hjust = 0.5, size = 14),
                legend.position = "top")
      }
    }

    # ----------------------------- CDF -------------------------------
    create_cdf_plot <- function() {
      dist <- input$distType

      if (dist %in% c("unif", "norm", "exp", "gamma", "weibull")) {
        x <- get_x()
        y <- switch(dist,
                    "unif" = punif(x, input$min_u, input$max_u),
                    "norm" = pnorm(x, input$mean_n, input$sd_n),
                    "exp" = pexp(x, input$lambda_e),
                    "gamma" = pgamma(x, input$alpha_g, scale = input$beta_g),
                    "weibull" = pweibull(x, input$lambda_w, scale = input$beta_w)
        )

        df <- data.frame(x, y, Distribution = get_dist_name(dist))

        ggplot(df, aes(x, y, color = Distribution)) +
          geom_line(size = 1.5) +
          scale_color_manual(values = dist_colors()[dist]) +
          labs(title = "Cumulative Distribution Function (CDF)",
               x = "x", y = "F(x)") +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(hjust = 0.5, size = 14),
                legend.position = "top")

      } else if (dist %in% c("binom", "pois", "geom", "nbinom")) {
        x <- switch(dist,
                    "binom" = 0:input$n_b,
                    "pois"  = 0:(input$lambda_p + 4*sqrt(input$lambda_p)),
                    "geom"  = 0:10,
                    "nbinom" = 0:30)

        p <- switch(dist,
                    "binom" = pbinom(x, input$n_b, input$p_b),
                    "pois" = ppois(x, input$lambda_p),
                    "geom" = pgeom(x, input$p_geom),
                    "nbinom" = pnbinom(x, size = input$r_n, prob = input$p_n))

        df <- data.frame(x, p, Distribution = get_dist_name(dist))

        ggplot(df, aes(x, p, color = Distribution)) +
          geom_point(size = 2) +
          geom_line(size = 1) +
          scale_color_manual(values = dist_colors()[dist]) +
          labs(title = "Cumulative Distribution Function (CDF)",
               x = "k", y = "F(k)") +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(hjust = 0.5, size = 14),
                legend.position = "top")
      }
    }

    # -------------------------- SIMULATION --------------------------
    create_sim_plot <- function() {
      dist <- input$distType
      n <- 2000  # sample size

      x <- switch(dist,
                  "unif" = runif(n, input$min_u, input$max_u),
                  "norm" = rnorm(n, input$mean_n, input$sd_n),
                  "exp" = rexp(n, input$lambda_e),
                  "gamma" = rgamma(n, input$alpha_g, scale = input$beta_g),
                  "weibull" = rweibull(n, input$lambda_w, scale = input$beta_w),
                  "binom" = rbinom(n, input$n_b, input$p_b),
                  "pois" = rpois(n, input$lambda_p),
                  "geom" = rgeom(n, input$p_geom),
                  "nbinom" = rnbinom(n, size = input$r_n, prob = input$p_n)
      )

      df <- data.frame(x)

      ggplot(df, aes(x)) +
        geom_histogram(bins = 40, color = "black", fill = "orange", alpha = 0.7) +
        labs(title = "Histogram of Random Samples",
             x = "Value", y = "Frequency") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    }

    # --------------------- ALL PLOTS TOGETHER -------------------------
    output$allPlots <- renderPlot({
      # Create the three plots
      p1 <- create_pdf_plot()
      p2 <- create_cdf_plot()
      p3 <- create_sim_plot()

      # Arrange them in a grid
      grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))
    })

    # ========== DISTRIBUTION COMPARISON ==========

    # Create comparison data frame
    comparison_data <- reactive({
      dists_to_compare <- c(input$distType, input$compare_dists)
      if (length(dists_to_compare) < 2) return(NULL)

      # Check if all distributions are either continuous or discrete
      all_continuous <- all(dists_to_compare %in%
                              c("unif", "norm", "exp", "gamma", "weibull"))
      all_discrete <- all(dists_to_compare %in%
                            c("binom", "pois", "geom", "nbinom"))

      if (!all_continuous && !all_discrete) {
        # Mixed types - use common x range
        x_min <- -3
        x_max <- 3
        x_seq <- seq(x_min, x_max, length.out = 400)
      } else if (all_continuous) {
        # All continuous - use union of x ranges
        x_ranges <- lapply(dists_to_compare, function(d) {
          switch(d,
                 "unif" = c(input$min_u - 1, input$max_u + 1),
                 "norm" = c(input$mean_n - 4*input$sd_n, input$mean_n + 4*input$sd_n),
                 "exp" = c(0, 6/input$lambda_e),
                 "gamma" = c(0, input$alpha_g * (1/input$beta_g) * 4),
                 "weibull" = c(0, input$beta_w * 4)
          )
        })
        x_min <- min(sapply(x_ranges, function(r) r[1]))
        x_max <- max(sapply(x_ranges, function(r) r[2]))
        x_seq <- seq(x_min, x_max, length.out = 400)
      } else {
        # All discrete - use union of x ranges
        x_min <- 0
        x_max <- max(sapply(dists_to_compare, function(d) {
          switch(d,
                 "binom" = input$n_b,
                 "pois" = input$lambda_p + 4*sqrt(input$lambda_p),
                 "geom" = 10,
                 "nbinom" = 30
          )
        }))
        x_seq <- round(seq(x_min, x_max, length.out = min(50, x_max - x_min + 1)))
      }

      # Create data for each distribution
      df_list <- list()
      colors <- dist_colors()

      for (dist in dists_to_compare) {
        # PDF/PMF values
        pdf_vals <- switch(dist,
                           "unif" = dunif(x_seq, input$min_u, input$max_u),
                           "norm" = dnorm(x_seq, input$mean_n, input$sd_n),
                           "exp" = dexp(x_seq, input$lambda_e),
                           "gamma" = dgamma(x_seq, input$alpha_g, scale = input$beta_g),
                           "weibull" = dweibull(x_seq, input$lambda_w, scale = input$beta_w),
                           "binom" = dbinom(x_seq, input$n_b, input$p_b),
                           "pois" = dpois(x_seq, input$lambda_p),
                           "geom" = dgeom(x_seq, input$p_geom),
                           "nbinom" = dnbinom(x_seq, size = input$r_n, prob = input$p_n)
        )

        # CDF values
        cdf_vals <- switch(dist,
                           "unif" = punif(x_seq, input$min_u, input$max_u),
                           "norm" = pnorm(x_seq, input$mean_n, input$sd_n),
                           "exp" = pexp(x_seq, input$lambda_e),
                           "gamma" = pgamma(x_seq, input$alpha_g, scale = input$beta_g),
                           "weibull" = pweibull(x_seq, input$lambda_w, scale = input$beta_w),
                           "binom" = pbinom(x_seq, input$n_b, input$p_b),
                           "pois" = ppois(x_seq, input$lambda_p),
                           "geom" = pgeom(x_seq, input$p_geom),
                           "nbinom" = pnbinom(x_seq, size = input$r_n, prob = input$p_n)
        )

        df_list[[dist]] <- data.frame(
          x = x_seq,
          pdf = pdf_vals,
          cdf = cdf_vals,
          Distribution = get_dist_name(dist),
          Color = colors[dist]
        )
      }

      do.call(rbind, df_list)
    })

    # Comparison PDF plot
    output$compare_pdf_plot <- renderPlot({
      comp_data <- comparison_data()
      if (is.null(comp_data) || nrow(comp_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Select at least one distribution to compare",
                          size = 6) +
                 theme_void())
      }

      # Determine if we should use lines or bars
      all_continuous <- all(unique(comp_data$Distribution) %in%
                              c("Uniform", "Normal", "Exponential", "Gamma", "Weibull"))

      if (all_continuous) {
        p <- ggplot(comp_data, aes(x = x, y = pdf, color = Distribution)) +
          geom_line(size = 1.2) +
          scale_color_manual(values = setNames(comp_data$Color, comp_data$Distribution)) +
          labs(title = "PDF Comparison",
               x = "x", y = "Density",
               subtitle = "Continuous Distributions") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                legend.position = "top")
      } else {
        # For discrete or mixed, use points and lines
        p <- ggplot(comp_data, aes(x = x, y = pdf, color = Distribution)) +
          geom_point(size = 2) +
          geom_line(size = 0.8, alpha = 0.7) +
          scale_color_manual(values = setNames(comp_data$Color, comp_data$Distribution)) +
          labs(title = "PMF/PDF Comparison",
               x = "x", y = "Probability/Density",
               subtitle = "Discrete/Continuous Distributions") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                legend.position = "top")
      }

      p
    })

    # Comparison CDF plot
    output$compare_cdf_plot <- renderPlot({
      comp_data <- comparison_data()
      if (is.null(comp_data) || nrow(comp_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Select at least one distribution to compare",
                          size = 6) +
                 theme_void())
      }

      p <- ggplot(comp_data, aes(x = x, y = cdf, color = Distribution)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = setNames(comp_data$Color, comp_data$Distribution)) +
        labs(title = "CDF Comparison",
             x = "x", y = "F(x)",
             subtitle = "Cumulative Distribution Functions") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              legend.position = "top")

      p
    })

    # ========== SAMPLE SIZE CALCULATOR ==========

    # Cochran's Formula Calculator
    observeEvent(input$calc_cochran, {
      p <- input$cochran_p
      e <- input$cochran_margin
      z <- qnorm(1 - (1 - input$cochran_confidence) / 2)

      # Validate inputs
      if (is.na(p) || p < 0.01 || p > 0.99) {
        output$cochran_result <- renderText({ "Error: Proportion must be between 0.01 and 0.99" })
        return()
      }
      if (is.na(e) || e < 0.01 || e > 0.2) {
        output$cochran_result <- renderText({ "Error: Margin of error must be between 0.01 and 0.2" })
        return()
      }

      # Cochran's formula for infinite population
      n_infinite <- (z^2 * p * (1 - p)) / (e^2)

      # Apply finite population correction if provided
      if (!is.na(input$cochran_population) && input$cochran_population > 0) {
        N <- input$cochran_population
        n_finite <- (n_infinite * N) / (n_infinite + (N - 1))
        result_text <- sprintf(
          paste("Cochran's Sample Size Calculation:\n",
                "--------------------------------\n",
                "Estimated proportion (p) = %.3f\n",
                "Margin of error (e) = %.3f\n",
                "Confidence level = %.1f%% (z = %.3f)\n",
                "Population size = %s\n\n",
                "Sample size (infinite population) = %.0f\n",
                "Sample size (finite population) = %.0f\n",
                "Required sample size = %.0f"),
          p, e, input$cochran_confidence * 100, z,
          format(N, big.mark = ","),
          ceiling(n_infinite), ceiling(n_finite), ceiling(n_finite)
        )
      } else {
        result_text <- sprintf(
          paste("Cochran's Sample Size Calculation:\n",
                "--------------------------------\n",
                "Estimated proportion (p) = %.3f\n",
                "Margin of error (e) = %.3f\n",
                "Confidence level = %.1f%% (z = %.3f)\n\n",
                "Required sample size = %.0f"),
          p, e, input$cochran_confidence * 100, z, ceiling(n_infinite)
        )
      }

      output$cochran_result <- renderText({ result_text })
    })

    # Cochran's formula plot
    output$cochran_plot <- renderPlot({
      # Create data for visualization
      p_values <- seq(0.01, 0.99, 0.01)
      z <- qnorm(1 - (1 - input$cochran_confidence) / 2)
      e <- input$cochran_margin

      n_values <- (z^2 * p_values * (1 - p_values)) / (e^2)

      df <- data.frame(Proportion = p_values, SampleSize = n_values)

      ggplot(df, aes(x = Proportion, y = SampleSize)) +
        geom_line(size = 1.2, color = "steelblue") +
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
        geom_point(aes(x = input$cochran_p, y = (z^2 * input$cochran_p * (1 - input$cochran_p)) / (e^2)),
                   color = "red", size = 3) +
        annotate("text", x = 0.5, y = max(n_values) * 1.05,
                 label = "Maximum at p = 0.5", color = "red") +
        labs(title = "Sample Size vs. Estimated Proportion",
             x = "Estimated Proportion (p)",
             y = "Required Sample Size (n)",
             subtitle = sprintf("Cochran's Formula: n = z² × p × (1-p) / e²\ne = %.2f, Confidence = %.0f%%",
                                e, input$cochran_confidence * 100)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle = element_text(hjust = 0.5, size = 12))
    })

    # Power Analysis Calculator
    observeEvent(input$calc_power, {
      alpha <- input$power_alpha
      power <- input$power_beta
      d <- input$power_cohen

      # Validate inputs
      if (is.na(alpha) || alpha < 0.001 || alpha > 0.2) {
        output$power_result <- renderText({ "Error: α must be between 0.001 and 0.2" })
        return()
      }
      if (is.na(power) || power < 0.5 || power > 0.99) {
        output$power_result <- renderText({ "Error: Power must be between 0.5 and 0.99" })
        return()
      }
      if (is.na(d) || d < 0.1 || d > 2.0) {
        output$power_result <- renderText({ "Error: Cohen's d must be between 0.1 and 2.0" })
        return()
      }

      # Calculate sample size using approximation formula
      if (input$power_test == "two.sample") {
        ratio <- input$power_ratio
        n1 <- (2 * (qnorm(1 - alpha/2) + qnorm(power))^2) / (d^2)
        n2 <- n1 * ratio
        result_text <- sprintf(
          paste("Power Analysis Sample Size:\n",
                "---------------------------\n",
                "Significance level (α) = %.3f\n",
                "Power (1-β) = %.3f\n",
                "Effect size (Cohen's d) = %.2f\n",
                "Test type = Two-sample t-test\n",
                "Ratio (n2/n1) = %.1f\n\n",
                "Required sample size per group:\n",
                "Group 1 (n1) = %.0f\n",
                "Group 2 (n2) = %.0f\n",
                "Total sample size = %.0f"),
          alpha, power, d, ratio, ceiling(n1), ceiling(n2), ceiling(n1 + n2)
        )
      } else if (input$power_test == "one.sample") {
        n <- ((qnorm(1 - alpha/2) + qnorm(power))^2) / (d^2)
        result_text <- sprintf(
          paste("Power Analysis Sample Size:\n",
                "---------------------------\n",
                "Significance level (α) = %.3f\n",
                "Power (1-β) = %.3f\n",
                "Effect size (Cohen's d) = %.2f\n",
                "Test type = One-sample t-test\n\n",
                "Required sample size = %.0f"),
          alpha, power, d, ceiling(n)
        )
      } else { # paired t-test
        n <- ((qnorm(1 - alpha/2) + qnorm(power))^2) / (d^2)
        result_text <- sprintf(
          paste("Power Analysis Sample Size:\n",
                "---------------------------\n",
                "Significance level (α) = %.3f\n",
                "Power (1-β) = %.3f\n",
                "Effect size (Cohen's d) = %.2f\n",
                "Test type = Paired t-test\n\n",
                "Required number of pairs = %.0f"),
          alpha, power, d, ceiling(n)
        )
      }

      output$power_result <- renderText({ result_text })
    })

    # Power analysis plot
    output$power_plot <- renderPlot({
      # Create data for visualization
      d_values <- seq(0.1, 2, 0.1)
      alpha <- input$power_alpha
      power <- input$power_beta

      # Calculate sample sizes for different effect sizes
      if (input$power_test == "two.sample") {
        n_values <- (2 * (qnorm(1 - alpha/2) + qnorm(power))^2) / (d_values^2)
        ylab <- "Sample Size per Group"
      } else {
        n_values <- ((qnorm(1 - alpha/2) + qnorm(power))^2) / (d_values^2)
        ylab <- ifelse(input$power_test == "one.sample",
                       "Sample Size", "Number of Pairs")
      }

      df <- data.frame(EffectSize = d_values, SampleSize = n_values)

      # Calculate current value
      current_n <- if (input$power_test == "two.sample") {
        (2 * (qnorm(1 - alpha/2) + qnorm(power))^2) / (input$power_cohen^2)
      } else {
        ((qnorm(1 - alpha/2) + qnorm(power))^2) / (input$power_cohen^2)
      }

      ggplot(df, aes(x = EffectSize, y = SampleSize)) +
        geom_line(size = 1.2, color = "darkgreen") +
        geom_point(aes(x = input$power_cohen, y = current_n),
                   color = "red", size = 3) +
        labs(title = "Sample Size vs. Effect Size",
             x = "Cohen's d (Effect Size)",
             y = ylab,
             subtitle = sprintf("Power = %.0f%%, α = %.3f, %s",
                                power * 100, alpha,
                                ifelse(input$power_test == "two.sample", "Two-sample t-test",
                                       ifelse(input$power_test == "one.sample", "One-sample t-test", "Paired t-test")))) +
        scale_x_continuous(breaks = seq(0, 2, 0.2)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle = element_text(hjust = 0.5, size = 12))
    })

    # ------------------------ Properties ------------------------------
    output$properties <- renderUI({
      dist <- input$distType

      if (is.null(dist) || dist == "") {
        return(tags$div(
          tags$h4("Select a distribution to see properties"),
          tags$p("Use the left panel to pick a distribution and adjust parameters.")
        ))
      }

      properties <- switch(dist,

                           "unif" = list(
                             name = "Uniform Distribution",
                             definition = "A continuous distribution where all intervals of the same length within [a, b] are equally likely.",
                             eq = "$$f(x) = \\frac{1}{b-a}, \\quad a \\le x \\le b$$",
                             mean = "$$E[X] = \\frac{a+b}{2}$$",
                             variance = "$$\\mathrm{Var}(X) = \\frac{(b-a)^2}{12}$$"
                           ),

                           "norm" = list(
                             name = "Normal Distribution",
                             definition = "A bell-shaped continuous distribution fully described by its mean \\(\\mu\\) and variance \\(\\sigma^2\\).",
                             eq = "$$f(x)=\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{1}{2}\\left(\\frac{x-\\mu}{\\sigma}\\right)^2}$$",
                             mean = "$$E[X] = \\mu$$",
                             variance = "$$\\mathrm{Var}(X)=\\sigma^2$$"
                           ),

                           "exp" = list(
                             name = "Exponential Distribution",
                             definition = "Models waiting time between Poisson events. Memoryless distribution.",
                             eq = "$$f(x)=\\lambda e^{-\\lambda x}, \\quad x\\ge0$$",
                             mean = "$$E[X] = 1/\\lambda$$",
                             variance = "$$\\mathrm{Var}(X) = 1/\\lambda^2$$"
                           ),

                           "gamma" = list(
                             name = "Gamma Distribution",
                             definition = "Generalization of the exponential distribution with a shape parameter \\(\\alpha\\). (Using rate \\(\\beta\\)).",
                             eq = "$$f(x)=\\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x},\\; x>0$$",
                             mean = "$$E[X] = \\frac{\\alpha}{\\beta}$$",
                             variance = "$$\\mathrm{Var}(X)=\\frac{\\alpha}{\\beta^2}$$"
                           ),

                           "weibull" = list(
                             name = "Weibull Distribution",
                             definition = "Used in reliability engineering to model life duration.",
                             eq = "$$f(x)=\\frac{k}{\\lambda} \\left(\\frac{x}{\\lambda}\\right)^{k-1} e^{-(x/\\lambda)^k},\\; x\\ge0$$",
                             mean = "$$E[X] = \\lambda \\Gamma\\left(1+\\frac{1}{k}\\right)$$",
                             variance = "$$\\mathrm{Var}(X)=\\lambda^2\\left[\\Gamma\\left(1+\\frac{2}{k}\\right) - \\left(\\Gamma\\left(1+\\frac{1}{k}\\right)\\right)^2\\right]$$"
                           ),

                           "binom" = list(
                             name = "Binomial Distribution",
                             definition = "Counts the number of successes in \\(n\\) independent Bernoulli trials.",
                             eq = "$$P(X=k)=\\binom{n}{k} p^k (1-p)^{n-k}$$",
                             mean = "$$E[X] = np$$",
                             variance = "$$\\mathrm{Var}(X)=np(1-p)$$"
                           ),

                           "pois" = list(
                             name = "Poisson Distribution",
                             definition = "Counts number of rare events occurring in a fixed interval.",
                             eq = "$$P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}$$",
                             mean = "$$E[X]=\\lambda$$",
                             variance = "$$\\mathrm{Var}(X)=\\lambda$$"
                           ),

                           "geom" = list(
                             name = "Geometric Distribution",
                             definition = "Counts number of failures before the first success (support \\(k=0,1,2,...\\)).",
                             eq = "$$P(X=k)=(1-p)^k p$$",
                             mean = "$$E[X]=\\dfrac{1-p}{p}$$",
                             variance = "$$\\mathrm{Var}(X)=\\dfrac{1-p}{p^2}$$"
                           ),

                           "nbinom" = list(
                             name = "Negative Binomial Distribution",
                             definition = "Counts number of failures before \\(r\\) successes occur.",
                             eq = "$$P(X=k)=\\binom{k+r-1}{k} p^r (1-p)^k$$",
                             mean = "$$E[X] = \\dfrac{r(1-p)}{p}$$",
                             variance = "$$\\mathrm{Var}(X)=\\dfrac{r(1-p)}{p^2}$$"
                           )
      )

      # Use withMathJax to render LaTeX properly
      withMathJax(
        tags$div(
          tags$h2(properties$name),
          tags$h4("Definition"),
          tags$p(properties$definition),
          tags$h4("Probability Formula (PDF/PMF)"),
          helpText(properties$eq),
          tags$h4("Mean"),
          helpText(properties$mean),
          tags$h4("Variance"),
          helpText(properties$variance)
        )
      )
    })

  }

  shinyApp(ui, server)

}
