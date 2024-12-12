      ### CONTAINS FUNCTIONS TO PLOT FIGURES AND TO SAVE FIGURES ###


   ## Functions to plot figures ##

# A function to plot the exploratory figure - a scatter plot:

plot_exploratory_figure <- function(data_subset){
  data_subset %>%
    ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + #specifies the data used to make the plot and the axes of the plot
    geom_point(fill = "orange", shape = 21, size = 3, alpha = 0.7) + #plots the data points on the plot: determines their colour, shape, size and transparency
    labs(
      title = "Body Mass versus Flipper Length",  #gives a title to the plot
      x = "Flipper Length (mm)",  #gives a title to the x-axis
      y = "Body Mass (g)"   #gives a title to the y-axis
    ) +
    theme_bw() + #applies a "black and white" aesthetic theme, allowing better readability
    theme(
      plot.title = element_text(hjust = 0.5), #positions the title it centrally on the horizontal axis
      plot.title.position = "plot")} #positions the title above the pot




# A function to plot the assessment of the assumptions figure

plot_assumptions_assessment_figure <- function(lm_model) {
  par(mfrow = c(1, 2))  # Set up the plotting layout (1 row, 2 columns)
  
  # plots a Q-Q plot 
  residuals <- resid(lm_model)  # extracts the residuals
  qqnorm(residuals)  # generates the Q-Q plot
  qqline(residuals, col = "red", lty = 2)  # Adds a red dotted reference line
  
  # pots a residuals vs fitted plot
  fitted_values <- fitted(lm_model)  # extracts the fitted values
  plot(fitted_values, residuals, 
    main = "Residuals vs Fitted Values", 
    xlab = "Fitted Values", 
    ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)  # adds an horizontal red dotted line at y=0
  par(mfrow = c(1, 1))  # resets to default the plotting layout
}


# A function to plot the result figure - scatter plot with the regression line and its equation:

plot_results_figure <- function(data_subset, lm_model) {
  #extract the coefficients from the linear model and form the equation
  slope <- round(coef(lm_model)[2], 2)
  intercept <- round(coef(lm_model)[1], 2)
  equation <- paste("y = ", intercept, "+", slope, "x")
  
  #create the figure
  data_subset %>%
    ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
    geom_point(fill = "orange", shape = 21, size = 3, alpha = 0.7) +  # Plot the data points
    labs(
      title = "Body Mass versus Flipper Length - results", 
      x = "Flipper Length (mm)", 
      y = "Body Mass (g)"
    ) +
    geom_smooth(method = "lm", col = "darkblue", size = 1, linetype = "dashed", alpha = 0.7) +  # Adds the regression line
    annotate(
      "text",  #adds a text annotation to the plot
      x = 190, #sets the x-coordinate for the position of the annotation on the plot
      y = 5500, #sets the y-coordinate 
      label = equation, #makes the text annotation be the equation
      color = "darkblue", size = 5) +  # Add the regression equation as text
    theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5), 
      plot.title.position = "plot"
    )
}


    ## Functions to save figures ##

# A function to save figures as png:
save_figure_png <- function(plot_function, data, filename, size, res, scaling, ...) {
  agg_png(filename, 
    width = size, 
    height = size, 
    units = "cm", 
    res = res, 
    scaling = scaling) # Creates a placeholder file using these parameters
  figure <- plot_function(data, ...) #calls the provided plotting function and data to make the appropriate figure
  print(figure)  #prints the figure to the empty file
  dev.off() #ends plotting functionality
}


# A function to save figures as svg:

save_figure_svg <- function(plot_function, data, filename, size, scaling, ...) {
  size_inches <- size / 2.54
  svglite(filename, 
    width = size_inches, 
    height = size_inches, 
    scaling = scaling) #creates a placeholder file using these parameters
  figure <- plot_function(data, ...) #calls the provided plotting function to make the appropriate figure
  print(figure) #print the figure to the empty file
  dev.off() #ends plotting functionality
}

