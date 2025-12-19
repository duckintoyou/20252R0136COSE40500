# a. Use an STL decomposition to calculate the trend-cycle and seasonal indices. (Experiment with having fixed or changing seasonality.)
# STL decomposition with fixed seasonality
fixed <- stl(bricksq, s.window = "periodic", robust = TRUE)

# STL decomposition with changing seasonality
changing <- stl(bricksq, s.window = 5,robust = TRUE)

# plot decomposed data
autoplot(stl_brick_fixed_st) +
  ggtitle("fixed seasonality")

autoplot(stl_brick_changing_st) +
  ggtitle("changing seasonality")
# can see changing seasonal component and smaller remainders.

# b. Compute and plot the seasonally adjusted data.
# plot data which are decomposed by STL with fixed seasonality
autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(fixed),
            series = "Trend-cycle") +
  autolayer(seasadj(fixed),
            series = "Seasonally Adjusted") +
  ggtitle("brick production",
          subtitle = "STL with fixed seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted"))

# plot data which are decomposed by STL with changing seasonality
autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(changing),
            series = "Trend-cycle") +
  autolayer(seasadj(changing),
            series = "Seasonally Adjusted") +
  ggtitle("brick production",
          subtitle = "STL with changing seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted"))

stl_brick_fixed_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast of data",
          subtitle = "STL decomposition with fixed seasonality")

stl_brick_changing_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast of data",
          subtitle = "STL decomposition with changing seasonality")

autoplot(stlf(bricksq))

stlf_brick = stlf(bricksq)
checkresiduals(stlf_brick)


stlf_brick_robust <- stlf(bricksq, robust = TRUE)
autoplot(stlf_brick_robust)
checkresiduals(stlf_brick_robust)

trainset_brick <- subset(bricksq, 
                         end = length(bricksq) - 8)
testset_brick <- subset(bricksq,
                        start = length(bricksq) - 7)

snaive_brick <- snaive(trainset_brick)
stlf_brick_part <- stlf(trainset_brick, robust = TRUE)

# plot data and forecast results
autoplot(bricksq, series = "Original") +
  autolayer(stlf_brick_part, PI = FALSE, size = 1,
            series = "stlf") +
  autolayer(snaive_brick, PI = FALSE, size = 1,
            series = "snaive") +
  scale_color_manual(values = c("gray50", "blue", "red"),
                     breaks = c("Original", "stlf", "snaive")) +
  scale_x_continuous(limits = c(1990, 1994.5)) +
  scale_y_continuous(limits = c(300, 600)) +
  guides(colour = guide_legend(title = "Data")) +
  ggtitle("Forecast from stlf and snaive")
