library(ggplot2)
library(nflplotR)
## variables to change: team_aggregate_metrics, pff_predicted, survival_probability ##

### Plot for team aggregated metrics ###

annotation <- data.frame(
  x = c(.75675,.75675, 0.7465, 0.7465),
  y = c(.96,.7, .96, .7),
  label = c("low expected pressure, handled well", "low expected pressure, handled poorly", "high expected pressure, handled well", "high expected pressure, handled poorly")
)

ggplot2::ggplot(team_aggregate_metrics, aes(x = pff_predicted, y = survival_probability)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(v_var = pff_predicted , h_var = survival_probability)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = poss_team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Context Implied Survival Probability",
    y = "Spatial Implied Survival Probability",
    caption = "",
    title = "Survival Probability Comparison"
  ) + #xlim(0.725, 0.777) + ylim(0.6, 1)
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  ) + geom_label(data=annotation, aes( x=x, y=y, label=label),                 , 
                 color="blue", 
                 size=6 , angle=45)


#### Plot for individuals at a position ####

annotation2 <- data.frame(
  x = c(.95125,.95125, 0.9365, 0.9365),
  y = c(.96,.88, .96, .88),
  label = c("low expected pressure, handled well", "low expected pressure, handled poorly", "high expected pressure, handled well", "high expected pressure, handled poorly")
)

ggplot2::ggplot(player_aggregated_metrics, aes(x = context_prob, y = spatial_prob)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(v_var = context_prob , h_var = spatial_prob)) +
  geom_label(aes(label = displayName)) +
  ggplot2::labs(
    x = "Context Implied Survival Probability",
    y = "Spatial Implied Survival Probability",
    caption = "",
    title = "Context Implied (PFF) vs. Spatial Implied (Voronoi Bin) Survival Probability") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  ) + geom_label(data=annotation2, aes( x=x, y=y, label=label),                 , 
                 color="blue", 
                 size=5 , angle=45)


player_aggregated_metrics %>% 
  ggplot(aes(displayName, space_metric)) + 
  geom_col(aes(fill = "blue"), position = "dodge") + 
  theme_bw()


for (i in 1:nrow(trials)) {
  print(trials[i, "displayName"])
}
