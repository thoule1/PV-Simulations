library(dplyr)
library(glue)

summary_text <- all_results %>%
  summarize(
    decision_rate = mean(decision),
    mean_effect = mean(mean_effect),
    coverage = mean(lower_95 < 0 & upper_95 > 0),
    mean_width = mean(upper_95 - lower_95),
    mean_p_lt_0.2 = mean(p_lt_0.2),
    mean_p_lt_0.5 = mean(p_lt_0.5)
  ) %>%
  mutate(
    text = glue(
      "Simulation Summary:\n",
      "-------------------\n",
      "Decision Rate (Pr(Effect > 0) > 0.95): {round(decision_rate * 100, 1)}%\n",
      "Mean Posterior Effect Estimate: {round(mean_effect, 3)}\n",
      "95% CI Coverage: {round(coverage * 100, 1)}%\n",
      "Mean CI Width: {round(mean_width, 3)}\n",
      "Pr(Effect < -0.2): {round(mean_p_lt_0.2 * 100, 1)}%\n",
      "Pr(Effect < -0.5): {round(mean_p_lt_0.5 * 100, 1)}%"
    )
  ) %>%
  pull(text)

library(ggplot2)

ci_plot <- ggplot(all_results, aes(y = sim)) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.3, color = "gray60") +
  geom_point(aes(x = mean_effect), color = "steelblue", size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Credible Intervals for Treatment Effect",
    x = "Treatment Effect (log-odds)",
    y = "Simulation"
  ) +
  theme_minimal()

library(patchwork)

text_plot <- ggplot() +
  annotate("text", x = 0, y = 1, label = summary_text,
           hjust = 0.5, vjust = .5, size = 4.5, family = "mono") +
  theme_void()

#combine the plots using gridextra
combined_plot <- ci_plot + text_plot +
  plot_layout(ncol = 2, heights = c(2, 1)) +
  plot_annotation(
    title = "Simulation Results",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

combined_plot
