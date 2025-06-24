summarize_simulation_results <- function(
    all_results,
    n_per_group,
    p_control,
    p_treatment
) {
  library(dplyr)
  library(glue)
  library(ggplot2)
  library(patchwork)
  
summary_text <- all_results %>%
    summarize(
      decision_rate  = mean(decision),
      mean_effect    = mean(mean_effect),
      mean_width     = mean(upper_95 - lower_95),
      mean_p_lt_0    = mean(p_lt_0),
      mean_p_lt_0.2  = mean(p_lt_0.2)
    ) %>%
    mutate(
      text = glue(
        "Decision Rate (Pr(Effect > 0% ARR) > 0.95): {round(decision_rate * 100, 1)}%\n",
        "Mean Posterior Effect Estimate: {round(mean_effect, 3)}\n",
        "Mean CrI Width: {round(mean_width, 3)}\n",
        "Mean Pr(Effect > 0% ARR): {round(mean_p_lt_0 * 100, 1)}%\n",
        "Mean Pr(Effect > 2.5% ARR): {round(mean_p_lt_0.2 * 100, 1)}%\n"
      )
    ) %>%
    pull(text)
  

ci_plot <- ggplot(all_results, aes(y = sim)) +
    geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.3, color = "gray60") +
    geom_point(aes(x = mean_effect), color = "steelblue", size = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(all_results$mean_effect, na.rm = TRUE), linetype = "dashed", colour = "darkgreen") +
    labs(
      x = "Treatment Effect (log-odds)",
      y = "Simulation"
    ) +
    theme_minimal()
  
  
text_plot <- ggplot() +
    annotate("text", x = 0, y = 1, label = summary_text,
             hjust = 0, vjust = 1, size = 4.5, family = "mono") +
    xlim(0, 1) + ylim(0, 1) +  # Keeps the text aligned top-left
    theme_void()
  

combined_plot <- text_plot / ci_plot +
    plot_layout(heights = c(1, 4)) +
    plot_annotation(
      title = paste0("Simulation Results: n/group = ", n_per_group,
                     " (", p_control, " vs ", p_treatment, ")"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    )
  
  
  return(combined_plot)
}

