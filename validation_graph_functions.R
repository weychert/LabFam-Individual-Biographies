generate_cohort_percet_plot <- function(data, by_grid = 5,
                                    min_limit_y = 0,
                                    limiy_y = 30, 
                                    fontname = "Calibri", 
                                    legend_z = 15, x = 3, y_limit = 30, 
                                    axis_font_size = 15, 
                                    margin_top = 0, margin_right = -20, 
                                    margin_bottom = 0, margin_left = 0, 
                                    decide_legend = "right", 
                                    title_y = "% Never Married", 
                                    title_x = "Cohort", 
                                    letter_x = "", 
                                    country_x = "BHPS&UKHLS", 
                                    color_vertical = "gray",
                                    linetype_1 = "dashed",
                                    external = "external",
                                    UNWEIHGTED_lib = "UNWEIHGTED_lib",
                                    UNWEIHGTED_upper = "UNWEIHGTED_upper",
                                    UNWEIHGTED_lower = "UNWEIHGTED_lower",
                                    internal = "internal",
                                    internal_upper = "internal_upper",
                                    internal_lower = "internal_lower",
                                    what_type = "Never Married",
                                    cohort = "cohort",
                                    true_weights = T
                                    ) {
  
  # Set font
  windowsFonts(A = windowsFont(fontname))
  
  color_values <- setNames(
    c("gray", "black", "#5c4eb1","#653465" ),
    c(paste0("% External ", what_type ," in England and Wales"),
      paste0("% External ", what_type),
      paste0("% Internal ", what_type),
      paste0("% LIB ", what_type))
  )
  
  color_values_1 <- setNames(
    c("black", "#5c4eb1","#653465" ),
      c(paste0("% External ", what_type),
      paste0("% Internal ", what_type),
      paste0("% LIB ", what_type)
  ))
  
  p <- ggplot(data) +
    
    # External 
    geom_point(aes(x = as.numeric(factor(.data[[cohort]])) - 0.2,
                   y = .data[[external]],
                   color = paste0("% External ", what_type )), size = 3) +
    
    # Internal
    geom_point(aes(x = as.numeric(factor(.data[[cohort]])) ,
                   y = .data[[internal]],
                   color = paste0("% Internal ", what_type)), size = 3) +
    
    geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]])),
                      ymin = .data[[internal_lower]],
                      ymax = .data[[internal_upper]],
                      color = paste0("% Internal ", what_type)), width = 0.1) +
    
    # Unweighted LIB 
    geom_point(aes(x = as.numeric(factor(.data[[cohort]])) + 0.2,
                   y = .data[[UNWEIHGTED_lib]],
                   color = paste0("% LIB ", what_type)), size = 3) +
    
    geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]])) + 0.2,
                      ymin = .data[[UNWEIHGTED_lower]], 
                      ymax = .data[[UNWEIHGTED_upper]],
                      color = paste0("% LIB ", what_type)), width = 0.1) +
    
    scale_x_continuous(breaks = 1:4, labels = data[,cohort]) +
    
    # Add labels and theme
    labs(x = title_x, y = title_y, title = paste0(letter_x, " ", country_x)) +
    
    guides(color = guide_legend(title = "Legend")) +
    
    # Customize colors in the legend
    scale_color_manual(
      values = c(color_values_1
      ),
      breaks = c(
        paste0("% External ", what_type ),
        paste0("% Internal ", what_type),
        paste0("% LIB ", what_type)
      )) +
    
    # Theme adjustments
    # theme_minimal() +
    theme(
      plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left),
      legend.position = decide_legend,
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = axis_font_size, 
                                 color = "black", family = "A"),
      axis.title.x = element_text(color = "black", size = axis_font_size, family = "A"),
      axis.title.y = element_text(color = "black", size = axis_font_size, family = "A"),
      plot.title = element_text(family = "A", face = "bold", size = 15, color = "black"),
      axis.text.y = element_text(color = "black", size = axis_font_size, family = "A"),
      legend.text = element_text(face = "bold", color = "black", size = legend_z, family = "A"),
      legend.title = element_text(face = "bold", color = "black", size = legend_z, family = "A"),
      # panel.grid.major = element_blank(),
      panel.grid.major.y = element_line(color = color_vertical, linetype = linetype_1), # keeps major grid lines.
      panel.grid.minor.y = element_blank(), # removes minor grid lines for clarity.
      panel.background = element_rect(fill = "white", color = NA),  # White background
      plot.background = element_rect(fill = "white", color = NA),    # White overall plot
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    
    # scale_y_continuous(limits = c(min_limit_y, limiy_y)) +
    scale_y_continuous(limits = c(min_limit_y, limiy_y), 
                       breaks = seq(min_limit_y, limiy_y, by = by_grid),
                       # minor_breaks = waiver() 
    )+
    # scale_y_continuous(expand = c(min_limit_y, limiy_y)) +
    # coord_cartesian(clip = "off") +
    
    geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), color = color_vertical, linetype = linetype_1)
  
  
  if (country_x  == "BHPS&UKHLS") {
    p = p+ geom_point(aes(x = as.numeric(factor(cohort)) - 0.4,
                          y = external_EW,
                          color = paste0("% External ", what_type ," in England and Wales")), size = 3)+
      scale_color_manual(
        values = c(
          color_values
        ),
        breaks = c(
          paste0("% External ", what_type ," in England and Wales"),
          paste0("% External ", what_type),
          paste0("% Internal ", what_type),
          paste0("% LIB ", what_type)
        ))
    
    return(p)
  }
  
  if (true_weights == T) {
    p = p+ geom_point(aes(x = as.numeric(factor(Wave)),
                          y = percent_cross_weights), size = 3, color = "gray")

    
    return(p)
  }
  
  
  return(p)
}

mab_graph <- function(data = usa, 
                      BORN_Y_category = "BORN_Y_category", 
                      CMAB = "CMAB",  
                      mean_NO_weigths_lib = "mean_lib_no_weigths",
                      letter_x = "",
                      lower_no_weigths = "lower_no_weigths",
                      upper_no_weigths = "upper_no_weigths",
                      linetype_1 = "dashed",
                      text_x = "", 
                      x = "", 
                      country_x = "",
                      limit_x = 20,
                      font_size_xaxis = 15, 
                      font_size_label = 12, 
                      pos_leg = "none", 
                      title_y = "Female Mean Age at Birth",
                      limit_y = 35,
                      include_UK = FALSE,  # New parameter to toggle UK data
                      CMAB_E_W = "CMAB_E_W",
                      external_EW = "external_EW",
                      by_int = 5,
                      mam =F
) {  
  
  # Example of consistent color values
  # color_labels <- c("HFD CMAB", "LIB CMAB")
  color_values_1 <- c("Exteranl" = "black", "LIB " = "#a454a3")
  
  color_values <- setNames(
    c("gray", "black", "#a454a3" ),
    c(
      paste0("HFD ",CMAB," - England and Wales"),
      paste0("HFD ",CMAB),
      paste0("LIB ",CMAB)
      )
  )
  
  p <- ggplot(data) +
    geom_point(aes(x = as.numeric(factor(.data[[BORN_Y_category]])), 
                   y = !!sym(CMAB), 
                   color = "External"), size = 3) +
    geom_text(aes(x = as.numeric(factor(.data[[BORN_Y_category]])), 
                  y = !!sym(CMAB), 
                  label = round(!!sym(CMAB), 2)), 
              vjust = -1, size = 4, color = "black") +
    
    # No weights
    geom_point(aes(x = as.numeric(factor(.data[[BORN_Y_category]]))+0.3, 
                   y = !!sym(mean_NO_weigths_lib), 
                   color = "LIB"), 
               size = 3, shape = 17) +
    geom_text(aes(x = as.numeric(factor(.data[[BORN_Y_category]]))+0.3, 
                  y = !!sym(mean_NO_weigths_lib), 
                  label = round(!!sym(mean_NO_weigths_lib), 2)), 
              vjust = -1, size = 4, color = "#a454a3") +
    
    geom_errorbar(aes(x = as.numeric(factor(.data[[BORN_Y_category]]))+0.3,
                      ymin = !!sym(lower_no_weigths), ymax = !!sym(upper_no_weigths), 
                      color =  "LIB"),
                  width = 0.1) +
    
    # Labels and titles
    labs(
      title = paste0(letter_x, country_x),
      x = "Birth Cohort",
      y = title_y,
      color = "Legend"
      # caption = text_x
    ) +
    
    # Theme adjustments
    # theme_minimal() +
    theme(
      legend.position = pos_leg,
      legend.title = element_text(family = "A", size = 15, color = "black", face = "bold"),
      legend.text =  element_text(family = "A", size = 15, color = "black", face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = font_size_xaxis, color = "black", family = "A"),
      axis.text.y = element_text(size = font_size_xaxis, color = "black", family = "A"),
      axis.title.x = element_text(face = "bold", color = "black", size = 10, family = "A"),
      axis.title.y = element_text(face = "bold", color = "black", size = 10, family = "A"),
      plot.title = element_text(family = "A", face = "bold", size = 15, color = "black"),
      plot.caption = element_text(family = "A", color = "black", size = 9, face = "italic", hjust = 0),
      # panel.grid.major = element_blank()
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed"), # keeps major grid lines.
      panel.grid.minor.y = element_blank(), # removes minor grid lines for clarity.
      panel.background = element_rect(fill = "white", color = NA),  # White background
      plot.background = element_rect(fill = "white", color = NA),    # White overall plot
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    
    # Adjust y-axis limits
    scale_y_continuous(limits = c(limit_x, limit_y), 
                       breaks = seq(limit_x, limit_y, by = by_int))
     
    if (country_x  != "BHPS&UKHLS") {
      p = p+
        # Manual color scale for legend
        
        scale_color_manual(
          values = c( "black", "#a454a3"),
          breaks = c("External", "LIB"
          )
        )+
        
        geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), color = "gray", linetype = "dashed")
      # return(p)
    }

  
  if (country_x  == "BHPS&UKHLS") {
    p = p + geom_point(aes(x = as.numeric(factor(.data[[BORN_Y_category]])) - 0.3,
                          y = !!sym(CMAB_E_W),
                          color = "England and Wales"), size = 3)+
      geom_text(aes(x = as.numeric(factor(.data[[BORN_Y_category]]))- 0.3, 
                    y = as.numeric(!!sym(CMAB_E_W)), 
                    label = round( as.numeric(!!sym(CMAB_E_W)), 2)), 
                vjust = -1, size = 4, color = "gray") +
      scale_color_manual(
        values = c("gray","black", "#a454a3"),
        breaks = c("England and Wales","External", "LIB"
        )
      )+geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), color = "gray", linetype = "dashed")
    
    
  }
  
  if (mam ==T) {
    p = p +scale_x_continuous(breaks = 1:4, labels = c("(2000,2004]", "(2005,2009]", "(2010,2014]", "(2015,2019]"))
    return(p)
    
  } else{
    p = p +  scale_x_continuous(breaks = 1:4, labels = c("1941-1950", "1951-1960", "1961-1970", "1971-1977")) 
    return(p)
  }
  
}

# ####################### with weights ###########################################
# generate_cohort_percet_plot <- function(data, 
#                                         min_limit_y = 0,
#                                         limiy_y = 30, 
#                                         fontname = "Calibri", 
#                                         legend_z = 15, x = 3, y_limit = 30, 
#                                         axis_font_size = 15, 
#                                         margin_top = 0, margin_right = -20, 
#                                         margin_bottom = 0, margin_left = 0, 
#                                         decide_legend = "right", 
#                                         title_y = "", 
#                                         title_x = "Cohort", 
#                                         letter_x = "", 
#                                         country_x = "BHPS&UKHLS", 
#                                         color_vertical = "gray",
#                                         linetype_1 = "dashed",
#                                         external = "external",
#                                         UNWEIHGTED_lib = "UNWEIHGTED_lib",
#                                         UNWEIHGTED_upper = "UNWEIHGTED_upper",
#                                         UNWEIHGTED_lower = "UNWEIHGTED_lower",
#                                         internal = "internal",
#                                         internal_upper = "internal_upper",
#                                         internal_lower = "internal_lower",
#                                         what_type = "",
#                                         cohort = "cohort",
#                                         true_weights = T
# ) {
#   
#   # Set font
#   windowsFonts(A = windowsFont(fontname))
# 
#   
#   color_values_1 <- setNames(
#     c("black", "#5c4eb1","gray","#653465", "red"),
#     c(paste0("% External ", what_type),
#       paste0("% Internal ", what_type),
#       paste0("% Internal weighted", what_type),
#       paste0("% LIB ", what_type),
#       paste0("% LIB weighted", what_type)
#     ))
#   
#   p <- ggplot(data) +
#     
#     # External 
#     geom_point(aes(x = as.numeric(factor(.data[[cohort]])) - 0.4,
#                    y = .data[[external]],
#                    color = paste0("% External ", what_type )), size = 3) +
#     
#     # Internal
#     geom_point(aes(x = as.numeric(factor(.data[[cohort]]))-0.2 ,
#                    y = internal,
#                    color = paste0("% Internal ", what_type)), size = 3, color = "#5c4eb1") +
#     
#     geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]]))-0.2,
#                       ymin = .data[[internal_lower]],
#                       ymax = .data[[internal_upper]],
#                       color = paste0("% Internal ", what_type)), width = 0.1) +
#     
#     # Internal weighted 
#     
#     geom_point(aes(x = as.numeric(factor(.data[[cohort]])),
#                           y = internal_w), size = 3, color = "gray")+
# 
# 
#     geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]])),
#                       ymin = weighted_internal_lower,
#                       ymax = weighted_internal_upper,
#                       color = paste0("% Internal weighted", what_type)), width = 0.1) +
#     # 
#     # Unweighted LIB 
#     geom_point(aes(x = as.numeric(factor(.data[[cohort]])) + 0.2,
#                    y = .data[[UNWEIHGTED_lib]],
#                    color = paste0("% LIB ", what_type)), size = 3) +
#     
#     geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]])) + 0.2,
#                       ymin = .data[[UNWEIHGTED_lower]], 
#                       ymax = .data[[UNWEIHGTED_upper]],
#                       color = paste0("% LIB ", what_type)), width = 0.1) +
#     
#     #### weighted
#     
#     geom_point(aes(x = as.numeric(factor(.data[[cohort]])) + 0.4,
#                    y = lib_w,
#                    color = paste0("% LIB weighted", what_type)), size = 3) +
#     
#     geom_errorbar(aes(x = as.numeric(factor(.data[[cohort]])) + 0.4,
#                       ymin = weighted_lib_lower, 
#                       ymax = weighted_lib_upper,
#                       color = paste0("% LIB weighted", what_type)), width = 0.1) +
#     
#     scale_x_continuous(breaks = 1:4, labels = data[,cohort]) +
#     
#     # Add labels and theme
#     labs(x = title_x, y = title_y, title = paste0(letter_x, " ", country_x)) +
#     
#     guides(color = guide_legend(title = "Legend")) +
#     
#     # Customize colors in the legend
#     scale_color_manual(
#       values = c(color_values_1
#       ),
#       breaks = c(
#         paste0("% External ", what_type),
#         paste0("% Internal ", what_type),
#         paste0("% Internal weighted", what_type),
#         paste0("% LIB ", what_type),
#         paste0("% LIB weighted", what_type)
#       )) +
#     
#     # Theme adjustments
#     # theme_minimal() +
#     theme(
#       plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left),
#       legend.position = decide_legend,
#       axis.text.x = element_text(angle = 0, hjust = 0.5, size = axis_font_size, 
#                                  color = "black", family = "A"),
#       axis.title.x = element_text(color = "black", size = axis_font_size, family = "A"),
#       axis.title.y = element_text(color = "black", size = axis_font_size, family = "A"),
#       plot.title = element_text(family = "A", face = "bold", size = 15, color = "black"),
#       axis.text.y = element_text(color = "black", size = axis_font_size, family = "A"),
#       legend.text = element_text(face = "bold", color = "black", size = legend_z, family = "A"),
#       legend.title = element_text(face = "bold", color = "black", size = legend_z, family = "A"),
#       # panel.grid.major = element_blank(),
#       panel.grid.major.y = element_line(color = color_vertical, linetype = linetype_1), # keeps major grid lines.
#       panel.grid.minor.y = element_blank(), # removes minor grid lines for clarity.
#       panel.background = element_rect(fill = "white", color = NA),  # White background
#       plot.background = element_rect(fill = "white", color = NA),    # White overall plot
#       axis.ticks.y = element_blank(),
#       axis.ticks.x = element_blank()
#     ) +
#     
#     scale_y_continuous(limits = c(min_limit_y, limiy_y), 
#                        breaks = seq(min_limit_y, limiy_y, by = 5),
#                        # minor_breaks = waiver() 
#     )+
# 
#     geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), color = color_vertical, linetype = linetype_1)
#   
#   
#   
#   return(p)
# }
