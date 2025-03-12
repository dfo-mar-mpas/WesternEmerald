theme_custom <- function(base_size = 12, base_family = "") { #this is a custom plotting theme to make plots more readable on coloured backgrounds 
  theme(
    # Transparent background
    panel.background = element_rect(fill = "transparent", color = "grey40"),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    # Grey text and lines
    text = element_text(color = "grey40", family = base_family, size = base_size),
    axis.text = element_text(color = "grey40", size = rel(0.9)),
    axis.title = element_text(color = "grey40", size = rel(1)),
    axis.ticks = element_line(color = "grey60"),
    axis.line = element_line(color = "grey60"),
    
    # Grid lines in grey
    panel.grid.major = element_line(color = "grey80", size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Legend styling
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = "top",
    legend.title = element_text(color = "grey40"),
    legend.text = element_text(color = "grey40"),
    
    # X-axis rotation (can be overridden later if needed)
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
    
    # Panel border
    panel.border = element_blank(),
    
    # Facet styling
    strip.background = element_rect(fill = "transparent", color = "grey60"),
    strip.text = element_text(color = "grey40", size = rel(0.9))
  )
}

theme_custom_2 <- function(base_size = 12, base_family = "") { #this is a custom plotting theme to make plots more readable on coloured backgrounds 
  theme(
    # Transparent background
    panel.background = element_rect(fill = "transparent", color = "grey40"),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    # Grey text and lines
    text = element_text(color = "grey40", family = base_family, size = base_size),
    axis.text = element_text(color = "grey40", size = rel(0.9)),
    axis.title = element_text(color = "grey40", size = rel(1)),
    axis.ticks = element_line(color = "grey60"),
    axis.line = element_line(color = "grey60"),
    
    # Grid lines in grey
    panel.grid.major = element_line(color = "grey80", size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Legend styling
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(color = "grey40"),
    legend.text = element_text(color = "grey40"),
    
    # X-axis rotation (can be overridden later if needed)
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
    
    # Panel border
    panel.border = element_blank(),
    
    # Facet styling
    strip.background = element_rect(fill = "transparent", color = "grey60"),
    strip.text = element_text(color = "grey40", size = rel(0.9))
  )
}