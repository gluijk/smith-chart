# Drawing the Smith chart
# www.overfitting.net
# https://www.overfitting.net/2025/11/la-carta-del-astuto-smith-con-r.html


library(Cairo)

# Functions

# Constant-resistance circle
draw_r_circle <- function(r, n = 4000, col = "gray60") {
    center <- r / (1 + r)
    radius <- 1 / (1 + r)
    theta <- seq(0, 2*pi, length = n)
    lines(center + radius * cos(theta),
          radius * sin(theta), col = col, lwd = 2)
}

# Constant-reactance arc (parametric method)
draw_x_arc <- function(x, r_min = 0, r_max = 500, n = 500, col = "gray60") {
    # r <- seq(r_min, r_max, length.out = n)  # linear sampling of r
    # EXP=3
    # r <- seq(r_min^(1/EXP), r_max^(1/EXP), length.out = n)^EXP  # exp sampling of r
    r <- exp(seq(log(r_min + 1), log(r_max + 1), length.out = n)) - 1  # log sampling of r
    z <- r + 1i * x
    Gamma <- (z - 1) / (z + 1)
    lines(c(Re(Gamma),1), c(Im(Gamma),0), col = col, lwd = 2)  # close arc up to (1,0)
}


CairoPNG("smithchart.png", width = 1920, height = 1920)

    labels = TRUE
    
    # Plotting parameters
    plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, axes = FALSE,
         xlab = ifelse(labels, "Re(Γ)", ""),
         ylab = ifelse(labels, "Im(Γ)", ""),
         main = ifelse(labels, "The Smith Chart", ""),
         cex.lab = 3, cex.main = 4,
         xaxt = "n", yaxt = "n")

    # Draw chart grid
    x_values <- c(0.2, 0.5, 1, 2, 5)  # Im axis arcs
    r_values <- c(0, x_values)  # Re axis circles

    for (r in r_values) draw_r_circle(r)
    for (x in x_values) {
        draw_x_arc( x)
        draw_x_arc(-x)
    }

    
    # Outer unit circle
    theta <- seq(0, 2*pi, length = 4000)
    lines(cos(theta), sin(theta), lwd = 4)
    
    # Axis lines
    abline(h = 0, v = 0, col = "black", lwd = 2)
    
    if (labels) {
        # Resistance labels
        for (r in r_values) {
            x_pos <- (r - 1) / (r + 1)
            text(x_pos, 0.04, labels = as.character(r), cex = 4, col = "blue")
        }
        
        # Reactance labels (upper and lower)
        for (x in x_values) {
            # Compute a reference point near r=0.2
            z <- 0.2 + 1i * x
            Gamma <- (z - 1) / (z + 1)
            text(Re(Gamma), Im(Gamma) + 0.04, paste0("+j", x),
                 cex = 4, col = "darkgreen")
    
            z <- 0.2 - 1i * x
            Gamma <- (z - 1) / (z + 1)
            text(Re(Gamma), Im(Gamma) - 0.04, paste0("-j", x),
                 cex = 4, col = "darkgreen")
        }

        # Reflection coefficient example
        Gamma <- 0.5 * exp(1i * pi/4)
        points(Re(Gamma), Im(Gamma), pch = 19, col = "red")
        text(Re(Gamma)+0.04, Im(Gamma), expression(Gamma), cex = 4, col = "red")
    }
    
dev.off()
