# How to make a publication quality plot
# libraries
library(ggpubr)
library(sjPlot)

#### Plot all figures together ####
# Must run ggplot first- plot1 and plot2 are ggplot plots

combined1=ggarrange(plot1, plot2, 
                labels = c("A", "B"),
                ncol = 1, nrow = 2,
                common.legend = TRUE,
                legend = "bottom")

#Plot figures with dpi=300
save_plot(combined1, "output_name.tif", width = 30, height = 20, dpi = 300,
          legend.textsize = 20, legend.titlesize = 20,
          legend.itemsize = 20)