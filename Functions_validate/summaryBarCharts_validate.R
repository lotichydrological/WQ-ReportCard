#summaryBarCharts = function(HUC10, segment, Data){
summaryBarCharts_validate = function(segment, Data){ 
  colors = c("#E9E9E0","#C2C2FF", "#009900", "#FFFF66", "#FF9933", "#CC3300")
  factorLevels = c("Data Gap", "Poor Resolution", "Good", "Acceptable","Concern", "Poor")
  aquaticLifeCounts = Data$Assessment[Data$UseClass == "Aquatic_Life" & Data$Assessment != ""]
  aquaticLifeCounts = factor(aquaticLifeCounts, levels = factorLevels, order = TRUE)
  recreationCounts = Data$Assessment[Data$UseClass == "Recreation" & Data$Assessment != ""]
  recreationCounts = factor(recreationCounts, levels = factorLevels, order = TRUE)
  humanHealthCounts = Data$Assessment[Data$UseClass == "Human_Health" & Data$Assessment != ""]
  humanHealthCounts = factor(humanHealthCounts, levels = factorLevels, order = TRUE)
  agricultureCounts = Data$Assessment[Data$UseClass == "Agriculture" & Data$Assessment != ""]
  agricultureCounts = factor(agricultureCounts, levels = factorLevels, order = TRUE)
  png(filename = paste(c("./Figures/bar_charts/",segment,"_summary_plot.png"),collapse=""), width = 6.5, height = 2.0, units = "in", res=400, pointsize=8)
  par(mfrow=c(1,4))
  par(mai=c(0.65,0.4,0.25,0))
  barplot(table(aquaticLifeCounts), col = colors, ylab = "Number of Water Quality Indicators", main = "Aquatic Life", las = 2)
  barplot(table(recreationCounts), col = colors, main = "Recreation", las = 2)
  barplot(table(humanHealthCounts), col = colors, main = "Human Health", las = 2)
  barplot(table(agricultureCounts), col = colors, main = "Agriculture", las = 2)
  dev.off()
}