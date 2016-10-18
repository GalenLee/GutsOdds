# Rule declare in order
# 0 in
# 1 prior player in by position
# 2 in
# 3 in
# 4 in ...

# rulesToTry <- c(1,25,50,75,100,125,150,160,170,171,172,173,174,175,176,177,178,179,180,190,200,225,250,275,300,325,350,375,400,425,455)
rulesToTry <- c(1, seq(5, 155, by = 15), 156:184, seq(185, 455, by = 15))
                
                
mat <- simpleGuts(rulesToTry, 1000)

library(plotly)

axis <- list(
  tick0 = 0,
  dtick = 25
)
scene = list(
  xaxis = axis,
  yaxis = axis,
  zaxis = list(tick0 = -1, dtick = .1),
  camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))
plot_ly(z = mat, x = rulesToTry, y=rulesToTry , type = "surface") %>%
   layout(title = "3D Scatter plot", scene = scene)

plot_ly(z = mat, x = rulesToTry, y=rulesToTry , type = "contour", 
        autocontour = FALSE, contours=list(start=-1, size=.05, end=1, coloring="fill")) %>%
  add_trace(x=1:100, y=1:100, type="line")


