theme_blankPitch = function(size=12) {
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.length=unit(0, "lines"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background=element_rect(fill="#FFFFFF", colour=NA),
    legend.key=element_rect(colour="#FFFFFF",fill="#FFFFFF"),
    legend.key.size=unit(1.2, "lines"),
    legend.text=element_text(size=size),
    legend.title=element_text(size=size, face="bold",hjust=0),
    strip.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = .5),
    panel.background=element_rect(fill="#FFFFFF",colour="#FFFFFF"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.spacing=element_blank(),
    plot.background=element_blank(),
    plot.margin=unit(c(0, 0, 0, 0), "lines"),
    plot.title=element_text(size=size*1.2),
    strip.text.y=element_text(colour="#FFFFFF",size=size,angle=270),
    strip.text.x=element_text(size=size*1))}

ymin <- 0
xmin <- 0

GoalWidth <- 732
penspot <- 1100
boxedgeW <- 4032
boxedgeL <- 1650
box6yardW <- 1832
box6yardL <- 550


TheBoxWidth <- c(((7040 / 2) + (boxedgeW / 2)),((7040 / 2) - (boxedgeW / 2)))
TheBoxHeight <- c(boxedgeL,10600-boxedgeL)
GoalPosts <- c(((7040 / 2) + (GoalWidth / 2)),((7040 / 2) - (GoalWidth / 2)))

box6yardWidth <- c(((7040 / 2) + (box6yardW / 2)),((7040 / 2) - (box6yardW / 2)))
box6yardHeight <- c(box6yardL,10600-box6yardL)

centreCirle_d <- 1830

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

Dleft <- circleFun(c((penspot),(7040/2)),centreCirle_d,npoints = 1000)
Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
Dright <- circleFun(c((10600-(penspot)),(7040/2)),centreCirle_d,npoints = 1000)
Dright <- Dright[which(Dright$x <= (10600-(boxedgeL))),]
center_circle <- circleFun(c((10600/2),(7040/2)),centreCirle_d,npoints = 100)
TopLeftCorner <- circleFun(c(xmin,7040),200,npoints = 1000)
TopRightCorner <- circleFun(c(10600,7040),200,npoints = 1000)
BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
BottomRightCorner <- circleFun(c(10600,ymin),200,npoints = 1000)
