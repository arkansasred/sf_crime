library(scales)
library(grid)
library(ggplot2)

GeomChangeArrow <- ggproto("GeomChangeArrow", Geom,
                               required_aes = c("x", "y","change"),
                               default_aes = aes(length = 0.05,linetype="solid",linewidth=4,size=0.75,color="red",alpha=0.8),
                               draw_key = draw_key_point,
                               draw_panel = function(data, panel_scales, coord) {
                                 data <- data[order(data$group), , drop = FALSE]
                                 ## Transform the data first
                                 coords <- coord_munch(coord, data, panel_scales)
                                 
                                 ##get direction of arrow
                                 coords$dir<-sign(coords$change)
                                 coords$pct_diff<-percent(coords$change)
                                 coords$color<-ifelse(coords$dir==1,"red","blue")
                                 coords$length<-rescale(coords$change,to=c(0.02,0.09))
                                 coords$linewidth<-rescale(coords$change,to=c(2,4))

                                 ## Construct a grid grob
                                 the_arrow<-grid::segmentsGrob(
                                   x0 = unit(coords$x,"npc"), x1 = unit(coords$x,"npc"),
                                   y0 = unit((coords$y+coords$length/2)*abs(coords$dir),"npc"),
                                   y1 = unit((coords$y-coords$length/2)*abs(coords$dir),"npc"),
                                   gp = grid::gpar(col = alpha(coords$color,coords$alpha),
                                                   lty=coords$linetype,lwd=coords$linewidth),
                                   arrow=arrow(angle=40,length=unit(abs(coords$dir*coords$length/3),"npc"),
                                               ends=ifelse(coords$dir==1,"first","last"))                                 
                                   )
                                 the_label<-grid::textGrob(
                                   label = coords$pct_diff,
                                   x = unit(coords$x + 0.03,"npc"),
                                   y = coords$y,
                                   gp = grid::gpar(fontface="italic",cex=rescale(coords$length,to=c(0.5,0.8)))
                                 )
                                 gTree(children=gList(the_arrow,the_label))
                               })



StatDirection <- ggproto("StatDirection", Stat, 
                  required_aes = c("x","y","change"),
                  
                  compute_group = function(data, scales) {
                    first_val = head(data$change)
                    last_val = tail(data$change)
                    pct_diff <- (first_val-last_val)/first_val
                    direction <- sign(pct_diff)
                    data.frame(x=data$x,y=data$y,pct_diff=percent(pct_diff),dir=direction)
                  }
)

geom_change_arrow <- function(mapping = NULL, data = NULL, stat="identity",
                             position = "identity", na.rm = FALSE, 
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomChangeArrow, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

