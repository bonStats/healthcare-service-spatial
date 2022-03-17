expand_bbox <- function(bbox, x_val, y_val){
  xrange <- bbox$xmax - bbox$xmin # range of x values
  yrange <- bbox$ymax - bbox$ymin # range of y values
  bbox_new <- bbox
  bbox_new[1] <- bbox_new[1] - (x_val * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (x_val * xrange) # xmax - right
  bbox_new[2] <- bbox_new[2] - (y_val * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (y_val * yrange) # ymax - top
  return(bbox_new)
}

unscale_vals <- function(x){
  
  sc_cent <- attr(x, "scaled:center")
  sc_scal <- attr(x, "scaled:scale")
  
  function(newx){newx*sc_scal + sc_cent}
  
}

bbox_to_polygon <- function(bb){
  
  stopifnot("bbox" %in% class(bb))
  
  dd <- matrix(
    c(  bb[["xmin"]],bb[["ymin"]],
        bb[["xmax"]],bb[["ymin"]],
        bb[["xmax"]],bb[["ymax"]],
        bb[["xmin"]],bb[["ymax"]],
        bb[["xmin"]],bb[["ymin"]]
    ),
    byrow = T, ncol = 2
  )
  
  st_polygon(list(dd))
  
}