# arrange table


generate_layout <- function(gg.heat,
                            gg.top = NULL,
                            gg.right = NULL,
                            gg.bottom = NULL,
                            gg.left = NULL,
                            gg.legend = NULL,
                            gg.title = NULL,
                            yt.axis = T,
                            yr.axis = T,
                            yt.axis.name = NULL,
                            yr.axis.name = NULL,
                            yt.plot.size = 0.3,
                            yr.plot.size = 0.3,
                            title = NULL,
                            title.size = 5,
                            bottom.label.size = 0.1,
                            left.label.size = 0.1) {

  # heatmap in first position
  layout <- gtable::gtable_filter(ggplot2::ggplotGrob(gg.heat), pattern = "panel", trim = TRUE,
                                  fixed = TRUE)


  ######### place scatter


  # location for right scatter plot
  if (!is.null(gg.right)) {
    layout <- gtable::gtable_add_cols(layout, grid::unit(yr.plot.size, "null"))
    if (yr.axis) {
      layout <- gtable::gtable_add_rows(layout, grid::unit(0.1, "null"))  # for axis
      layout <- gtable::gtable_add_rows(layout, grid::unit(0.1, "null"))  # for axis.name
    }
  }





  # location for top scatter plot
  if (!is.null(gg.top)) {
    layout <- gtable::gtable_add_rows(layout, grid::unit(yt.plot.size, "null"), 0)
    if (yt.axis) {
      layout <- gtable::gtable_add_cols(layout, grid::unit(0.1, "null"), 0)  # for axis
      layout <- gtable::gtable_add_cols(layout, grid::unit(0.1, "null"), 0)  # for axis.name
    }
  }




  # place bottom label
  if (!is.null(gg.bottom) && is.null(gg.right) |
      (!is.null(gg.bottom) && !is.null(gg.right) && !yr.axis)) {
    layout <- gtable::gtable_add_rows(layout, grid::unit(bottom.label.size, "null"))
  } else if ((bottom.label.size > 0.1) && !is.null(gg.bottom) && yr.axis) {
    layout <- gtable::gtable_add_rows(layout, grid::unit(bottom.label.size - 0.2, "null"))
  }

  # place left label (only add a spot if we don't have residual axes)
  if ((!is.null(gg.left) && !yt.axis && !is.null(gg.top)) |
      (!is.null(gg.left) && is.null(gg.top))) {
    layout <- gtable::gtable_add_cols(layout, grid::unit(left.label.size, "null"), 0)
  } else if ((left.label.size > 0.1) && !is.null(gg.left) && !is.null(gg.top) && yt.axis) {
    layout <- gtable::gtable_add_cols(layout, grid::unit(left.label.size - 0.2, "null"), 0)
  }



  # location for legend
  if (!is.null(gg.legend)) {
    layout <- gtable::gtable_add_rows(layout, grid::unit(0.2, "null"))
  }


  if (!is.null(title)) {

    layout <- gtable::gtable_add_rows(layout, grid::unit(0.2, "null"), pos = 0)

  }



  return(layout)
}










generate_grobs <- function(layout,
                           gg.top = NULL,
                           gg.right = NULL,
                           gg.bottom = NULL,
                           gg.left = NULL,
                           gg.legend = NULL,
                           gg.title = NULL,
                           yt.axis = T,
                           yr.axis = T,
                           yt.axis.name = T,
                           yr.axis.name = T,
                           title = NULL,
                           title.size = 5,
                           padding = 1,
                           bottom.label.size = 0.1,
                           left.label.size = 0.1) {



  if (!is.null(gg.legend)) {
    t <- 2
    l <- 1
    if (!is.null(gg.top))
      t <- t + 1
    if ((is.null(gg.right) && !is.null(gg.bottom)) |
        (!is.null(gg.right) && !yr.axis))
      t <- t + 1
    if (!is.null(gg.right) && yr.axis)
      t <- t + 2
    if (!is.null(gg.top) && yt.axis)
      l <- l + 2
    if (is.null(gg.top) && !is.null(gg.left))
      l <- l + 1
    if (!is.null(gg.title))
      t <- t + 1
    if (bottom.label.size > 0.1  && !is.null(gg.bottom) && !is.null(gg.right) && yr.axis)
      t <- t + 1
    if (left.label.size > 0.1 && !is.null(gg.left) && !is.null(gg.top) && yt.axis)
      l <- l + 1

    layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.legend),
                                                          pattern = "guide-box", trim = TRUE, fixed = TRUE), t = t, l = l)
  }






  ############ placing scatterplot grobs

  # placing the scatterplot on the right (position is affected by axes/legend above
  # and to the left)
  if (!is.null(gg.right)) {
    t <- 1
    l <- 2
    if (!is.null(gg.top))
      t <- t + 1
    if (is.null(gg.top) && !is.null(gg.left))
      l <- l + 1
    if (!is.null(gg.top) && yt.axis)
      l <- l + 2
    if (!is.null(gg.title))
      t <- t + 1
    if (left.label.size > 0.1 && !is.null(gg.top) && yt.axis)
      l <- l + 1


    layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                          pattern = "panel", trim = TRUE, fixed = TRUE), t = t, l = l)
    if (yr.axis) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                            pattern = "axis-b", trim = TRUE, fixed = TRUE), t = (t + 1), l = l)
    }
    if (yr.axis && !is.null(yr.axis.name)) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                            pattern = "xlab", trim = TRUE, fixed = TRUE), t = (t + 2), l = l)
    }
    if (yr.axis && is.null(yr.axis.name)) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                            pattern = "xlab", trim = TRUE, fixed = TRUE), t = (t + 1), l = l)
    }
  }




  # placing the scatterplot on the top (position is affected by axes/legend above and
  # to the left)
  if (!is.null(gg.top)) {
    t <- 1
    l <- 1
    if (!is.null(gg.left) && !yt.axis && is.null(yt.axis.name)) {
      l <- l + 1
    }
    if (yt.axis)
      l <- l + 2
    if (!is.null(gg.title))
      t <- t + 1
    if (left.label.size > 0.1 && !is.null(gg.top) && yt.axis)
      l <- l + 1


    layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                          pattern = "panel", trim = TRUE, fixed = TRUE), t = t, l = l)
    if (yt.axis) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                            pattern = "axis-l", trim = TRUE, fixed = TRUE), t = t, l = (l - 1))
    }
    if (yt.axis & !is.null(yt.axis.name)) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                            pattern = "ylab", trim = TRUE, fixed = TRUE), t = t, l = (l - 2))
    }
    if (yt.axis & is.null(yt.axis.name)) {
      layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                            pattern = "ylab", trim = TRUE, fixed = TRUE), t = t, l = (l - 1))
    }
  }







  ############# Placing label grobs




  # placing the label on the left (position is affected by axes/legend/residuals
  # above and to the left)
  if (!is.null(gg.left)) {
    t <- 1
    l <- 1

    # if (resid.axis) l <- l + 1
    if ((left.label.size <= 0.1) && !is.null(gg.top) && yt.axis && !is.null(yt.axis.name))
      l <- l + 1
    if (!is.null(gg.top))
      t <- t + 1
    if (!is.null(gg.title))
      t <- t + 1

    r <- l
    if (left.label.size > 0.1 && !is.null(gg.top) && yt.axis)
      r <- r + 2


    layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.left),
                                                          pattern = "panel", trim = TRUE, fixed = TRUE), t = t, l = l, r = r)

  }



  # placing the label on the bottom (position is affected by axes/legend/residuals
  # above and to the left)
  if (!is.null(gg.bottom)) {
    t <- 2
    l <- 1

    if (!is.null(gg.top))
      t <- t + 1
    if (is.null(gg.top) && !is.null(gg.left))
      l <- l + 1
    if (!is.null(gg.top) && yt.axis)
      l <- l + 2
    if (!is.null(gg.title))
      t <- t + 1

    b <- t
    if (bottom.label.size > 0.1 && !is.null(gg.right) && yr.axis)
      b <- t + 2
    if (left.label.size > 0.1 && !is.null(gg.left) && !is.null(gg.top) && yt.axis)
        l <- l + 1



    layout <- gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.bottom),
                                                          pattern = "panel", trim = TRUE, fixed = TRUE),
                                      t = t, l = l, b = b)

  }






  ####### Place title grobs #################
  if (!is.null(title)) {


    # place text
    t <- 1

    if (!is.null(gg.right)) {
      l <- ncol(layout) - 1
    } else {
      l <- ncol(layout)
    }


    layout <-gtable::gtable_add_grob(layout, gtable::gtable_filter(ggplot2::ggplotGrob(gg.title),
                                                          pattern = "panel",
                                                          trim = TRUE,
                                                          fixed = TRUE), t = t, l = l)
  }



  # add padding:
  layout <- gtable::gtable_add_padding(layout, grid::unit(padding, "cm"))

return(layout)


}

