# arrange table
generate_layout <- function(gg.heat,
                            gg.top = NULL,
                            gg.right = NULL,
                            gg.bottom = NULL,
                            gg.left = NULL,
                            gg.legend = NULL,
                            gg.title = NULL,
                            gg.row.title = NULL,
                            gg.column.title = NULL,
                            row.dendrogram = F,
                            col.dendrogram = F,
                            yt.axis = T,
                            yr.axis = T,
                            yt.axis.name = NULL,
                            yr.axis.name = NULL,
                            yt.plot.size = 0.3,
                            yr.plot.size = 0.3,
                            title = NULL,
                            title.size = 5,
                            bottom.label.size = 0.1,
                            left.label.size = 0.1,
                            legend.height = 0.2) {

  # Generate the gtable object whose cells correspond to the heatmap
  # and additional elements such as the top and right plot, and labels.


  # if there are dendrograms, remove the axes
  if (row.dendrogram) {
    yr.axis <- F
  }

  if (col.dendrogram) {
    yt.axis <- F
  }

  # heatmap in first position
  layout <- gtable::gtable_filter(ggplot2::ggplotGrob(gg.heat),
                                  pattern = "panel", trim = TRUE,
                                  fixed = TRUE)

  # location for right scatter plot
  if (!is.null(gg.right)) {
    # add a column to the right for the right plot
    layout <- gtable::gtable_add_cols(layout, grid::unit(yr.plot.size, "null"))
    if (yr.axis) {
      # add a row at the bottom for each of the axis and axis name
      # for axis
      layout <- gtable::gtable_add_rows(layout, grid::unit(0.1, "null"))
      # for axis.name
      layout <- gtable::gtable_add_rows(layout, grid::unit(0.05, "null"))
    }
  }

  # location for top scatter plot
  if (!is.null(gg.top)) {
    # add a row at the top for the top plot
    layout <- gtable::gtable_add_rows(layout,
                                      grid::unit(yt.plot.size, "null"), 0)
    if (yt.axis) {
      # add a column to the left for each of the axis and axis name
      # for axis
      layout <- gtable::gtable_add_cols(layout, grid::unit(0.1, "null"), 0)
      # for axis.name
      layout <- gtable::gtable_add_cols(layout, grid::unit(0.1, "null"), 0)
    }
  }

  # place bottom label
  if (!is.null(gg.bottom) && is.null(gg.right) |
      (!is.null(gg.bottom) && !is.null(gg.right) && !yr.axis)) {
    # condition 1: there is no additional right-plot, OR
    #              there is an additional right plot but no right axis
    # add a row at the bottom for the bottom label
    layout <- gtable::gtable_add_rows(layout,
                                      grid::unit(bottom.label.size,
                                                 "null"))
  } else if ( (bottom.label.size > 0.15) && !is.null(gg.bottom) &&
             !is.null(gg.right) && yr.axis) {
    # condition 2: there is an additional right-plot with an axis AND,
    #              the size of the bottom label is larger than 0.2
    #              (0.2 is the size of the axis)
    # add a row at the bottom for the bottom label whose size is equal to the
    # amount of extra width after accounting for the axis rows
    layout <- gtable::gtable_add_rows(layout,
                                      grid::unit(bottom.label.size - 0.15,
                                                 "null"))
  }

  # place left label
  if ( (!is.null(gg.left) && !yt.axis && !is.null(gg.top)) |
      (!is.null(gg.left) && is.null(gg.top)) ) {
    # condition 1: there is no additional top plot OR,
    #              there is an additional top plot but no top axis
    # add a column to the left for the left labels
    layout <- gtable::gtable_add_cols(layout,
                                      grid::unit(left.label.size,
                                                 "null"), 0)
  } else if ( (left.label.size > 0.2) && !is.null(gg.left) &&
             !is.null(gg.top) && yt.axis) {
    # condition 2: there is an additional top-plot with an axis AND
    #              the size of the top label is larger than 0.2
    #              (0.2 is the size of the axis)
    #
    layout <- gtable::gtable_add_cols(layout,
                                      grid::unit(left.label.size - 0.2,
                                                 "null"), 0)
  }

  # add row title position
  if (!is.null(gg.row.title)) {
    # if a row title is provided, add a column on the left
    # when either of the two conditions occur:
    #   - there is a top plot but there are no top axis
    #   - there are left labels and no top plot
    #   - there are left labels that extend beyond the top plot axis
    if (!(!is.null(gg.top) && yt.axis) |
        is.null(gg.top) && !is.null(gg.left) |
        !is.null(gg.top) && yt.axis && !is.null(gg.left) && left.label.size >= 0.2) {
      layout <- gtable::gtable_add_cols(layout, grid::unit(0.1, "null"), pos = 0)
    }
  }

  # add column title position
  if ( (!is.null(gg.column.title) && is.null(gg.right)) |
      (!is.null(gg.column.title) && !is.null(gg.right) && !yr.axis) |
      !is.null(gg.column.title) && !is.null(gg.right) && yr.axis &&
      (bottom.label.size > 0.2) ){
    # if a column title is provided, add a row at the bottom provided that we
    # satisfy at least one of the following conditions:
    #   Condition 1: there is no right plot
    #   Condition 2: there is a right plot but no right axis
    #   Condition 3: there is a right plot, and a right axis,
    #                but the width of this axis is larger than 0.2
    # if none of these conditions are satisfied, that means that there already
    # exists an appropriate row at the bottom for the column title.
    layout <- gtable::gtable_add_rows(layout, grid::unit(0.1, "null"), pos = -1)
  }

  # location for legend
  if (!is.null(gg.legend)) {
    # We always want the legend to be either two rows below the bottom of
    # the heatmap (if no bottom labels), or two rows below the bottom labels
    # Add a blank row for the following conditions:
    #   - there is a column name
    #   - there is a right plot but there are no right-plot axes AND
    #     there is no column name,
    #   - there is a right plot, with and axis, but the bottom labels
    #     are larger than 0.2
    #   - if there a right plot with axis AND column name AND no bottom labels
    if (is.null(gg.column.title) |
        ((!is.null(gg.right) && yr.axis == F) &&
          is.null(gg.column.title)) |
        (!is.null(gg.right) && bottom.label.size > 0.2) |
        (!is.null(gg.right) && yr.axis &&
         !is.null(gg.column.title) && is.null(gg.bottom))) {
     # layout <- gtable::gtable_add_rows(layout,
    #                                    grid::unit(0.1, "null"))
    }

    # the legend is always in the very bottom row
    # the only time we do not need to add a row for it is when there are no
    # bottom labels and there is a right plot and axis
    if (!(is.null(gg.bottom) && !is.null(gg.right) && yr.axis)) {
      layout <- gtable::gtable_add_rows(layout, grid::unit(legend.height, "null"))
    }
  }

  # add title
  if (!is.null(gg.title)) {
    # the title is always in the topmost row
    layout <- gtable::gtable_add_rows(layout, grid::unit(0.2, "null"), pos = 0)
  }

  return(layout)
}

# fill table with ggplot grobs
generate_grobs <- function(layout,
                           gg.top = NULL,
                           gg.right = NULL,
                           gg.bottom = NULL,
                           gg.left = NULL,
                           gg.legend = NULL,
                           gg.title = NULL,
                           gg.row.title = NULL,
                           gg.column.title = NULL,
                           row.dendrogram = F,
                           col.dendrogram = F,
                           yt.axis = T,
                           yr.axis = T,
                           yt.axis.name = T,
                           yr.axis.name = T,
                           padding = 1,
                           bottom.label.size = 0.1,
                           left.label.size = 0.1) {

  # if there is a row dendrogram, make sure that gg.right is not null
  if (row.dendrogram) {
    yr.axis <- F
  }

  # if there is a col dendrogram, make sure that gg.top is not null
  if (col.dendrogram) {
    yt.axis <- F
  }



  # Place legend grob in appropriate gtable cell in the bottom row
  # Grobs affecting legend position:
  #   - top plot axis
  #   - left labels
  #   - row title
  if (!is.null(gg.legend)) {
    # begin by placing the legend in the bottom row and in the right-most column
    t <- nrow(layout)
    l <- ncol(layout)
    # if there exists a right plot, move the legend one column to the left
    if (!is.null(gg.right)) {
      l <- l - 1
    }

    # place the legend in its final position
    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.legend),
                                                            pattern = "guide-box",
                                                            trim = TRUE, fixed = TRUE),
                                      t = t, l = l)
  }


  # Place the additional plot in the right-most column
  # Grobs affecting the position of the right plot:
  #   - title
  #   - top plot
  if (!is.null(gg.right)) {
    # begin by placing the right plot in the top row and
    # the right-most column
    t <- 1
    l <- ncol(layout)
    # if there is a top plot, move the right plot down one row
    if (!is.null(gg.top)) {
      t <- t + 1
    }
    # if there is a title, move the right plot down one row
    if (!is.null(gg.title)) {
      t <- t + 1
    }
    # place the right plot in the specified position
    # if it is a dendrogram, get the denrogram

    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                            pattern = "panel", trim = TRUE,
                                                            fixed = TRUE),
                                      t = t, l = l)

    # place the right plot axis (ignoring the axis name) in the specified position,
    # one row below the right plot
    if (yr.axis) {
      layout <- gtable::gtable_add_grob(layout,
                                        gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                              pattern = "axis-b",
                                                              trim = TRUE, fixed = TRUE),
                                        t = (t + 1), l = l)
    }
    # place the right plot axis name in the row below the axis position
    if (yr.axis && !is.null(yr.axis.name)) {
      layout <- gtable::gtable_add_grob(layout,
                                        gtable::gtable_filter(ggplot2::ggplotGrob(gg.right),
                                                              pattern = "xlab",
                                                              trim = TRUE, fixed = TRUE),
                                        t = (t + 2), l = l)
    }
  }


  # Place the additional plot in the top row
  # Grobs affecting the position of the top plot:
  #   - title
  #   - top plot
  if (!is.null(gg.top)) {
    # start by placing the top plot in the top row and the right-most column
    t <- 1
    l <- ncol(layout)
    # if there is a title, move the top plot one row down
    if (!is.null(gg.title)) {
      t <- t + 1
    }
    # if there is a right plot, move the top plot one column left
    if (!is.null(gg.right)) {
      l <- l - 1
    }
    # place the top plot in the specified position
    # If it is a dendrogram, extract the dendrogram

    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                            pattern = "panel",
                                                            trim = TRUE, fixed = TRUE),
                                      t = t, l = l)

    # place the top plot axis (ignoring the axis name) in the specified position,
    # one column to the left of the top plot
    if (yt.axis) {
      layout <- gtable::gtable_add_grob(layout,
                                        gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                              pattern = "axis-l",
                                                              trim = TRUE, fixed = TRUE),
                                        t = t, l = (l - 1))
    }
    # place the top plot axis name in the column to the left of the axis position
    if (yt.axis & !is.null(yt.axis.name)) {
      layout <- gtable::gtable_add_grob(layout,
                                        gtable::gtable_filter(ggplot2::ggplotGrob(gg.top),
                                                              pattern = "ylab",
                                                              trim = TRUE, fixed = TRUE),
                                        t = t, l = (l - 2))
    }
  }


  # Place the label on the left
  # Grobs affecting the left-label position:
  #   - top plot
  #   - row title
  #   - title
  if (!is.null(gg.left)) {
    # begin by placing the left label in the top row and the left-most column
    t <- 1
    l <- 1

    # if there is a top plot, move the left labels one column down
    if (!is.null(gg.top)) {
      t <- t + 1
    }
    # if there is a top plot with axis and axis label, and the size of the
    # left labels are not wider than the top plot axis, move the left
    # labels one column to the right
    if ((left.label.size <= 0.2) && !is.null(gg.top) &&
        yt.axis && !is.null(yt.axis.name)) {
      l <- l + 1
    }
    # if there is a title, move the left labels one row down
    if (!is.null(gg.title)) {
      t <- t + 1
    }
    # if there is a row title and a col has been added for it
    # (this only happens when any of the following conditions occur:
    #   - there is a top plot but there are no top axis
    #   - there are left labels and no top plot
    #   - there are left labels that extend beyond the top plot axis
    # move the left labels one column to the right
    if (!is.null(gg.row.title) &&
        (!(!is.null(gg.top) && yt.axis) |
         is.null(gg.top) && !is.null(gg.left) |
         !is.null(gg.top) && yt.axis && !is.null(gg.left) && left.label.size >= 0.2)) {
      l <- l + 1
    }
    # if the width of the left labels are larger than 0.2
    # (in which case they are wider than the top plot axes),
    # ensure that the left labels fill the entire space by
    # extending the right-side of the grob
    r <- l
    if (left.label.size > 0.2 && !is.null(gg.top) && yt.axis) {
      r <- r + 2 # add 1 for the top plot axis itself and 1 for the axis title
    }

    # place the label grob in the specified location
    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.left),
                                                            pattern = "panel", trim = TRUE, fixed = TRUE),
                                      t = t, l = l, r = r)
  }


  # Place the label on the bottom
  # Grobs affecting the bottom-label position:
  #   - top plot
  #   - row title
  #   - title
  #   - right plot
  if (!is.null(gg.bottom)) {
    # begin by placing the bottom labels in the second row (below the heatmap)
    # and the right-most column
    t <- 2
    l <- ncol(layout)

    # if there is a title, move the bottom labels one row down
    if (!is.null(gg.title)) {
      t <- t + 1
    }
    # if there is a top plot, move the bottom labels one row down
    if (!is.null(gg.top)) {
      t <- t + 1
    }
    # if there is a right plot, move the bottom labels one column to the left
    if (!is.null(gg.right)) {
      l <- l - 1
    }
    # if there is a right-plot with axis and the bottom label is wider than
    # the axis, then ensure that the bottom label is equal to the specified
    # length
    b <- t
    if (bottom.label.size > 0.2 && !is.null(gg.right) && yr.axis) {
      b <- t + 2
    }

    # place the grob in the appropriate position
    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.bottom),
                                                            pattern = "panel",
                                                            trim = TRUE, fixed = TRUE),
                                      t = t, l = l, b = b)
    # save the value b
    b.bottom <- b
  }


  # Place title grobs in the top row
  # Grobs affecting the title position:
  #     - right plot
  if (!is.null(gg.title)) {
    # begin by placing the title in the top row and right-most column
    t <- 1
    l <- ncol(layout)
    # if there is a right plot, move the title one column to the left
    if (!is.null(gg.right)) {
      l <- l - 1
    }

    # place the grob in the specified position
    layout <-gtable::gtable_add_grob(layout,
                                     gtable::gtable_filter(ggplot2::ggplotGrob(gg.title),
                                                           pattern = "panel",
                                                           trim = TRUE, fixed = TRUE),
                                     t = t, l = l)
  }


  # Place the row title to the left of the heatmap and row labels
  # Grobs affecting the row title position
  #   - title
  #   - top plot
  if (!is.null(gg.row.title)) {
    # begin by placing the column names in the left-most column and the top row
    l <- 1
    t <- 1
    # if there is a overall plot title, move the column names down one row
    if (!is.null(gg.title)) {
      t <- t + 1
    }
    # if there is a top plot, move the column names down one row
    if (!is.null(gg.top)) {
      t <- t + 1
    }
    # if there is a top plot with axis but no row labels,
    # move the row title one columns to the right
    if (!is.null(gg.top) && yt.axis && is.null(gg.left)) {
      l <- l + 1
    }



    # place the row title grob in the specified position
    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.row.title),
                                                            pattern = "panel",
                                                            trim = TRUE, fixed = TRUE),
                                      t = t, l = l)
  }

  # Place the column title below the heatmap and column labels, but above the legend
  # Grobs affecting the column title position
  #   - title
  #   - top plot

  if (!is.null(gg.column.title)) {
    # if there are bottom labels, place the column title in the row below them
    if (!is.null(gg.bottom)) {
      t <- b.bottom + 1
    } else {
      # otherwise, begin by placing the column title below the heatmap
      t <- 2
      if (!is.null(gg.title)) {
        t <- t + 1
      }
      if (!is.null(gg.top)) {
        t <- t + 1
      }
    }

    # place the grob in the right-most column
    l <- ncol(layout)
    # if there is a right-plot, move the column title one column to the left
    if (!is.null(gg.right)) {
      l <- l - 1
    }

    # place the column title in the appropriate place
    layout <- gtable::gtable_add_grob(layout,
                                      gtable::gtable_filter(ggplot2::ggplotGrob(gg.column.title),
                                                            pattern = "panel",
                                                            trim = TRUE, fixed = TRUE),
                                      t = t, l = l)
  }

  # add padding:
  layout <- gtable::gtable_add_padding(layout, grid::unit(padding, "cm"))
  # gtable::gtable_show_layout(layout)
return(layout)


}

