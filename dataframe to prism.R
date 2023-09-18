# NOTE: This code will be much easier to understand if you are familiar with the XML layout of
# PRISM .pzfx files. Open in text editor to see locations of various nodes

# df: dataframe to be analyzed
# h: headers for each graph to present
# x: x axis on figures
# l: legend on figures
# y: numeric value to be plotted
# xsort_desc: sort x axis items alphabetically
# lsort_desc: sort legend items alphabetically
# logy: log10 transform data in final graph

graph_prism <- function(df, h, x, y, l, xsort_desc = F, lsort_desc = F, logy = F) {
  
  # stops messaging every time you use summarize function
  options(dplyr.summarise.inform = FALSE)

  # if passed an empty df, function will throw error
  if (nrow(df) == 0) {
    stop("You've entered an empty dataframe")
  }
  
  # creating new columns in the df that concatenates the columns for the header, x axis, and legend
  df_data <- df %>%
    unite("header", h, sep = " ") %>%
    unite("x_axis", x, sep = " ") %>%
    unite("legend", l, sep = " ")
  
  # renaming colname to be graphed "yval"
  colnames(df_data)[colnames(df_data) == y] <- "yval" 
  
  # reading in a template file (blank PRISM file that is graphically pre-formatted)
  # the data tables in PRISM are encoded using XML
  prism_xml <- read_xml("template.pzfx") %>%
    # deleting namespace from xml file (required to edit file, but will be replaced after editing tables)
    xml_ns_strip() %>%
    # formats XML file as a list of nodes that can be edited
    xmlParse()
  
  # vector of graph headers that will be produced from functions
  graph_list <- sort(unique(df_data$header))
  
  # loop for each graph in graphlist
  for (graph_no in 1:length(graph_list)) {
    
    # isolating the data for this graph
    # loop_data should only include rows that match the specific header and rows that contain the value to be plotted
    loop_data <- df_data[df_data$header == graph_list[graph_no] & !is.na(df_data$yval),]

    # identifying the maximum number of replicates of all x_axis and legend conditions
    # this information is needed as an input for the XML file 
    # open PRISM file in a text editor to view XML layout
    replicates <- loop_data %>%
      group_by(x_axis, legend) %>%
      summarize(n())
    
    replicate_no <- max(replicates[, 3])
    
    # identifying each condition to be plotted on the x axis
    unique_x_axis <-
      sort(unique(loop_data$x_axis), decreasing = xsort_desc)
    
    # identifying each condition to be plotted in the legend
    unique_legend <-
      sort(unique(loop_data$legend), decreasing = lsort_desc)
    
    # counting the number of conditions in the legend
    legend_no <- length(unique_legend)
    
    # creating an NA matrix that can be filled in with desired values
    # number of rows: number of x_axis conditions
    # number of cols: max replicate number * number of legend conditions
    
    prism_output <- data.frame(matrix(
      NA,
      nrow = length(unique_x_axis),
      ncol = replicate_no * legend_no
    ),
    row.names = unique_x_axis)
    
    # loop to create matrix that will be converted into XML table data
    for (i in unique_x_axis) {
      for (j in 1:legend_no) {
        
        # creating a df for all data points each specific x_axis/legend condition called inner_loop_data
        inner_loop_data <- filter(loop_data, x_axis == i & legend == unique_legend[[j]])
        
        # only insert data if there is data to insert
        if (nrow(inner_loop_data) > 0) {
          
          # for grouped graphs, first replicate_no columns are for first legend item
          # this code displaces the column index to align with the proper legend 
          start_col_index <- (1 + (j - 1) * replicate_no)
          end_col_index <- (nrow(inner_loop_data) + (j - 1) * replicate_no)
          
          prism_output[i, start_col_index:end_col_index] <- inner_loop_data$yval
        }
      }
    }
    
    # if logy is true, log10 transform data
    if (logy) {
      prism_output <- log10(prism_output)
    }
    
    # pull PRISM node for the table you want to edit
    # if there are more than 20 tables, this program creates a new .pzfx file
    # 20 * floor((graph_no - 1) / 20) accounts for indices higher than 20, starting over at index 1
    tableNode <-
      getNodeSet(prism_xml, "//Table")[[graph_no - 20 * floor((graph_no - 1) / 20)]]
    
    # grouped tables need to have an attribute for number of replicates 
    addAttributes(tableNode, Replicates = replicate_no)
    
    # assigning name of the graph to the first subnode of the table node
    xmlValue(tableNode[[1]]) = graph_list[[graph_no]]
    
    # creating an empty vector for row names 
    x_axis_names <- c()
    
    # appending row names to vector as xml nodes of type <d/>
    for (i in row.names(prism_output)) {
      x_axis_names <- append(x_axis_names, newXMLNode("d", i))
    }
    
    # inserting x_axis_names into appropriate location in parent table node
    addChildren(tableNode[[2]][[1]], x_axis_names)
    
    # adding a child node "Ycolumn" to the parent table node
    # "YColumn" has its own child node "Title", which contains the legend info for that column
    for (i in 1:legend_no) {
      addChildren(tableNode, addChildren(
        newXMLNode("YColumn"),
        newXMLNode("Title", unique_legend[i])
      ))
      
      # each "YColumn" contains replicate_no subcolumns that contain data points for that condition
      # this loop extracts data from each subcolumn
      for (j in 1:replicate_no) {
        
        # create empty vector for subcolumn data
        subcol_data <- c()
        
        # for each row in this subcolumn, add new <d/> node containing data if the value exists,
        # otherwise, add empty <d/> node
        for (k in 1:nrow(prism_output)) {
          if (!is.na(prism_output[k, j + ((i - 1) * replicate_no)])) {
            subcol_data <-
              append(subcol_data, newXMLNode("d", prism_output[k, j + ((i - 1) * replicate_no)]))
          }
          
          else {
            subcol_data <- append(subcol_data, newXMLNode("d"))
          }
          
        }
        
        # add "Subcolumn" node to appropriate "Ycolumn" node, containing a series of <d/> values for the data
        addChildren(tableNode[[2 + i]], addChildren(newXMLNode("Subcolumn"), subcol_data))
        
      }
      
    }
    
    # if all data tables (or the maximum number: 20) have been filled in, prepare file for export
    if (graph_no == length(graph_list) || graph_no %% 20 == 0) {
      
      # add back namespace information that was stripped when initially read
      addAttributes(getNodeSet(prism_xml, "//GraphPadPrismFile")[[1]], xmlns =
                      "http://graphpad.com/prism/Prism.htm")
      
      # save file
      # Note: files will be easily overwritten if not saved elsewhere
      saveXML(prism_xml,
              paste0(
                paste(h, collapse = " "),
                ceiling(graph_no / 20),
                ".pzfx"
              ))
      
      # open file
      shell.exec(paste0(
        paste(h, collapse = " "),
        ceiling(graph_no / 20),
        ".pzfx"
      ))
      
      # reset prism_xml file to template and strip namespaces if more graphs need to be made
      if (graph_no != length(graph_list)) {
        prism_xml <- read_xml("template.pzfx") %>%
          xml_ns_strip() %>%
          xmlParse()
      }

    }
    
  }
  
}

