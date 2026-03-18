library(magick)
library(tidyverse)
library(shiny)
library(shinythemes)
library(zip)

ui <- fluidPage(
        theme = shinythemes::shinytheme("superhero"),
        titlePanel("Create Your Very Own Overlay Mosaic Crochet Patterns!"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("file", label = h3("Upload Image")),
                        sliderInput("RowNumSlider", 
                                    label = h3("Desired Number of Rows"), 
                                    min = 1, 
                                    max = 300, 
                                    value = 150),
                        sliderInput("PatNumSlider", 
                                    label = h3("Desired Number of Patterns"), 
                                    min = 1, 
                                    max = 5, 
                                    value = 3),
                        sliderInput("BrightnessSlider",
                                    label = h3("Change Image Brightness"),
                                    min = 50,
                                    max = 150,
                                    value = 100),
                        actionButton("Go", label = "Generate Pattern"),
                        downloadButton("DownloadPattern", "Download Pattern"),
                        h3("How Do I Create My Pattern?"),
                        h5("Simply upload any image file (.png, .jpg, etc.) to the 
                        dropbox above, and use the sliders to adjust various parameters. 
                        For the most predictable and visually appealing results, it is 
                        recommended to use images with high levels of contrast (best 
                        if black-and-white). It is also recommended to avoid realistic 
                        or highly detailed images (cartoon-style or pixelated images work 
                        best). Once you are happy with the pattern, click on the download 
                        button to download a zipped folder containing all of the patterns, 
                        an overview of the final product, and a text file with detailed 
                        instructions for every row. Specific mosaic sizing may vary 
                        depending on gauge."),
                ),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Overview", plotOutput("processed_image")),
                                tabPanel("Pattern 1", plotOutput("Pattern_1")),
                                tabPanel("Pattern 2", plotOutput("Pattern_2")),
                                tabPanel("Pattern 3", plotOutput("Pattern_3")),
                                tabPanel("Pattern 4", plotOutput("Pattern_4")),
                                tabPanel("Pattern 5", plotOutput("Pattern_5"))
                        )
                )
        )
)
server <- function(input, output) {
        processed_image <- eventReactive(input$Go, {
                req(input$file)
                image <- magick::image_read(input$file$datapath)
                image_scaled <- magick::image_scale(image, paste0("x",input$RowNumSlider))
                image_bright <- magick::image_modulate(
                        image_scaled,
                        brightness = input$BrightnessSlider
                )
                image_out <- magick::image_quantize(
                        image_bright,
                        max = 2,
                        colorspace = "gray",
                        dither = FALSE
                )
                # Converting to dataframe
                image_ras <- as.raster(image_out)
                image_matrix <- as.matrix(image_ras)
                image_matrix <- image_matrix[nrow(image_matrix):1,]
                image_matrix <- image_matrix[,ncol(image_matrix):1]
                image_df <- as.data.frame(image_matrix) %>%
                        mutate(row = row_number()) %>%
                        pivot_longer(-row, names_to = "col", values_to = "color")
                image_df$col <- as.numeric(gsub("V", "", image_df$col))
                image_df <- image_df %>%
                        mutate(color = ifelse(color %in% c("#000000ff", "#ffffffff"),
                                              color,
                                              ifelse(substr(color, 2, 3) < "80", "#000000ff", "#ffffffff")))
                image_df$stitch <- ""
                
                # Implementing mosaic crochet stitch logic
                st_per_row <- length(unique(image_df$col))
                for (i in 1:nrow(image_df)) {
                        if (i <= st_per_row) {
                                image_df$stitch[i] <- ""
                        } else if (image_df$row[i] %% 2 == 1) {
                                if (image_df$color[i - st_per_row] == "#000000ff") {
                                        image_df$stitch[i] <- "X"
                                } else {
                                        image_df$stitch[i] <- ""
                                }
                        } else {
                                if (image_df$color[i - st_per_row] == "#ffffffff") {
                                        image_df$stitch[i] <- "X"
                                } else {
                                        image_df$stitch[i] <- ""
                                }
                        }
                }
                
                # Removing stacked X stitches
                first_stitch_of_third_row <- 2 * st_per_row + 1
                for (i in first_stitch_of_third_row:nrow(image_df)) {
                        if (image_df$stitch[i] == "") {
                                image_df$stitch[i] <- image_df$stitch[i]
                        } else if (image_df$stitch[i - st_per_row] == "X") {
                                image_df$stitch[i] <- ""
                                if (image_df$color[i - st_per_row] == "#000000ff") {
                                        image_df$color[i - st_per_row] <- "#ffffffff"
                                } else {image_df$color[i - st_per_row] <- "#000000ff"}
                        }
                }

                # Adding row color indicators
                image_df <- image_df %>% mutate(color = case_when(
                        col %in% c(1, 2, st_per_row-1, st_per_row) & row %% 2 == 1 ~ "#000000ff",
                        col %in% c(1, 2, st_per_row-1, st_per_row) & row %% 2 == 0 ~ "#ffffffff",
                        TRUE ~ color
                ))
                image_df <- image_df %>% mutate(stitch = case_when(
                        col %in% c(1, 2, st_per_row-1, st_per_row) ~ "S",
                        TRUE ~ stitch
                ))
                
                # Creating crochet chart
                image_df$stitch <- factor(image_df$stitch, levels = c("", "X", "S"))
                image_df$color <- factor(image_df$color, levels = c("#ffffffff", "#000000ff"))
                
                image_df
        })
        
        
        
        # PLOT OUTPUTS
        output$processed_image <- renderPlot({
                image_df <- processed_image()
                ggplot(image_df, aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        output$Pattern_1 <- renderPlot({
                image_df <- processed_image()
                pattern_number <- 1
                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                
                if (pattern_number > input$PatNumSlider) {return(NULL)}
                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                        st_per_pattern <- rows_per_pattern
                } else {
                        if (pattern_number == input$PatNumSlider) {
                                st_per_pattern <- rows_per_pattern - remainder + 
                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                        } else {
                                st_per_pattern <- rows_per_pattern - remainder
                        }
                }
                
                first_row <- st_per_pattern * (pattern_number - 1) + 1
                last_row <- first_row + st_per_pattern - 1
                ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        geom_text(aes(label = stitch), color = "steelblue", size = 3) +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        output$Pattern_2 <- renderPlot({
                image_df <- processed_image()
                pattern_number <- 2
                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                
                if (pattern_number > input$PatNumSlider) {return(NULL)}
                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                        st_per_pattern <- rows_per_pattern
                } else {
                        if (pattern_number == input$PatNumSlider) {
                                st_per_pattern <- rows_per_pattern - remainder + 
                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                        } else {
                                st_per_pattern <- rows_per_pattern - remainder
                        }
                }
                
                first_row <- st_per_pattern * (pattern_number - 1) + 1
                last_row <- first_row + st_per_pattern - 1
                ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        geom_text(aes(label = stitch), color = "steelblue", size = 3) +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        output$Pattern_3 <- renderPlot({
                image_df <- processed_image()
                pattern_number <- 3
                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                
                if (pattern_number > input$PatNumSlider) {return(NULL)}
                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                        st_per_pattern <- rows_per_pattern
                } else {
                        if (pattern_number == input$PatNumSlider) {
                                st_per_pattern <- rows_per_pattern - remainder + 
                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                        } else {
                                st_per_pattern <- rows_per_pattern - remainder
                        }
                }
                
                first_row <- st_per_pattern * (pattern_number - 1) + 1
                last_row <- first_row + st_per_pattern - 1
                ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        geom_text(aes(label = stitch), color = "steelblue", size = 3) +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        output$Pattern_4 <- renderPlot({
                image_df <- processed_image()
                pattern_number <- 4
                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                
                if (pattern_number > input$PatNumSlider) {return(NULL)}
                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                        st_per_pattern <- rows_per_pattern
                } else {
                        if (pattern_number == input$PatNumSlider) {
                                st_per_pattern <- rows_per_pattern - remainder + 
                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                        } else {
                                st_per_pattern <- rows_per_pattern - remainder
                        }
                }
                
                first_row <- st_per_pattern * (pattern_number - 1) + 1
                last_row <- first_row + st_per_pattern - 1
                ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        geom_text(aes(label = stitch), color = "steelblue", size = 3) +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        output$Pattern_5 <- renderPlot({
                image_df <- processed_image()
                pattern_number <- 5
                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                
                if (pattern_number > input$PatNumSlider) {return(NULL)}
                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                        st_per_pattern <- rows_per_pattern
                } else {
                        if (pattern_number == input$PatNumSlider) {
                                st_per_pattern <- rows_per_pattern - remainder + 
                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                        } else {
                                st_per_pattern <- rows_per_pattern - remainder
                        }
                }
                
                first_row <- st_per_pattern * (pattern_number - 1) + 1
                last_row <- first_row + st_per_pattern - 1
                ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                        geom_tile(color = "gray") +
                        scale_x_reverse() +
                        geom_text(aes(label = stitch), color = "steelblue", size = 3) +
                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                        scale_y_continuous(breaks = unique(image_df$row)) +
                        theme_void() +
                        theme(legend.position = "none") +
                        coord_fixed()
        })
        
        
        
        
        # PATTERN DOWNLOAD
        output$DownloadPattern <- downloadHandler(
                filename = function() {
                        paste0("crochet_pattern_", Sys.Date(), ".zip")
                },
                content = function(file) {
                        # Create a temporary folder
                        tmpdir <- tempdir()
                        pattern_files <- c()
                        image_df <- processed_image()
                        
                        tmpfile <- file.path(tmpdir, "Full_Pattern_Overview.jpg")
                        jpeg(tmpfile, width = 5000, height = 5000, res = 300)
                        print(
                                ggplot(image_df, aes(col, row, fill = color)) +
                                        geom_tile(color = "gray") +
                                        scale_x_reverse() +
                                        scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                                        scale_y_continuous(breaks = unique(image_df$row)) +
                                        theme_void() +
                                        theme(legend.position = "none") +
                                        coord_fixed()
                        )
                        dev.off()
                        pattern_files <- c(pattern_files, tmpfile)
                        
                        # --- Create Pattern Files ---
                        for (i in 1:input$PatNumSlider) {
                                pattern_number <- i
                                rows_per_pattern <- input$RowNumSlider / input$PatNumSlider
                                remainder <- (input$RowNumSlider %% input$PatNumSlider) / input$PatNumSlider
                                
                                if (input$RowNumSlider %% input$PatNumSlider == 0) {
                                        st_per_pattern <- rows_per_pattern
                                } else {
                                        if (pattern_number == input$PatNumSlider) {
                                                st_per_pattern <- rows_per_pattern - remainder + 
                                                        (input$RowNumSlider - ((rows_per_pattern - remainder) * input$PatNumSlider))
                                        } else {
                                                st_per_pattern <- rows_per_pattern - remainder
                                        }
                                }
                                
                                first_row <- st_per_pattern * (pattern_number - 1) + 1
                                last_row <- first_row + st_per_pattern - 1
                                tmpfile <- file.path(tmpdir, paste0("pattern_", i, ".jpg"))
                                jpeg(tmpfile, width = 5000, height = 5000, res = 300)
                                print(
                                        ggplot(image_df %>% filter(row %in% first_row:last_row), aes(col, row, fill = color)) +
                                                geom_tile(color = "gray") +
                                                scale_x_reverse() +
                                                geom_text(aes(label = stitch), color = "steelblue", size = 1.2) +
                                                scale_fill_manual(values = c("#ffffffff" = "white", "#000000ff" = "black")) +
                                                scale_y_continuous(breaks = unique(image_df$row)) +
                                                theme_void() +
                                                theme(legend.position = "none") +
                                                coord_fixed()
                                )
                                dev.off()
                                pattern_files <- c(pattern_files, tmpfile)
                        }
                        
                        # --- Create Instructions Text File ---
                        instructions_file <- file.path(tmpdir, "instructions.txt")
                        instructions_text <- c(
                                "Overlay Mosaic Crochet Pattern Instructions:",
                                "1. The full pattern overview file shows what the finished product will look like.",
                                "2. Follow the pattern row by row, starting from the bottom right and working leftward",
                                "3. Symbols:",
                                "   - 'X' = Front Loop Double Crochet",
                                "   - 'S' = Single Crochet",
                                "   - blank = Single Crochet Back Loop Only",
                                "4. If the above instructions are unclear, I would advise looking up 'Overlay Mosaic Crochet Basics'",
                                "",
                                "Descriptives:",
                                "Stitches per Row:", length(unique(image_df$col)),
                                "Number of Rows:", input$RowNumSlider,
                                "Total Number of Stitches:", length(unique(image_df$col)) * input$RowNumSlider,
                                "",
                                "Detailed Row Instructions:"
                        )
                        row_instructions <- c()
                        for (i in 1:input$RowNumSlider) {
                                stitch_vector <- image_df %>% filter(row == i) %>% 
                                        arrange(col) %>% pull(stitch) %>% as.character()
                                # Run-length encoding
                                r <- rle(stitch_vector)
                                
                                # Map values
                                vals <- ifelse(r$values == "", "BLO", r$values)
                                
                                # Create compressed string
                                instruction <- paste0(r$lengths, vals, collapse = ", ")
                                
                                # Store with row label
                                row_instructions <- c(
                                        row_instructions,
                                        paste0("Row ", i, ": ", instruction)
                                )
                        }
                        instructions_text <- c(instructions_text, row_instructions)
                        writeLines(instructions_text, instructions_file)
                        pattern_files <- c(pattern_files, instructions_file)
                        
                        # --- Zip The Files ---
                        zip::zipr(zipfile = file, files = pattern_files, root = tmpdir)
                },
                contentType = "application/zip"
        )
}

shinyApp(ui, server)












