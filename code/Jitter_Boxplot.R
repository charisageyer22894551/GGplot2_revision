# JITTER BOXPLOT FUNCTION

Jitter_Boxplot <- function(data_frame, Title, Subtitle, Caption, Xlab, Ylab, Alpha_Set = 0.5) {

    # Dataframe should be tidy, and be of the form:
    # date  | Ticker  |   Identifier  |  Return

    if( !"Identifier" %in% names(data_frame) ) stop("\n\nERROR:::::>Please provide valid Identifier column!\n\n")
    if( !"Date" %in% names(data_frame) ) stop("Please provide valid Date column ")
    if( class(data_frame$Date) != "Date" ) stop("Date column not of class Date ")

    g1 <-
        ggplot(data_frame) +

        geom_boxplot(aes(x = Identifier, y = Return, fill = Country), alpha = Alpha_Set) +

        geom_jitter(aes(x = Identifier, y = Return, color = Country, alpha = Alpha_Set)) +

        theme_bw() +

        guides(color = FALSE, fill = FALSE, alpha = FALSE) +
        # Add titles:
        labs(title = Title,
             subtitle = Subtitle,
             caption = Caption,
             x = Xlab, y = Ylab) +

        scale_color_npg() + # Now we use fill...

        scale_fill_npg() # Now we use fill...

    g1

}


