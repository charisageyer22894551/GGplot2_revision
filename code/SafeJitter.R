# SAFEJITTER
SafeJitter <- purrr::safely(Jitter_Boxplot)
# Let's first break it to see the error produced:
Result <-SafeJitter(data_frame = plot_data,
                    Title = "BRICS Return Histograms",
                    Subtitle = "Transparency is key",
                    Caption = "Data was downloaded from Bloomberg",
                    Xlab = "", Ylab = "Distribution",
                    Alpha_Set = 0.4)

print(Result$error)