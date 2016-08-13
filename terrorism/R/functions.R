freq.and.proportion <- function(data, on) {
    # Given a dataframe and a particular column we would like to tabularize,
    # determine basic frequency and proportion statistics and return a
    # dataframe.

    # Tabularize :data on column :on, rename default columns
    df <- as.data.frame.table(table(data[ , on]))
    colnames(df) <- c(on, "frequency")

    # Calculate sum of frequency and build :proportion column
    sum.value <- sum(df$frequency)
    df$proportion <- df$frequency / sum.value

    # Return dataframe sorted on :frequency
    df <- arrange(df, desc(frequency))
    return(df)
}
