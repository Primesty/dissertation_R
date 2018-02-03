## Use clean text with punctuation with this function!!!

clean_files_liwc <- function(dataframe){
        for(i in dataframe$part_id) {
                write.table(dataframe[dataframe$part_id == i,]$clean_w_punct, paste("part", i, "_liwc.txt", sep=""),
                    col.names = FALSE, row.names = FALSE,  sep = "\t", quote = FALSE)
        }
}

