

#' A .txt creator for LIWC analysis
#'
#' This function creates individual .txt files from participants cleaned tweets from a dataframe
#' @param dataframe A dataframe object.
#' @keywords text-file creation
#' @export
#' @examples
#' clean_files_liwc(dataframe)

clean_files_liwc <- function(dataframe){
        for(i in dataframe$part_id) {
                write.table(dataframe[dataframe$part_id == i,]$clean_w_punct, paste("part", i, "_liwc.txt", sep=""),
                    col.names = FALSE, row.names = FALSE,  sep = "\t", quote = FALSE)
        }
}

