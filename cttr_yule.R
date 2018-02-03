
# Calculate CTTR and Yule’s K one by one ----------------------------------

cttr_yule <- function(dataframe, part_id){
        require(koRpus)
        # Set path
        
        set.kRp.env(TT.cmd = "/Users/Matthias/tree_tagger/cmd/tree-tagger-german", lang = "de")
        
        # Option 1
        
        tagged.text <- treetag(tweet_data_ger[tweet_data_ger$part_id == part_id,]$clean, format = "obj", treetagger="kRp.env",
                               lang="de", TT.options=list(path="~/Users/Matthias/TreeTagger/bin/treetagger/cmd/tree-tagger-german",
                                                          preset="de"))
        cttr <- CTTR(tagged.text)@CTTR
        yules_k <- K.ld(tagged.text)@K.ld # use @ to subset S4 class!!!
        
        result <- data.frame(part_id = part_id, cttr = cttr, yules_k = yules_k)
        
        return(result)
}