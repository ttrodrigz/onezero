shapley <- function(data, incl.all = TRUE, incl, need = 1) {
    

    # separate weight vector from items
    wgt_vec <- data[["weight"]]
    item_df <- data[grepl("item_", names(data))]
    
    # early return if including partial and none are provided
    if (! incl.all) {
        if (length(incl) == 0) {
            return(0)
        } else {
            item_df <- item_df[names(item_df) %in% incl]
        }
    }

    # determine if respondent was reached based on threshold 
    n_reached <- rowSums(item_df, na.rm = TRUE)
    reached   <- n_reached >= need
    p_reached <- weighted.mean(
        x = reached,
        w = wgt_vec
    )
    
    # convert into shares of reach
        reach_share <- map_df(item_df, function(x) 
        
        # zero out contribs if not reached
        ifelse(reached, x * wgt_vec / n_reached, 0))
    
    # final calcs
    share_sum   <- map_dbl(reach_share, sum, na.rm = TRUE)
    share_sum_1 <- share_sum / sum(share_sum)
    
    sort(share_sum_1 * p_reached, decreasing = TRUE)
    
}