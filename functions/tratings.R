tratings <- function(year, format){
  
  # Vector of column names 
  cols <- c("team", "wins", "losses", "win_pct", "mov", "orating", "drating",
            "netrating", "mov_adj", "orating_ajd", "drating_adj", "netrating_ajd")
  
  # Retrieving a single year
  if(length(year) == 1){
    return(paste0("https://www.basketball-reference.com/leagues/NBA_", 
                  year, "_ratings.html") %>% 
             read_html() %>% 
             html_nodes("#ratings") %>% 
             html_table() %>% 
             as.data.frame() %>% 
             row_to_names(1) %>% # first row is column names
             dplyr::select(-Rk, -Conf, -Div) %>% # useless columns
             magrittr::set_colnames(cols) %>% 
             mutate_all(as.character) %>% 
             mutate_at(.vars = vars(wins:netrating_ajd), 
                       as.numeric) %>% 
             mutate(orating_rel = orating - mean(orating),
                    drating_rel = drating - mean(drating)))
  }
  
  else if(length(year) > 1){
    out <- matrix(nrow = 30*length(year), ncol = 13)
    
    for(i in 1:length(year)){
      out[((i-1)*30+1):(i*30),] <- paste0("https://www.basketball-reference.com/leagues/NBA_", 
                                          year[i], "_ratings.html") %>% 
        read_html() %>% 
        html_nodes("#ratings") %>% 
        html_table() %>% 
        as.data.frame() %>% 
        row_to_names(1) %>% 
        dplyr::select(-Rk, -Conf, -Div) %>%
        magrittr::set_colnames(cols) %>% 
        mutate(season = year[i]) %>% 
        as.matrix()
    }
    
    if(format == "long"){
      # to df class and setting column names (with year added)
      return(
        
        as.data.frame(out) %>% 
          magrittr::set_colnames(c(cols, "season")) %>% 
          dplyr::select(team, season, everything()) %>% 
          mutate_all(as.character) %>% 
          mutate_at(.vars = vars(wins:netrating_ajd), 
                    as.numeric) %>% 
          group_by(season) %>% 
          mutate(orating_rel = orating - mean(orating),
                 drating_rel = drating - mean(drating)) %>% 
          group_by(team) %>% 
          arrange(season) %>% 
          mutate(
            orating_previous = lag(orating, default = first(orating), order_by = season),
            orating_rel_previous = lag(orating_rel, default = first(orating_rel), order_by = season),
            orating_rel_change = orating_rel - lag(orating_rel, default = first(orating_rel), order_by = season),
            drating_previous = lag(drating, default = first(drating), order_by = season),
            drating_rel_previous = lag(drating_rel, default = first(drating_rel), order_by = season),
            drating_rel_change = drating_rel - lag(drating_rel, default = first(drating_rel), order_by = season)
          ) %>% 
          ungroup()
      )
    }
    
    if(format == "wide"){
      return(
        
        as.data.frame(out) %>% 
          magrittr::set_colnames(c(cols, "season")) %>% 
          dplyr::select(team, season, everything()) %>% 
          mutate_all(as.character) %>%
          mutate_at(.vars = vars(wins:netrating_ajd), 
                    as.numeric) %>% 
          group_by(season) %>% 
          mutate(orating_rel = orating - mean(orating),
                 drating_rel = drating - mean(drating)) %>% 
          pivot_wider(names_from = "season", values_from = wins:netrating_ajd))
    }
  }
}