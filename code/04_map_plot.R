# Part 04: High-level map plot of run

flag <- makeIcon('https://www.flaticon.com/svg/static/icons/svg/395/395841.svg', 25, 25)
cols <- c('red', 'orange', 'yellow', 'green', 'purple')

plot_run_path <- function(session){
  run_meta <- run_data %>% filter(Session == session)
  lng <- map_lists[[session]]$longitude
  lat <- map_lists[[session]]$latitude
  dst <- map_lists[[session]]$distance
  col_i <- 1
  
  m <- leaflet() %>%
    addTiles() %>%
    addPolylines(lng, lat, fill = F, group = 'Whole run') %>% 
    addMarkers(lng[1], lat[1], popup = htmltools::htmlEscape(paste0('Date: ', run_meta %>% pull(Date), 
                                                                    ', Pace: ', seconds.to.hms(60*(run_meta %>% pull(Pace))))), 
               icon = flag, group = 'Whole run')
  grps <- NULL
  for(dist_char in c(paste0(c(1, 5, 10), 'k'), 'HM', 'M')){

    if(dist_char == 'HM') distance <- 21097
    else if(dist_char == 'M') distance <- 42195
    else distance <- 1000*(strsplit(dist_char, 'k')[[1]][1] %>% as.numeric)

    if(max(dst) > distance){
      strt <- which(dst %in% run_meta[, paste0('BestPaceStart', distance)])[1]
      end <- which(dst - .999*(dst[strt] + distance) > 0)[1]
      m %<>% addPolylines(lng[strt:end]+0.0001*col_i, lat[strt:end], col = cols[col_i], group = dist_char) %>% 
        addMarkers(lng[strt], lat[strt], 
                   popup = htmltools::htmlEscape(paste0('Best ',  dist_char, ' time: ', seconds.to.hms(60*run_meta[, paste0('BestPace', distance)]*distance/1000), 
                                                        ', started at ',run_meta[, paste0('BestPaceStart', distance)], 'm')), icon = flag, group = dist_char)
      grps <- c(grps, dist_char)
      col_i <- col_i + 1
    }
  }
      
  if(length(grps) > 0)
    m %<>% addLayersControl(overlayGroups = c('Whole run', grps), options = layersControlOptions(collapsed = F)) %>%
    hideGroup(grps)
  m
}

# plot_run_path(run_data %>% filter(map) %>% filter(Distance > 10) %>% pull(Session) %>% sample(size = 1)) # works
# plot_run_path(run_data %>% filter(!map) %>% pull(Session) %>% sample(size = 1)) # error, TODO: abfangen
# plot_run_path(run_data %>% filter(map) %>% filter(Distance < 1) %>% pull(Session) %>% sample(size = 1)) # works
# plot_run_path(run_data %>% filter(map) %>% filter(Distance > 21) %>% pull(Session) %>% sample(size = 1)) # works
# augsburgHM2020 <- run_data %>% filter(map) %>% filter(Distance > 20) %>% arrange(Pace) %>% slice(1) %>% pull(Session)
# plot_run_path(augsburgHM2020)

# TODO: animate path -> probably through javascript

