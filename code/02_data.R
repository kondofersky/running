# Part 02: Data import and preparation
# data read in ------------------------------------------------------------
data_prepared <- T
if(!data_prepared){
  # Path to be adapted on each computer, I use the runalyze gdpr backup zip file
  
  # extract and separate files in different formats:
  # json: all activities have a json file associated
  # tcx: all activities with gps or heart rate tracking prior to switch to Garmin watch have tcx files
  # fit: all activities tracked with Garmin watches have fit files
  unzip('C:/Users/RP6kondi/Downloads/46203-gdpr-backup.zip', exdir = 'C:/Users/RP6kondi/Documents/46203-gdpr-backup')
  lf <- list.files('C:/Users/RP6kondi/Documents/46203-gdpr-backup/activities/', full.names = T)
  lf_end <- sapply(strsplit(lf, '\\.'), function(i) i[2])
  lf_json <- lf[lf_end == 'json']
  lf_tcx <- lf[lf_end == 'tcx']
  lf_zip <- lf[lf_end == 'zip']
  
  # build main running table with one row per activity
  run_data <- NULL
  for(i in 1:length(lf_json)){
    a <- fromJSON(lf_json[i], flatten = T)
    if(a$sport == 'Running' & !is.null(a$distance)){
      run_data <- rbind(run_data, data.frame(
        Session = unlist(strsplit(substr(sapply(strsplit(lf_json[i], '/'), function(i) i[7]), 12, 100), '.json')), 
        Sport = a$sport,
        #Date = as_date(a$created/60/60/24), # doesnt really work bcs of secret offset?
        #Date = as.Date(a$created/9573, origin = '1560-11-09'), # works probably -> not
        Date = as.Date(substr(sapply(strsplit(lf_json[i], '/'), function(i) i[7]), 1, 10)),
        Distance = a$distance,
        Duration = a$duration/60,
        Pause = ifelse(length(a$elapsedTime/60) > 0, a$elapsedTime/60, a$duration/60) - a$duration/60,
        EleUp = ifelse(is.null(a$elevationUp), 0, a$elevationUp),
        EleDown = ifelse(is.null(a$elevationDown), 0, a$elevationDown),
        HRavg = ifelse(is.null(a$hrAvg), NA, a$hrAvg),
        Vo2max = ifelse(is.null(a$vo2max), NA, a$vo2max),
        Temperature = ifelse(is.null(a$temperature), NA, a$temperature)
      ))}
  }
  run_data %<>% as_tibble
  run_data %<>% mutate(Pace = Duration/Distance)
  run_data %<>% mutate(Session = as.character(Session))

  # extract map and/or hr data from tcx files
  map_list <- list()
  j <- 1
  for(i in 1:length(lf_tcx)){
    cat(j, ', ', sep = '')
    if(runif(1) < .1) cat('\n')
    tried <- try(a <- readTCX(lf_tcx[i]), silent = T)
    if(class(tried) != 'try-error'){
      a %<>% select(time, distance, heart_rate, latitude, longitude) %>% as_tibble
      a %<>% mutate(Session = unlist(strsplit(substr(sapply(strsplit(lf_tcx[i], '/'), function(i) i[7]), 12, 100), '.tcx')))
      map_list[[j]] <- a
      j <- j+1
    }
  } # duration around 10 mins too long --> fuck trackeR and go through xml?
  names(map_list) <- sapply(map_list, function(i) i$Session[1])
  
  # extract map and/or hr data from fit files
  map_list2 <- list()
  j <- 1
  for(i in 1:length(lf_zip)){
    cat(j, ', ', sep = '')
    if(runif(1) < .1) cat('\n')
    dir.create('tmp')
    unzip(lf_zip[i], exdir = 'tmp')
    fit_filename <- list.files('tmp')
    tried <- try(fit_file <- read.fit(paste0('tmp/', fit_filename)), silent = T)
    if(class(tried) != 'try-error' & fit_file$sport$sport == 1){
      if('position_lat' %in% colnames(fit_file$record))
        fit_file$record %<>% rename(latitude = position_lat, longitude = position_long, time = timestamp) %>% as_tibble
      else
        fit_file$record %<>% rename(time = timestamp) %>% mutate(latitude = NA, longitude = NA) %>% as_tibble
      fit_file$record %<>% mutate(time = as_datetime(time, origin = '1989-12-31', tz = 'CET'))
      fit_file$record %<>% select(time, distance, heart_rate, latitude, longitude)
      fit_file$record %<>% mutate(Session = unlist(strsplit(substr(sapply(strsplit(lf_zip[i], '/'), function(i) i[7]), 12, 100), '.zip')))
      map_list2[[j]] <- fit_file$record
      j <- j+1
    }
    unlink('tmp', recursive = T)
  } # duration around 2 mins ok, could be slightly optimised 10%-30%
  names(map_list2) <- sapply(map_list2, function(i) i$Session[1])

  # combine both lists
  map_lists <- c(map_list, map_list2)
  
  # faulty map tracks (found out later but removing here in order not to compromise further calculations)
  faulty <- c('1529051458227', '1521196211209', '1538575319258', '1591374697887')
  map_lists <- map_lists[-which(names(map_lists) %in% faulty)]
  
  # remove some non-running Sesssions (e.g. snowboard)
  map_lists <- map_lists[which(names(map_lists) %in% run_data$Session)]
  
  
  # extract which of the activities with tcx/fit have either some map or some hr data (or both)
  # tcx_fit_sessions <- c(do.call(c, sapply(map_list, function(i) i$Session[1], simplify = F)),
  #                       do.call(c, sapply(map_list2, function(i) i$Session[1], simplify = F)))
  # map_sessions <- c(do.call(c, sapply(map_list, function(i) if_else(!is.na(i$latitude[1]), i$Session[1], NULL), simplify = F)),
  #                   do.call(c, sapply(map_list2, function(i) if_else(!is.na(i$latitude[1]), i$Session[1], NULL), simplify = F))) %>% na.omit
  # hr_sessions <- c(do.call(c, sapply(map_list, function(i) if_else(!is.na(i$heart_rate[1]), i$Session[1], NULL), simplify = F)),
  #                  do.call(c, sapply(map_list2, function(i) if_else(!is.na(i$heart_rate[1]), i$Session[1], NULL), simplify = F))) %>% na.omit
  tcx_fit_sessions <- do.call(c, sapply(map_lists, function(i) i$Session[1], simplify = F))
  map_sessions <- do.call(c, sapply(map_lists, function(i) if_else(!is.na(i$latitude[1]), i$Session[1], NULL), simplify = F)) %>% na.omit
  hr_sessions <- do.call(c, sapply(map_lists, function(i) if_else(!is.na(i$heart_rate[1]), i$Session[1], NULL), simplify = F)) %>% na.omit

  run_data %<>% mutate(details = Session %in% tcx_fit_sessions, map = Session %in% map_sessions, hr_details = Session %in% hr_sessions)
  

  # function to extract best segment of a session for a given distance
  determine_best_pace_per_distance <- function(ses, distance = 1000){
    
    # extract map data for session
    cur_run <- map_lists[[ses]]
    
    # no map/hr data, return NA
    if(is.null(cur_run)){ 
      returnlist <- c(NA, NA)
      names(returnlist) <- c(paste0('BestPace', distance), paste0('BestPaceStart', distance))
      return(returnlist)
    }
    
    # calculate pace for each trackpoint which has enough distance in front of it (if not pace is NA)
    cur_run$pace <- NA
    for(i in 1:nrow(cur_run)){
      dist_start <- cur_run$distance[i]
      ind_dist_reached <- which(cur_run$distance - (cur_run$distance[i] + distance*.999) > 0)[1] # .999 trick solves some rounding issues, basically means I am happy also with 99,9% of the distance covered
      dist_end <- cur_run$distance[ind_dist_reached]
      time_start <- cur_run$time[i]
      time_end <- cur_run$time[ind_dist_reached]
      time_elapsed <- as.numeric(difftime(time_end, time_start, units = 'mins'))
      exact_distance <- dist_end - dist_start
      cur_run$pace[i] <- time_elapsed/exact_distance*1000
    }
    
    # if we do not find any trackpoints which are covering 99.9% of the distance, return NA
    if(all(is.na(cur_run$pace))){
      returnlist <- c(NA, NA)
      names(returnlist) <- c(paste0('BestPace', distance), paste0('BestPaceStart', distance))
      return(returnlist)
    }
    
    # if we found at least one pace value, determine minimum pace and return the pace as well as the distance trackpoint where the fastest segment started
    best_pace <- min(cur_run$pace, na.rm = T)
    best_pace_start <- cur_run$distance[which.min(cur_run$pace)]
    returnlist <- c()
    returnlist[1] <- best_pace
    returnlist[2] <- best_pace_start
    names(returnlist) <- c(paste0('BestPace', distance), paste0('BestPaceStart', distance))
    return(returnlist)
    
  }

  
  # ses <- as.character(run_data %>% slice(3) %>% pull(Session))
  # determine_best_pace_per_distance(ses)
  # determine_best_pace_per_distance(ses, 3000)
  
  # Vectorize the function to be able to handle not only one session but multiples
  determine_best_pace_per_distance <- Vectorize(determine_best_pace_per_distance)
  
  # calculate best pace and starting point for each of the 5 distances of interest: 1k, 5k, 10k, HM, M
  for(d in c(1000, 5000, 10000, 21097, 42195)){
    cat(d, '\t')
    tmp <- run_data %>% pull(Session) %>% determine_best_pace_per_distance(distance = d) %>% t
    run_data %<>% cbind(tmp)
    rm(tmp)
  }
  
  # activities, which do not have map/hr data, can also have BestPaces by assuming a constant pace and including pauses if there were any
  run_data %<>% mutate(BestPace1000 = if_else(is.na(BestPace1000) & Distance >= 1, (Duration + Pause)/Distance, BestPace1000))
  run_data %<>% mutate(BestPace5000 = if_else(is.na(BestPace5000) & Distance >= 5, (Duration + Pause)/Distance, BestPace5000))
  run_data %<>% mutate(BestPace10000 = if_else(is.na(BestPace10000) & Distance >= 10, (Duration + Pause)/Distance, BestPace10000))
  run_data %<>% mutate(BestPace21097 = if_else(is.na(BestPace21097) & Distance >= 21.09, (Duration + Pause)/Distance, BestPace21097))
  run_data %<>% mutate(BestPace42195 = if_else(is.na(BestPace42195) & Distance >= 42.19, (Duration + Pause)/Distance, BestPace42195))
  
  save(map_lists, run_data, file = 'data/runalyze_prepared.Rdata')

  # example for github
  set.seed(1234)
  run_data %<>% sample_n(10)
  map_lists <- map_lists[which(names(map_lists) %in% run_data$Session)]
  save(map_lists, run_data, file = 'data/runalyze_prepared_example.Rdata')
}

# load('data/runalyze_prepared.Rdata')
# load('data/runalyze_prepared_example.Rdata')



