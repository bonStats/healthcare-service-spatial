
#### Libraries ####

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(ggspatial)
library(RobustGaSP)
library(purrr)
library(stringr)
library(patchwork)
source("helper-funs.R")

####

#### Read in data ####

# city info
source("data/city_hub_long_lats.R")
loc_names <- c("Brisbane","Ipswich", "Gold Coast", "Maroochydore", "Logan", "Noosa Heads")
short_loc_names <- c("Br","Ip", "GC", "Ma", "Lo", "No")
offset = list( c(0.035,-0.035), c(-0.07,0), c(0.12,0), c(0.12,0), c(-0.07,0), c(0.12,0) )

loc_marks <- list(bris_cc, 
                  ip_cc,
                  gc_cc,
                  ma_cc,
                  st_point(c(153.0619, -27.7750)),
                  st_point(c(153.0930,-26.3980))
)

city_locs <- st_sf(name = loc_names, 
                   short_name = short_loc_names, 
                   geometry = loc_marks, 
                   crs = 4283)

city_locs_offset <- city_locs %>% 
  mutate( geometry = geometry + offset)

st_crs(city_locs_offset) <- 4283

# boundary info

mapdf <- readRDS("data/qld_sa2_geometry.rds")

# distance/time data
trvl_sa2 <- readRDS("data/sa2_centroids_time_dist.rds")

# services and alternative names
service_list_df <- 
  tribble(~standard_name, ~sa2_name, ~subj_name,
          "Dietitian", "Dietitian", "Diet",
          "Exercise", "ExercisePhys", "ExPhys",
          "GP", "GP", "GP",
          "Neurology", "Neurology", "Neurology",
          "Neurosurgery", "Neurosurgery", "Neurosurgery",
          "Opthamology", "Opthamology2", "Opht",
          "Orthopaedic", "OrthopaedicSurgery", "OrtSurg",
          "Physiotherapy", "Physiotherapy", "Physiotherapy",
          "Plastic", "PlasticSurgery", "Plastic",
          "Psychiatry", "Psychiatry", "Psychiatry",
          "Psychologist", "Psychologist", "Psychologist",
          "OldVRehabilitation", "Rehabilitation_Medicine", NA,
          "Speech", "SpeechPathologist", "SpPath",
          "Urology", "Urology", "Urology",
          "Hospital", "hospital", "Hospital",
          "Dental", NA, "Dental",
          "Occupational", "OT", "OT",
          "Rehabilitation", "Rehab", "Rehab"
  )

sl_nna <- complete.cases(service_list_df)
keep_services <- service_list_df[sl_nna,]

## Filter to region of interest

st_crs(trvl_sa2) <- 4283

# previous selected areas:
selected_areas <- readRDS("data/sa2_selected_main_plots.rds")

# create final datasets

# filter to wanted SA2 areas and overlapping services
data_trvl_sa2 <- trvl_sa2 %>%
  filter(SA2_Code %in% selected_areas,
         service_type %in% keep_services$sa2_name) %>% 
  mutate(
    service_type = factor(service_type, 
                          levels = keep_services$sa2_name, 
                          labels = keep_services$standard_name),
    obs_id = paste0("SA2_",SA2_Code)
  )

# check locations per service and NA's
data_trvl_sa2$service_type %>% table(useNA = 'always')

## Plots to check

mapdf %>% filter(sa2_main_2016 %in% selected_areas) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = data_trvl_sa2, colour = "red", alpha = 0.01)

ggplot(data_trvl_sa2, aes(x = log(dist_to_city), y = log(min_duration))) +
  geom_point() +
  facet_wrap(~service_type)

#### Modelling ####

# prep data

# define test and train set
shared_vars <- colnames(data_trvl_sa2)

full_df <- data_trvl_sa2 %>% 
    # make non-zero for log: any travel estimated less than a minute, make 1 minute
    mutate(min_duration = ifelse(min_duration <= 1, 1, min_duration)) %>%
    # use all data as training data
    mutate(train = T)

# fitting parameters
gp_nug = 1e-08
initial_starts = 8

service_list <- keep_services$standard_name

gp_fits <- vector(mode = "list", length = length(service_list))
names(gp_fits) <- service_list

# iterate through different services (using time = min_duration as outcome)

for(service in service_list){
  gp_fits[[service]] <- list()
  
  gp_fits[[service]]$full_data <- full_df %>% 
    filter(service_type == service) %>% 
    mutate(
      log_min_duration = log(min_duration),
      log_dist_to_city = log(dist_to_city),
      scval = scale(log_min_duration),
      scdist_to_city = scale(log_dist_to_city)
    )
  
  
  gp_fits[[service]]$coords <- st_coordinates(gp_fits[[service]]$full_data) %>% scale()
  
  gp_fits[[service]]$trend <- cbind(intercept = rep(1, nrow(gp_fits[[service]] $coords)), 
                                    scdist_to_city = gp_fits[[service]]$full_data$scdist_to_city
  )
  
  cat("\nFit:",service,"\n\n")
  
  train_id <- gp_fits[[service]]$full_data$train
  
  gp_fits[[service]]$fit <- rgasp(design = gp_fits[[service]]$coords[train_id,], 
                                  response = gp_fits[[service]]$full_data$scval[train_id], 
                                  trend = gp_fits[[service]]$trend[train_id,],
                                  nugget = gp_nug, # for very small error (may need for fitting)
                                  kernel_type = "pow_exp", alpha = c(1,1), isotropic = F,
                                  num_initial_values=initial_starts)
}

#### Plotting Prediction Maps ####

all_qld <- mapdf %>% st_union

pred_sa2 <- mapdf %>% filter(sa2_main_2016 %in% selected_areas)

st_crs(pred_sa2) <- NA_crs_

plot_box <- st_bbox(pred_sa2) %>% 
  expand_bbox(x_val = 0.05, y_val = 0.05)

zoom_plot_box <- plot_box
zoom_plot_box["xmin"] <- 152.55
zoom_plot_box["xmax"] <- 153.45
zoom_plot_box["ymin"] <- -27.85
zoom_plot_box["ymax"] <- -27.15

attr(zoom_plot_box, "crs") <- st_crs(city_locs)

plot_map_df <- mapdf %>% 
  st_crop( plot_box )

# setup grid - find detached islands
plot_map_df$sa2_name_2016 %>% {.[str_detect(.,c("Island"))]}
plot_map_df$sa2_name_2016 %>% {.[str_detect(.,c("Port"))]}

snm_replacement <- plot_map_df %>% 
  filter(sa2_name_2016 == "Scarborough - Newport - Moreton Island") %>%
  st_cast(to = "POLYGON") %>%
  filter(st_coordinates(st_centroid(geometry))[,"X"] < 153.1)

grid_inter_map <-  plot_map_df %>%
  filter(sa2_name_2016 != "Redland Islands") %>%
  filter(sa2_name_2016 != "Brisbane Port - Lytton") %>%
  filter(sa2_name_2016 != "Scarborough - Newport - Moreton Island") %>%
  add_row(snm_replacement) %>% st_union()

st_crs(grid_inter_map) <- NA_crs_

ggplot() +
  geom_sf(aes(geometry = geometry), data = grid_inter_map) +
  geom_sf(aes(geometry = geometry), data = pred_sa2)


predict_grid <- pred_sa2 %>%
  st_make_grid(n = c(100, 400), square = F)

predict_grid_points <- st_centroid(predict_grid)

grid_point_in_geom <- st_within(predict_grid_points, grid_inter_map, sparse = F)

predict_grid <- predict_grid %>% st_as_sf() %>%
  filter(grid_point_in_geom[,1]) %>%
  rename(geometry = x)

# iterate through services for predictions in batches
batchsize = 100

for(service in service_list){
  
  gp_fits[[service]]$predictdata <- list()
  
  sc_dist_to_city <- attributes(gp_fits[[service]]$full_data$scdist_to_city)
  sc_coords <- attributes(gp_fits[[service]]$coords)
  unsc_value <- unscale_vals(gp_fits[[service]]$full_data$scval)
  
  gp_fits[[service]]$predictdata$full_data <- predict_grid_points %>% st_as_sf() %>%
    filter(grid_point_in_geom[,1]) %>%
    rename(geometry = x) %>%
    mutate(
      dist_to_city = st_distance(geometry, bris_cc),
      scdist_to_city = scale(dist_to_city, center = sc_dist_to_city$`scaled:center`, scale = sc_dist_to_city$`scaled:scale`)
    )
  
  gp_fits[[service]]$predictdata$coords <- st_coordinates(gp_fits[[service]]$predictdata$full_data) %>% scale(center = sc_coords$`scaled:center`, scale = sc_coords$`scaled:scale`)
  
  gp_fits[[service]]$predictdata$trend <- cbind(intercept = rep(1, nrow(gp_fits[[service]]$predictdata$coords)), 
                                                dist_to_city = gp_fits[[service]]$predictdata$full_data$scdist_to_city
  )
  
  pred_id <- 1:nrow(gp_fits[[service]]$predictdata$coords)
  
  batches <- split(pred_id, ceiling(seq_along(pred_id)/batchsize))
  prediction_list <- list()
  
  for(bat in batches){
    
    pred_temp <- Sample(gp_fits[[service]]$fit,  
                        testing_input = gp_fits[[service]]$predictdata$coords[bat,], 
                        testing_trend = gp_fits[[service]]$predictdata$trend[bat,],
                        num_sample = 1000) %>%
      unsc_value()
    
    prediction_list <- c(prediction_list, list(pred_temp))
    #cat("batch\t")
  }
  
  gp_fits[[service]]$predict <- do.call("rbind",prediction_list)
  
  gp_fits[[service]]$summary_pred <- bind_cols(
    predict_grid, 
    mean = rowMeans(gp_fits[[service]]$predict),
    upper_90 = apply(gp_fits[[service]]$predict, 1, quantile, prob = 0.95),
    lower_90 = apply(gp_fits[[service]]$predict, 1, quantile, prob = 0.05),
    median =  apply(gp_fits[[service]]$predict, 1, quantile, prob = 0.5),
    q_80 =  apply(gp_fits[[service]]$predict, 1, quantile, prob = 0.8),
    service = service
  )
  
  st_crs(gp_fits[[service]]$summary_pred) <- 4283
  cat(service, "done\n")
}

# extract plot data

plot_data <- map_df(gp_fits, ~.x$summary_pred)

bg_col <- "black"
bg_fill <- "lightgrey"

# example plot
plot_gp_hp <- ggplot(st_crop(all_qld, plot_box )) +
  geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
  geom_sf(aes(geometry = geometry, fill = mean, colour = mean), data = filter(plot_data, service %in% c("GP"))) +
  geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
  geom_sf(aes(geometry = geometry), data = city_locs, size = 1, colour = "darkgoldenrod1") +
  geom_sf_text(aes(geometry = geometry, label = short_name), data = city_locs_offset) +
  coord_sf(datum = NA) +
  ylab("") + xlab("") +
  facet_wrap(~service) +
  scale_fill_continuous("Travel time", low = "white", high = "darkblue") +
  scale_colour_continuous("Travel time", low = "white", high = "darkblue")

# plots with 80% chance travel time > threshold
plot_data <- plot_data %>% 
  mutate(`Access within 15 mins` = ifelse(q_80 > log(15), "No", "Yes"),
         `Access within 30 mins` = ifelse(q_80 > log(30), "No", "Yes"),
         `Access within 45 mins` = ifelse(q_80 > log(45), "No", "Yes"),
         `Access within 60 mins` = ifelse(q_80 > log(60), "No", "Yes"),
         `Travel time (mins)` = cut(q_80, breaks = log(c(0,15,30,45,60,Inf)), right = F, labels = c("< 15","< 30","< 45","< 60","> 60"))
  )


long_plot_data <- plot_data %>% 
  as_tibble() %>% 
  pivot_longer(cols = starts_with("Access within"), names_to = "access_level", values_to = "accessible") %>%
  st_as_sf()

access_plot_list <- setNames(vector(mode = "list", length = length(service_list)), service_list)
access_plot_list3 <- access_plot_list2 <- access_plot_list

save_plots <- TRUE

for(svc in service_list){
  
  access_plot_list[[svc]] <- # order cats
    ggplot(st_crop(all_qld, plot_box )) +
    geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
    geom_sf(aes(geometry = geometry, fill = accessible, colour = accessible), data = filter(long_plot_data, service %in% svc)) +
    geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
    geom_sf(aes(geometry = geometry), data = city_locs, size = 1, colour = "darkgoldenrod1") +
    geom_sf_text(aes(geometry = geometry, label = short_name), data = city_locs_offset) +
    coord_sf(datum = NA) +
    ylab("") + xlab("") +
    facet_wrap(~access_level, nrow = 1) +
    scale_fill_manual("Accessible?", values = c("Yes" = "steelblue1","No" = "tomato3")) +
    scale_colour_manual("Accessible?", values = c("Yes" = "steelblue1","No" = "tomato3")) +
    ggtitle(svc) +
    ggspatial::annotation_scale()
  # ggsn::scalebar(long_plot_data, dist = 10, dist_unit = "km", transform = T)
  
  if(save_plots) ggsave(access_plot_list[[svc]], filename = paste0("plots/p80_access/",svc,".pdf"), width = 16, height = 8)
  
  p1 <- ggplot(st_crop(all_qld, plot_box )) +
    geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
    geom_sf(aes(geometry = geometry, fill = `Travel time (mins)`, colour = `Travel time (mins)`), data = filter(plot_data, service %in% svc)) +
    geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
    geom_sf(aes(geometry = geometry), data = city_locs, size = 0.5, colour = "white", shape = 3) +
    geom_sf(data = st_as_sfc(zoom_plot_box), fill = NA, color = "black") +
    geom_sf_text(aes(geometry = geometry, label = short_name), data = filter(city_locs_offset, short_name %in% c("No","Ma","GC")),  colour = "black") +
    coord_sf(datum = NA) +
    ylab("") + xlab("") +
    scale_fill_manual(values = c("green2","yellowgreen","orange","orangered","darkred")) +
    scale_colour_manual(values = c("green2","yellowgreen","orange","orangered","darkred")) +
    ggtitle(svc) + theme_light() +
    guides(fill='none', colour = 'none') +
    ggspatial::annotation_scale()
  
  p2 <- ggplot(st_crop(all_qld, zoom_plot_box )) +
    geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
    geom_sf(aes(geometry = geometry, fill = `Travel time (mins)`, colour = `Travel time (mins)`), data = st_crop(filter(plot_data, service %in% svc),zoom_plot_box)) +
    geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
    geom_sf(aes(geometry = geometry), data = filter(city_locs, short_name %in% c("Br","Lo","Ip")), size = 4, colour = "white", shape = 3) +
    geom_sf(data = st_as_sfc(zoom_plot_box), fill = NA, color = "black") +
    geom_sf_text(aes(geometry = geometry, label = short_name), size = 7.5, data = filter(city_locs_offset, short_name %in% c("Br","Lo","Ip")),  colour = "white") +
    coord_sf(datum = NA) +
    ylab("") + xlab("") +
    scale_fill_manual(values = c("green2","yellowgreen","orange","orangered","darkred"), drop = F) +
    scale_colour_manual(values = c("green2","yellowgreen","orange","orangered","darkred"), drop = F) +
    theme_minimal() +
    theme(legend.title = element_text(size = 20), legend.text =  element_text(size = 17)) +
    ggspatial::annotation_scale()
  
  access_plot_list2[[svc]] <- p1 + p2
  
  if(save_plots) ggsave(access_plot_list2[[svc]], filename = paste0("plots/p80_access/inset/",svc,"_inset.pdf"), width = 14, height = 8)
  
}

## Multi panel plots

# Br, Ip, GC, Ma, Lo, No
offset2 = list( c(0.01,-0.01), c(0,0), c(-0.095,-0.35), c(0.09,0), c(-0.01,0), c(0.075,0) )

city_locs_offset2 <- city_locs_offset %>% 
  mutate(geometry = geometry + offset2)

st_crs(city_locs_offset2) <- 4283

for(svc in service_list){
  
  p1 <- ggplot(st_crop(all_qld, plot_box )) +
    geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
    geom_sf(aes(geometry = geometry, fill = `Travel time (mins)`, colour = `Travel time (mins)`), data = filter(plot_data, service %in% svc)) +
    geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
    geom_sf(aes(geometry = geometry), data = city_locs, size = 0.5, colour = "white", shape = 3) +
    geom_sf(data = st_as_sfc(zoom_plot_box), fill = NA, color = "black") +
    geom_sf_text(aes(geometry = geometry, label = short_name), data = filter(city_locs_offset2, short_name %in% c("No","Ma","GC")),  colour = "black") +
    coord_sf(datum = NA) +
    ylab("") + xlab("") +
    scale_fill_manual(values = c("green2","yellowgreen","orange","orangered","darkred")) +
    scale_colour_manual(values = c("green2","yellowgreen","orange","orangered","darkred")) +
    ggtitle(svc) + theme_light() +
    guides(fill='none', colour = 'none') +
    ggspatial::annotation_scale(location = "bl")
  
  p2 <- ggplot(st_crop(all_qld, zoom_plot_box )) +
    geom_sf(aes(geometry = geometry), fill = bg_fill, colour = bg_col ) +
    geom_sf(aes(geometry = geometry, fill = `Travel time (mins)`, colour = `Travel time (mins)`), data = st_crop(filter(plot_data, service %in% svc),zoom_plot_box)) +
    geom_sf(aes(geometry = geometry), fill = NA, colour = bg_col) +
    geom_sf(aes(geometry = geometry), data = filter(city_locs, short_name %in% c("Br","Lo","Ip")), size = 3, colour = "white", shape = 3) +
    geom_sf(data = st_as_sfc(zoom_plot_box), fill = NA, color = "black") +
    geom_sf_text(aes(geometry = geometry, label = short_name), size = 5, data = filter(city_locs_offset2, short_name %in% c("Br","Lo","Ip")),  colour = "white") +
    coord_sf(datum = NA) +
    ylab("") + xlab("") +
    scale_fill_manual("Travel time\n(minutes)", values = c("green2","yellowgreen","orange","orangered","darkred"), drop = F) +
    scale_colour_manual("Travel time\n(minutes)", values = c("green2","yellowgreen","orange","orangered","darkred"), drop = F) +
    theme_minimal() +
    theme(legend.title = element_text(size = 15), legend.text =  element_text(size = 12)) +
    ggspatial::annotation_scale(location = "bl", pad_x = unit(0.45, "cm"), pad_y = unit(0.4, "cm"), text_col = "white")
  
  access_plot_list3[[svc]] <- p1 + p2
  
}

plot1 <- ((access_plot_list3[["Neurology"]] + access_plot_list3[["Neurosurgery"]]) / 
            (access_plot_list3[["Opthamology"]] + access_plot_list3[["Orthopaedic"]]) /
            (access_plot_list3[["Plastic"]] + access_plot_list3[["Psychiatry"]]) /
            (access_plot_list3[["Rehabilitation"]] + access_plot_list3[["Urology"]]) 
) + plot_layout(guides = 'collect')

if(save_plots) ggsave(plot1, filename = "plots/paper-p80-inset-2x4-1.pdf", width = 11, height = 12)

plot2 <- ((access_plot_list3[["Dietitian"]] + access_plot_list3[["Exercise"]]) / 
            (access_plot_list3[["Occupational"]] + access_plot_list3[["Physiotherapy"]]) /
            (access_plot_list3[["Psychologist"]] + access_plot_list3[["Speech"]])
) + plot_layout(guides = 'collect')

if(save_plots) ggsave(plot2, filename = "plots/paper-p80-inset-2x3-1.pdf", width = 11, height = 12 * (3/4))

plot3 <- (  (access_plot_list3[["Plastic"]] + access_plot_list3[["Psychiatry"]]) /
              (access_plot_list3[["Rehabilitation"]] + access_plot_list3[["Urology"]]) 
) + plot_layout(guides = 'collect')

if(save_plots) ggsave(plot3, filename = "plots/paper-p80-inset-2x2-1.pdf", width = 11, height = 12 * (1/2))

plot4 <- (  (access_plot_list3[["Neurology"]] + access_plot_list3[["Neurosurgery"]]) / 
              (access_plot_list3[["Opthamology"]] + access_plot_list3[["Orthopaedic"]])
) + plot_layout(guides = 'collect')

if(save_plots) ggsave(plot4, filename = "plots/paper-p80-inset-2x2-2.pdf", width = 11, height = 12 * (1/2))

#### Tables ####

db_data <- readRDS("data/sa2_year_disability.rds") %>%
  select(sa2_main_2016, sa2_name_2016, Year,
         `Persons with a disability (no.)`, 
         `Persons with a profound or severe core activity limitation (no.)`,
         `Persons with a moderate or mild core activity limitation (no.)`,
         `Persons who have need for assistance with core activities (%)`)

db_data %>% group_by(Year) %>% summarise_all(~ mean(is.na(.)))

disability_2018 <- db_data %>% 
  filter(Year == 2018) %>% 
  select(sa2_main_2016, sa2_name_2016, n_disability = `Persons with a disability (no.)`)

disability_2018 %>% summarise(mean(is.na(n_disability)))

db_pop <- readRDS("data/sa2_year_population.rds") %>% 
  select(sa2_main_2016, sa2_name_2016, Year, `Persons - Total (no.)`)

pop_2018 <- db_pop %>% 
  filter(Year == 2018) %>% 
  select(sa2_main_2016, sa2_name_2016, n_person = `Persons - Total (no.)`)

pop_2018 %>% summarise(mean(is.na(n_person)))

point_data <- plot_data %>% 
  mutate(geometry = st_centroid(geometry))

matched_predict_to_sa2 <- st_within(point_data, mapdf)
all(sapply(matched_predict_to_sa2, length) == 1)

matched_predict_to_sa2 <- c(matched_predict_to_sa2, recursive = T)
table(matched_predict_to_sa2)

point_data <- point_data %>% mutate(match_id = matched_predict_to_sa2) %>%
  select(-starts_with("Access"))

point_data_table <- left_join(
  point_data, 
  as_tibble(mapdf) %>% select(sa2_main_2016, sa2_name_2016) %>% mutate(match_id = 1:n())
)

point_data_table <- point_data_table %>% left_join(pop_2018)
point_data_table <- point_data_table %>% left_join(disability_2018)

# prep km dist data
sa2_dist_km <- trvl_sa2 %>% 
  tibble() %>% 
  select(SA2_Code, service_type, dist_km) %>% 
  mutate(
    service_type = factor(service_type, 
                          levels = keep_services$sa2_name, 
                          labels = keep_services$standard_name)
  ) %>%
  rename(sa2_main_2016 = SA2_Code, service = service_type, centroid_dist_km = dist_km)

pop_access <- point_data_table %>% as_tibble() %>% 
  group_by(sa2_name_2016, sa2_main_2016, service) %>%
  summarise(mean_time_access = mean(exp(mean)),
            pop_by_mean_time_access = mean(exp(mean) * n_person),
            dis_by_mean_time_access = mean(exp(mean) * n_disability),
            n_person = first(n_person),
            n_disability = first(n_disability)) %>%
  mutate(prop_disability = n_disability / n_person) %>%
  group_by(service) %>% 
  mutate(scaled_dis_by_mean_time_access = dis_by_mean_time_access / max(dis_by_mean_time_access, na.rm = T),
         scaled_pop_by_mean_time_access = pop_by_mean_time_access / max(pop_by_mean_time_access, na.rm = T)
  ) %>% left_join(sa2_dist_km)

for(svc in service_list){
  temp_tb <- pop_access %>% filter(service == svc) %>% arrange(-scaled_dis_by_mean_time_access) %>% head(n = 20)
  write_csv(temp_tb, paste0("tables/mean_access_by_pop/",svc,"_disabled_pop_by_access_top_20.csv"))
}


ranks_by_service <- pop_access %>% group_split() %>%
  map_df(~ arrange(., -scaled_dis_by_mean_time_access) %>% mutate(rank = 1:n()))

ranks_by_service %>% group_by(sa2_name_2016) %>% summarise(max_rank = max(rank)) %>% 
  arrange(max_rank) %>% 
  write_csv(paste0("tables/sa2_appears_in_all_top_n.csv"))

# This is doing extra work, SA2 is only at one point, so averaging across location can occur as a first step, rather than the last one.

saveRDS(pop_access, "data/SDA_pop_access.rds")
