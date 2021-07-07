library(tidyverse)
tictoc::tic()
read_rds("data/uu_clean_data.rds") %>% 
  select(-tipo_doc,-otro_dni,
         -(apellido_paterno:otro_telefono),
         -nombre_completo,
         -username,
         -fecha_nacimiento,
         -contains("gps"),-latitud,-longitud) %>% 
  # select(dni,nombres) %>% 
  # distinct()
  # slice(1:10) %>%
  # glimpse()
  # filter(is.na(dni)) %>% 
  # filter(is.na(telefono)) %>%
  # naniar::vis_miss()
  # naniar::miss_var_summary()
  # mutate(across(.cols = c(dni,telefono),
  #               .fns = epitrix::hash_names, full = FALSE))
  mutate(hash=pmap(.l = select(.,dni,nombres),
                   .f = epitrix::hash_names,
                   # hashfun = "fast")) %>% 
                   hashfun = "secure")) %>%
  unnest(hash) %>% 
  select(-dni,-nombres,-label,-hash) %>% 
  select(hash_short, everything()) %>% 
  # glimpse()
  write_rds("data/uu_clean_data_hash.rds")
tictoc::toc()
#95.83 sec elapsed

read_rds("data/uu_clean_data_hash.rds") %>% 
  # count(username) %>% 
  # avallecam::print_inf()
  distinct()
  # count(hash_short) %>% 
  # arrange(desc(n))
  # glimpse()
  
