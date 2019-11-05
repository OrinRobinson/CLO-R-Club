

library(rjags)
library(dplyr)

load("RWBL_occ_demo_JAGS.dat")
names(dat_long)
head(dat_long)

    ###format data for JAGS
    ## create lookup tables

    site_year_lookup = dat_long %>%
      group_by(Route.Point, year) %>%
      summarize() %>%
      ungroup() %>%
      mutate(
        site_year_ID = 1:n()
      )

    site_lookup = dat_long %>%
      group_by(Route.Point) %>%
      summarize() %>%
      ungroup() %>%
      mutate(
        site_ID = 1:n()
      )

    site_area_lookup = dat_long %>%
      group_by(Route.Point) %>%
      summarize(
        area = unique(Route)
      ) %>%
      ungroup() %>%
      arrange(Route.Point)

    area_lookup = dat_long %>%
      group_by(Route) %>%
      summarize() %>%
      ungroup() %>%
      mutate(
        area_ID = 1:n()
      )

    observer_lookup = dat_long %>%
      group_by(Obs) %>%
      summarize() %>%
      ungroup() %>%
      mutate(
        observer_ID = 1:n()
      )

    # specify loop dimensions
    nyearsites = site_year_lookup %>% nrow()
    nyears = nlevels(unique(as.factor(dat_long$year)))
    nsites = site_lookup %>% nrow()
    nareas = area_lookup %>% nrow()
    nobservers = observer_lookup %>% nrow()
    nobservations = dat_long %>% nrow()
    constant_variables = list(nyears = nyears, nsites = nsites, nareas = nareas, nobservers = nobservers, nobservations = nobservations)

    # set up observation-level data
    dat_long_obs = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      left_join(observer_lookup, by = "Obs") %>%
      transmute(
        site_year_ID,
        observer_ID,
        occurrence = Occurr,
        temp = scale(Temp),
        wind = scale(Windspeed),
        cloud = scale(Clouds),
        jdate = scale(as.numeric(jdate))
      )

    ## set up detection covariates and response
    detection_yearsite = matrix(dat_long_obs$site_year_ID, nrow = nobservations, ncol = 1)
    detection_observer = matrix(dat_long_obs$observer_ID, nrow = nobservations, ncol = 1)
    detection_occ = matrix(dat_long_obs$occurrence, nrow = nobservations, ncol = 1)
    detection_intercept = matrix(1, nrow = nobservations, ncol = 1)
    detection_cloud = matrix(dat_long_obs$cloud, nrow = nobservations, ncol = 1)
    detection_wind = matrix(dat_long_obs$wind, nrow = nobservations, ncol = 1)
    detection_temp = matrix(dat_long_obs$temp, nrow = nobservations, ncol = 1)
    detection_jdate = matrix(dat_long_obs$jdate, nrow = nobservations, ncol = 1)

    # set up ecological-level data
    ## get covariates by site and center them

    crop_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        crop500, crop1k, crop2k, crop3k, crop4k, crop5k, crop10k, crop15k, crop20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(crop_by_site$site_year_ID == 1:nrow(crop_by_site)))

    abundance_crop_linear = matrix(NA, nyearsites, ncol(crop_by_site) - 1)
    abundance_crop_quadratic = matrix(NA, nyearsites, ncol(crop_by_site) - 1)

    for (col in 1:ncol(abundance_crop_linear)) {
      poly = poly(crop_by_site[[col+1]], degree = 2)#,raw = TRUE

      abundance_crop_linear[, col] = poly[, 1]
      abundance_crop_quadratic[, col] = poly[, 2]
    }
    # for (col in 1:ncol(abundance_crop_linear)) {
    #   abundance_crop_linear[,col] = scale(abundance_crop_linear[,col],scale = FALSE)
    #   abundance_crop_quadratic[,col] = scale(abundance_crop_quadratic[,col])
    # }

    crp_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        crp500, crp1k, crp2k, crp3k, crp4k, crp5k, crp10k, crp15k, crp20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(crp_by_site$site_year_ID == 1:nrow(crp_by_site)))

    abundance_crp_linear = matrix(NA, nyearsites, ncol(crp_by_site) - 1)
    abundance_crp_quadratic = matrix(NA, nyearsites, ncol(crp_by_site) - 1)

    for (col in 1:ncol(abundance_crp_linear)) {
      poly = poly(crp_by_site[[col+1]], degree = 2)

      abundance_crp_linear[, col] = poly[, 1]
      abundance_crp_quadratic[, col] = poly[, 2]
    }


    grain_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        grain500, grain1k, grain2k, grain3k, grain4k, grain5k, grain10k, grain15k, grain20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(grain_by_site$site_year_ID == 1:nrow(grain_by_site)))

    abundance_grain_linear = matrix(NA, nyearsites, ncol(grain_by_site) - 1)
    abundance_grain_quadratic = matrix(NA, nyearsites, ncol(grain_by_site) - 1)

    for (col in 1:ncol(abundance_grain_linear)) {
      poly = poly(grain_by_site[[col+1]], degree = 2)

      abundance_grain_linear[, col] = poly[, 1]
      abundance_grain_quadratic[, col] = poly[, 2]
    }


    grass_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        grass500, grass1k, grass2k, grass3k, grass4k, grass5k, grass10k, grass15k, grass20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(grass_by_site$site_year_ID == 1:nrow(grass_by_site)))

    abundance_grass_linear = matrix(NA, nyearsites, ncol(grass_by_site) - 1)
    abundance_grass_quadratic = matrix(NA, nyearsites, ncol(grass_by_site) - 1)

    for (col in 1:ncol(abundance_grass_linear)) {
      poly = poly(grass_by_site[[col+1]], degree = 2)

      abundance_grass_linear[, col] = poly[, 1]
      abundance_grass_quadratic[, col] = poly[, 2]
    }


    tree_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        tree500, tree1k, tree2k, tree3k, tree4k, tree5k, tree10k, tree15k, tree20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(tree_by_site$site_year_ID == 1:nrow(tree_by_site)))

    abundance_tree_linear = matrix(NA, nyearsites, ncol(tree_by_site) - 1)
    abundance_tree_quadratic = matrix(NA, nyearsites, ncol(tree_by_site) - 1)

    for (col in 1:ncol(abundance_tree_linear)) {
      poly = poly(tree_by_site[[col+1]], degree = 2)

      abundance_tree_linear[, col] = poly[, 1]
      abundance_tree_quadratic[, col] = poly[, 2]
    }

    wet_by_site = dat_long %>%
      left_join(site_year_lookup, by = c("Route.Point", "year")) %>%
      group_by(site_year_ID) %>%
      mutate(
        rep = 1:n()
      ) %>%
      ungroup() %>%
      filter(rep == 1) %>%
      dplyr::select(
        site_year_ID,
        wet500, wet1k, wet2k, wet3k, wet4k, wet5k, wet10k, wet15k, wet20k
      ) %>%
      arrange(site_year_ID)

    stopifnot(all(wet_by_site$site_year_ID == 1:nrow(wet_by_site)))

    abundance_wet_linear = matrix(NA, nyearsites, ncol(wet_by_site) - 1)
    abundance_wet_quadratic = matrix(NA, nyearsites, ncol(wet_by_site) - 1)

    for (col in 1:ncol(abundance_wet_linear)) {
      poly = poly(wet_by_site[[col+1]], degree = 2)

      abundance_wet_linear[, col] = poly[, 1]
      abundance_wet_quadratic[, col] = poly[, 2]
    }



    ## year-intercept
    abundance_year_long = 1 #site_year_lookup$year
    abundance_year = matrix(1, nyearsites, nyears) # year-intercept
    #
    ####################################
    ## Run  in JAGS                   ##
    ####################################

    # define data for gibbs sampling
    jags_data = list(
      "nsites" = nsites * nyears,
      "nobservations" = nobservations,
      "nobservers" = nobservers,
      "abundance_covariate_year" = abundance_year,
      "abundance_covariate_1_linear" = abundance_crop_linear,
      "abundance_covariate_2_linear" = abundance_crp_linear,
      "abundance_covariate_3_linear" = abundance_grass_linear,
      "abundance_covariate_4_linear" = abundance_grain_linear,
      "abundance_covariate_5_linear" = abundance_tree_linear,
      "abundance_covariate_6_linear" = abundance_wet_linear,
      "abundance_covariate_1_quadratic" = abundance_crop_quadratic,
      "abundance_covariate_2_quadratic" = abundance_crp_quadratic,
      "abundance_covariate_3_quadratic" = abundance_grass_quadratic,
      "abundance_covariate_4_quadratic" = abundance_grain_quadratic,
      "abundance_covariate_5_quadratic" = abundance_tree_quadratic,
      "abundance_covariate_6_quadratic" = abundance_wet_quadratic,
      "detection_covariate_0" = c(detection_intercept),
      "detection_covariate_1" = c(detection_cloud),
      "detection_covariate_2" = c(detection_wind),
      "detection_covariate_3" = c(detection_temp),
      "detection_covariate_4" = c(detection_jdate),
      "observed_o" = c(detection_occ),
      "observation_sites" = c(detection_yearsite),
      "observers" = c(detection_observer)
    )

    # check jags data for NaN
    # JAGS will crash if missing value in predictor
    # Jags will not crash if missing value in detection history
    if (any(sapply(jags_data, function(x) { any(is.nan(x))}))) {
      NaN_check = sapply(jags_data, function(x) { any(is.nan(x))})

      stop(paste0("Some input data are NaN:\n", paste(names(NaN_check[NaN_check]), collapse = "\n")))
    }

    # check jags data for NA
    if (any(sapply(jags_data, function(x) { any(is.na(x))}))) {
      NA_check = sapply(jags_data, function(x) { any(is.na(x))})

      stop(paste0("Some input data are NA:\n", paste(names(NA_check[NA_check]), collapse = "\n")))
    }


    # define initial value for Occupancy
    # only *necessary* for hierarchical models where there can't be conflicting data between levels
    jags_inits = list(
      "True_occ" = rep(1, nsites)
    )

    # initialize JAGS
    # this is a warm-up period for MCMC models where it tries different kinds of sampling procedures so it can be more efficient in the 'real' model run
    jags_model = jags.model(
      file = "jags_all_sq_poly_occur.txt", #this is your model file
      data = jags_data,
      inits = jags_inits,
      n.chains = 1,
      n.adapt = 1000,
      quiet = FALSE
    )

    # run JAGS
    # record names of variables you want to 'monitor'
    samples_jags = jags.samples(
      jags_model,
      variable.names = c(
        "alpha.0","alpha.1","alpha.2","alpha.3","alpha.4","alpha.5","alpha.6",
        "alpha.1sq","alpha.2sq","alpha.3sq","alpha.4sq","alpha.5sq","alpha.6sq",
        "beta.0","beta.1","beta.2","beta.3","beta.4",
        "sigma.observer",
        "u.observer",
        "mu_p",
        "occu",
        "True_occ",
        "p"
      ),
      n.iter = 5000
    )

    str(samples_jags) # [n_observations, mcmc_iterations, n_mcmc_chains]

    # # check that sampling went well
    plot(samples_jags$alpha.0)
    plot(samples_jags$alpha.1)
    plot(samples_jags$beta.0)

    # # analyze coefficients
    all_variables = names(samples_jags)
    selected_variables = all_variables[grepl("alpha", all_variables) |
                                         grepl("beta", all_variables)]

    selected_means = sapply(samples_jags[selected_variables], function(x) {mean(x)})
    selected_lower_quantiles = sapply(samples_jags[selected_variables],
                                      function(x) {quantile(x, .025)})
    selected_upper_quantiles = sapply(samples_jags[selected_variables],
                                      function(x) {quantile(x, .975)})

    results_summary = data.frame("mean" = selected_means,  "lower_ci" = selected_lower_quantiles,
                                 "upper_ci" = selected_upper_quantiles, row.names = selected_variables)
    #
    results_summary

    # # Response curve plot
    newdat1<-data.frame(sc1=seq(min(jags_data$abundance_covariate_6_linear[,1]),max(jags_data$abundance_covariate_6_linear[,1]),length=5000),
                        sc1s=seq(min(jags_data$abundance_covariate_6_quadratic[,1]),max(jags_data$abundance_covariate_6_quadratic[,1]),length=5000))
    fitmat<- matrix(ncol=length(samples_jags$alpha.0), nrow=nrow(newdat1))
    for (i in 1:5000) fitmat[i,]<- plogis((samples_jags$alpha.6[,,] * newdat1$sc1[i] + samples_jags$alpha.6sq[,,] * newdat1$sc1s[i]))
    newdat1$mean<- apply(fitmat,1,mean)
    newdat1$lower<-apply(fitmat,1,quantile,prob=0.025)
    newdat1$upper<-apply(fitmat,1,quantile,prob=0.975)
    plot(1, type="n", xlab="Proportion Wetland", ylab="Occupancy", xlim=c(round(min(newdat1$sc1),digits=2), round(max(newdat1$sc1),digits=2)), ylim=c(0,1),
         cex.axis = 1.3, cex.lab=1.3)
    lines(newdat1$sc1,newdat1$mean,lty=1);lines(newdat1$sc1,newdat1$upper,lty=2, col="gray");lines(newdat1$sc1,newdat1$lower,lty=2, col="gray")
