#' csltostan.R
#'
#' @author C.Marsh
#' @description A function that will generate a STAN file that will replicate a Casal2 model.
#' this function assumes that the config.csl2 file has !include statements that have the model description
#' This is a note that if there are any model configurations in this file other that !includes we will not process them
#' There should be a warning for this case.
#' @date 15/4/2018
#' @copyright BigBlueData ltd
#'

csltostan = function(csl2_files, path = "", stan_file_name = "Mymodel.stan") {
  ###########################
  ## The function starts here
  ###########################
  ## read them in and append as a single file
  File = NULL;
  STAN_file = make.filename(path = path, file = stan_file_name)

  n_files = length(csl2_files)
  for(i in 1:n_files) {
    filename = make.filename(path = path, file = csl2_files[i])
    this_file = scan(filename, what = "", sep = "\n")
    # remove whitespace and comments
    this_file = cleanfile(file = this_file)
    # append
    File = append(File, this_file)
  }

  File <- as.vector(tapply(File, 1:length(File),strip))

  ## Now the fun begins
  blocks = get.lines(File, starts.with = "\\@", fixed = F)
  block_ndx = which(grepl("\\@", File))

  ### Brainstorming here
  ### what we need to know are

  # annual cycle
  # order of processes in each time step
  # what time steps observations are attached to
  # estimated parameters - we will need to make sure that data and parameters are properly taken account for
  # need to identify mortality block in each time step, because this will determine when precache values

  ## Global variables that we will call upon lists
  model = list();
  time_step = list();
  processes = list();
  observations = list();
  derived_quantities = list();
  selectivities = list()
  estimates = list();
  data = list();
  categories = list();
  b0_initialised = TRUE;
  ##################################
  ## information to take from @model
  ##################################

  model_ndx = which(grepl("@model",blocks));
  for(i in (block_ndx[model_ndx] + 1):(block_ndx[model_ndx + 1] - 1)) {
    temp <- string.to.vector.of.words(File[i])
    model[[casefold(as.character(temp[1]), upper = FALSE)]] = temp[-1]
  }

  ## Processes
  process_ndx = which(grepl("@process",blocks));
  label = "";
  for(j in 1:length(process_ndx)) {
    in_table = FALSE;
    table_name = ""
    row_ndx = 1;
    Colnames = NULL
    temp_matrix = NULL
    recruitment_block = FALSE
    for(i in (block_ndx[process_ndx[j]]):(block_ndx[process_ndx[j] + 1] - 1)) {
      temp <- string.to.vector.of.words(File[i])
      if (i == block_ndx[process_ndx[j]]) {
        label = temp[2]
        next
      }
      ## Catch tables
      if (temp[1] == "table") {
        in_table = TRUE;
        table_name = temp[2]
        next;
      }
      if (temp[1] == "end_table") {
        in_table = FALSE;
        row_ndx = 1;
        colnames(temp_matrix) = Colnames
        rownames(temp_matrix) = NULL
        processes[[as.character(label)]][[table_name]] = temp_matrix
        temp_matrix = NULL
        next
      }

      if (in_table) {
        if(row_ndx == 1) {
          Colnames = temp
          row_ndx = row_ndx + 1;
          next;
        }
        temp_matrix = rbind(temp_matrix, temp)
      } else {
        processes[[as.character(label)]][[casefold(as.character(temp[1]), upper = FALSE)]] = list("value" = temp[-1])
      }
      ## look out for recruitment processes
      if (temp[2] == "recruitment_beverton_holt")
        recruitment_block = TRUE;
      if (recruitment_block & casefold(temp[1], upper = FALSE) == "r0")
        processes[[as.character(label)]][["b0_initialised"]] = list("value" = "F")
      if (recruitment_block & casefold(temp[1], upper = FALSE) == "b0")
        processes[[as.character(label)]][["b0_initialised"]] = list("value" = "T")
    }
  }

  ## Observations
  obs_ndx = which(grepl("@observation",blocks));
  label = "";
  for(j in 1:length(obs_ndx)) {
    in_table = FALSE;
    table_name = ""
    row_ndx = 1;
    Colnames = NULL
    temp_matrix = NULL
    for(i in (block_ndx[obs_ndx[j]]):(block_ndx[obs_ndx[j] + 1] - 1)) {
      temp <- string.to.vector.of.words(File[i])
      if (i == block_ndx[obs_ndx[j]]) {
        label = temp[2]
        next
      }
      ## Catch tables
      if (temp[1] == "table") {
        in_table = TRUE;
        table_name = temp[2]
        next;
      }
      if (temp[1] == "end_table") {
        in_table = FALSE;
        row_ndx = 1;
        colnames(temp_matrix) = NULL
        rownames(temp_matrix) = NULL
        observations[[as.character(label)]][[table_name]] = temp_matrix
        temp_matrix = NULL
        next
      }

      if (in_table) {
        temp_matrix = rbind(temp_matrix, temp)
      } else {
        observations[[as.character(label)]][[casefold(as.character(temp[1]), upper = FALSE)]] = list("value" = temp[-1])
      }
    }
  }

  # derived_quantitities
  dq_ndx = which(grepl("@derived_quantity",blocks));
  DQ_label = ""
  for(i in (block_ndx[dq_ndx]):(block_ndx[dq_ndx + 1] - 1)) {
    temp <- string.to.vector.of.words(File[i])
    if (i == block_ndx[dq_ndx]) {
      DQ_label = temp[2];
      next;
    }
    derived_quantities[[DQ_label]][[temp[1]]] = list("value" = temp[-1])
  }

  # time_steps
  time_step_ndx = which(grepl("@time_step",blocks));
  time_step_label = ""
  for(i in (block_ndx[time_step_ndx]):(block_ndx[time_step_ndx + 1] - 1)) {
    temp <- string.to.vector.of.words(File[i])
    if (i == block_ndx[time_step_ndx]) {
      time_step_label = temp[2];
      next;
    }
    time_step[[time_step_label]] = temp[-1]
  }

  # Categories
  category_ndx = which(grepl("@categories",blocks));
  for(i in (block_ndx[category_ndx]):(block_ndx[category_ndx + 1] - 1)) {
    temp <- string.to.vector.of.words(File[i])
    if (i == block_ndx[category_ndx]) {
      next;
    }
    categories[[temp[1]]] = temp[-1]
  }

  # Selectivities
  selectivity_ndx = which(grepl("@selectivity",blocks));
  for(j in 1:length(selectivity_ndx)) {
    label = ""
    for(i in (block_ndx[selectivity_ndx[j]]):(block_ndx[selectivity_ndx[j] + 1] - 1)) {
      temp <- string.to.vector.of.words(File[i])
      if (i == block_ndx[selectivity_ndx[j]]) {
        label = temp[2]
        next;
      }
      selectivities[[label]][[temp[1]]] = list("value" = temp[-1])
    }
  }

  ## perhaps here is a good opportunity to look back through the model
  ## and convert all the short hand syntax
  ## i.e replace * with all categores and category .+. syntax, expand : operators
  ## TODO


  #########################
  ## Create the output file
  #########################
  header = "// Converting a Casal2 file to STAN (hopefully)\n// If you use this in the future then you should definitely shout Craig Marsh a beer!!!\n"
  write(header, file = STAN_file);

  ## structure the STAN model

  ## functions section
  # - selectivity functions
  # - length-at-age
  # - weight-at-age
  # - likelihood calculations


  ## Data inputs
  ## list some parameters that will be garunteed to be user defined, others can be appended later
  data =
    c("/*",
      "* Data",
      "* This is user defined switches, this will have to be added later as we go through the processes and observations we will want to append to this",
      "*/\n",
      "data {",
      "// model dimensions",
      "int min_age;",
      "int max_age;",
      "int n_ages;",
      "int n_years;",
      "int n_categories",
      "int plus_group"
    )

  ## transformed data
  transformed_data =
    c("/*",
      "* Transformed Data",
      "* Create containers that are like derived quantities from data that don't change during estimation, e.g ages",
      "*/\n",
      "transformed data {",
      "\tvector[n_ages] ages;",
      "\tint initialisation_phase;",
      "int age_iter = 1;",
      "\tfor (age in min_age:max_age) {",
      "\tages[age_iter] = age;",
      "\t++age_iter;",
      "}",
      "initialisation_phase = 0;"
    )
  ## could put growth containers in the same place, otherwise calculate them on the fly
  ## or any other non estimated quantity that does not depend on model derived quantities



  ## Transformed Parameters
  # - initial calculations
  #   - selectivity calculations
  #   - growth calculations, otherwise we may want to do these on the fly with the time-step component
  # - Equilibrium Calculation
  #   - This may involve re-writing the model blah!!
  # - The actual model, which is what we are after
  # - storage containers
  #   -numbers at age [n_age * n_category, n_year + 1] ## this is the partition and order of categories will be important as we increase complexity of models
  #   -DQ's and pre-DQ's

  transformed_parameters =
    c("/*",
      "* Transformed Data",
      "* This is where the magic happens, all the model calculations etc",
      "*/\n",
      "transformed parameters{",
      "\t// Define the partition",
      "\tvector[n_age] ages; // vector of ages",
      "\trow_vector[n_age] temp_partition; // a temporary partition in a given year that will be used to do manipulation",
      "\treal c_diff;",
      "\treal max_rel_diff;",
      "\tvector[n_categories] plus_group;",
      "\tvector[n_categories] old_plus_group;"
    )
  ## I am thinking of haveing multiple matricies for each category, this way we can kind of hard code this stuff
  for (i in 1:length(categories$names)) {
    this_partition = paste("\tmatrix[n_years + 1, n_ages] numbers_at_age_", categories$names[i], ";",sep = "");
    transformed_parameters = c(transformed_parameters, this_partition);
  }
  ## so now each category has its own numbers at age matrix and the STAN code will just be a bit ugly.
  ## I am thinking this will be better, because from my current thoughts, the alternative is to have a single matrix with the dimensions
  ## [n_years + 1, n_ages * n_categories] but in C++ it is difficult to pull out partial rows e.g. if I wanted to pull out the numbers at age
  ## Fof the first category in R you could do this numbers_at_age[t,(1:n_ages) * c * n_ages], and you skip along each category, but C++ this is
  ## difficult if not impossible so hence why I am flagging that idea
  ## pre-caching for initialisation
  for (i in 1:length(categories$names)) {
    this_cached_partition = paste("\trow_vector[n_ages] cache_initial_numbers_at_age_", categories$names[i], ";",sep = "");
    transformed_parameters = c(transformed_parameters, this_cached_partition);
  }

  ## Derived quantities
  for (i in 1:length(names(derived_quantities))) {
    this_DQ = c(
      paste0("\tvector[n_years + 1] derived_quantity_", names(derived_quantities)[i], ";"),
      paste0("\treal pre_derived_quantity_", names(derived_quantities)[i], ";"),
      paste0("\treal post_derived_quantity_", names(derived_quantities)[i], ";"))

    transformed_parameters = c(transformed_parameters, this_DQ);
  }


  ###########################
  ## Initialise the partition
  ###########################

  ## initialisation section the annual cycle minus F
  n_time_steps = length(time_step)

  init_line = c(
    "\t//////////////////////////",
    "\t// Initialisation section",
    "\t//////////////////////////",
    "\t\n",
    "\tfor (init in 1:n_ages) {"
  )
  write(init_line, STAN_file, append = TRUE);

  ## Do some preliminary calculations that will be used all over the model setup
  ## Are there any Derived quantities in this time_step
  mortality_blockin_each_time_step = list()
  DQs_in_time_step_ndx = vector()

  dq_storer = 1;
  for (time_step_ndx in 1:n_time_steps) {
    DQs_in_this_time_step = vector()
    this_time_step_label = names(time_step)[time_step_ndx]
    for (dq_ndx in 1:length(DQ)) {
      if (DQ[[dq_ndx]]$time_step$value == this_time_step_label) {
        ## we have to precache for this derived_quantity in this time step
        DQs_in_time_step_ndx[dq_storer] = time_step_ndx
      }
    }

    ## identify the mortality block
    these_process = time_step[[time_step_ndx]]
    n_processes = length(these_process)
    mortality_block = vector();
    for(process_ndx in 1:n_processes) {
      mort_block_counter = 1;
      this_process_ndx = which(these_process[process_ndx] == names(processes))
      if (processes[[this_process_ndx]]$type$value == "mortality_instantaneous") {
        mortality_block[mort_block_counter] = process_ndx
        mort_block_counter = mort_block_counter + 1;
      }
    }
    mortality_blockin_each_time_step[[time_step_ndx]] = mortality_block
    dq_storer = dq_storer + 1
  }


  ## Iterate over each time step and write them out seperatly
  for (time_step_ndx in 1:n_time_steps) {
    init_line = c(
      "\t\t//////////////////////////",
      paste0("\t\t// Time step ", time_step_ndx),
      "\t\t//////////////////////////"
    )
    for(process_ndx in 1:n_processes) {
      ## Check if we are about to enter a mortality block, if we are, we will need to write out some pre-caching syntax
      if (process_ndx == min(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Pre mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[time_step_ndx]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");\n")
                )
              }
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }
      ## I am going to write out functions for each time step in helper functions to make it kinda modular
      ## we can just pass the function the list object with the subcommands and that function can translate that
      ## into STAN code, you may need to pass it an additional command, where by you give category index so that
      ## it can access numbers at age correctly.
      this_process_ndx = which(these_process[process_ndx] == names(processes))

      if (processes[[this_process_ndx]]$type$value == "ageing") {
        ageing_syntax = c(
          "\t\t////////////",
          "\t\t// Ageing",
          "\t\t////////////"
        )
        ageing_syntax = c(ageing_syntax,apply_ageing(ageing_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE))
        write(ageing_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "recruitment_beverton_holt") {
        recruit_syntax = c(
          "\t\t//////////////////////////////",
          "\t\t// Beverton holt recruitment",
          "\t\t//////////////////////////////"
        )
        recruit_syntax = c(recruit_syntax, apply_recruitment_beverton_holt(recruitment_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE, b0_initialised = processes[[this_process_ndx]]$b0_initialised$value))
        write(recruit_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "mortality_instantaneous") {

        mortality_syntax = c(
          "\t\t//////////////////////////////",
          "\t\t// Instantaneous Mortality",
          "\t\t//////////////////////////////"
        )
        mortality_syntax = c(mortality_syntax, apply_mortality_instantaneous(mortality_list = processes[[this_process_ndx]], year_ndx, initialisation_phase = TRUE, observation = FALSE, time_step_ndx = time_step_ndx))
        write(mortality_syntax, STAN_file, append = TRUE);

      }
      ## Check if we are exiting a mortality block, if so do the final calculations
      if (process_ndx == max(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Post mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[dq]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");")
                )
              }
              this_line = c(this_line,
                            paste0("\t\tderived_quantity_", names(derived_quantities[dq]), "[1] = pre_derived_quantity_", names(derived_quantities[dq])," + (post_derived_quantity_", names(derived_quantities[dq]), " - pre_derived_quantity_", names(derived_quantities[dq]),");\n"))
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }
    }
  }
  write("\t}", STAN_file, append = TRUE);

  plus_group_syntax = c(
    "\tif (plus_group) {")
  for (c in 1:length(categories$names)) {
    this_line = paste("\t\tcache_initial_numbers_at_age_", categories$names[c], " = numbers_at_age_",categories$names[c],"[1,];",sep = "");
    plus_group_syntax = c(plus_group_syntax, this_line)
  }
  ## run the annual cycle once to approximate the infinite geometric series
  plus_group_syntax = c(plus_group_syntax,"\t\t// Execute annual cycle once")
  write(plus_group_syntax, STAN_file, append = TRUE);

  for (time_step_ndx in 1:n_time_steps) {
    for(process_ndx in 1:n_processes) {
      ## Check if we are about to enter a mortality block, if we are, we will need to write out some pre-caching syntax
      if (process_ndx == min(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Pre mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[time_step_ndx]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");\n")
                )
              }
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }

      this_process_ndx = which(these_process[process_ndx] == names(processes))

      if (processes[[this_process_ndx]]$type$value == "ageing") {

        ageing_syntax = apply_ageing(ageing_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE)
        write(ageing_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "recruitment_beverton_holt") {

        recruit_syntax = apply_recruitment_beverton_holt(recruitment_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE, b0_initialised = processes[[this_process_ndx]]$b0_initialised$value)
        write(recruit_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "mortality_instantaneous") {

        mortality_syntax = apply_mortality_instantaneous(mortality_list = processes[[this_process_ndx]], year_ndx, initialisation_phase = TRUE, observation = FALSE, time_step_ndx = time_step_ndx)
        write(mortality_syntax, STAN_file, append = TRUE);
      }
      ## Check if we are exiting a mortality block, if so do the final calculations
      if (process_ndx == max(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Post mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[dq]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");")
                )
              }
              this_line = c(this_line,
                            paste0("\t\tderived_quantity_", names(derived_quantities[dq]), "[1] = pre_derived_quantity_", names(derived_quantities[dq])," + (post_derived_quantity_", names(derived_quantities[dq]), " - pre_derived_quantity_", names(derived_quantities[dq]),");\n"))
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }
    }
  }


  ## Now do plus group approximation and re-scale if we did b0_initialisation
  plus_group_syntax = c(
    "\t\tc_diff = 0.0;")
  for (c in 1:length(categories$names)) {
    this_line = c(
      paste0("\t\tif (cache_initial_numbers_at_age_", categories$names[c],"[n_ages] > 0) {"),
      paste0("\t\t\tc_diff = numbers_at_age_",categories$names[c],"[1,n_ages] / cache_initial_numbers_at_age_", categories$names[c],"[n_ages];"),
      "\t\t\tprint(c_diff);",
      "\t\t\tif (c_diff > 0.99) ",
      "\t\t\t\tc_diff = 0.99;",
      "\t\t\telse if (c_diff < 0.0)",
      "\t\t\t\tc_diff = 0.0;",
      paste0("\t\t\tnumbers_at_age_",categories$names[c],"[1,n_ages] = cache_initial_numbers_at_age_", categories$names[c],"[n_ages];"),
      paste0("\t\t\tnumbers_at_age_",categories$names[c],"[1,n_ages] = cache_initial_numbers_at_age_", categories$names[c],"[n_ages] * 1 / (1 - c);"),
      "\t\t} else {",
      paste0("\t\t\tnumbers_at_age_",categories$names[c],"[1,n_ages] = cache_initial_numbers_at_age_", categories$names[c],"[n_ages];"),
      "\t\t}",
      "\t}"
    )
    plus_group_syntax = c(plus_group_syntax, this_line)
  }
  write(plus_group_syntax, STAN_file, append = TRUE);


  next_section = c(
    "\tmax_rel_diff = 1e18;")

  for (c in 1:length(categories$names)) {
    next_section = c(next_section,paste0("\told_plus_group[",c,"] = numbers_at_age_",categories$names[c],"[1,n_ages];"))
  }
  next_section = c(next_section,
                   "\twhile (max_rel_diff > 0.005) {",
                   "\t\t// Execute annual cycle")

  write(next_section, STAN_file, append = TRUE);

  for (time_step_ndx in 1:n_time_steps) {
    for(process_ndx in 1:n_processes) {
      ## Check if we are about to enter a mortality block, if we are, we will need to write out some pre-caching syntax
      if (process_ndx == min(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Pre mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[time_step_ndx]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpre_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");\n")
                )
              }
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }

      this_process_ndx = which(these_process[process_ndx] == names(processes))

      if (processes[[this_process_ndx]]$type$value == "ageing") {

        ageing_syntax = apply_ageing(ageing_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE)
        write(ageing_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "recruitment_beverton_holt") {

        recruit_syntax = apply_recruitment_beverton_holt(recruitment_list = processes[[this_process_ndx]], year_ndx = 1, initialisation_phase = TRUE, b0_initialised = processes[[this_process_ndx]]$b0_initialised$value)
        write(recruit_syntax, STAN_file, append = TRUE);

      } else if (processes[[this_process_ndx]]$type$value == "mortality_instantaneous") {

        mortality_syntax = apply_mortality_instantaneous(mortality_list = processes[[this_process_ndx]], year_ndx, initialisation_phase = TRUE, observation = FALSE, time_step_ndx = time_step_ndx)
        write(mortality_syntax, STAN_file, append = TRUE);
      }
      ## Check if we are exiting a mortality block, if so do the final calculations
      if (process_ndx == max(mortality_blockin_each_time_step[[time_step_ndx]])) {
        ## Do pre-cache syntax here, we are at the beginning of a mortality block
        if (time_step_ndx %in% DQs_in_time_step_ndx) {
          write("\t\t// Post mortality Calculations", STAN_file, append = TRUE);
          ## find the DQ and store a preexecuted value
          for (dq in 1:length(derived_quantities)) {
            if (derived_quantities[[dq]]$time_step$value == model$time_steps[time_step_ndx]) {
              this_line = c(paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " = 0.0;"))
              for (c in 1:length(derived_quantities[[dq]]$categories$value)) {
                this_category_age_length = categories$age_lengths[derived_quantities[[dq]]$categories$value[c] == categories$names]
                this_line = c(this_line,
                              paste0("\t\tpost_derived_quantity_", names(derived_quantities[dq]), " += sum(numbers_at_age_", derived_quantities[[dq]]$categories$value[c],"[1,] .* ", derived_quantities[[dq]]$selectivities$value, " .* ", this_category_age_length,");")
                )
              }
              this_line = c(this_line,
                            paste0("\t\tderived_quantity_", names(derived_quantities[dq]), "[1] = pre_derived_quantity_", names(derived_quantities[dq])," + (post_derived_quantity_", names(derived_quantities[dq]), " - pre_derived_quantity_", names(derived_quantities[dq]),");\n"))
              write(this_line, STAN_file, append = TRUE);
            }
          }
        }
      }
    }
  }
  next_section = c(
    "\t\t// Compare plus groups so that we are at equilibrium",
    "\t\tmax_rel_diff = 0;")
  for (c in 1:length(categories$names)) {
    next_section = c(next_section,paste0("\t\tplus_group[",c,"] = numbers_at_age_",categories$names[c],"[1,n_ages];"),
                     paste0("\t\tif (old_plus_group[",c,"] != 0.0) {"),
                     paste0("\t\t\tif (fabs((plus_group[",c,"] - old_plus_group[",c,"]) / old_plus_group[",c,"]) > max_rel_diff)"),
                     paste0("\t\t\t\tmax_rel_diff = fabs((plus_group[",c,"] - old_plus_group[",c,"]) / old_plus_group[",c,"]);"),
                     "\t\t}",
                     paste0("\t\told_plus_group[",c,"] = plus_group[",c,"];"),
                     "\t}"
    )
  }
  write(next_section, STAN_file, append = TRUE);

}

