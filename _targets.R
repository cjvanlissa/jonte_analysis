# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(metafor)
library(stats)
library(dplyr)
library(parallel)
library(data.table)
# library(tarchetypes) # Load other packages as needed.
#vars
#colnames(df)
# Set target options:
tar_option_set(
  packages = c("dplyr", "metafor") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)
#vars <- c("yi", "vi", unique(unlist(moderator_list)))
# Run the R scripts in the R/ folder with your custom functions:
tar_source() # for some reason this adds var1Locate back to the moderator list
# tar_source("other_functions.R") # Source other scripts as needed.

predictors <- c(
  # Non-instructional (17)
  "Pub.Year", "Design", "Control", "Criteria", "Gender", "Ethnicty",
  "Implementer", "Setting", "Group", "Frequency", "Sessions", "Duration",
  "FOI", "Funded", "Measure", "Operation", "Outcome",
  # Instructional (13)
  "Interv.Led Inst", "Explicit.In", "Concrete Rep.", "Mult.Rep",
  "Specific Facts", "Counting Alg.Inst.", "Timed Fluency", "Untimed Fluency",
  "Games", "Indep.Prac.", "Corr.Feedback", "Err.Corr.Proc.", "Incentives"
)

set.seed(7218)
# Replace the target list below with your own:
list(
  tar_target(
    name = df,
    command = load_data(predictors = predictors)
  )
  , tar_target(
    name = df_split,
    command = split_train_test(df, cluster_variable = "id_exp", train_size = .7)
  )
  , tar_target(
    name = dat,
    command = create_folds(df_split, cluster_variable = "id_exp", k = 2)
  )
  , tar_target(
    name = res_brma,
    command = do_brma(dat, yvar = "yi", cluster_var = "id_exp")
  )
  , tar_target(
   name = res_metaforest,
   command = do_metaforest(dat)
  )
  , tar_target(
    name = res_mlrf,
    command = do_mlrf(dat, yvar = "yi")
  )
  , tar_target(
    name = analysis_results,
    command = eval_results(dat, models = list(brma = res_brma , metaforest = res_metaforest
                                              , mlrf = res_mlrf
                                              ))
  )
  , tar_target(
    name = final_model,
    command = final_brma(df, res_brma)
  )
  , tarchetypes::tar_render(name = manuscript, path = "manuscript.rmd", output_file = "index.html", cue = tar_cue("always"))
)


