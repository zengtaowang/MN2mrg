library(tidyverse)
library(mrgsolve)
library(mrggsave)
library(yspec)
library(yaml)
library(pmforest)

source(here::here("utility-functions.R"))

####
## function to create a forest plot
## as specified in a yaml file
forest_yaml <- function(yaml) {

  ## Read plot specifications from yaml
  if(file.exists(yaml)) {
    spec <- yaml::read_yaml(yaml)
  } else {
    stop("\nYAML file not found!\n\n")
  }

  ## Read yspec data specifications
  if(length(spec$data_spec) == 1) {
    if(file.exists(spec$data_spec)) {
      dataspec <- ys_load(spec$data_spec)
    } else {
      stop("\nData specification file not found!\n\n")
    }
  }

  ## file stem for output
  stem <- spec$output_stem

  ## output file type (png or pdf)
  type <- set_option(spec,"output_filetype","png")
  ## width and height for plots
  ## default to 7 by 7, if not specified
  width <- set_option(spec,"width",7)
  height <- set_option(spec,"height",7)
  ## font sizes for plots
  text.size <- set_option(spec,"text.size",3.5)
  shape.size <- set_option(spec,"shape.size",3.0)
  ## shaded interval
  shade <- set_option(spec,"shaded.interval",c(0.8,1.25))

  ## set options for mrggsave and pmplots
  options(mrg.script=paste0("forest.yaml.R : ",yaml),   ## script and yaml file name for documentation
          mrggsave.dir=spec$output_directory,         ## output file for plots
          mrggsave.dev=type,                          ## output file type
          mrggsave.width=width,                       ## width of figure output in inches
          mrggsave.height=height,                     ## height of figure output in inches
          mrggsave.path.type="none")                  ## no RStudio project root in this directory

  ## create directory for figures if it does not already exist
  if (!dir.exists(spec$output_directory)) dir.create(spec$output_directory)

  ## load MRGsolve model
  if(file.exists(spec$input_model)) {
    mod <- mread(spec$input_model)
  } else {
    stop(paste0("\nFile ",spec$input_model," dose not exist!\n\n"))
  }

  ## vector of covariates to include in forest plot
  covariates <- names(spec$covariates)
  ## vector of non-THETA parameters in mrgsolve model
  params <- param(mod) %>% as.data.frame() %>% select(!contains("THETA"))

  ## check that all requested covariates are included in model parameters
  check.param <- map(covariates,function(.cov) {
    if(!(.cov %in% names(params))) {
      cat("\n",.cov,"not in model parameters!\n\n")
      return(1)
    } else {
      return(0)
    }
  }) %>% unlist()
  if(sum(check.param) > 0) stop("Requested covariates not in model parameters")

  refCov <- params %>% select(!any_of(covariates))
  if(ncol(refCov) > 0) {
    catch <- map(names(refCov),function(.x) {
      cat("\n",.x,"set to reference value of",refCov[[.x]],"\n")
    }) %>% unlist()
  }

  ##### generate posteriors ######

  nRep <- spec$nRep
  unc.method <- names(spec$uncertainty)
  unc.set <- 0
  if(unc.method == "covariance") {
    ## specified covariance file
    covFile <- spec$uncertainty$covariance
    ## corresponding EXT file
    extFile <- str_replace(covFile,"cov","ext")

    ## read parameter estimates from ext file
    if(file.exists(extFile)) {
      ext <- read_ext(extFile) %>%
        filter(ITERATION == -1E9) %>%
        select(starts_with("THETA"))
    } else {
      stop("\nEXT file",extFile,"file not found!\n\n")
    }

    ## read covariance matrix from cov file
    if(file.exists(covFile)) {
      cov <- read_ext(covFile) %>%
        filter(str_detect(NAME,"THETA")) %>%
        select(starts_with("THETA"))
    } else {
      stop("\nCovariance matrix file",covFile,"file not found!\n\n")
    }

    ## generate multi-variate normal distribution
    post <- MASS::mvrnorm(n=nRep,mu=as.numeric(ext),Sigma=as.matrix(cov)) %>%
      as.data.frame() %>%
      setNames(names(ext)) %>%
      mutate(IREP=1:n())

    if(nrow(post) == nRep) unc.set <- 1
  }

  if(unc.method == "bootstrap") {
    ## specified bootstrap file
    bootFile <- spec$uncertainty$bootstrap

    ## read parameter estimates from bootstrap file
    if(file.exists(bootFile)) {
      boot <- read.csv(bootFile)
    } else {
      stop("\nBootstrap file",bootFile,"file not found!\n\n")
    }
    ### check to see if this is PsN output
    ### if it is, strip out estimates from original model run
    if(names(boot)[1] == "model") boot <- boot %>% filter(model > 0)

    post <- boot %>%
      select(starts_with("THETA")) %>%
      drop_na() %>%
      mutate(IREP=1:n())

    unc.set <- 1
  }

  if(unc.method == "bayes") {
    ## specified bootstrap file
    extFiles <- spec$uncertainty$bayes

    files.ok <- 1
    ext <- map_df(1:length(extFiles),function(.i) {
      if(file.exists(extFiles[.i])) {
        out <- read_ext(extFiles[.i]) %>%
          filter(ITERATION > 0) %>%
          mutate(CHAIN=.i) %>%
          select(all_of(starts_with("THETA")),CHAIN)
      } else {
        cat("\nEXT file",extFiles[.i],"file not found!\n\n")
        files.ok <- 0
        out <- NULL
      }
      return(out)
    })

    if(files.ok == 0) stop("EXT files for Bayesian posteriors not found.")

    post <- slice_sample(ext,n=nRep) %>%
      mutate(IREP=1:n())

    if(nrow(post) == nRep) unc.set <- 1
  }

  if(unc.set == 0) stop("\nError generationg posteriors!\n\n")


  ### set up dosing event record
  type <- spec$regimen$type
  amt <- spec$regimen$amt
  cmt <- set_option(spec$regimen,"cmt",1)
  ii <- set_option(spec$regimen,"ii",24)
  addl <- set_option(spec$regimen,"addl",1)
  tau <- set_option(spec$regimen,"tau",24)
  pred <- set_option(spec$regimen,"pred","Y")
  if(type == "single")   dosing <- ev(amt=amt,cmt=cmt)
  if(type == "ss")       dosing <- ev(amt=amt,cmt=cmt,ss=1,ii=ii)
  if(type == "multiple") dosing <- ev(time=-1*ii*addl,amt=amt,cmt=cmt,ii=ii,addl=addl)

  ### reference simulations

  ## find reference values
  ref.values <- map_df(covariates,function(.x) {
    cbind.data.frame(name=.x,
                     value=spec$covariates[[.x]]$reference)
  }) %>% pivot_wider()

  ## create idata for reference values
  idata <- ref.values %>% crossing(post)

  ## collect endpoint options
  ends <- names(spec$endpoint)
  ## if parameter forest plot requested
  endParam <- spec$endpoint$parameter$param

  #### check that mrgsolve model produces required output
  modout <- c(outvars(mod)$cmt,outvars(mod)$capture)
  if(!(pred %in% modout)) {
    stop("\n",pred,"not included in MRGsolve model output.\n\n")
  }
  if(("aucinf" %in% ends) & (!("CL" %in% modout))) {
    stop("\nCL not included in MRGsolve model output.\n\n")
  }
  if(!is.null(endParam)) {
    if(!(endParam %in% modout)) {
      stop("\n",endParam,"not included in MRGsolve model output.\n\n")
    }
  }

  ## simulate reference population
  refsim <- mod %>%
    zero_re() %>%
    ev(dosing) %>%
    idata_set(idata) %>%
    carry_out(IREP) %>%
    mrgsim_df(recsort=3,obsonly=TRUE,tgrid=seq(0,tau,0.1))

  ref <- refsim %>%
    group_by(IREP) %>%
    summarize(AUC=mrgmisc::auc_partial(time,!!sym(pred)),
              CMAX=max(!!sym(pred)),
              CMIN=min(!!sym(pred)),
              CAVG=mean(!!sym(pred))) %>%
    ungroup()

  if("aucinf" %in% ends) {
    ref2 <- refsim %>%
      group_by(IREP) %>%
      summarize(CL=median(CL),
                AUCINF=amt/CL) %>%
      ungroup()
    ref <- ref %>%
      left_join(ref2,by=c("IREP"))
  }

  if(!is.null(endParam)) {
    ref2 <- refsim %>%
      group_by(IREP) %>%
      summarize(PARAM=median(!!sym(endParam))) %>%
      ungroup()
    ref <- ref %>%
      left_join(ref2,by=c("IREP"))
  }

  ## build list of covariate values
  x <- map(covariates,function(.x) {
    spec$covariates[[.x]]$values
  }) %>% setNames(covariates)

  ## iterate over covariate values
  out <- imap_dfr(x, function(values,col) {
    # Make an idata set
    idata <- tibble(!!sym(col) := values, LVL = seq_along(values))
    idata <- crossing(post, idata)
    sim <- mod %>%
      zero_re() %>%
      ev(dosing) %>%
      idata_set(idata) %>%
      carry_out(IREP,LVL) %>%
      mrgsim_df(recsort=3,obsonly=TRUE,tgrid=seq(0,tau,0.1))

    simsum <- sim %>%
      group_by(IREP,LVL) %>%
      summarize(AUC=mrgmisc::auc_partial(time,!!sym(pred)),
                CMAX=max(!!sym(pred)),
                CMIN=min(!!sym(pred)),
                CAVG=mean(!!sym(pred))) %>%
      ungroup() %>%
      mutate(name=col,
             value=values[LVL])

    if("aucinf" %in% ends) {
      simsum2 <- sim %>%
        group_by(IREP,LVL) %>%
        summarize(CL=median(CL),
                  AUCINF=amt/CL) %>%
        ungroup()
      simsum <- simsum %>%
        left_join(simsum2,by=c("IREP","LVL"))
    }

    if(!is.null(endParam)) {
      simsum2 <- sim %>%
        group_by(IREP,LVL) %>%
        summarize(PARAM=median(!!sym(endParam))) %>%
        ungroup()
      simsum <- simsum %>%
        left_join(simsum2,by=c("IREP","LVL"))
    }

    ## get factor decode from data spec
    colf <- paste0(col,"_f")
    simsum <- simsum %>%
      mutate(!!sym(col) := value) %>%
      ys_add_factors(dataspec)

    if(colf %in% names(simsum)) {
      simsum <- simsum %>%
        select(-value) %>%
        rename(value = !!sym(colf))
    }

    # Process renames
    if(rlang::is_named(values)) {
      simsum <- mutate(simsum, value := factor(value, labels = names(values), levels = values) )
    } else {
      simsum <- mutate(simsum, value = fct_inorder(as.character(value)))
    }
    simsum
  })

  ## get units for each covariate from dataspec
  units <- map_df(covariates,function(.x) {
    cbind.data.frame(name=.x,
                     unit=ys_get_unit(dataspec[[.x]]),
                     short=ys_get_short(dataspec[[.x]]))
  })

  plist <- map(ends,function(.x) {

    ## set endpoint column names
    end.name <- case_when(.x == "auc" ~ "AUC",
                          .x == "cmax" ~ "CMAX",
                          .x == "cmin" ~ "CMIN",
                          .x == "cavg" ~ "CAVG",
                          .x == "aucinf" ~ "AUCINF",
                          .x == "parameter" ~ "PARAM")
    ref.name <- paste0("REF",end.name)
    rel.name <- paste0("REL",end.name)

    ## set plot label
    label <- spec$endpoint[[.x]]$label

    ## calculate ratio relative to reference
    ## add short names and units
    outx <- out %>%
      left_join(ref %>% select(IREP,!!sym(ref.name) := !!sym(end.name))) %>%
      mutate(!!sym(rel.name) := !!sym(end.name)/!!sym(ref.name))  %>%
      left_join(units) %>%
      mutate(cov_level=paste(value,unit))

    ## make sure covariate factors are ordered correctly
    fout <- map_df(covariates,function(.x) {
      outx %>% filter(name == .x) %>%
        mutate(cov_factor=factor(value,levels=value,labels=cov_level)) %>%
        arrange(cov_factor)
    })

    # Summarize for forest plot
    sum_data <- pmforest::summarize_data(
      data = fout,
      value = rel.name,
      group = "short",
      group_level = "cov_factor",
      probs = c(0.025, 0.975),
      statistic = "median"
    ) %>%
      arrange(desc(group_level))

    # Create plot
    p <- plot_forest(
      data = sum_data,
      shaded_interval = shade,
      text_size = text.size,
      shape_size = shape.size,
      shapes = "circle",
      vline_intercept = 1,
      x_lab = paste0("Ratio of ",label," Relative to Reference"),
      CI_label = "Median [95% CI]",
      plot_width = width,
      annotate_CI = TRUE,
      nrow = 1)

    return(p)
  })

  ## export results
  pfiles <- mrggsave(plist,stem=stem,onefile=FALSE)

  ## add to quarto file if specified
  if(spec$quarto_output) {
    qmd <- paste0(spec$output_directory,"/",spec$output_stem,".qmd")
    init_quarto(qmd,stem=spec$output_stem,title="Forest Plots")

    map(1:length(pfiles),function(.x) {
      plotTitle <- spec$endpoint[[.x]]$label
      plot_quarto(qmd=qmd,
                  path=basename(pfiles[.x]),
                  title=plotTitle,
                  caption=NULL)
    })
    render_quarto(qmd)
  }
}

yaml <- here::here("forest.yaml")
forest_yaml(yaml)
