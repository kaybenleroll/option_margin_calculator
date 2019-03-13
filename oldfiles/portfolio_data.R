

process.option.portfolio <- function(portfolio.dt, margin.spread.lst) {

    flag.slicing.iteration <- TRUE;

    margin.lst <- list();

    while(flag.slicing.iteration) {
        sample.lst <- sample.asset.lines(portfolio.dt, margin.spread.lst);

        if(is.null(sample.lst$asset.dt)) {
            flag.slicing.iteration <- FALSE;
        } else {
            if(!is.na(sample.lst$margin)) {
                portfolio.dt <- remove.sample.from.portfolio(portfolio.dt, sample.lst$asset.dt);

                sample.lst$new.portfolio <- portfolio.dt;

                margin.lst <- c(margin.lst, list(sample.lst));
            }
        }

        if(dim(portfolio.dt)[1] == 0) { flag.slicing.iteration <- FALSE }
    }

    return(margin.lst);
}


remove.sample.from.portfolio <- function(portfolio.dt, sample.dt) {
    stopifnot(all(sample.dt$asset.symbol %in% portfolio.dt$asset.symbol));

    row.idx <- match(sample.dt$asset.symbol, portfolio.dt$asset.symbol);

    stopifnot(all(sign(portfolio.dt[row.idx]$position) == sign(sample.dt$position)));
    stopifnot(all(abs (portfolio.dt[row.idx]$position) >= abs (sample.dt$position)));

    portfolio.dt$position[row.idx] <- portfolio.dt$position[row.idx] - sample.dt$position;

    portfolio.dt <- portfolio.dt[position != 0];

    return(portfolio.dt);
}


sample.asset.lines <- function(portfolio.dt, margin.spread.lst, max.iter = 100) {
    margin.dims         <- names(margin.spread.lst);
    margin.dims.numeric <- as.numeric(margin.dims);

    stopifnot(all(is.numeric(margin.dims.numeric)));

    if(dim(portfolio.dt)[1] < max(margin.dims.numeric)) {
        margin.dims.numeric <- margin.dims.numeric[margin.dims.numeric <= dim(portfolio.dt)[1]];

        margin.dims <- as.character(margin.dims.numeric);
    }


    iter.count   <- 1;
    have.spread  <- FALSE;
    spread.count <- lapply(margin.spread.lst, length);



    while(!have.spread & iter.count <= max.iter) {
        sample.dim       <- sample(margin.dims, 1, replace = TRUE);
        spread.use       <- margin.spread.lst[[ sample.dim ]];
        spread.dim.count <- spread.count[[ sample.dim ]];

        row.idx   <- sort(sample(1:dim(portfolio.dt)[1], as.numeric(sample.dim), replace = FALSE));
        sample.dt <- portfolio.dt[row.idx];

        ### Construct some positions for the spreads
        min.size <- min(abs(sample.dt$position * sample.dt$contract.size));

        trial.spread.dt          <- sample.dt;
        trial.spread.dt$position <- (min.size / sample.dt$contract.size) * sign(sample.dt$position);

        if(sample.dim == '3' & !('equity' %in% sample.dt)) {
            butterfly.dt <- sample.dt;

            positions <- sample.dt$position * c(1, 0.5, 1);
            fly.count <- trunc(min(abs(positions)));

            butterfly.dt$position <- fly.count * sign(butterfly.dt$position) * c(1, 2, 1);
        }



        spread.lst <- lapply(1:spread.dim.count, function(order.idx) {
            check.func.name <- spread.use[ order.idx ];

            if(sample.dim == '3' & grepl('butterfly', check.func.name)) {
                use.sample.dt <- butterfly.dt;
            } else {
                use.sample.dt <- trial.spread.dt;
            }

            calc.func.name <- gsub("check.margin", "calculate.margin", check.func.name);

            if(get(check.func.name)(use.sample.dt, assert = FALSE)) {
                margin <- get(calc.func.name)(use.sample.dt);
            } else {
                use.sample.dt <- NULL;
                margin        <- NA;
            }

            return(list(spread   = gsub("check\\.margin\\.", "", check.func.name),
                        asset.dt = use.sample.dt,
                        margin   = margin));
        });

        spread.margin <- sapply(spread.lst, function(x) x$margin);

        if(any(!is.na(spread.margin))) {
            have.spread <- TRUE;

            sample.lst <- spread.lst[[ match(min(spread.margin, na.rm = TRUE), spread.margin) ]];
        } else {
            iter.count <- iter.count + 1;

            sample.lst <- list(margin = NA, asset.dt = NULL);
        }
    }


    return(sample.lst);
}
