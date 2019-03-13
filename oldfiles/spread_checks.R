
has.asset.table.columns <- function(asset.dt) {
    stopifnot('asset.symbol'  %in% names(asset.dt));
    stopifnot('position'      %in% names(asset.dt));
    stopifnot('asset.type'    %in% names(asset.dt));
    stopifnot('margin.perc'   %in% names(asset.dt));
    stopifnot('price'         %in% names(asset.dt));
    stopifnot('contract.size' %in% names(asset.dt));


    return(TRUE);
}


has.option.table.columns <- function(asset.dt) {
    stopifnot(has.asset.table.columns(asset.dt));

    stopifnot('expiry'     %in% names(asset.dt));
    stopifnot('strike'     %in% names(asset.dt));
    stopifnot('symbol'     %in% names(asset.dt));
    stopifnot('callput'    %in% names(asset.dt));
    stopifnot('underlying' %in% names(asset.dt));

    return(TRUE);
}


check.margin.equity <- function(asset.dt, assert = TRUE) {
    dim.asset <- 1;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.asset.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (asset.dt$asset.type == 'equity');

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
    }

    check <- check.dim & check.asset.type;

    return(check);
}


check.margin.unhedged.long.option <- function(asset.dt, assert = TRUE) {
    dim.asset <- 1;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (asset.dt$asset.type == 'option');
    check.position   <- (asset.dt$position > 0);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.position);
    }

    check <- check.dim & check.asset.type & check.position;

    return(check);
}


check.margin.unhedged.short.option <- function(asset.dt, assert = TRUE) {
    dim.asset <- 1;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (asset.dt$asset.type == 'option');
    check.position   <- (asset.dt$position < 0);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.position);
    }

    check <- check.dim & check.asset.type & check.position;

    return(check);
}


check.margin.callput.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.callput    <- (all(asset.dt$callput == 'C')) | (all(asset.dt$callput == 'P'));
    check.position   <- (asset.dt$position[1] == - asset.dt$position[2]);

    ### This checks that the short option position expires at or before the long option position
    check.expiry     <- (asset.dt$expiry[asset.dt$position < 0] <= asset.dt$expiry[asset.dt$position > 0]);


    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.callput);
        stopifnot(check.position);
        stopifnot(check.expiry);
    }

    check <- check.dim & check.asset.type & check.callput & check.position;

    return(check);
}


check.margin.short.call.short.put.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.callput    <- (all(c('C', 'P') %in% asset.dt$callput));
    check.position   <- (asset.dt$position[1] == asset.dt$position[2]) & (asset.dt$position[1] < 0);
    check.expiry     <- (length(unique(asset.dt$expiry)) == 1);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.callput);
        stopifnot(check.position);
        stopifnot(check.expiry);
    }

    check <- check.dim & check.asset.type & check.callput & check.position & check.expiry;

    return(check);
}


check.margin.long.call.long.put.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.callput    <- (all(c('C', 'P') %in% asset.dt$callput));
    check.position   <- (asset.dt$position[1] == asset.dt$position[2]) & (asset.dt$position[1] > 0);
    check.expiry     <- (length(unique(asset.dt$expiry)) == 1);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.callput);
        stopifnot(check.position);
        stopifnot(check.expiry);
    }

    check <- check.dim & check.asset.type & check.callput & check.position & check.expiry;

    return(check);
}


check.margin.long.call.short.call.long.put.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 3;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.longcall   <- asset.dt[, any('C' == callput & position > 0)];
    check.shortcall  <- asset.dt[, any('C' == callput & position < 0)];
    check.longput    <- asset.dt[, any('P' == callput & position > 0)];
    check.position   <- (abs(asset.dt$position[1]) == abs(asset.dt$position[2])) & (abs(asset.dt$position[2]) == abs(asset.dt$position[3]));
    check.expiry     <- (length(unique(asset.dt$expiry)) == 1);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.longcall);
        stopifnot(check.shortcall);
        stopifnot(check.longput);
        stopifnot(check.position);
        stopifnot(check.expiry);
    }

    check <- check.dim & check.asset.type & check.longcall & check.shortcall & check.longput & check.position & check.expiry;

    return(check);
}


check.margin.box.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 4;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.long.call  <- asset.dt[, any('C' == callput & position > 0)];
    check.short.call <- asset.dt[, any('C' == callput & position < 0)];
    check.long.put   <- asset.dt[, any('P' == callput & position > 0)];
    check.short.put  <- asset.dt[, any('P' == callput & position < 0)];
    check.expiry     <- (length(unique(asset.dt$expiry)) == 1);
    check.position   <- (length(unique(abs(asset.dt$position))) == 1);
    check.strikes    <- (asset.dt[callput == 'C' & position > 0]$strike == asset.dt[callput == 'P' & position < 0]$strike) &
                        (asset.dt[callput == 'C' & position < 0]$strike == asset.dt[callput == 'P' & position > 0]$strike);

    if(length(check.strikes) < 1) { check.strikes <- FALSE }

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.long.call);
        stopifnot(check.short.call);
        stopifnot(check.long.put);
        stopifnot(check.short.put);
        stopifnot(check.position);
        stopifnot(check.expiry);
        stopifnot(check.strikes);
    }

    check <- check.dim & check.asset.type & check.long.call & check.short.call & check.long.put & check.short.put & check.expiry & check.position & check.strikes;

    return(check);
}


check.margin.long.butterfly.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 3;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim           <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type    <- (all(asset.dt$asset.type == 'option'));
    check.expiry        <- (length(unique(asset.dt$expiry)) == 1);
    check.callput       <- (all(asset.dt$callput == 'C') | all(asset.dt$callput == 'P'));
    check.strikes       <- asset.dt[order(strike)][, diff(strike)[1] == diff(strike)[2]];
    check.position      <- asset.dt[order(strike)][, (position[1] * position[3] > 0)];
    check.position.size <- asset.dt[order(strike)][, (position[2] == -2 * position[1]) & (position[2] == -2 * position[3])];
    check.long.spread   <- asset.dt[order(strike)][, position[2] < 0];

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.expiry);
        stopifnot(check.callput);
        stopifnot(check.strikes);
        stopifnot(check.position);
        stopifnot(check.position.size);
        stopifnot(check.long.spread);
    }

    check <- check.dim & check.asset.type & check.expiry & check.callput & check.strikes & check.position & check.position.size & check.long.spread;

    return(check);
}


check.margin.short.butterfly.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 3;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim           <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type    <- (all(asset.dt$asset.type == 'option'));
    check.expiry        <- (length(unique(asset.dt$expiry)) == 1);
    check.callput       <- (all(asset.dt$callput == 'C') | all(asset.dt$callput == 'P'));
    check.strikes       <- asset.dt[order(strike)][, diff(strike)[1] == diff(strike)[2]];
    check.position      <- asset.dt[order(strike)][, (position[1] * position[3] > 0)];
    check.position.size <- asset.dt[order(strike)][, (position[2] == -2 * position[1]) & (position[2] == -2 * position[3])];
    check.short.spread  <- asset.dt[order(strike)][, position[2] > 0];

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.expiry);
        stopifnot(check.callput);
        stopifnot(check.strikes);
        stopifnot(check.position);
        stopifnot(check.position.size);
        stopifnot(check.short.spread);
    }

    check <- check.dim & check.asset.type & check.expiry & check.callput & check.strikes & check.position & check.position.size & check.short.spread;

    return(check);
}


check.margin.long.condor.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 4;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim            <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type     <- (all(asset.dt$asset.type == 'option'));
    check.expiry         <- (length(unique(asset.dt$expiry)) == 1);
    check.strikes        <- asset.dt[order(strike)][, all(diff(strike) == diff(strike)[1])];
    check.callput        <- (all(asset.dt$callput == 'C') | all(asset.dt$callput == 'P'));
    check.long.position  <- asset.dt[order(strike)][, (position[1] > 0 & position[1] == position[4])];
    check.short.position <- asset.dt[order(strike)][, (position[2] < 0 & position[2] == position[3])];
    check.position       <- asset.dt[order(strike)][, (position[1] == - position[2])];

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.expiry);
        stopifnot(check.strikes);
        stopifnot(check.callput);
        stopifnot(check.long.position);
        stopifnot(check.short.position);
        stopifnot(check.position);
    }

    check <- check.dim & check.asset.type & check.expiry & check.strikes & check.callput & check.long.position & check.short.position & check.position;

    return(check);
}


check.margin.short.iron.butterfly.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 4;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim           <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type    <- (all(asset.dt$asset.type == 'option'));
    check.expiry        <- (length(unique(asset.dt$expiry)) == 1);
    check.strikes       <- asset.dt[order(strike)][, (diff(strike)[1] == diff(strike)[3]) & (strike[2] == strike[3])];
    check.position      <- asset.dt[order(strike)][, all(abs(position) == abs(position)[1])];
    check.long.put      <- asset.dt[order(strike)][, position[1] > 0 & callput[1] == 'P'];
    check.long.call     <- asset.dt[order(strike)][, position[4] > 0 & callput[4] == 'C'];
    check.short.meat    <- asset.dt[order(strike, callput)][, position[2] < 0 & callput[2] == 'C' & position[3] < 0 & callput[3] == 'P'];


    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.expiry);
        stopifnot(check.strikes);
        stopifnot(check.position);
        stopifnot(check.long.put);
        stopifnot(check.long.call);
        stopifnot(check.short.meat);
    }

    check <- check.dim & check.asset.type & check.expiry & check.strikes & check.position & check.long.put & check.long.call & check.short.meat;

    return(check);
}


check.margin.short.iron.condor.spread <- function(asset.dt, assert = TRUE) {
    dim.asset <- 4;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.asset.type <- (all(asset.dt$asset.type == 'option'));
    check.expiry     <- (length(unique(asset.dt$expiry)) == 1);
    check.strikes    <- asset.dt[order(strike)][, all(diff(strike) == diff(strike)[1])];
    check.long.put   <- asset.dt[order(strike)][, (callput[1] == 'P' & position[1] > 0)];
    check.short.put  <- asset.dt[order(strike)][, (callput[2] == 'P' & position[2] < 0)];
    check.short.call <- asset.dt[order(strike)][, (callput[3] == 'C' & position[3] < 0)];
    check.long.call  <- asset.dt[order(strike)][, (callput[4] == 'C' & position[4] > 0)];
    check.position   <- asset.dt[order(strike)][, all(abs(position)[1] == abs(position))];

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.asset.type);
        stopifnot(check.expiry);
        stopifnot(check.strikes);
        stopifnot(check.long.put);
        stopifnot(check.short.put);
        stopifnot(check.short.call);
        stopifnot(check.long.call);
        stopifnot(check.position);
    }

    check <- check.dim & check.asset.type & check.expiry & check.strikes & check.long.put & check.short.put & check.short.call & check.long.call & check.position;

    return(check);
}


check.margin.covered.call <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim        <- (dim(asset.dt)[1] == dim.asset);
    check.option     <- ('option' %in% asset.dt$asset.type);
    check.equity     <- ('equity' %in% asset.dt$asset.type);
    check.short.call <- asset.dt[asset.type == 'option', any(callput == 'C' & position < 0)];
    check.position   <- (asset.dt[, sum(contract.size * position)] == 0);

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.short.call);
        stopifnot(check.position);
    }

    check <- check.dim & check.option & check.equity & check.short.call & check.position;

    return(check);
}


check.margin.protective.put <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim      <- (dim(asset.dt)[1] == dim.asset);
    check.option   <- ('option' %in% asset.dt$asset.type);
    check.equity   <- ('equity' %in% asset.dt$asset.type);
    check.long.put <- asset.dt[asset.type == 'option', any(callput == 'P' & position > 0)];
    check.position <- (asset.dt[, (contract.size * position)[1] == (contract.size * position)[2]]);


    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.long.put);
        stopifnot(check.position);
    }

    check <- check.dim & check.option & check.equity & check.long.put & check.position;

    return(check);
}


check.margin.protected.short <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim       <- (dim(asset.dt)[1] == dim.asset);
    check.option    <- ('option' %in% asset.dt$asset.type);
    check.equity    <- ('equity' %in% asset.dt$asset.type);
    check.long.call <- asset.dt[asset.type == 'option', any(callput == 'C' & position > 0)];
    check.position  <- (asset.dt[, sum(contract.size * position)] == 0);


    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.long.call);
        stopifnot(check.position);
    }

    check <- check.dim & check.option & check.equity & check.long.call & check.position;

    return(check);
}


check.margin.covered.put <- function(asset.dt, assert = TRUE) {
    dim.asset <- 2;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim       <- (dim(asset.dt)[1] == dim.asset);
    check.option    <- ('option' %in% asset.dt$asset.type);
    check.equity    <- ('equity' %in% asset.dt$asset.type);
    check.short.put <- asset.dt[asset.type == 'option', any(callput == 'P' & position < 0)];
    check.position  <- (asset.dt[, (contract.size * position)[1] == (contract.size * position)[2]]);


    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.short.put);
        stopifnot(check.position);
    }

    check <- check.dim & check.option & check.equity & check.short.put & check.position;

    return(check);
}


check.margin.conversion <- function(asset.dt, assert = TRUE) {
    dim.asset <- 3;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim              <- (dim(asset.dt)[1] == dim.asset);
    check.option           <- ('option' %in% asset.dt$asset.type);
    check.equity           <- ('equity' %in% asset.dt$asset.type);
    check.expiry           <- asset.dt[asset.type == 'option', any(expiry[position < 0] <= expiry[position > 0])];
    check.long.equity      <- asset.dt[asset.type == 'equity', any(position > 0)];
    check.long.put         <- asset.dt[asset.type == 'option', any(callput == 'P' & position > 0)];
    check.short.call       <- asset.dt[asset.type == 'option', any(callput == 'C' & position < 0)];
    check.option.position  <- asset.dt[asset.type == 'option', sum(position) == 0]
    check.equity.position  <- asset.dt[, any((contract.size * position)[asset.type == 'option' & position < 0] + position[asset.type == 'equity'] == 0)];

    if(length(check.equity.position) == 0) { check.equity.position <- FALSE }

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.long.equity);
        stopifnot(check.long.put);
        stopifnot(check.short.call);
        stopifnot(check.option.position);
        stopifnot(check.equity.position);
    }

    check <- check.dim & check.option & check.equity & check.expiry & check.long.equity & check.long.put & check.short.call & check.option.position & check.equity.position;

    return(check);
}


check.margin.reverse.conversion <- function(asset.dt, assert = TRUE) {
    dim.asset <- 3;

    if(is.null(asset.dt)) { return(dim.asset) }

    stopifnot(has.option.table.columns(asset.dt));

    check.dim              <- (dim(asset.dt)[1] == dim.asset);
    check.option           <- ('option' %in% asset.dt$asset.type);
    check.equity           <- ('equity' %in% asset.dt$asset.type);
    check.expiry           <- asset.dt[asset.type == 'option', any(expiry[position < 0] <= expiry[position > 0])];
    check.short.equity     <- asset.dt[asset.type == 'equity', any(position < 0)];
    check.long.call        <- asset.dt[asset.type == 'option', any(callput == 'C' & position > 0)];
    check.short.put        <- asset.dt[asset.type == 'option', any(callput == 'P' & position < 0)];
    check.option.position  <- asset.dt[asset.type == 'option', sum(position) == 0]
    check.equity.position  <- asset.dt[, any((contract.size * position)[asset.type == 'option' & position > 0] + position[asset.type == 'equity'] == 0)];

    if(length(check.equity.position) == 0) { check.equity.position <- FALSE }

    if(assert) {
        stopifnot(check.dim);
        stopifnot(check.option);
        stopifnot(check.equity);
        stopifnot(check.expiry);
        stopifnot(check.short.equity);
        stopifnot(check.long.call);
        stopifnot(check.short.put);
        stopifnot(check.option.position);
        stopifnot(check.equity.position);
    }

    check <- check.dim & check.option & check.equity & check.expiry & check.short.equity & check.long.call & check.short.put & check.option.position & check.equity.position;

    return(check);
}


generate.check.margin.list <- function() {
    function.names <- ls(pattern = "^check.margin", env = .GlobalEnv);

    names.dims.lst <- list();

    names.dims.lst <- sapply(function.names, function(func.name) get(func.name)(NULL));


    margin.functions.lst <- list();

    for(i in 1:length(names.dims.lst)) {
        func.dim <- as.character(names.dims.lst[i]);


        margin.functions.lst[[func.dim]] <- c(margin.functions.lst[[func.dim]], names(names.dims.lst)[i]);
    }


    return(margin.functions.lst);
}
