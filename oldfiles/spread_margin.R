
### IIROC Rule #5310
calculate.margin.equity <- function(asset.dt) {
    stopifnot(has.asset.table.columns(asset.dt));
    stopifnot(check.margin.equity(asset.dt));

    stopifnot(dim(asset.dt)[1] == 1);
    stopifnot(asset.dt$asset.type == 'equity');



    margin <- asset.dt[, abs(position) * price * margin.perc];

    return(margin);
}


calculate.margin.option.underlying <- function(asset.dt) {
    stopifnot(has.option.table.columns(asset.dt));

    stopifnot(dim(asset.dt)[1] == 1);
    stopifnot(asset.dt$asset.type == 'option');


    underlying.asset.dt <- asset.dt[, list(asset.symbol = symbol, asset.type = 'equity', position = position * contract.size, contract.size = 1, price = underlying, margin.perc)];

    underlying.margin <- calculate.margin.equity(underlying.asset.dt);

    return(underlying.margin);
}


### IIROC Rule #5720
calculate.margin.unhedged.long.option <- function(asset.dt) {
    stopifnot(check.margin.unhedged.long.option(asset.dt));


    asset.dt[, intrinsic.value := max(ifelse(callput == 'C', (underlying - strike), (strike - underlying)), 0)];
    asset.dt[, time.value      := max(price - intrinsic.value, 0)];


    underlying.margin <- calculate.margin.option.underlying(asset.dt);
    amount.inthemoney <- asset.dt[, position * contract.size * intrinsic.value];
    amount.time       <- asset.dt[, position * contract.size * time.value];

    part.i  <- min(underlying.margin, amount.inthemoney);
    part.ii <- amount.time;

    margin <- part.i + part.ii;

    return(margin);
}


### IIROC Rule #5721
calculate.margin.unhedged.short.option <- function(asset.dt) {
    stopifnot(check.margin.unhedged.short.option(asset.dt));


    asset.dt[, money := ifelse(callput == 'C', (underlying - strike), (strike - underlying))];


    underlying.margin <- calculate.margin.option.underlying(asset.dt);

    part.i  <- underlying.margin;
    part.ii <- asset.dt[, ifelse(money < 0, abs(position) * contract.size * money, 0)];

    margin <- part.i + part.ii;

    return(margin);
}


### IIROC Rule #5730
calculate.margin.callput.spread <- function(asset.dt) {
    stopifnot(check.margin.callput.spread(asset.dt));

    asset.dt[, money           := ifelse(callput == 'C', (underlying - strike), (strike - underlying))];
    asset.dt[, intrinsic.value := pmax(money, 0)];
    asset.dt[, time.value      := pmax(price - intrinsic.value, 0)];

    spread.price <- asset.dt[, sum(position * contract.size * price)];

    if(spread.price < 0) {
        short.margin <- calculate.margin.unhedged.short.option(asset.dt[position < 0]);
        spread.loss  <- abs(min(0, asset.dt[, sum(position * contract.size * money)]));

        part.i  <- short.margin;
        part.ii <- spread.loss;

        margin <- min(part.i, part.ii);
    } else {
        margin <- spread.price;
    }

    return(margin);
}


### IIROC Rule #5731
calculate.margin.short.call.short.put.spread <- function(asset.dt) {
    stopifnot(check.margin.short.call.short.put.spread(asset.dt));

    put.margin  <- calculate.margin.unhedged.short.option(asset.dt[callput == 'P']);
    call.margin <- calculate.margin.unhedged.short.option(asset.dt[callput == 'C']);

    put.exercise  <- asset.dt[callput == 'P', position * contract.size * strike];
    call.exercise <- asset.dt[callput == 'C', position * contract.size * strike];

    part.i <- max(put.margin, call.margin);
    part.ii <- put.exercise - call.exercise;


    margin <- max(part.i, part.ii);

    return(margin);
}


### IIROC Rule #5732
calculate.margin.long.call.long.put.spread <- function(asset.dt) {
    stopifnot(check.margin.long.call.long.put.spread(asset.dt));

    put.margin  <- calculate.margin.unhedged.long.option(asset.dt[callput == 'P']);
    call.margin <- calculate.margin.unhedged.long.option(asset.dt[callput == 'C']);

    put.market  <- asset.dt[callput == 'P', position * contract.size * price];
    call.market <- asset.dt[callput == 'C', position * contract.size * price];

    put.exercise  <- asset.dt[callput == 'P', position * contract.size * strike];
    call.exercise <- asset.dt[callput == 'C', position * contract.size * strike];

    part.i  <- put.margin + call.margin;
    part.ii <- put.market + call.market - min(0, put.exercise - call.exercise);

    margin <- min(part.i, part.ii);

    return(margin);
}


### IIROC Rule #5733
calculate.margin.long.call.short.call.long.put.spread <- function(asset.dt) {
    stopifnot(check.margin.long.call.short.call.long.put.spread(asset.dt));

    longcall.market  <- asset.dt[callput == 'C' & position > 0, position * contract.size * price];
    shortcall.market <- asset.dt[callput == 'C' & position < 0, position * contract.size * price];
    longput.market   <- asset.dt[callput == 'P' & position > 0, position * contract.size * price];

    part.i   <- longcall.market;
    part.ii  <- longput.market;
    part.iii <- shortcall.market;


    longcall.exercise  <- asset.dt[callput == 'C' & position > 0, position * contract.size * strike];
    shortcall.exercise <- asset.dt[callput == 'C' & position < 0, abs(position) * contract.size * strike];
    longput.exercise   <- asset.dt[callput == 'P' & position > 0, position * contract.size * strike];

    exercise.a <- longcall.exercise - shortcall.exercise;
    exercise.b <- longcall.exercise - longput.exercise;

    part.iv <- max(exercise.a, exercise.b);


    margin <- part.i + part.ii + part.iii + part.iv;


    return(margin);
}


### IIROC #5735
calculate.margin.box.spread <- function(asset.dt) {
    stopifnot(check.margin.box.spread(asset.dt));

    call.exercise <- asset.dt[callput == 'C' & position > 0, position * contract.size * strike];
    put.exercise  <- asset.dt[callput == 'P' & position > 0, position * contract.size * strike];

    net.exercise <- call.exercise - put.exercise;
    part.i <- net.exercise;


    market.value <- asset.dt[, sum(position * contract.size * strike)];
    part.ii <- market.value;


    margin <- min(part.i, part.ii);

    return(margin);
}


### IIROC #5736
calculate.margin.long.butterfly.spread <- function(asset.dt) {
    stopifnot(check.margin.long.butterfly.spread(asset.dt));

    margin <- asset.dt[, sum(position * contract.size * price)];

    return(margin);
}


### IIROC #5737
calculate.margin.short.butterfly.spread <- function(asset.dt) {
    stopifnot(check.margin.short.butterfly.spread(asset.dt));

    asset.dt[, intrinsic := ifelse(callput == 'C', pmax(0, underlying - strike), pmax(0, strike - underlying))];

    margin <- asset.dt[, abs(sum(position * contract.size * intrinsic))];

    return(margin);
}


### IIROC #5738
calculate.margin.long.condor.spread <- function(asset.dt) {
    stopifnot(check.margin.long.condor.spread(asset.dt));

    margin <- asset.dt[, max(0, sum(position * contract.size * price))];

    return(margin);
}


### IIROC #5739
calculate.margin.short.iron.butterfly.spread <- function(asset.dt) {
    stopifnot(check.margin.short.iron.butterfly.spread(asset.dt));

    margin <- asset.dt[order(strike), position[1] * contract.size[1] * (strike[2] - strike[1])];

    return(margin);
}


### IIROC #5740
calculate.margin.short.iron.condor.spread <- function(asset.dt) {
    stopifnot(check.margin.short.iron.condor.spread(asset.dt));

    margin <- asset.dt[order(strike), position[1] * contract.size[1] * (strike[2] - strike[1])];

    return(margin);
}


### IIROC #5750
calculate.margin.covered.call <- function(asset.dt) {
    stopifnot(check.margin.covered.call(asset.dt));

    margin.underlying <- calculate.margin.equity(asset.dt[asset.type == 'equity']);
    part.i <- margin.underlying;


    call.exercise <- asset.dt[asset.type == 'option', abs(position) * contract.size * strike];
    loan.value    <- asset.dt[asset.type == 'equity', abs(position) * contract.size * price] - margin.underlying;
    part.ii <- max(0, call.exercise - loan.value);


    margin <- min(part.i, part.ii);

    return(margin);
}


### IIROC #5751
calculate.margin.protective.put <- function(asset.dt) {
    stopifnot(check.margin.protective.put(asset.dt));

    margin.underlying <- calculate.margin.equity(asset.dt[asset.type == 'equity']);
    part.i <- margin.underlying;


    put.exercise <- asset.dt[asset.type == 'option', position * contract.size * strike];
    market.value <- asset.dt[asset.type == 'equity', position * contract.size * price];
    put.value    <- asset.dt[asset.type == 'option', position * contract.size * price];
    part.ii <- max(0, market.value + put.value - put.exercise);


    margin <- min(part.i, part.ii);

    return(margin);
}


### IIROC #5752
calculate.margin.protected.short <- function(asset.dt) {
    stopifnot(check.margin.protected.short(asset.dt));

    call.value <- asset.dt[asset.type == 'option', position * contract.size * price];
    part.i <- call.value;

    call.exercise     <- asset.dt[asset.type == 'option', position * contract.size * strike];
    margin.underlying <- calculate.margin.equity(asset.dt[asset.type == 'equity']);
    part.ii <- min(call.exercise, margin.underlying);


    margin <- part.i + part.ii;

    return(margin);
}


### IIROC #5753
calculate.margin.covered.put <- function(asset.dt) {
    stopifnot(check.margin.covered.put(asset.dt));

    margin.underlying <- calculate.margin.equity(asset.dt[asset.type == 'equity']);
    part.i <- margin.underlying;

    asset.dt[, money := pmax(0, strike - underlying)];
    put.exercise <- asset.dt[asset.type == 'option', abs(position) * contract.size * strike];
    money.value  <- asset.dt[asset.type == 'option', abs(position) * contract.size * money];

    part.ii <- max(0, put.exercise - money.value);


    margin <- min(part.i, part.ii);

    return(margin);
}


### IIROC #5754
calculate.margin.conversion <- function(asset.dt) {
    stopifnot(check.margin.conversion(asset.dt));

    market.underlying <- asset.dt[asset.type == 'equity', abs(position) * contract.size * price];

    market.long.put   <- asset.dt[callput == 'P', abs(position) * contract.size * price];
    market.short.call <- asset.dt[callput == 'C', abs(position) * contract.size * price];
    exercise.call     <- asset.dt[callput == 'C', abs(position) * contract.size * strike];
    exercise.put      <- asset.dt[callput == 'P', abs(position) * contract.size * strike];

    exercise.diff <- min(market.underlying - exercise.call, market.underlying - exercise.put);


    margin <- market.long.put - market.short.call + exercise.diff;

    return(margin);
}


calculate.margin.reverse.conversion <- function(asset.dt) {
    stopifnot(check.margin.reverse.conversion(asset.dt));

    market.underlying <- asset.dt[asset.type == 'equity', abs(position) * contract.size * price];

    market.long.call  <- asset.dt[callput == 'C', abs(position) * contract.size * price];
    market.short.put  <- asset.dt[callput == 'P', abs(position) * contract.size * price];
    exercise.call     <- asset.dt[callput == 'C', abs(position) * contract.size * strike];
    exercise.put      <- asset.dt[callput == 'P', abs(position) * contract.size * strike];

    exercise.diff <- min(exercise.call - market.underlying, exercise.put - market.underlying);


    margin <- market.long.call - market.short.put + exercise.diff;

    return(margin);
}
