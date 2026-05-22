Xvalid_all_custom <- function (mf, maxL, n.fold, minbucket, minsplit, cp, lookahead, 
          sss, alpha.endcut, a, multi.start, n.starts, fold_rowids = NULL) 
{
  browser()
  if(!is.null(fold_rowids)){
    n.fold = length(fold_rowids)
  }
  N <- nrow(mf)
  if (n.fold > N | n.fold <= 1) 
    stop("n.fold is not acceptable")
  if (maxL > N) {
    warning("The maximum number of split is too large")
    maxL <- N - 1
  }
  pred <- matrix(NA, nrow = N, ncol = maxL + 1)
  inx <- sample(1:N)
  inx.xvalid <- as.numeric(cut(1:N, n.fold))
  for (i in 1:n.fold) {
    inx.test <- inx[inx.xvalid == i]
    if(!is.null(fold_rowids)){
      inx.test <- fold_rowids[[i]]
    }
    test <- mf[inx.test, ]
    train <- mf[-inx.test, ]
    if (sss == FALSE) {
      fit.train <- do.call(REmrt_GS_, list(mf = train, 
                                           maxL, minbucket, minsplit, cp, lookahead))
    }
    else {
      fit.train <- do.call(REmrt_SSS, list(mf = train, 
                                           maxL = maxL, minbucket = minbucket, minsplit = minsplit, 
                                           cp = cp, lookahead = lookahead, alpha.endcut = alpha.endcut, 
                                           a = a, multi.start = multi.start, n.starts = n.starts))
    }
    yi.train <- model.response(fit.train$data)
    vi.train <- c(t(fit.train$data["(vi)"]))
    tau2 <- fit.train$tree$tau2
    nsplt <- nrow(fit.train$tree)
    train.y <- .ComputeY(fit.train$node.split, yi.train, 
                         vi.train, tau2)
    test.nodes <- prednode_cpp(fit.train, test)
    test.y <- .PredY(train.y, test.nodes)
    if (any(is.na(test.y))) {
      inx.NA <- which(is.na(test.y), arr.ind = TRUE)
      test.y <- .ReplaceNA(inx.NA, test.y, yi.train, vi.train, 
                           tau2)
    }
    pred[inx.test, 1:nsplt] <- test.y
  }
  y <- model.response(mf)
  if (!is.null(dim(pred))) {
    x.error <- apply(pred, 2, function(x) sum((y - x)^2)/sum((y - 
                                                                mean(y))^2))
    sdx.error <- apply(pred, 2, function(x) sqrt(sum(((y - 
                                                         x)^2 - mean((y - x)^2))^2))/sum((y - mean(y))^2))
  }
  else {
    x.error <- sum((y - pred)^2)/sum((y - mean(y))^2)
    sdx.error <- sqrt(sum(((y - pred)^2 - mean((y - pred)^2))^2))/sum((y - 
                                                                         mean(y))^2)
  }
  cbind(x.error, sdx.error)
}


rmert_custom <- function (formula, data, vi, c.pruning = 0, maxL = 5, minsplit = 6, 
          cp = 1e-05, minbucket = 3, xval = 10, lookahead = TRUE, 
          sss = TRUE, alpha.endcut = 0.02, a = 50, multi.start = TRUE, 
          n.starts = 3, perm = 25, seed = NULL, ...) 
{
  Call <- match.call()
  indx <- match(c("formula", "data", "vi"), names(Call), nomatch = 0L)
  if (indx[1] == 0L) 
    stop("a 'formula' argument is required")
  if (indx[3] == 0L) 
    stop("The sampling variances need to be specified")
  if (!is.logical(lookahead)) 
    stop("The 'lookahead' argument needs to be a logical value")
  if (maxL < 2 & (lookahead == TRUE)) 
    stop("The 'maxL' should be at least 2 when applying look-ahead strategy")
  temp <- Call[c(1L, indx)]
  temp[[1L]] <- quote(stats::model.frame)
  mf <- eval.parent(temp)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  cv.res <- Xvalid_all_custom(mf, maxL = maxL, n.fold = xval, minbucket = minbucket, 
                       minsplit = minsplit, cp = cp, lookahead = lookahead, 
                       sss = sss, alpha.endcut = alpha.endcut, a = a, multi.start = multi.start, 
                       n.starts = n.starts, fold_rowids = dat$fold_rownums)
  mindex <- which.min(cv.res[, 1])
  cp.minse <- cv.res[mindex, 1] + c.pruning * cv.res[mindex, 
                                                     2]
  cp.row <- min(which(cv.res[, 1] <= cp.minse))
  rownames(cv.res) <- paste0(0:maxL, " splits")
  if (cp.row == 1) {
    warning("no moderator effect was detected")
    y <- model.response(mf)
    vi <- c(t(mf["(vi)"]))
    wts <- 1/vi
    wy <- wts * y
    wy2 <- wts * y^2
    n <- length(y)
    Q <- sum(wy2) - (sum(wy))^2/sum(wts)
    df <- nrow(mf) - 1
    C <- sum(wts) - sum(wts^2)/sum(wts)
    tau2 <- max(0, (Q - df)/C)
    vi.star <- vi + tau2
    g <- sum(y/vi.star)/sum(1/vi.star)
    pval.Q <- pchisq(Q, df, lower.tail = FALSE)
    se <- 1/sqrt(sum(1/vi.star))
    zval <- g/se
    pval <- pnorm(abs(zval), lower.tail = FALSE) * 2
    ci.lb <- g - qnorm(0.975) * se
    ci.ub <- g + qnorm(0.975) * se
    res <- REmrt_GS_(mf, maxL = maxL, minsplit = minsplit, 
                     cp = cp, minbucket = minbucket, lookahead = lookahead)
    init.tree <- res$tree
    init.tree <- as.list(init.tree)
    init.tree$initial <- n
    init.tree$tree <- as.data.frame(res$tree)
    init.tree$call <- Call
    init.tree$pruned <- FALSE
    class(init.tree) <- "REmrt"
    res.f <- list(n = n, Q = Q, df = df, pval.Q = pval.Q, 
                  tau2 = tau2, g = g, se = se, zval = zval, pval = pval, 
                  ci.lb = ci.lb, ci.ub = ci.ub, call = Call, data = mf, 
                  cptable = cv.res, initial.tree = init.tree, formula = formula)
  }
  else {
    y <- model.response(mf)
    vi <- c(t(mf["(vi)"]))
    if (sss == FALSE) {
      res <- REmrt_GS_(mf, maxL = maxL, minsplit = minsplit, 
                       cp = cp, minbucket = minbucket, lookahead = lookahead)
    }
    else {
      res <- REmrt_SSS(mf, maxL = maxL, minbucket = minbucket, 
                       minsplit = minsplit, cp = cp, lookahead = lookahead, 
                       alpha.endcut = alpha.endcut, a = a, multi.start = multi.start, 
                       n.starts = n.starts)
    }
    prunedTree <- res$tree[1:cp.row, ]
    tau2 <- res$tree$tau2[cp.row]
    vi.star <- vi + tau2
    subnodes <- res$node.split[[cp.row]]
    wy.star <- y/vi.star
    n <- tapply(y, subnodes, length)
    g <- tapply(wy.star, subnodes, sum)/tapply(1/vi.star, 
                                               subnodes, sum)
    df <- cp.row - 1
    Qb <- res$tree$Qb[cp.row]
    pval.Qb <- pchisq(Qb, df, lower.tail = FALSE)
    se <- tapply(vi.star, subnodes, function(x) sqrt(1/sum(1/x)))
    zval <- g/se
    pval <- pnorm(abs(zval), lower.tail = FALSE) * 2
    ci.lb <- g - qnorm(0.975) * se
    ci.ub <- g + qnorm(0.975) * se
    mod.names <- unique(prunedTree$mod[!is.na(prunedTree$mod)])
    mf$term.node <- subnodes
    init.tree <- res$tree
    init.tree <- as.list(init.tree)
    init.tree$initial <- tapply(y, res$node.split[[NCOL(res$node.split)]], 
                                length)
    init.tree$tree <- as.data.frame(res$tree)
    init.tree$call <- Call
    init.tree$pruned <- FALSE
    class(init.tree) <- "REmrt"
    res.f <- list(tree = prunedTree, n = n, moderators = mod.names, 
                  Qb = Qb, tau2 = tau2, df = df, pval.Qb = pval.Qb, 
                  g = g, se = se, zval = zval, pval = pval, ci.lb = ci.lb, 
                  ci.ub = ci.ub, call = Call, cptable = cv.res, cpt = res$cpt, 
                  data = mf, initial.tree = init.tree, formula = formula)
    if (!is.null(perm)) {
      if (!is.numeric(perm) | length(perm) > 1) 
        stop("perm needs to be a possitive integer")
      Qps <- permuteRE(mf, nsplit = cp.row - 1, P = perm, 
                       sss = sss, lookahead = lookahead, minbucket = minbucket, 
                       minsplit = minsplit, cp = cp, alpha.endcut = alpha.endcut, 
                       a = a, multi.start = multi.start, n.starts = n.starts)
      pval.perm <- (sum(Qps >= Qb) + 1)/(perm + 1)
      res.f$pval.perm <- pval.perm
    }
  }
  class(res.f) <- "REmrt"
  res.f
}
