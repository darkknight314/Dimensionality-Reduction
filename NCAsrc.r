nca <- function( x
                 , labels
                 , A_init = diag(ncol(x))
                 , N_iter=2*1e2
                 , learning_rate = 0.01
){
  x <- as.matrix(x)
  #labels <- as.factor(labels)
  
  A <- A_init
  
  N <- nrow(x)
  stopifnot(NROW(x) == length(labels))
  
  p <- numeric(N)
  p_cum <- numeric(N_iter)
  for (it in seq_len(N_iter)){
    for (i in seq_len(N)){
      # softmax, with LOO
      D <- tcrossprod(A, x)       # (dA, N)
      D <- (D - as.numeric(D[,i]))
      p_ik <- exp(-colSums(D*D))       # (N)
      
      p_ik[i] <- 0
      softmax <- sum(p_ik)
      if (softmax > .Machine$double.eps){
        p_ik <- p_ik/sum(p_ik)             # (N)
      }
      # end softmax
      
      # neighbors that predict the correct label
      correct_label <- labels == labels[i]  # (N)
      
      p[i] <- sum(p_ik[correct_label])
      d    <- t(t(x) - as.numeric(x[i,]))  # (N, dx)
      pd <- p_ik * d                    # (N, dx)
      
      g <- (p[i]*crossprod(d, pd)) - crossprod(d[correct_label,], pd[correct_label,]) # (dx, dx)
      A <- A + learning_rate * (A %*% g) # (dx, dA)
      # d  <- t(x) - as.numeric(x[i,])  # (dx, N)
      # d2 <- p_ik * colSums(d * d) # (N)
      #
      # A <- A + learning_rate * A * (p[i]*sum(d2) - sum(d2[correct_label]))
    }
    p_cum[it] <- sum(p)
  }
  
  list( A = A
        , p = p
        , A_norm = A/A[1,1]
        , p_cum=p_cum
  )
}
