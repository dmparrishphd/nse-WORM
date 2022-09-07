function ( mod, obs ) {
    stopifnot (
        is.numeric ( mod ) ,
        is.numeric ( obs ) ,
        length ( mod ) == length ( obs ) )
    if  (
            length ( mod ) < 1
        ||  any ( is.na ( mod ) )
        ||  any ( is.na ( obs ) ) ) return ( NA_real_ )
    DIFF <- mod - obs
    SSE  <- sum ( DIFF * DIFF )
    MEAN <- mean ( obs )
    DEV  <- sum ( ( obs - MEAN ) ^ 2 )
    1 - SSE / DEV }
