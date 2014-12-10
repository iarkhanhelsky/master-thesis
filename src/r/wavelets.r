
wavelet.haar <- function(t, j = 0, k = 0) {
  haar_base <- function(t)
  {
    ifelse(t > 0 & t < 0.5,
           1,
           ifelse(t >= 0.5 & t < 1, -1, 0)
    );
  };
  
  haar_base(2**j * (t - k));
}