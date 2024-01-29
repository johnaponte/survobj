# Test multiple events
# By JJAV 20240129

test_that(
  "Renewal Produce multiple episodes hr",
  {
    sobj <- s_exponential(fail = 0.4, t= 1)
    i = 1
    n = 1000
    hr <- rep(1,n)
    v<- matrix(sobj$rsurvhr(hr), ncol = 1)
    while(min(v[,i])< 1){
      i = i+1
      v<- cbind(v,renewhr(sobj, hr, prevtime = v[,i-1] ))
      expect_true(all(v[,i]>v[,i-1]))
  }
   expect_gt(ncol(v),1)
 }
)


test_that(
  "Renewal Produce multiple episodes aft",
  {
    sobj <- s_exponential(fail = 0.4, t= 1)
    i = 1
    n = 1000
    aft <- rep(1,n)
    v<- matrix(sobj$rsurvhr(aft), ncol = 1)
    while(min(v[,i])< 1){
      i = i+1
      v<- cbind(v,renewaft(sobj, aft, prevtime = v[,i-1] ))
      expect_true(all(v[,i]>v[,i-1]))
    }
    expect_gt(ncol(v),1)
  }
)

test_that(
  "nhpp Produce multiple episodes hr",
  {
    sobj <- s_exponential(fail = 0.4, t= 1)
    i = 1
    n = 1000
    hr <- rep(1,n)
    v<- matrix(sobj$rsurvhr(hr), ncol = 1)
    while(min(v[,i])< 1){
      i = i+1
      v <- cbind(v,nhpphr(sobj, hr, v[,i-1]))
      expect_true(all(v[,i]>v[,i-1]))
      }
    expect_gt(ncol(v),1)
  }
)


test_that(
  "nhpp Produce multiple episodes aft",
  {
    sobj <- s_exponential(fail = 0.4, t= 1)
    i = 1
    n = 1000
    aft <- rep(1,n)
    v<- matrix(sobj$rsurvhr(aft), ncol = 1)
    while(min(v[,i])< 1){
      i = i+1
      v <- cbind(v,nhppaft(sobj, aft, v[,i-1]))
      expect_true(all(v[,i]>v[,i-1]))
      }
    expect_gt(ncol(v),1)
  }
)
