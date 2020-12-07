library(dplyr)
library(isotree)
library(ggplot2)
### Bimodal Ghosting Example
col1 = rnorm(500, mean = 5, sd = 1)
col2 = rnorm(500, mean = 25, sd = 1)

testdata = data.frame(cbind(c(col1, col2), c(col2, col1)))

iso_demo = isolation.forest(
  testdata, 
  ndim=1,
  ntrees=100,
  max_depth = 4,
  prob_pick_pooled_gain=0,
  prob_pick_avg_gain=0,
  output_score = FALSE)

Z1 <- predict(iso_demo, testdata)
testdata$scores <- Z1

p1 = ggplot(testdata, aes(x=X1, y=X2, color = scores))+
  theme_bw()+
  theme(legend.position = "none")+
  geom_point()+
  coord_cartesian(xlim=c(0,35), ylim = c(0,35))+
  labs(title = "Training Sample", x="Feature 1", y="Feature 2")

col1 = seq(0, 35,.1)

test_2 = expand.grid('X1' = col1, 'X2' = col1)

Z1 <- predict(iso_demo, test_2)
test_2$scores <- Z1

p2 = ggplot(test_2, aes(x=X1, y=X2, z = scores))+
  theme_bw()+
  theme(legend.position = "none")+
  geom_contour_filled(bins=20)+
  labs(title="Ghosting Effect, ndim = 1", x="Feature 1", y="Feature 2")

### Fixing With Extended
col1 = rnorm(500, mean = 5, sd = 1)
col2 = rnorm(500, mean = 25, sd = 1)

testdata = data.frame(cbind(c(col1, col2), c(col2, col1)))

iso_demo = isolation.forest(
  testdata, 
  ndim=2,
  ntrees=100,
  max_depth = 4,
  prob_pick_pooled_gain=0,
  prob_pick_avg_gain=0,
  output_score = FALSE)

Z1 <- predict(iso_demo, testdata)
testdata$scores <- Z1
col1 = seq(0, 35,.1)
test_2 = expand.grid('X1' = col1, 'X2' = col1)

Z1 <- predict(iso_demo, test_2)
test_2$scores <- Z1

p3 = ggplot(test_2, aes(x=X1, y=X2, z = scores))+
  theme_bw()+
  theme(legend.position = "none")+
  geom_contour_filled(bins=20)+
  labs(title="Ghosting Effect, ndim = 2", x="Feature 1", y="Feature 2")
