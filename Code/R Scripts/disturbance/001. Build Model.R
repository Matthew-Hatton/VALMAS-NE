## Following procedure conceptualised by Dr. Douglas Spiers, build a linear model to predict the seabed disturbance rates

rm(list = ls()) # reset
library(MiMeMo.tools) # everything we need

prop_disturbed <- read.csv("./Objects/physics/Prop disturbed.csv") %>%  # proportional disturbance rates for different regional implementations of StrathE2E for mud, sand, and gravel sediment types in the inshore and offshore regions
 filter(!region == "CS" | !shore == "in" | !sediment == "mud") # remove outlier

mod0 <- lm(formula = log(dist) ~ depth + sediment + shore, data = prop_disturbed) # build model
summary(mod0)

mod <- step(mod0) # suggests we can remove sediment

mod <- lm(formula = log(dist) ~ depth + shore,data = prop_disturbed)

newdata <- data.frame(depth = 200,
                      shore = "off")
predict(mod,newdata)

saveRDS(mod,"./Objects/physics/Disturbance Model.rds")
