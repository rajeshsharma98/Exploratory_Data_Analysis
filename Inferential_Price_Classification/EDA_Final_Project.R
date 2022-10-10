#loading all necessary libraries
library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(measurements)
library(arm)
library(nnet)
library(MASS)

#loading dataset
mobile_data = read.csv("/Users/ashish/Downloads/train.csv")

#Data pre-processing(removing undesired columns and merging relative columns)
mobile_data$sc_length = sqrt(mobile_data$sc_h^2 + mobile_data$sc_w^2)
mobile_data$performance = mobile_data$clock_speed * mobile_data$n_cores
mobile_data$px_res = mobile_data$px_height * mobile_data$px_width

undesired <- c('three_g', 'm_dep', 'sc_h', 'sc_w', 'px_height', 'px_width', 
               'talk_time', 'fc', 'blue', 'mobile_wt', 'wifi', 'clock_speed', 'n_cores')

mobile_data <- mobile_data %>% dplyr :: select(-one_of(undesired))

mobile_data <- mobile_data %>% relocate(price_range, .after = last_col())

#factoring price range in terms of ordinal variables 
mobile_data$price_range <- recode_factor(mobile_data$price_range, "0" = "low", "1" = "medium", "2" = "high", "3" = "very high")

#splitting data into 80:20 train and test data for model accuracy purpose
train_data = mobile_data[1:1600,]
test_data = mobile_data[1601:2000,]
true_class = test_data[,ncol(test_data)]
test_data = mobile_data[1601:2000, -(ncol(test_data))]

#pairs plot to find linear correlation between all the predictors
#co-orelation and pairs plot of continuous variables
continuous <- c('battery_power', 'int_memory', 'pc', 'ram', 'sc_length', 'performance', 'px_res')
ggpairs(train_data[, continuous]) + 
  labs(title = "Pairs plot of all the continous variables")


#bar plots of categorical columns
categorical <- c('dual_sim', 'four_g', 'touch_screen')

ggplot() + geom_bar(aes(x = price_range, fill = price_range), data = train_data) + facet_wrap(~ train_data$dual_sim) + 
  theme(axis.text.x=element_text(angle=45, hjust=0.9)) + 
  labs(x = "Price Range",
       y = "Count",
       title = "Bar plot of count of mobile phones \n with dual sim or not \n (Categorized on basis of price range)",
  )
ggplot() + geom_bar(aes(x = price_range, fill = price_range), data = train_data) + facet_wrap(~ train_data$four_g) + 
  theme(axis.text.x=element_text(angle=45, hjust=0.9)) + 
  labs(x = "Price Range",
       y = "Count",
       title = "Bar plot of count of mobile phones \n with four_g or not \n (Categorized on basis of price range)",
  )
ggplot() + geom_bar(aes(x = price_range, fill = price_range), data = train_data) + facet_wrap(~ train_data$touch_screen) + 
  theme(axis.text.x=element_text(angle=45, hjust=0.9)) + 
  labs(x = "Price Range",
       y = "Count",
       title = "Bar plot of count of mobile phones \n with touch screen or not \n (Categorized on basis of price range)",
  )

#mean_value of each continuous predictor variable for all price-ranges
mean_ram = aggregate(train_data$ram, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_ram)  + geom_point() + 
  labs(x = "mean of ram",
       y = "price-range",
       title = "Scatter plot for mean value of ram for each price range",
  )

mean_battery = aggregate(train_data$battery_power, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_battery)  + geom_point() + 
  labs(x = "mean of battery-power",
       y = "price-range",
       title = "Scatter plot for mean value of battery-power for each price range",
  )

mean_sc = aggregate(train_data$sc_length, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_sc)  + geom_point() + 
  labs(x = "mean of screen-lenght",
       y = "price-range",
       title = "Scatter plot for mean value of screen-length for each price range",
  )

mean_mem = aggregate(train_data$int_memory, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_mem)  + geom_point() + 
  labs(x = "mean of internal-memory",
       y = "price-range",
       title = "Scatter plot for mean value of internal-memory for each price range",
  )

mean_pc = aggregate(train_data$pc, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_pc)  + geom_point() + 
  labs(x = "mean of primary camera",
       y = "price-range",
       title = "Scatter plot for mean value of primary camera for each price range",
  )

mean_px = aggregate(train_data$px_res, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_px)  + geom_point() + 
  labs(x = "mean of pixel resolution",
       y = "price-range",
       title = "Scatter plot for mean value of pixel-resolution for each price range",
  )

mean_per = aggregate(train_data$performance, by = list(train_data$price_range), FUN = mean)
ggplot(aes(x = Group.1, y = x), data = mean_per)  + geom_point() + 
  labs(x = "mean of performance",
       y = "price-range",
       title = "Scatter plot for mean value of performance for each price range",
  )


#bivariate graphs
#with ram
ggplot(aes(x = battery_power, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "battery-power",
       y = "RAM",
       title = "Scatter plot of ram vs battery-power \n (Categorized on basis of price range)",
  )
ggplot(aes(x = int_memory, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess")  +
  labs(x = "internal-memory",
       y = "RAM",
       title = "Scatter plot of ram vs internal-memory \n (Categorized on basis of price range)",
  )
ggplot(aes(x = pc, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "primary-camera",
       y = "RAM",
       title = "Scatter plot of ram vs priamry-camera \n (Categorized on basis of price range)",
  )
ggplot(aes(x = sc_length, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "screen-length",
       y = "RAM",
       title = "Scatter plot of ram vs screen-length \n (Categorized on basis of price range)",
  )
ggplot(aes(x = performance, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "Performance",
       y = "RAM",
       title = "Scatter plot of ram vs Performance \n (Categorized on basis of price range)",
  )
ggplot(aes(x = px_res, y = ram, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "pixel-resolution",
       y = "RAM",
       title = "Scatter plot of ram vs pixel-resolution \n (Categorized on basis of price range)",
  )

#with battery power
ggplot(aes(x = int_memory, y = battery_power, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "internal-memory",
       y = "battery-power",
       title = "Scatter plot of battery-power vs internal-memory \n (Categorized on basis of price range)",
  )
ggplot(aes(x = pc, y = battery_power, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "primary-camera",
       y = "battery-power",
       title = "Scatter plot of battery-power vs primary-camera \n (Categorized on basis of price range)",
  )
ggplot(aes(x = sc_length, y = battery_power, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "screen-length",
       y = "battery-power",
       title = "Scatter plot of battery-power vs screen-length \n (Categorized on basis of price range)",
  )
ggplot(aes(x = performance, y = battery_power, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "performance",
       y = "battery-power",
       title = "Scatter plot of battery-power vs performance \n (Categorized on basis of price range)",
  )
ggplot(aes(x = px_res, y = battery_power, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "pixel-resolution",
       y = "battery-power",
       title = "Scatter plot of battery-power vs pixel-resolution \n (Categorized on basis of price range)",
  )

#with px_res
ggplot(aes(x = int_memory, y = px_res, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "internal-memory",
       y = "pixel-resoltuion",
       title = "Scatter plot of pixel-resolution vs internal-memory \n (Categorized on basis of price range)",
  )
ggplot(aes(x = pc, y = px_res, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "primary-camera",
       y = "pixel-resoltuion",
       title = "Scatter plot of pixel-resolution vs primary-camera \n (Categorized on basis of price range)",
  )
ggplot(aes(x = sc_length, y = px_res, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "screen-length",
       y = "pixel-resoltuion",
       title = "Scatter plot of pixel-resolution vs screen-length \n (Categorized on basis of price range)",
  )
ggplot(aes(x = performance, y = px_res, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "performance",
       y = "pixel-resoltuion",
       title = "Scatter plot of pixel-resolution vs performance \n (Categorized on basis of price range)",
  )

#with int_memory
ggplot(aes(x = pc, y = int_memory, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "primary-camera",
       y = "internal-memory",
       title = "Scatter plot of internal-memory vs primary-camera \n (Categorized on basis of price range)",
  )
ggplot(aes(x = sc_length, y = int_memory, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "screen-length",
       y = "internal-memory",
       title = "Scatter plot of internal-memory vs screen-length \n (Categorized on basis of price range)",
  )
ggplot(aes(x = performance, y = int_memory, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "performance",
       y = "internal-memory",
       title = "Scatter plot of internal-memory vs performance \n (Categorized on basis of price range)",
  )

#with sc_length
ggplot(aes(x = pc, y = sc_length, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "primary-camera",
       y = "screen-length",
       title = "Scatter plot of screen-length vs primary-camera \n (Categorized on basis of price range)",
  )
ggplot(aes(x = performance, y = sc_length, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "performance",
       y = "screen-length",
       title = "Scatter plot of screen-length vs performance \n (Categorized on basis of price range)",
  )

#with primary camera
ggplot(aes(x = performance, y = pc, color = price_range), data = train_data) + geom_point()  + geom_smooth(method = "loess") + 
  labs(x = "performance",
       y = "primary-camera",
       title = "Scatter plot of primary-camera vs performance \n (Categorized on basis of price range)",
  )

#model fitting with various predictors
#ram as predictor
test1 = multinom(price_range ~ ram, data = train_data)
predict_class1_train = predict(test1, train_data)
predict_class1_test = predict(test1, test_data)
acc1_train = 100*sum(predict_class1_train == train_data$price_range)/nrow(train_data)
acc1_test = 100*sum(predict_class1_test == true_class)/nrow(test_data)

#battery power as predictor
test1_bp = multinom(price_range ~ battery_power, data = train_data)
predict_class1_train = predict(test1_bp, train_data)
predict_class1_test = predict(test1_bp, test_data)
acc1_train_bp = 100*sum(predict_class1_train == train_data$price_range)/nrow(train_data)
acc1_test_bp = 100*sum(predict_class1_test == true_class)/nrow(test_data)

#pixel resolution as predictor
test1_pr = multinom(price_range ~ px_res, data = train_data)
predict_class1_train = predict(test1_pr, train_data)
predict_class1_test = predict(test1_pr, test_data)
acc1_train_pr = 100*sum(predict_class1_train == train_data$price_range)/nrow(train_data)
acc1_test_pr = 100*sum(predict_class1_test == true_class)/nrow(test_data)

#ram and battery power as predictor
test2_rb = multinom(price_range ~ ram + battery_power, data = train_data)
predict_class2_train = predict(test2_rb, train_data)
predict_class2_test = predict(test2_rb, test_data)
acc2_train_rb = 100*sum(predict_class2_train == train_data$price_range)/nrow(train_data)
acc2_test_rb = 100*sum(predict_class2_test == true_class)/nrow(test_data)

#ram and pixel resolution as predictor
test2_rp = multinom(price_range ~ ram + px_res, data = train_data)
predict_class2_train = predict(test2_rp, train_data)
predict_class2_test = predict(test2_rp, test_data)
acc2_train_rp = 100*sum(predict_class2_train == train_data$price_range)/nrow(train_data)
acc2_test_rp = 100*sum(predict_class2_test == true_class)/nrow(test_data)

#ram, battery power, and pixel resolution as predictor
test3 = multinom(price_range ~ ram + battery_power + px_res, data = train_data)
predict_class3_train = predict(test3, train_data)
predict_class3_test = predict(test3, test_data)
acc3_train = 100*sum(predict_class3_train == train_data$price_range)/nrow(train_data)
acc3_test = 100*sum(predict_class3_test == true_class)/nrow(test_data)

#all continuous variables as predictor
test4 = multinom(price_range ~ ram + battery_power + px_res + performance + pc + int_memory + sc_length, data = train_data)
predict_class4_train = predict(test4, train_data)
predict_class4_test = predict(test4, test_data)
acc4_train = 100*sum(predict_class4_train == train_data$price_range)/nrow(train_data)
acc4_test = 100*sum(predict_class4_test == true_class)/nrow(test_data) 

