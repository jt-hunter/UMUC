


df <- read.csv(file="/Users/jean_taylor_1/Documents/Stuff/Python/Solar weather scrape/df_final_7-8.csv", head=TRUE, sep=",")


#### 1

ggplot(df, aes(x = newdirect, y = Corrected.Upscaled.Measurement..MW., color = Temp..F.)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_gradient(low="blue", high="red") +
  scale_x_continuous(name = 'Direct Irradiance') +
  scale_y_continuous(name = 'Electrical Generaation')

##### 2 

ggplot(df, aes(x = humidity, y = Corrected.Upscaled.Measurement..MW., color = pressurre)) +
  geom_point() +
  scale_color_gradient(name = 'Pressure', low="black", high="white") +
  scale_x_continuous(name = 'Humidity') +
  scale_y_continuous(name = 'Electrical Generaation')

##### 3

ggplot(df, aes(x = Temp..F., y = Corrected.Upscaled.Measurement..MW., color = perc_direct)) +
  geom_point() +
  scale_color_gradient(name = 'Percent Direct Irradiance', low="red", high="green") +
  scale_x_continuous(name = 'Temperature') +
  scale_y_continuous(name = 'Electrical Generaation')

### 4

ggplot(df, aes(x = perc_direct, y = Corrected.Upscaled.Measurement..MW., color = Temp..F.)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orangered1") +
  scale_color_gradient(name = 'Temperature', low="black", high="cyan1") +
  scale_x_continuous(name = 'Percent Direct') +
  scale_y_continuous(name = 'Electrical Generaation')


