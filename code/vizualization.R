## data vizualization
# set-up =======================================================================

library(ggplot2)
library(readr)

setosa_subset= read_csv('./output/setosa_subset_data.csv')
new_data = read_csv('./output/new_data_for_prediction.csv')

# make and save a simple plot
simple_fig = ggplot(data = setosa_subset) +
  geom_point(aes(x = Sepal.Length, y = Petal.Width)) +
  labs(x = "sepal length", y = "petal width") +
  theme_bw()
ggsave('./figs/simple_iris_fig.png', simple_fig)


# plot prediction
prediction_plot = ggplot(data = new_data) +
  geom_line(aes(x = Petal.Length, y = prediction),
            size = 2, colour = 'red') +
  labs(x = "petal length", y = "sepal length prediction") +
  theme_bw()
ggsave('./figs/prediction_plot_simple.png', prediction_plot)
