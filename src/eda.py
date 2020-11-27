"""
Produces exploratory analysis of a dataframe, saves output to new folder

"""

import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os


class EDA():
    """
    Class that runs exploratory analysis

    """

    if not os.path.exists('plots'):
        os.makedirs('plots')

    def __init__(self, df, y):
        """
        Initializes variables for the class
        """
        self.df = df
        self.y_string = y
        self.y = df[y]

    def inspect(self):
        """
        Quick inspection of the dataframe

        """
        inspect = {}
        inspect['head'] = self.df.head()
        inspect['describe'] = self.df.describe().T
        inspect['dtypes'] = self.df.dtypes
        return inspect

    def _dist_plot(self, df, var):
        """
        Creates a distribution plot and saves output

        """
        plt.figure()
        sns_plot = sns.distplot(df[var], color='b').get_figure()
        sns_plot.savefig("plots/dist_plot_{0}.png".format(var))
        return sns_plot

    def variation(self):
        """
        Creates distribution plots for all numerical values in a dataframe

        """
        numerical = self.df.select_dtypes(include=np.number)
        for col in numerical.columns:
            self._dist_plot(df=numerical, var=col)

    def _scatter_matrix(self):
        """
        Creates a scatterplot matrix and saves the output

        """
        numerical = self.df.select_dtypes(include=np.number)
        plt.figure()
        sns_plot = sns.pairplot(numerical)
        sns_plot.savefig("plots/scatter_matrix_plot.png".format())
        plt.close()

    def _box_plot(self, var_x):
        """
        creates a boxplot and saves output

        """
        plt.figure()
        sns_plot = sns.boxplot(x=var_x, y=self.y, data=self.df).get_figure()

        sns_plot.savefig("plots/box_plot_{0}_{1}.png".format(var_x, self.y_string))
        plt.close()

    def _scatter_plot(self, var_x):
        """
        Creates scatterplot and saves output

        """
        sns_plot = sns.lmplot(x=var_x, y=self.y_string, data=self.df)
        sns_plot.savefig(
            "plots/correlation_{0}_{1}.png".format(var_x, self.y_string))

    def covariation(self):
        """
        Runs the box and wiskers plots on categorical variables
        and scatterplot on all numerical variables

        """
        self._scatter_matrix()

        categorical = self.df.select_dtypes(include=['bool', 'category'])
        for col in categorical.columns:
            self._box_plot(var_x=col)

        numerical = self.df.select_dtypes(include=np.number)
        for col in numerical.columns:
            self._scatter_plot(var_x=col)

    def run(self):
        """
        The main function of the class
        """
        self.variation()
        self.covariation()


if __name__ == "__main__":

    df = pd.read_csv("udemy_development.csv", index_col=None, thousands=',')

    df['Course ID'] = df['Course ID'].astype('str')

    # #Run analysis for each of these categories in development and write a blog post about them
    # df = df.loc[df['Sub Category'].isin(['Data Science','Programming Languages', 'Web Development'])]

    #best development courses on Udemy
    df = df.loc[df['Sub Category'].isin(['Web Development'])]

    for item in ['Sub Category', 'Category']:
        df[item] = df[item].astype('category')


    eda = EDA(df=df, y='Avg Rating')
    eda.run()

    eda = EDA(df=df, y='Enrollment')
    eda.run()

    print(eda.inspect()['describe'])
