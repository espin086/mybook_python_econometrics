"""
Produces exploratory analysis of a dataframe

"""

import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


class EDA():
    """
    Class that runs exploratory analysis

    """

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
        sns_plot.savefig("dist_plot_{0}.png".format(var))
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
        sns_plot.savefig("scatter_matrix_plot.png".format())
        plt.close()

    def _box_plot(self, var_x):
        """
        creates a boxplot and saves output

        """
        plt.figure()
        sns_plot = sns.boxplot(x=var_x, y=self.y, data=self.df).get_figure()
        sns_plot.savefig("box_plot_{0}_{1}.png".format(var_x, self.y_string))
        plt.close()

    def _scatter_plot(self, var_x):
        """
        Creates scatterplot and saves output

        """
        sns_plot = sns.lmplot(x=var_x, y=self.y_string, data=self.df)
        sns_plot.savefig(
            "correlation_{0}_{1}.png".format(var_x, self.y_string))

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
    def rename_cols_and_save(xls_name):
        df = pd.read_excel("../data/{0}.xls".format(xls_name), index_col=None, header=None)
        if xls_name == 'hprice1':
            names_dict = {0:'price',
                        1:'assess',
                        2:'bdrms',
                        3:'lotsize',
                        4:'sqrft',
                        5:'colonial',
                        6:'lprice',
                        7:'lassess',
                        8:'llotsize',
                        9:'lsqrft',
                        }
        elif xls_name == 'saving':
            names_dict = {0:'sav',
                        1:'inc',
                        2:'size',
                        3:'edu',
                        4:'age',
                        5:'black',
                        6:'cons',
                        }

        df.rename(columns = names_dict, inplace = True)
        df.to_csv("../data/{0}.csv".format(xls_name), index=False)
        return df

    df = rename_cols_and_save(xls_name='hprice1')

    df.colonial = df.colonial.astype('category')


    eda = EDA(df=df, y='price')
    eda.run()
