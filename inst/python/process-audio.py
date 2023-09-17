
import pandas as pd
import scipy
import numpy as np



d = {'col1': [0, 1, 2, 3], 'col2': pd.Series([2, 3], index=[2, 3])}

df2 = pd.DataFrame(np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
                   columns=['a', 'b', 'c'])
