# Goal:
# Using SciPy from Julia

ENV["JULIA_CONDAPKG_BACKEND"] = "Null"
ENV["JULIA_PYTHONCALL_EXE"] = expanduser("~/PyVenv/bin/python3")

import PythonCall

scipy=PythonCall.pyimport("scipy")

println(scipy.__version__)

m_binom_obj=scipy.stats.binom(10, 0.5)

println(m_binom_obj.mean())
