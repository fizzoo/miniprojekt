#!/usr/bin/python
# -*- coding: utf-8 -*-

from pylab import *


def DFT(x):
    """Returnerar signal x: tidsdomän -> frekvensdomän.
    In, ut är komplext. |x| bins.
    """
    N = len(x)
    X = zeros(N) + 0j
    for k in range(N):
        for n in range(N):
            X[k] += x[n]*(e**(-1j*2*pi*k*n/N))
    return X

def DFTI(x):
    """Returnerar signal x: frekvensdomän -> tidsdomän.
    In, ut är komplext. |x| bins.
    """
    N = len(x)
    X = zeros(N) + 0j
    for k in range(N):
        for n in range(N):
            X[k] += x[n]*(e**(1j*2*pi*k*n/N))
        X[k] /= N
    return X

t = arange(0.0, 2.0, 0.01)
s = sin(2*pi*t*90 + 2*pi*t*40)
subplot(2, 2, 1)
plot(t, s)

d = DFT(s)

subplot(2, 2, 3)
plot(t, real(d))
subplot(2, 2, 4)
plot(t, imag(d))

subplot(2, 2, 2)
plot(t, abs(d))
show()
