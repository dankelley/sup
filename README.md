# sup

`sup` is an R package for simple uncertainty propagation, designed mainly as a
teaching supplement.  For detailed work, the `errors` package (Ucar et al,
2018) is superior.  The two packages are contrasted below; note that `errors`
uses the parenthesis notation, whereas `sup` uses the +- notation.

*sup example*

```R
> library(sup)
> as.sup(5, .01) / as.sup(1, .01)
5 +- 0.0509902
```

*errors example*

```R
> library(errors)
> x <- 5
> y <- 1
> errors(x) <- 0.01
> errors(y) <- 0.01
> x
5.00(1)
> y
1.00(1)
> x / y
5.00(5)
```

**References**

* Ku, Harry.
“Notes on the Use of Propagation of Error Formulas.”
Journal of Research of the National Bureau of Standards-C. Engineering and Instrumentation 70C, no. 4 (December 1966): 263–73.
\url{https://nvlpubs.nist.gov/nistpubs/jres/70C/jresv70Cn4p263_A1b.pdf}

* Taylor, Barry N., and Chris E. Kuyatt.
“Guidelines for Evaluating and Expressing the Uncertainty of NIST Measurement Results.”
NIST Technical Note. Gaithersburg, MD, USA:
U.S. Department of Commerce Technology Administration: National Institute of Standards and Technology, 1994.
\url{https://www.nist.gov/pml/nist-technical-note-1297}

* Ucar, Iñaki, Edzer Pebesma, and Arturo Azcorra.
“Measurement Errors in R.” The R Journal 10, no. 2 (2018): 549–57.
\url{https://doi.org/10.32614/RJ-2018-075}

* Wikipedia.
“Propagation of Uncertainty.”
In Wikipedia, November 26, 2019.
\url{https://en.wikipedia.org/w/index.php?title=Propagation_of_ uncertainty&oldid=928003036}


