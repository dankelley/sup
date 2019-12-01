# sup

`sup` is an R package for simple uncertainty propagation, designed mainly as a
teaching supplement.  For detailed work, the `errors` package (Ucar et al,
2018) is superior.  The two packages are contrasted below; note that `errors`
uses the parenthesis notation, whereas `sup` uses the +- notation.

*sup example*

```R
> library(sup)
> c(5,.01) %/% c(1, .01)
5 +- 0.0509902
```

*errors example*

```R
> library(errors)
> x<-5
> y<-1
> errors(x)<-0.01
> errors(y)<-0.01
> x
5.00(1)
> y
1.00(1)
> x/y
5.00(5)
```

**References**

* Ucar, Iñaki, Edzer Pebesma, and Arturo Azcorra. “Measurement Errors in R.”
  The R Journal 10, no. 2 (2018): 549–57. https://doi.org/10.32614/RJ-2018-075

