'\$' cabal run
Input file path:
> input/church-zero.in
Expression:
'$\lambda$'f. '$\lambda$'x. x
Type:
'$\alpha \to \beta \to \beta$'
Call-by-Name Translation:
Command:
let v0(f, k0) =
  let v1(x, k1) =
    x(k1)
  in
    k0(v1)
in
  k(v0)
Expected Continuation Type:
'$\forall\alpha,\beta.\ \neg\neg(\neg\neg\alpha,\ \neg\neg(\neg\neg\beta,\ \neg\beta))$'
Inferred Continuation Type:
'$\forall\alpha,\beta.\ \neg\neg(\alpha,\ \neg\neg(\neg\beta,\ \beta))$'
Do the types match?
Yes

Call-by-Value Translation:
Command:
let v0(f, k0) =
  let v1(x, k1) =
    k1(x)
  in
    k0(v1)
in
  k(v0)
Expected Continuation Type:
'$\forall\alpha,\beta.\ \neg\neg(\alpha,\ \neg\neg(\beta,\ \neg\beta))$'
Inferred Continuation Type:
'$\forall\alpha,\beta.\ \neg\neg(\alpha,\ \neg\neg(\beta,\ \neg\beta))$'
Do the types match?
Yes

CPS expression saved in output/church-zero.hs