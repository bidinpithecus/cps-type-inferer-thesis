let v0(f, k0) =
  let v1(x, k1) =
    x(k1)
  in
    k0(v1)
in
  k(v0)
