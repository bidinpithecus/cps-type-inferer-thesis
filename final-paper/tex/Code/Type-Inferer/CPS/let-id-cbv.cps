let k0(id) =
  let k2(v1) =
    let k3(v2) =
      v1(v2, k)
    in
      k3(id)
  in
    k2(id)
in
  let v0(x, k1) =
    k1(x)
  in
    k0(v0)
