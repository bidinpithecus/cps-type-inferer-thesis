let v0(x, k0) =
    let k1(v1) =
        let k2(v2) =
            v1(v2, k0)
        in
            k2(x)
    in
        k1(x)
in
    k(v0)
