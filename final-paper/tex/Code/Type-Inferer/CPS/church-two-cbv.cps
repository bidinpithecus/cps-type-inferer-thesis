let v0(f, k0) =
    let v1(x, k1) =
        let k2(v2) =
            let k3(v3) =
                v2(v3, k1)
            in
                let k4(v4) =
                    let k5(v5) =
                        v4(v5, k3)
                    in
                        k5(x)
                in
                    k4(f)
        in
            k2(f)
    in
        k0(v1)
in
    k(v0)
