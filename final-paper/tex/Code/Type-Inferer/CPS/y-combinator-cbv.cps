let v0(f, k0) =
    let k1(v1) =
        let k2(v2) =
            v1(v2, k0)
        in
            let v8(x, k8) =
                let k9(v9) =
                    let k10(v10) =
                        v9(v10, k8)
                    in
                        let k11(v11) =
                            let k12(v12) =
                                v11(v12, k10)
                            in
                                k12(x)
                        in
                            k11(x)
                in
                    k9(f)
            in
                k2(v8)
    in
        let v3(x, k3) =
            let k4(v4) =
                let k5(v5) =
                    v4(v5, k3)
                in
                    let k6(v6) =
                        let k7(v7) =
                            v6(v7, k5)
                        in
                            k7(x)
                    in
                        k6(x)
            in
                k4(f)
        in
            k1(v3)
in
    k(v0)