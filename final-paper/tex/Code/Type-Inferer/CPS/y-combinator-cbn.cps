let v0(f, k0) =
    let k1(v1) =
        let v2(k2) =
            let v8(x, k8) =
                let k9(v9) =
                    let v10(k10) =
                        let k11(v11) =
                            let v12(k12) =
                                x(k12)
                            in
                                v11(v12, k10)
                        in
                            x(k11)
                    in
                        v9(v10, k8)
                in
                    f(k9)
            in
                k2(v8)
        in
            v1(v2, k0)
    in
        let v3(x, k3) =
            let k4(v4) =
                let v5(k5) =
                    let k6(v6) =
                        let v7(k7) =
                            x(k7)
                        in
                            v6(v7, k5)
                    in
                        x(k6)
                in
                    v4(v5, k3)
            in
                f(k4)
        in
            k1(v3)
in
    k(v0)