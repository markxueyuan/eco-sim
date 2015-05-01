(ns eco-sim.ricardo-price-model)

(defn mach-cost
  [input r t]
  (* (Math/pow (+ 1 r) t)
     input))

(defn mach-costs
  [value term r]
  (let [terms (range 1 term)
        input (/ value (- term 1))]
    (->> terms
         (map (partial mach-cost input r))
         (reduce +))))

(defn invest [labor machinery term]
  (fn [r w]
    (let [lc (* labor w)
          mc (mach-costs (* machinery w) term r)
          pc (* r (+ lc mc))]
      (+ lc mc pc))))


((invest 100 300 5) 0.5 1)

((invest 300 100 3) 0.5 1)

((invest 100 300 5) 0.1 2)

((invest 300 100 3) 0.1 2)

