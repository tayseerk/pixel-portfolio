(lang dune 3.16)

(name pixel_portfolio)

(generate_opam_files true)


(authors "Byron S. Mitchell Rivera" "Tayseer Karrossi")
(maintainers "tkarros1@jh.edu" "bmitch41@jh.edu")

(package
 (name pixel_portfolio)
 (synopsis "Pixel Portfolio stochastic market simulator")
 (description
  "A functional stock-simulation engine featuring GBM and OU price dynamics,
   CLI commands, and persistence hooks.")
 (depends
  (core (>= 0.17.1))
  (cmdliner (>= 1.2.0))
  (ppx_jane (>= 0.17.0))
  (sexplib (>= 0.17.0))
  (ounit2 (>= 2.2.7))
  (qcheck (>= 0.21))
  (bisect_ppx (>= 2.8.3))
  (core_bench (>= 0.17.0))))