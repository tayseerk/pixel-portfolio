(lang dune 3.10)

(name pixel_portfolio)

(generate_opam_files true)

(package
 (name pixel_portfolio)
 (depends
  (ounit2 (>= 2.2.7))
  (core (>= 0.17.1))
  (owl (>= 1.1))
  (cmdliner (>= 1.2.0))
  (yojson (>= 2.1.0))
  (ppx_jane (>= 0.17.0))
  (sexplib (>= 0.17.0))
  (qcheck (>= 0.21))
  (bisect_ppx (>= 2.8.3))
  (core_bench (>= 0.17.0))))