# pixel-portfolio
Repository for FPSE Final Project, Group Members: Tayseer Karrossi, Byron S. Mitchell Rivera

Overview of the project:

Pixel Portfolio is a command-line stock-simulation game that models a small universe of assets whose prices evolve through realistic stochastic processes, including Geometric Brownian Motion and Ornstein–Uhlenbeck mean reversion. The purpose of the project is to create a functional, fully deterministic market engine that supports market and stop loss orders, equity tracking, leveling, and persistent save/load functionality. We are intensely interested in the stock market, day trading, and the financial world, which motivated us to build a simulator that reflects real trading dynamics rather than a trivial game. This project allows us to combine financial modeling with OCaml’s functional abstractions while producing a reusable stochastic-simulation library as required in FPSE. It will mainly be used to test our knowledge of the stock market, as we would like to understand what actually drives it. Tayseer has minimal experience in the stock market during the COVID quarantine, and I would like to start trading stocks.  

Current libraries:
  - Sexplib
  - Bisect_ppx
  - Cmdliner 
  - ppx_jane
  - Core
  - OUnit2
  - QCheck

What works:
- GBM and OU model
- Engine: tick advances prices
- Portfolio: cash/position updates
- Persistence: sexp save and load, automatic backup when starting new game
- Cmdliner and its commands
- prompts when game is loaded and when playing

What is not working:
- buy option orders (high level, may not implement for this beginner stock sim game)

How to run
- dune build
- dune exec pixel_portfolio (then game starts everything is explained in the terminal/cmdliner)

running coverage:
BISECT_ENABLE=YES BISECT_FILE=$(pwd)/_build/default/bisect \
  dune runtest --instrument-with bisect_ppx --force

bisect-ppx-report html -o _coverage _build/default/bisect*.coverage

open _coverage/index.html

plan for completion (DONE):

  By November 16th - make sure .mli files are good and no more major design changes will occur (after TA feedback)

  By nov 19th - make sure .ml files are good and no major changes

  By nov 21st - portfolio updates and equity calculations

  By november 23rd - submitting a market order will produce an Order.execution and update the portfolio and cash

  Nov 26th - tick moves prices using simple noise and whichever model (GBM will be done first, then OU)

  Nov 28th - CLI command will simulate N amount og steps and show final prices and equity. 

  Nov 30th - Save an Engine.t and load it back

  Dec 2nd - add tests and make sure everything works 

  Until Dec 5th - check in with TA see if there is anything else we need to fix or finalize
