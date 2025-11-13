# pixel-portfolio
Repository for FPSE Final Project, Group Members: Tayseer Karrossi, Byron S. Mitchell Rivera

Current libraries:
  - Owl (works on both pcs)
  - Yojson (we might not need this bc bisect_ppx has S-expression but we will see - from TA)
  - Sexplib
  - Bisect_ppx
  - Cmdliner (works on both pcs)
  - ppx_jane

  command examples: 
dune exec pixel_portfolio -- buy AAPL --qty 10
dune exec pixel_portfolio -- buy TSLA --qty 5 --price 200.50
dune exec pixel_portfolio -- buy GOOGL --qty 1 --price 120.00
dune exec pixel_portfolio -- sell AAPL --qty 3
dune exec pixel_portfolio -- sell MSFT --qty 2 --price 350.00
dune exec pixel_portfolio -- portfolio
dune exec pixel_portfolio -- tick
dune exec pixel_portfolio -- simulate --steps 10
dune exec pixel_portfolio -- simulate --steps 25
dune exec pixel_portfolio -- save --file game.sexp
dune exec pixel_portfolio -- load --file game.sexp

plan for completion:

  By November 16th - make sure .mli files are good and no more major design changes will occur (after TA feedback)

  By nov 19th - make sure .ml files are good and no major changes

  By nov 21st - portfolio updates and equity calculations

  By november 23rd - submitting a market order will produce an Order.execution and update the portfolio and cash

  Nov 26th - tick moves prices using simple noise and whichever model (GBM will be done first, then OU)

  Nov 28th - CLI command will simulate N amount og steps and show final prices and equity. 

  Nov 30th - Save an Engine.t and load it back

  Dec 2nd - add tests and make sure everything works 

  Until Dec 5th - check in with TA see if there is anything else we need to fix or finalize

Important to note: 

  At first we were doing well but when Byron tried to pull Tayseer’s part from the repo, it said to rebase. Byron rebased and basically deleted most of his code. In the end we decided to start again because we ruined the project.

    We started the project again, making two different branches plus the main one. We got scared so everything Byron pushed Tayseer just copy pasted from the repo instead of pulling. At the end we ended up working on Tayseer’s laptop, just to make sure we did not ruin it again!

Overview of the project:

Pixel Portfolio is a command-line stock-simulation game that models a small universe of assets whose prices evolve through realistic stochastic processes, including Geometric Brownian Motion, Ornstein–Uhlenbeck mean reversion, correlated movement, and optional volatility-switches. The purpose of the project is to create a functional, fully deterministic market engine that supports market and limit orders, transaction fees, equity tracking, leveling, and persistent save/load functionality. We are intensely interested in the stock market, day trading, and the financial world, which motivated us to build a simulator that reflects real trading dynamics rather than a trivial game. This project allows us to combine financial modeling with OCaml’s functional abstractions while producing a reusable stochastic-simulation library as required in FPSE. It will mainly be used to test our knowledge of the stock market, as we would like to understand what actually drives it. Tayseer has minimal experience in the stock market during the COVID quarantine, and I would like to start trading stocks.  


