
Pixel Portfolio
Tayseer Karrossi, Byron S. Mitchell Rivera  

Pixel Portfolio is a command-line stock-simulation game that models a small universe of assets whose prices evolve through realistic stochastic processes, including Geometric Brownian Motion and Ornstein–Uhlenbeck mean reversion. The purpose of the project is to create a functional, fully deterministic market engine that supports market and stop loss orders, equity tracking, leveling, and persistent save/load functionality. We are intensely interested in the stock market, day trading, and the financial world, which motivated us to build a simulator that reflects real trading dynamics rather than a trivial game. This project allows us to combine financial modeling with OCaml’s functional abstractions while producing a reusable stochastic-simulation library as required in FPSE. It will mainly be used to test our knowledge of the stock market, as we would like to understand what actually drives it. Tayseer has minimal experience in the stock market during the COVID quarantine, and I would like to start trading stocks.  

Current libraries:
  - Sexplib
  - Bisect_ppx
  - Cmdliner 
  - ppx_jane
  - Core
  - OUnit2
  - QCheck

The project provides a functional, fully deterministic market engine with:
- Interactive CLI commands via Cmdliner
- Market and stop-loss order types
- Real-time portfolio tracking with long and short positions
- Player leveling system based on equity growth
- Persistent game state using S-expression

Run Game:

dune build
dune exec pixel_portfolio

On first launch, you'll be prompted to:
1. Choose difficulty (easy/medium/hard) - affects starting cash
2. View the help menu with all available commands

Game Commands: 

- `portfolio [state]` - View cash, equity, positions, and open orders
- `simulate [steps] [state]` - Advance the market by N steps (default: 1, max: 160)
- `buy <ticker> <qty> [state]` - Market buy order
- `sell <ticker> <qty> [price] [state]` - Market sell (no price) or stop-loss sell (with price)
- `cancel <id> [state]` - Cancel an open order by ID
- `new [easy|medium|hard] [state]` - Start a new game (overwrites current)
- `save <file> [state]` - Save current state to a file
- `load <file> [state]` - Load a saved game
- `help` - Display command list
- `exit` - Quit the game

Running Tests :

dune test

Run Coverage:

BISECT_ENABLE=YES BISECT_FILE=$(pwd)/_build/default/bisect \
  dune runtest --instrument-with bisect_ppx --force

bisect-ppx-report html -o _coverage _build/default/bisect*.coverage

open _coverage/index.html

Known Limitations

- No options trading 
- Uncorrelated assets - Each ticker's noise is independent (struggled to test these so we did not implement)

