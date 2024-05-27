# Malaborio 

Juggling challenges game. 

This game was made using Guile Hoot and the handy game jam template:

https://gitlab.com/spritely/guile-hoot-game-jam-template

Some of the instructions for running the game are reproduced here.

## Tutorial

The fastest way to get everything you need is to use [GNU
Guix](https://guix.gnu.org), a wonderful package manager written in
Scheme.

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

To build the game, run:

```
make
```

To launch a development web server, run:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your
web browser.  We recommend using Mozilla Firefox or Google Chrome.
Hoot is not supported on Safari at this time.
