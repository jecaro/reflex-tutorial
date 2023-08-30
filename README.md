# reflex-tutorial

This repository is a playground for me to learn FRP in Haskell with [reflex]. 
It is based on this [tutorial].

Additionally, it also showcase:

- how to make [haskell-language-server] available in `ob shell`: see 
  [default.nix](./default.nix)
- how to integrate and use [tailwindcss] with [reflex] (based on the repository 
  [obelisk-tailwind-example])
- how to do some [basic routing](./frontend/src/Frontend.hs) from the frontend
- how to [call Javascript from Haskell](./frontend/src/Javascript.hs)
- how to access the [local storage](./frontend/src/LocalStorage.hs) from 
  Haskell
- a simple [Tic Tac Toe game](./frontend/src/TicTacToe.hs)

# Getting started

Open a shell with the [obelisk] available:

```bash
$ ./shell-obelisk.sh
```

Run the web app with instant recompilation when any file is changed:

```bash
$ ob run
```

Then open http://localhost:8000 in your browser to see the app in action.

Create the android application:

```bash
$ nix-build -A android.frontend -o result-android
```

Plug your android phone and install the application (needs USB debugging):

```bash
$ ./result-android/bin/deploy
```

[haskell-language-server]: https://github.com/haskell/haskell-language-server
[obelisk-tailwind-example]: https://github.com/obsidiansystems/obelisk-tailwind-example
[obelisk]: https://github.com/obsidiansystems/obelisk
[reflex]: https://reflex-frp.org/
[tailwindcss]: https://tailwindcss.com/
[tutorial]: https://reflex-frp.org/tutorial

