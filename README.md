# MapView

A system for plotting a stream of coordinates on a map.
In our usecase, that stream of coordinates comes from RTTY telemetry.

# How it works

This is a simple set of Haskell programs which allow for plotting a set of
coordinate data received from minimodem via RTTY telemetry from the
[HABP-2](https://wiki.w8upd.org/wiki/Planning/UA-HABP2) balloon project.

It depends on `minimodem` and calls out to it using the Shelly library for
Haskell. On each successfully received line, `rttyparser` will output a JSON
file.

In `mapupdater`, we use the fsnotify Haskell library to determine when the JSON
file changes. When a change is detected, we push out new coordinates to clients
via websockets. The client updates its Google Map and displays the new location.

Because it uses the Google Maps API, clients must have an active internet
connection for this to work.

# Using it

* `yum install minimodem haskell-platform`
* `cabal update && cabal install cabal-install`
* Clone the repository
* `cabal sandbox init`
* `cabal install`
* `.cabal-sandbox/bin/mapupdater`
* Go to http://localhost:8088/ in a browser
* In another terminal, `.cabal-sandbox/bin/rttyparser`
* Then wait.

# RTTY Messages

RTTY messages should be in the form of
`callsign:longitude:latitude:altitude:time`.
Other fields can be included, but they aren't currently used.

**All messages must end with \n in order to be parsed.** This is how we know
to start a new parse.

# License

Everything in this repository is released under the MIT license, but keep in
mind that minimodem is GPL3+.