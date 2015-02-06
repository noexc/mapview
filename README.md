# MapView

MapView is a system for plotting a stream of coordinates on a map, based on
telemetry received from high altitude ballon launches.

It is written and maintained by Ricky Elrod originally for the University of
Akron Amateur Radio Club (W8UPD), and released under the MIT license. It is
now maintained under the name of the Northeast Ohio Experimenter's Club.

We use the system in conjunction with RTTY telemetry, but it is easy to modify
the system to work with other forms of telemetry - anything minimodem can
support. In the case that minimodem won't do the mode that you need, just
find a modem for your mode that can output received data as a stream to STDOUT,
edit the configuration file and make appropriate changes, and the system should
continue to work.

# How it works

MapView is a set of Haskell programs that work together to get data to clients
through the use of websockets, and client side code to view this data. The way
it works is like this:

[a receiver radio] -> [mapview-telemetryparser] -> [mapview-send] -> [clients]

It depends on `minimodem` (by default) and calls out to it using the Shelly
library for Haskell. On each successfully received line,
`mapview-telemetryparser` will output a JSON file.

In `mapview-send`, we use the fsnotify Haskell library to determine when the
JSON file changes. When a change is detected, we push out new coordinates to
clients via websockets. The client updates its Google Map and displays the new
location.

Because it uses the Google Maps API, clients must have an active internet
connection for this to work.

# The three parts of MapView

MapView is broken into three parts: the parser, the websocket broadcaster, and
the client side code that listens for websocket broadcasts with new telemetry
data.

## `mapview-telemetryparser`

`mapview-telemetryparser` is the first step of the system. It shells out to
`minimodem` (by default) by using the awesome Shelly.hs library. Every time a
newline is sent to it, it will attempt to parse the line. If it does so
successfully, it will convert the data it receives to JSON and save it to a
file.

Future revisions of `mapview-telemetryparser` could send it to some kind of
public message bus instead, along with other telemetry data, so that clients
could consume more data and work with it as they please.

This is all that `mapview-telemetryparser` does: parse, convert to
machine-readable, save.

## `mapview-send`

`mapview-send` listens for changes to the file that `mapview-telemetryparser`
writes to. (Future revisions of it could consume a public message bus and pass
data along from that source). It also allows clients to connect to it via
websockets. When it detects a change to the file it monitors, it passes the new
contents of the file to all clients who are connected (i.e., the clients receive
a new block of JSON).

## `mapview-generate-charts`

`mapview-generate-charts` exists for quickly re-processing telemetry data and
generating charts of parses which were deemed by Trifecta to be a success.
Examples of these charts can be found on the
[NBP-1 page](https://noexc.org/wiki/NBP1#Received_Data).

Originally written for summarizing data after a launch, this tool can also be
used in the field to visualize what is happening to the balloon.

## mapview-psc

mapview-psc is documented in its
[own repository](https://github.com/noexc/mapview-psc). It is a static site with
the unique property that the javascript code that interacts with the above parts
of MapView are written in [PureScript](http://docs.purescript.org/). This allows
us to better reason about the code and gives us more confidence that the code
does what it should, since we can rely on a type system to keep us in check.

# Getting set up (Fedora)
* `yum install minimodem ghc cabal-install`
* `cabal update && cabal install cabal-install`
* `echo 'export PATH=~/.cabal/bin:$PATH' >> ~/.bashrc` # be able to access the new cabal version in the future.
* `export PATH=~/.cabal/bin:$PATH'` # be able to access it right away, too
* `git clone git://github.com/noexc/mapview.git && cd mapview`
* `cabal sandbox init`
* `cabal install`

# Running it
* `.cabal-sandbox/bin/telemetryparser`
* In another terminal, `.cabal-sandbox/bin/mapview-send`
* Follow directions in the [mapview-psc repository](https://github.com/noexc/mapview-psc)
* Then wait.

# Telemetry Messages

The parser is currently hardcoded. Future work can and should go into making
it be configurable (by having a Haskell file effectively act as a configuration
file, similar to xmonad).

Telemetry messages should currently be in the form of
`:callsign:longitude:latitude:altitude:time:`.
Other fields can be included, but they aren't currently used.

Any line not in this format will be dropped, so e.g. training sequences are fine
so long as they appear on their own line.

**All messages must end with \n in order to be parsed.** This is how we know
to start a new parse.

# License

Everything in this repository is released under the MIT license, but keep in
mind that minimodem is GPL3+.
