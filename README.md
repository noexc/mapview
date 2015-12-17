<p align="center">
  <img src="https://raw.github.com/noexc/mapview/v3/marketing/logo.png" />
</p>

MapView is a **Haskell library** for building GPS-based tracking systems, based
on telemetry downlinks (usually via RF, and typically done via amateur radio).

The primary usecase is for *high-altitude balloon tracking*.

Given any kind of downlink which includes GPS coordinates (and other optional
information), the goal of MapView is to make it easy to write programs to
process this data. MapView itself contians a "standard library" of callback
functions which can be called at any point where data is to be processed (for
example, before it is parsed, after it is parsed, or after other callbacks are
run).

We provide a set of libraries and callback functions, you use them as you
choose. Your "configuration file" is actually the program that does the work, in
a vein similar to how Xmonad works.

What you do with the data is up to you, though we also provide (in other
repositories) some standard extensions to this core library. For example, we
have a library ([mapview-websocket](https://github.com/noexc/mapview-websocket))
used for sending parsed telemetry packets to websocket clients in a structured
way, useful for live-plotting the latest location on a browser-based map.

In [mapview-noexc](https://github.com/noexc/mapview-noexc), we provide full
configurations (programs) that we (the Northeast Ohio Experimenter's Club) have
used in production, along with a set of callbacks specific to our flights. These
can be used by you to inspire the creation of new callback functions, or pulled
directly from our repository, as you see fit.

MapView is written and maintained by Ricky Elrod and NOEXC originally for the
University of Akron Amateur Radio Club (W8UPD), and released under the **MIT
license**. It is now maintained under the name of the Northeast Ohio
Experimenter's Club.

We use the system in conjunction with RTTY telemetry, but it is easy to modify
the system to work with other forms of telemetry.
